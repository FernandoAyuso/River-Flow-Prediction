#DATA PREPARATION WITH FEATURE GENERATION
#install.packages("RSQLite")
library(RSQLite)

#Example of parameters to use
#intervalsPastRain <- c(0, 36, 72, 108, 114, 120, 126, 132, 138, 144, 150, 156, 162, 168, 174, 180, 186, 192, 198, 204, 210, 216, 252, 288, 324, 360, 396, 432, 468, 504, 1000)
#intervalsPastCMS <- c(36, 72, 108, 144, 180, 216, 252, 288, 324, 360, 396, 432, 468, 504, 1000)
#intervalsPastTemp <- c(156, 300, 504)
#intervalsFutuRain <- c( 36, 72, 108, 144, 180, 215, 252, 288)
#intervalsFutuCMS  <- c( 72, 144, 215, 288)
#intervalsFutuTemp <- c( 72, 144, 215, 288)

#######################################################################################################################################
# Function that receiving a dataframe and the parameters of the past and future Rain, CMS and tem will return a dataset
# with the parameters generated and will save the dataframe as csv with the name indicated.
# Parameters
#  dfTestSubset: the dataframe to work on
#  intervalsPastRain: past rain parameters to generate bbased on the desinged ABT
#  intervalsPastCMS: past flows to bring to the ABT
#  intervalsPastTemp: past temperatures to bring
#  intervalsFutuRain: to be used as forecast weather
#  intervalsFutuCMS: to be used as Target variables
#  intervalsFutuTemp: to be used as forecast weather as well
#  saveFileName
# Output
#  original dataframe modified with indicated intervals to be used as ABT
#######################################################################################################################################
featureCreator <- function(dfTestSubset, intervalsPastRain, intervalsPastCMS, intervalsPastTemp, intervalsFutuRain, intervalsFutuCMS, intervalsFutuTemp,  saveFileName){
  print("FeatureCreator. Start, intervalsPastRain, Past")
  print(Sys.time)
  print(nrow(dfTestSubset))
  dfTestSubset <- addDataIntervals(dfTestSubset,'Rainmm',intervalsPastRain, TRUE, "Past")
  head(dfTestSubset,120)
  print(nrow(dfTestSubset))
  print("FeatureCreator. dfTestSubset,'FlowCMS',intervalsPastCMS, FALSE, Past")
  dfTestSubset <- addDataIntervals(dfTestSubset,'FlowCMS',intervalsPastCMS, FALSE, "Past")
  head(dfTestSubset)
  print(nrow(dfTestSubset))
  print("featureCreator. data,'Temp',intervalsPastTemp, TRUE, Past")
  dfTestSubset <- addDataIntervals(dfTestSubset,'Temp',intervalsPastTemp, FALSE, "Past")
  head(dfTestSubset)
  print("featureCreator. dfTestSubset,'Rainmm',intervalsFutuRain, TRUE, Future")
  dfTestSubset <- addDataIntervals(dfTestSubset,'Rainmm',intervalsFutuRain, TRUE, "Future")
  head(dfTestSubset)
  print("featureCreator. dfTestSubset,'FlowCMS',intervalsFutuCMS, FALSE, Future")
  dfTestSubset <- addDataIntervals(dfTestSubset,'FlowCMS',intervalsFutuCMS, FALSE, "Future")
  head(dfTestSubset)
  print("featureCreator. dfTestSubset,'Rainmm',intervalsFutuTemp, False, Future")
  dfTestSubset <- addDataIntervals(dfTestSubset,'Temp',intervalsFutuTemp, FALSE, "Future")
  head(dfTestSubset)
  dfTestSubset <- na.omit(dfTestSubset) #Remove lines with NAs
  
  fileName <- paste(saveFileName, " ", as.character(Sys.time(),".csv"))
  write.csv(dfTestSubset, file = fileName)
  print(Sys.time)
  print("featureCreator. Completed")
  return (dfTestSubset)
}


#######################################################################################################################################
# Function to add intervals based on a column provided and a number of columns.
# parameters
#  data: the dataframe to work on
#  colToRead: the data to use to create intervals
#  intervalsToAdd: intervals to create or to bring data from
#  Cummulative will indicate if the data of the inteval has to be summed up, like in rain. CMS and Temp do not noed this sum
#  past Param, indicating if the intervals have to be applied for the past or for the future(provide weather forecst or target variables)
# Output
#  original dataframe modified with the intervals sent in intervalsToAdd
#######################################################################################################################################
addDataIntervals <- function(data, colToRead, intervalsToAdd, Cummulative, pastParam){
  #print(paste("Startubg addDataIntervals with : intervalsToadd", intervalsToAdd, " Cummulative", Cummulative  , " Past: ", pastParam))
  x<-length(intervalsToAdd) 
  if(Cummulative==TRUE){x<-x-1}
  for(i in 1:x ){ 
    from <- intervalsToAdd[i]
    to <- intervalsToAdd[i+1] 
    #print(paste("From and to", from, "    to:", to))
    if (Cummulative==TRUE){
      tempWidth <- to-from
      newColName <- paste(colToRead,"x",from, "x", to -1, "C",sep='')  #print(paste("width ", tempWidth, "from", from  , " To ", to-1))
    }else{
      tempWidth <- 1
      newColName <- paste(colToRead,"x",from, "S",sep='')  #print(paste("width ", tempWidth, "from", from  , " To ", to-1))
    }
    
    newColData <- rollapply(data[,colToRead], width=tempWidth, sum, by=1)
    
    data <- head(data, length(newColData))
    if (pastParam == "Past" ){
      if(Cummulative==TRUE){
        shiftInt <- -to
      }else{
        shiftInt <- -from
        }
      newColName <- paste("Past", newColName, sep="")
    } else{
      shiftInt <- from
      newColName <- paste("Future", newColName, sep="")
    }
    data[newColName] <- NA
    #print(newColName)
    #print (nrow(data))
    #print (length(shift(newColData, shiftInt)))
    data[,ncol(data)] <- shift(newColData, shiftInt)
    names(newColData)[1] <- newColName
  }
  head(data)
  return(data)
}


#######################################################################################################################################
# Function simply get a colun and swift it several rows up or down. Used for CMS and Temp
# parameters
#  x: column to shif
#  shift_by: number of rows to move it, can be possitive or negative to deal with past and future
# Output
#  the table indicated to be shifted by the nuber indicated in shift_by.
#  
#  Main soure of this function: #https://www.r-bloggers.com/generating-a-laglead-variables/
#######################################################################################################################################

shift<-function(x, shift_by){
  stopifnot(is.numeric(shift_by))
  stopifnot(is.numeric(x))
  if (length(shift_by)>1)
    return(sapply(shift_by,shift, x=x))
  out<-NULL
  abs_shift_by=abs(shift_by)
  if (shift_by > 0 )
    out<-c(tail(x,-abs_shift_by),rep(NA,abs_shift_by))
  else if (shift_by < 0 )
    out<-c(rep(NA,abs_shift_by), head(x,-abs_shift_by))
  else
    out<-x
  out
}


#######################################################################################################################################
# Function that records experiment into sql lite and into CSV file
# parameters
#  modelType: model executed KNN, SVM..
#  modelParameters: paremeters used
#  caretDesc: caret full descripton
#  modelFileName: name of the model saved as R file
#  saveFileName: name of data file used for experiment
#  targeTest: test target variable
#  targetPredict: test predicted variable
#  NumRows: number of rows in experiment
#  executionTime: time taken by model to exectute
#  RMSEparamVal: Validation RMSE
#  R2paramVal: Validation R2
#  RMSEparamTest: Test RMSE
#  R2paramTest: Test R2
#  dbnameToUse: Name of the SQLite database to save the info
# Output
#  line in the SQLite databes and new row in the ExperimentResults.csv file
#  
#  Main soure of this function: #https://www.r-bloggers.com/generating-a-laglead-variables/
#######################################################################################################################################

recordExperiment <- function(modelType, modelParameters, caretDesc, modelFileName, saveFileName, targeTest, targetPredict, NumRows, executionTime, RMSEparamVal, R2paramVal, RMSEparamTest, R2paramTest, dbnameToUse){
  db = dbConnect(SQLite(), dbname=dbnameToUse)
  insertString <-paste("INSERT INTO Experiment(Date, modelType, modelParameters, caretDesc, modelFileName, saveFileName, TargetTest, TargetPredict, numRows, executionTime, RMSEVal, R2Val, RMSETest, R2Test) 
                       VALUES('", as.character(Sys.time()),"', '",modelType,"','",modelParameters,"','",caretDesc,"','",modelFileName,"','",saveFileName,"','",paste(targeTest,collapse=","),"','",paste(targetPredict,collapse=","),"','",NumRows,"','",executionTime,"','",RMSEparamVal,"','",R2paramVal,"','",RMSEparamTest,"','",R2paramTest,"')")
  dbSendQuery(conn = db, insertString)
  tempDataFrame <- dbGetQuery(db, "select * from Experiment")
  write.csv(tempDataFrame, file = "ExperimentResults.csv")
}






