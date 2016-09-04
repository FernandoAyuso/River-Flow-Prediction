### FILE CONTAINING ALL THE CODE USED IN THE EXECUTION OF THE EXPERIMENTS 

#install.packages("dataQualityR")
#install.packages("sqldf")
library(dataQualityR)
library(sqldf)



#######################################################################################################################################
# Lag Analysis - Cross Correlation
# section to identify the maximun lag and to create the graph
#######################################################################################################################################

dfRiverTest <- read.csv(file = "A029_raw.csv")
head(dfRiverTest, 1000)
lagCr <- 500
(lagCr * 5)/60
Find_Max_CCF<- function(a,b, lagCr) { 
  d <- ccf(a, b, lag=lagCr, plot = FALSE) 
  cor = d$acf[,,1] 
  lag = d$lag[,,1] 
  res = data.frame(cor,lag) 
  res_max = res[which.max(res$cor),] 
  print(res$cor)
  return(res_max) 
}

maxccf <- Find_Max_CCF(dfRiverTest$FlowCMS,dfRiverTest$Rainmm, lagCr)
ccfResutl <- ccf(dfRiverTest$FlowCMS,dfRiverTest$Rainmm, lag.max=lagCr, plot=TRUE, main="Cross Correlaion lag: FlowCMS - Rainmm  ")
print (paste("Lag in number: ", maxccf))
print (paste("Lag in hours: ", (maxccf * 5)/60))
print (paste("Lag in days: ", ((maxccf * 5)/60)/24))
abline(h=ccfResutl$acf[501], col="blue")



#######################################################################################################################################
# Data Quality Report
#######################################################################################################################################
tempData <- read.csv(file = "A029_raw.csv")
checkDataQuality(tempData, "DELETEFILE")



#######################################################################################################################################
# ABT identification. Generation of teh 16 ABT and exectuion against every model
# It is highly recommended to review the documentation to fully understand this code.
#######################################################################################################################################

############### ############### ############### ############### ############### 
############### ABT1 - New standard

fileName <- "EXPERIMENT_ABT1.csv"
#numRows <- 10000
targetName <- "FutureFlowCMSx72S"
expData <- read.csv( fileName, sep=",", header=T)
expData$X <- NULL
names(expData)[names(expData)==targetName] <- "TARGET"  # NAME TARGET to the target variable.

expDataIndexes <- createDataPartition(expData$TARGET, p = .020)[[1]]
expDataB <- expData[ expDataIndexes, ]
inTrain <- createDataPartition(expDataB$TARGET, p = .91)[[1]]
expDataTrain <- expDataB[ inTrain, ]
expDataTest  <- expDataB[-inTrain, ]

############### ABT1  all in
expDataTrainABT1a <- expDataTrain # subset(expDataTrain, select = -c( ))
expDataTesABT1a <-expDataTest # subset(expDataTest, select = -c(  ))
modelKNNABT1a <-    executeKNN(expDataTrainABT1a, expDataTesABT1a, fileName) 
modelRF1ABT1a <-   executeRF(expDataTrainABT1a, expDataTesABT1a, 100, fileName)
modelSVMLABT1a <-   executeSVML(expDataTrainABT1a, expDataTesABT1a, fileName)

############### ABT1  - ff
expDataTrainABT1b <- subset(expDataTrainABT1a, select = -c(FutureTempx72S ))
expDataTesABT1b <-subset(expDataTesABT1a, select = -c(FutureTempx72S))
modelKNNABT1b <-    executeKNN(expDataTrainABT1b, expDataTesABT1b, fileName)
modelRF1ABT1b <-   executeRF(expDataTrainABT1b, expDataTesABT1b, 100, fileName)
modelSVMRABT1b <-    executeSVM(expDataTrainABT1b, expDataTesABT1b, fileName)

############### ABT1  - 1000
expDataTrainABT1c <- subset(expDataTrainABT1a, select = -c(PastRainmmx504x999C, PastFlowCMSx1000S ))
expDataTesABT1c <-subset(expDataTesABT1a, select = -c(PastRainmmx504x999C, PastFlowCMSx1000S))
modelKNNABT1c <-    executeKNN(expDataTrainABT1c, expDataTesABT1c, fileName)
modelRF1ABT1c <-   executeRF(expDataTrainABT1b, expDataTesABT1c, 100, fileName)
modelSVMABT1c <-    executeSVM(expDataTrainABT1c, expDataTesABT1c, fileName)

############### ABT1 - no ff no 1000 interval
expDataTrainABT1c <- subset(expDataTrainABT1a, select = -c(FutureTempx72S, FutureRainmmx36x71C, FutureRainmmx72x107C, PastRainmmx504x999C, PastFlowCMSx1000S   ))
expDataTesABT1c <-subset(expDataTesABT1a, select = -c(FutureTempx72S, FutureRainmmx36x71C, FutureRainmmx72x107C ,PastRainmmx504x999C, PastFlowCMSx1000S))
modelKNNABT1d <-    executeKNN(expDataTrainABT1c, expDataTesABT1c, fileName)
modelRF1ABT1d <-   executeRF(expDataTrainABT1c, expDataTesABT1c, 100, fileName)
modelSVMABT1d <-    executeSVM(expDataTrainABT1c, expDataTesABT1c, fileName)


############### ############### ############### ############### ############### 
############### ABT2
fileName <- "EXPERIMENT_ABT2.csv"
#numRows <- 10000
targetName <- "FutureFlowCMSx72S"
expData <- read.csv( fileName, sep=",", header=T)
expData$X <- NULL
names(expData)[names(expData)==targetName] <- "TARGET"  # NAME TARGET to the target variable.
nrow(expData)
expDataIndexes <- createDataPartition(expData$TARGET, p = .020)[[1]]
expDataB <- expData[ expDataIndexes, ]
inTrain <- createDataPartition(expDataB$TARGET, p = .91)[[1]]
expDataTrain <- expDataB[ inTrain, ]
expDataTest  <- expDataB[-inTrain, ]

Sys.time()
expDataTrainABT2a <- expDataTrain # subset(expDataTrain, select = -c( ))
expDataTesABT2a <-expDataTest # subset(expDataTest, select = -c(  ))
names(expDataTrainABT2a)
names(expDataTesABT2a)
modelKNNABT2a <-    executeKNN(expDataTrainABT2a, expDataTesABT2a, fileName) 
modelRF1ABT2a <-   executeRF(expDataTrainABT2a, expDataTesABT2a, 100, fileName)
modelSVMRABT2a <-   executeSVM(expDataTrainABT2a, expDataTesABT2a, fileName)

############### ABT2  - ff
expDataTrainABT2b <- subset(expDataTrainABT2a, select = -c(FutureTempx72S ))
expDataTesABT2b <-subset(expDataTesABT2a, select = -c(FutureTempx72S))
modelKNNABT2b <-    executeKNN(expDataTrainABT2b, expDataTesABT2b, fileName)
modelRF1ABT2b <-   executeRF(expDataTrainABT2b, expDataTesABT2b, 100, fileName)
modelSVMRABT2b <-    executeSVM(expDataTrainABT2b, expDataTesABT2b, fileName)

############### ABT2  - 1000
expDataTrainABT2c <- subset(expDataTrainABT2a, select = -c(PastRainmmx504x999C, PastFlowCMSx1000S ))
expDataTesABT2c <-subset(expDataTesABT2a, select = -c(PastRainmmx504x999C, PastFlowCMSx1000S))
modelKNNABT2c <-    executeKNN(expDataTrainABT2c, expDataTesABT2c, fileName)
modelRF1ABT2c <-   executeRF(expDataTrainABT2b, expDataTesABT2c, 100, fileName)
modelSVMRABT2c <-    executeSVM(expDataTrainABT2c, expDataTesABT2c, fileName)

############### ABT2 - no ff no 1000 interval
expDataTrainABT2c <- subset(expDataTrainABT2a, select = -c(FutureTempx72S, FutureRainmmx36x71C, FutureRainmmx72x107C, PastRainmmx504x999C, PastFlowCMSx1000S   ))
expDataTesABT2c <-subset(expDataTesABT2a, select = -c(FutureTempx72S, FutureRainmmx36x71C, FutureRainmmx72x107C ,PastRainmmx504x999C, PastFlowCMSx1000S))
modelKNNABT2d <-    executeKNN(expDataTrainABT2c, expDataTesABT2c, fileName)
modelRF1ABT2d <-   executeRF(expDataTrainABT2c, expDataTesABT2c, 100, fileName)
modelSVMRABT2d <-    executeSVM(expDataTrainABT2c, expDataTesABT2c, fileName)


############### ############### ############### ############### ############### 
############### ABT3 Generation
fileName <- "EXPERIMENT_GUM5.csv"
#numRows <- 10000
targetName <- "FutureFlowCMSx72S"
expData <- read.csv( fileName, sep=",", header=T)
expData$X <- NULL
names(expData)[names(expData)==targetName] <- "TARGET"  # NAME TARGET to the target variable.
nrow(expData)
expDataIndexes <- createDataPartition(expData$TARGET, p = .020)[[1]]
expDataB <- expData[ expDataIndexes, ]
inTrain <- createDataPartition(expDataB$TARGET, p = .91)[[1]]
#nrowPartnum <- round(nrow(expData)*0.9, digits = 0)
expDataTrain <- expDataB[ inTrain, ]
expDataTest  <- expDataB[-inTrain, ]

Sys.time()
expDataTrainABT3a <- expDataTrain # subset(expDataTrain, select = -c( ))
expDataTesABT3a <-expDataTest # subset(expDataTest, select = -c(  ))
modelKNNGUM5a <-    executeKNN(expDataTrainABT3a, expDataTesABT3a, fileName) 
modelRF1GUM5a <-   executeRF(expDataTrainABT3a, expDataTesABT3a, 100, fileName)
modelSVMRGUM5a <-   executeSVM(expDataTrainABT3a, expDataTesABT3a, fileName)

############### ABT3  - ff
expDataTrainABT3b <- subset(expDataTrainABT3a, select = -c(FutureTempx72S ))
expDataTesABT3b <-subset(expDataTesABT3a, select = -c(FutureTempx72S))
modelKNNGUM5b <-    executeKNN(expDataTrainABT3b, expDataTesABT3b, fileName)
modelRF1GUM5b <-   executeRF(expDataTrainABT3b, expDataTesABT3b, 100, fileName)
modelSVMRGUM5b <-    executeSVM(expDataTrainABT3b, expDataTesABT3b, fileName)

############### ABT3  - 1000
expDataTrainABT3c <- subset(expDataTrainABT3a, select = -c(PastRainmmx504x999C, PastFlowCMSx1000S ))
expDataTesABT3c <-subset(expDataTesABT3a, select = -c(PastRainmmx504x999C, PastFlowCMSx1000S))
modelKNNGUM5c <-    executeKNN(expDataTrainABT3c, expDataTesABT3c, fileName)
modelRF1GUM5c <-   executeRF(expDataTrainABT3b, expDataTesABT3c, 100, fileName)
modelSVMRGUM5c <-    executeSVM(expDataTrainABT3c, expDataTesABT3c, fileName)

############### ABT3 - no ff no 1000 interval
expDataTrainABT3c <- subset(expDataTrainABT3a, select = -c(FutureTempx72S, FutureRainmmx36x71C, FutureRainmmx72x107C, PastRainmmx504x999C, PastFlowCMSx1000S   ))
expDataTesABT3c <-subset(expDataTesABT3a, select = -c(FutureTempx72S, FutureRainmmx36x71C, FutureRainmmx72x107C ,PastRainmmx504x999C, PastFlowCMSx1000S))
modelKNNGUM5d <-    executeKNN(expDataTrainABT3c, expDataTesABT3c, fileName)
modelRF1GUM5d <-   executeRF(expDataTrainABT3c, expDataTesABT3c, 100, fileName)
modelSVMRGUM5d <-    executeSVM(expDataTrainABT3c, expDataTesABT3c, fileName)


############### ############### ############### ############### ############### 
############### ABT4 Generation
fileName <- "EXPERIMENT_GUM5.csv"
#numRows <- 10000
targetName <- "FutureFlowCMSx72S"
expData <- read.csv( fileName, sep=",", header=T)
expData$X <- NULL
names(expData)[names(expData)==targetName] <- "TARGET"  # NAME TARGET to the target variable.
expDataIndexes <- createDataPartition(expData$TARGET, p = .020)[[1]]
expDataB <- expData[ expDataIndexes, ]
inTrain <- createDataPartition(expDataB$TARGET, p = .91)[[1]]
#nrowPartnum <- round(nrow(expData)*0.9, digits = 0)
expDataTrain <- expDataB[ inTrain, ]
expDataTest  <- expDataB[-inTrain, ]

Sys.time()
expDataTrainABT4a <- expDataTrain # subset(expDataTrain, select = -c( ))
expDataTesABT4a <-expDataTest # subset(expDataTest, select = -c(  ))
modelKNNABT4a <-    executeKNN(expDataTrainABT4a, expDataTesABT4a, fileName) 
modelRF1ABT4a <-   executeRF(expDataTrainABT4a, expDataTesABT4a, 100, fileName)
modelSVMRABT4a <-   executeSVM(expDataTrainABT4a, expDataTesABT4a, fileName)

############### ABT4  - ff
expDataTrainABT4b <- subset(expDataTrainABT4a, select = -c(FutureTempx72S ))
expDataTesABT4b <-subset(expDataTesABT4a, select = -c(FutureTempx72S))

modelKNNABT4b <-    executeKNN(expDataTrainABT4b, expDataTesABT4b, fileName)
modelRF1ABT4b <-   executeRF(expDataTrainABT4b, expDataTesABT4b, 100, fileName)
modelSVMRABT4b <-    executeSVM(expDataTrainABT4b, expDataTesABT4b, fileName)

############### ABT4  - 1000
expDataTrainABT4c <- subset(expDataTrainABT4a, select = -c(PastRainmmx504x999C, PastFlowCMSx1000S ))
expDataTesABT4c <-subset(expDataTesABT4a, select = -c(PastRainmmx504x999C, PastFlowCMSx1000S))
modelKNNABT4c <-    executeKNN(expDataTrainABT4c, expDataTesABT4c, fileName)
modelRF1ABT4c <-   executeRF(expDataTrainABT4b, expDataTesABT4c, 100, fileName)
modelSVMRABT4c <-    executeSVM(expDataTrainABT4c, expDataTesABT4c, fileName)

############### ABT4 - no ff no 1000 interval
expDataTrainABT4c <- subset(expDataTrainABT4a, select = -c(FutureTempx72S, FutureRainmmx36x71C, FutureRainmmx72x107C, PastRainmmx504x999C, PastFlowCMSx1000S   ))
expDataTesABT4c <-subset(expDataTesABT4a, select = -c(FutureTempx72S, FutureRainmmx36x71C, FutureRainmmx72x107C ,PastRainmmx504x999C, PastFlowCMSx1000S))
modelKNNABT4d <-    executeKNN(expDataTrainABT4c, expDataTesABT4c, fileName)
modelRF1ABT4d <-   executeRF(expDataTrainABT4c, expDataTesABT4c, 100, fileName)
modelSVMRABT4d <-    executeSVM(expDataTrainABT4c, expDataTesABT4c, fileName)

Sys.time()

############### Visualise individual models
plot(modelKNNABT1a, main="modelKNNABT1a")


############### ABT Results. generate comparable results.

resultsABTs <- resamples(list(KKN1a=modelKNNABT1a, RF1a=modelRF1ABT1a, SVM1a=modelSVMLABT1a, 
                              KKN1b=modelKNNABT1b, RF1b=modelRF1ABT1b, SVM1b=modelSVMRABT1b, 
                              KKN1c=modelKNNABT1c, RF1c=modelRF1ABT1c, SVM1c=modelSVMABT1c, 
                              KKN1d=modelKNNABT1d, RF1d=modelRF1ABT1d, SVM1d=modelSVMABT1d, 
                              KKN2a=modelKNNABT2a, RF2a=modelRF1ABT2a, SVM2a=modelSVMRABT2a, 
                              KKN2b=modelKNNABT2b, RF2b=modelRF1ABT2b, SVM2b=modelSVMRABT2b, 
                              KKN2c=modelKNNABT2c, RF2c=modelRF1ABT2c, SVM2c=modelSVMRABT2c, 
                              KKN2d=modelKNNABT2d, RF2d=modelRF1ABT2d, SVM2d=modelSVMRABT2d, 
                              KKN3a=modelKNNGUM5a, RF3a=modelRF1GUM5a, SVM3a=modelSVMRGUM5a, 
                              KKN3b=modelKNNGUM5b, RF3b=modelRF1GUM5b, SVM3b=modelSVMRGUM5b, 
                              KKN3c=modelKNNGUM5c, RF3c=modelRF1GUM5c, SVM3c=modelSVMRGUM5c, 
                              KKN3d=modelKNNGUM5d, RF3d=modelRF1GUM5d, SVM3d=modelSVMRGUM5d, 
                              KKN4a=modelKNNABT4a, RF4a=modelRF1ABT4a, SVM4a=modelSVMRABT4a, 
                              KKN4b=modelKNNABT4b, RF4b=modelRF1ABT4b, SVM4b=modelSVMRABT4b, 
                              KKN4c=modelKNNABT4c, RF4c=modelRF1ABT4c, SVM4c=modelSVMRABT4c, 
                              KKN4d=modelKNNABT4d, RF4d=modelRF1ABT4d, SVM4d=modelSVMRABT4d))
# Visualise
summary(resultsABTs)
bwplot(resultsABTs,main="Basic Models/ABT comparison - 12h Lead - 4ABT/4Ops each")





#######################################################################################################################################
# Execute model iterations for the experiments
#######################################################################################################################################
# After each execution the parametres are changed in the model implementation.
modelKNN_Base <-    executeKNN(expDataTrain, expDataTest, fileName)
modelKNN_Ini <-    executeKNN(expDataTrain, expDataTest, fileName)
modelKNN_Sec <-    executeKNN(expDataTrain, expDataTest, fileName)
plot(modelKNN_Base, main="modelKNN_Base")
plot(modelKNN_Ini, main="modelKNN_Ini")
plot(modelKNN_Sec, main="modelKNN_Sec")

modelKKNN_Ini <-    executeKNN(expDataTrain, expDataTest, fileName)
modelKKNN_Sec <-    executeKNN(expDataTrain, expDataTest, fileName)
modelKKNN_Thi <-    executeKNN(expDataTrain, expDataTest, fileName)
modelKKNN_Fou <-    executeKNN(expDataTrain, expDataTest, fileName)
plot(modelKKNN_Ini, main="modelKKNN_Ini")
plot(modelKKNN_Sec, main="modelKKNN_Sec")
plot(modelKKNN_Thi, main="modelKKNN_Thi")
plot(modelKKNN_Fou, main="modelKKNN_Fou")

modelSVMR_Ini <-  executeSVM(expDataTrain, expDataTest, fileName)
modelSVMRS_Ini <- executeSVMRS(expDataTrain, expDataTest, fileName)
modelSVMRC_Ini <- executeSVMRC(expDataTrain, expDataTest, fileName)
modelSVML_Ini <-  executeSVML(expDataTrain, expDataTest, fileName)
modelSVML2_Ini <- executeSVML2(expDataTrain, expDataTest, fileName)
modelSVML_Sec <-  executeSVML(expDataTrain, expDataTest, fileName)
plot(modelSVMR_Ini, main="modelSVMR_Ini")
plot(modelSVMRS_Ini, main="modelSVMRS_Ini")
plot(modelSVMRC_Ini, main="modelSVMRC_Ini")
plot(modelSVML_Ini, main="modelSVML_Ini")
plot(modelSVML2_Ini, main="modelSVML2_Ini")
plot(modelSVML_Sec, main="modelSVML_Sec")


modelRF100_Ini <-   executeRF(expDataTrain, expDataTest, 100, fileName)
modelRF500_Ini <-   executeRF(expDataTrain, expDataTest, 500, fileName)
modelRF100_Sec <-   executeRF(expDataTrain, expDataTest, 100, fileName)
modelRF500_Sec <-   executeRF(expDataTrain, expDataTest, 500, fileName)
modelRF750_Sec <-   executeRF(expDataTrain, expDataTest, 500, fileName)
modelRF1000_Sec <-  executeRF(expDataTrain, expDataTest, 1000, fileName)
modelRF1000_Thi <-   executeRF(expDataTrain, expDataTest, 500, fileName)
modelRF1000_Fou <-  executeRF(expDataTrain, expDataTest, 1000, fileName)# Executed in the next section as a bigger sample is used, here just for reference
plot(modelRF100_Ini, main="modelRF100_Ini")
plot(modelRF500_Ini, main="modelRF500_Ini")
plot(modelRF100_Sec, main="modelRF100_Sec")
plot(modelRF500_Sec, main="modelRF500_Sec")
plot(modelRF750_Sec, main="modelRF750_Sec")
plot(modelRF1000_Sec, main="modelRF1000_Sec")
plot(modelRF1000_Thi, main="modelRF1000_Thi")


resultsALLIterationsfinal <- resamples(list(modelKNN_Base=modelKNN_Base, modelKNN_Ini=modelKNN_Ini, modelKNN_Sec=modelKNN_Sec, 
                                            modelKKNN_Ini=modelKKNN_Ini, modelKKNN_Sec=modelKKNN_Sec, modelKKNN_Thi=modelKKNN_Thi, modelKKNN_Fou=modelKKNN_Fou, 
                                            modelRF100_Ini=modelRF100_Ini, modelRF500_Ini=modelRF500_Ini, modelRF100_Sec=modelRF100_Sec, modelRF500_Sec=modelRF500_Sec, 
                                            modelRF750_Sec=modelRF750_Sec, modelRF1000_Sec=modelRF1000_Sec, modelRF1000_Thi=modelRF1000_Thi, modelRF1000_Fou=modelRF1000_Fou))
                                            
summary(resultsALLIterationsfinal)
bwplot(resultsALLIterationsfinal,main="Flow forecast Model comparison")


# Note the experiments will have to be run as many iterations are needed. Different names can be generated to then compare the models.

#######################################################################################################################################
# 6 hour lead experiment based on best perfomrer with 50,000
#######################################################################################################################################

### Final EXECUTION 50.000 with 1000 tree.

############### 
fileName <- "EXPERIMENT_ABT.csv"
tempData <- read.csv(file = "A029_raw.csv")
targetName <- "FutureFlowCMSx72S"

tempData$X <- NULL
names(tempData)[names(tempData)==targetName] <- "TARGET"  # NAME TARGET to the target variable.
nrow(tempData)
expDataIndexes <- createDataPartition(tempData$TARGET, p = .092)[[1]]
expDataB <- tempData[ expDataIndexes, ]
inTrain <- createDataPartition(expDataB$TARGET, p = .909)[[1]] #inteval to get a fair split similar to the testing data splits
expDataTrain <- expDataB[ inTrain, ]
expDataTest  <- expDataB[-inTrain, ]
nrow(expDataTrain)
nrow(expDataTest)

# Generate model agains data
Sys.time()
modelRF1000_50000_6h <-  executeRF(expDataTrain, expDataTest, 1000, fileName)
Sys.time() # 4 hours exection
predictedTARGET <- predict(modelRF1000_50000_6h, expDataTest)
# Obtain results
RMSE(predictedTARGET, expDataTest$TARGET)
R2(predictedTARGET, expDataTest$TARGET)

# visualise data and residuals
ggplot(expDataTest, aes(x = predictedTARGET, y = expDataTest$TARGET)) +  # Set up canvas with outcome variable on y-axis
  geom_segment(aes(xend = predictedTARGET, yend = expDataTest$TARGET)) +
  geom_point()  + # Plot the actual points
  geom_point(aes(y = predictedTARGET), shape = 1) +
  ggtitle("Predicted Versus Observed 6 hour lead flow (CMS) - standarised") +
  labs(x="Predicted",y="Observed") +
  theme_bw() +
  geom_smooth(method = "lm", se = FALSE, color = "lightgrey")


modelResiduals <- lm(predictedTARGET~expDataTest$TARGET)  #Create a linear model
resid(modelResiduals)
hist(modelResiduals$residuals, xlab ="Residuals", main = "Residuals histogram RF final model- 6 hour lead")



#######################################################################################################################################
# 12 hour lead experiment based in the 6 hour lead experiment.
#######################################################################################################################################
tempData1 <- read.csv( "A029_clean.csv", sep=",", header=T)

# Generate ABT.
intervalsPastRain <- c(0, 36,72, 108, 114, 120, 126, 132, 138, 144, 150, 156, 162, 168, 174, 180, 192, 204, 216, 252,288, 324, 360, 396, 432, 468, 504, 1000)
intervalsPastCMS <- c(36, 72, 108, 144, 180, 216, 252, 288, 324, 360, 396, 432, 468, 504, 1000)
IntervalsPastTemp <- c(156, 300, 504)
intervalsFutuRain <- c( 0, 36, 72, 108, 144)
intervalsFutuCMS  <- c( 72, 144)
intervalsFutuTemp <- c( 72, 144)

tempData$FlowCMS<-as.numeric(as.character(tempData$FlowCMS))
tempData$Temp<-as.numeric(as.character(tempData$Temp))
tempData$Rainmm<-as.numeric(as.character(tempData$Rainmm))
tempData$X <- NULL
tempData$Date <- NULL
tempData$Levelm <- NULL

tempData <- normaliseDataframe(tempData)
saveFileName <-fileName
tempData <- featureCreator (tempData_short, intervalsPastRain, intervalsPastCMS, IntervalsPastTemp, intervalsFutuRain, intervalsFutuCMS, intervalsFutuTemp,  saveFileName)
tempData$FutureRainmmx0x35C <- NULL
targetName <- "FutureFlowCMSx72S" # Use target name for 6h first.
names(tempData)[names(tempData)==targetName] <- "TARGET"  # NAME TARGET to the target variable.
fileName <- "12hourLeadExperiment.csv" #backup data
write.csv(tempData, file = fileName)

###Choose the data that was not used in the previous experiment base ond the indexed expDataIndexesBB
expDataA <- tempData[ -expDataIndexes, ] # REMOVE DATA ALREADY USED.
expDataIndexesBB <- createDataPartition(tempData$TARGET, p = .092)[[1]] # percentage to get aproximatelly 50,000 rows
expDataTest12h <- tempData[ expDataIndexesBB, ]
expDataTest12h <- na.omit(expDataTest12h) # Remove lines with NAs
inTrain <- createDataPartition(expDataTest12h$TARGET, p = .909)[[1]] # create test and training splits
expDataTrain <- expDataTest12h[ inTrain, ]
expDataTest  <- expDataTest12h[-inTrain, ]

# CREATING THE PREDICTED DATA with the 6h model
expDataTrain$FutureFlowCMSx72S_Predicted <- predict(modelRF1000_50000_6h, expDataTrain) #expDataTest_sub)
expDataTest$FutureFlowCMSx72S_Predicted <- predict(modelRF1000_50000_6h, expDataTest) #expDataTest_sub)
write.csv(expDataTrain, file = "Train12h")# temporary backups of sets if needed.
write.csv(expDataTest, file = "Test12h")

# UPDATING TARGET NAMES, OLD AND NEW.
names(expDataTrain)[names(expDataTrain)=="FutureFlowCMSx72S_Predicted"] <- "FutureFlowCMSx72S"  # NAME TARGET to the target variable.
expDataTrain$TARGET<- NULL
names(expDataTrain)[names(expDataTrain)=="FutureFlowCMSx144S"] <- "TARGET"  # NAME TARGET to the target variable.

names(expDataTest)[names(expDataTest)=="FutureFlowCMSx72S_Predicted"] <- "FutureFlowCMSx72S"  # NAME TARGET to the target variable.
expDataTest$TARGET<- NULL
names(expDataTest)[names(expDataTest)=="FutureFlowCMSx144S"] <- "TARGET"  # NAME TARGET to the target variable.

modelRF1000_50000_12h <- executeRF(expDataTrain, expDataTest, 1000, fileName)#GENERATE NEW MODEL

#MAKE THE PREDICTION FOR THE TEST DATA WITH THE NEW MODEL
predictedTARGET <- predict(modelRF1000_50000_12h, expDataTest)


# VISUALISE
ggplot(expDataTest, aes(x = predictedTARGET, y = expDataTest$TARGET)) +  # Set up canvas with outcome variable on y-axis
  geom_segment(aes(xend = predictedTARGET, yend = expDataTest$TARGET)) +
  geom_point()  + # Plot the actual points
  geom_point(aes(y = predictedTARGET), shape = 1) +
  ggtitle("Predicted Versus Observed 12 hour lead flow (CMS) - standarised") +
  labs(x="Predicted",y="Observed") +
  theme_bw() +
  geom_smooth(method = "lm", se = FALSE, color = "lightgrey")


modelResiduals <- lm(predictedTARGET~expDataTest$TARGET)  #Create a linear model
resid(modelResiduals)
hist(modelResiduals$residuals, xlab ="Residuals", main = "Residuals histogram RF final model- 12 hour lead")


