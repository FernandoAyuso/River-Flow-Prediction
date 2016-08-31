# Machine learning execution


# load libraries
#install.packages("caret")
#install.packages("ISLR")
#install.packages("AppliedPredictiveModeling")
#install.packages("randomForest")
#install.packages("kernlab")
#install.packages("kknn")
#install.packages("mlbench")
#install.packages("doMC")

library(caret)
library(ISLR)
library(AppliedPredictiveModeling)
library(randomForest)
library(kernlab)
library(kknn)
library(mlbench)
library(doMC)

# Pararell # Checking number of cores and use them all but one for the OS
nCores <-parallel:::detectCores()
registerDoMC(cores = nCores-1)


#######################################################################################################################################
# Function that Execute SVM Radial
# Parameters
#  expDataTrain: train data
#  expDataTest: test data
#  fileName: name of file to use to save the model as R file
# Output
#  caret object with the model generated.
#######################################################################################################################################
executeSVM <- function (expDataTrain, expDataTest,fileName){
  print ("Start execution executeSVMRadial")
  totTime <- Sys.time()
  sigDist <- sigest(TARGET ~ ., data = expDataTrain, frac = 1)# identifies the right Sigma.
  tuneGrid <- data.frame(sigma = as.vector(sigDist)[1],
                         C = 2^(-2:7))
  modelFit <- train(TARGET ~ .,
                  data = expDataTrain,
                  method = "svmRadial", # Method to execute
                  metric = "RMSE",
                  preProc = c("center", "scale"),
                  tuneGrid = tuneGrid, 
                  trControl = trainControl(method = "repeatedcv",
                                           number = 10, 
                                           repeats = 1)) #Repeated 10-fold cross-validation can be specified
  predictedTARGET <- predict(modelFit, expDataTest) 
  print (modelFit)
  modelParameters <- toString(as.vector(tuneGrid)) 
  caretDesc <- toString(as.vector(modelFit$finalModel@param))
  modelFileName <- paste("//Users/Fernando/Dropbox/3 Project/R/","/models/", "SVM Radial ", toString(Sys.time()), "RData", sep="")
  save(modelFit,file = modelFileName)
  recordExperiment("SVM Radial",modelParameters, caretDesc, modelFileName,
                   fileName, expDataTest$TARGET, predictedTARGET, 
                   nrow(expDataTrain) + nrow(expDataTest), Sys.time() - totTime,
                   RMSE(predictedTARGET, expDataTest$TARGET), R2(predictedTARGET, expDataTest$TARGET), getTrainPerf(modelFit)[1,1], getTrainPerf(modelFit)[1,2]) # Metric resuls
  print(paste("Total time executeSVMRadial", Sys.time() - totTime))
  return (modelFit)
}


#######################################################################################################################################
# Function that Execute SVM RadialSigma Implementation
# Parameters
#  expDataTrain: train data
#  expDataTest: test data
#  fileName: name of file to use to save the model as R file
# Output
#  caret object with the model generated.
#######################################################################################################################################
executeSVMRS <- function (expDataTrain, expDataTest,fileName){
  print ("Start execution executeSVMSigma")
  totTime <- Sys.time()
  sigDist <- sigest(TARGET ~ ., data = expDataTrain, frac = 1)
  tuneGrid <- data.frame(sigma = as.vector(sigDist)[1], 
                         C = 2^(-2:7))
  modelFit <- train(TARGET ~ .,
                    data = expDataTrain,
                    method = "svmRadialSigma", # Method to execute
                    metric = "RMSE",
                    preProc = c("center", "scale"), 
                    tuneGrid = tuneGrid, 
                    trControl = trainControl(method = "repeatedcv",
                                             number = 10,
                                             repeats = 1)) #Repeated 10-fold cross-validation can be specified
  predictedTARGET <- predict(modelFit, expDataTest) 
  print (modelFit)
  modelParameters <- toString(as.vector(tuneGrid)) 
  caretDesc <- toString(as.vector(modelFit$finalModel@param))
  modelFileName <- paste("//Users/Fernando/Dropbox/3 Project/R/","/models/", "SVM Radial Sigma", toString(Sys.time()), "RData", sep="")
  save(modelFit,file = modelFileName)
  recordExperiment("SVM Radial Sigma",modelParameters, caretDesc, modelFileName,
                   fileName, expDataTest$TARGET, predictedTARGET, 
                   nrow(expDataTrain) + nrow(expDataTest), Sys.time() - totTime,
                   RMSE(predictedTARGET, expDataTest$TARGET), R2(predictedTARGET, expDataTest$TARGET), getTrainPerf(modelFit)[1,1], getTrainPerf(modelFit)[1,2]) # Metric resuls
  print(paste("Total time executeSVMSigma", Sys.time() - totTime))
  return (modelFit)
}


#######################################################################################################################################
# Function that Execute SVM Radial Cost implementation 
# Parameters
#  expDataTrain: train data
#  expDataTest: test data
#  fileName: name of file to use to save the model as R file
# Output
#  caret object with the model generated.
#######################################################################################################################################
executeSVMRC <- function (expDataTrain, expDataTest,fileName){
  print ("Start execution executeSVM Radial Cost")
  totTime <- Sys.time()
  sigDist <- sigest(TARGET ~ ., data = expDataTrain, frac = 1)
  tuneGrid <- data.frame(C = 2^(-2:7))
  modelFit <- train(TARGET ~ .,
                    data = expDataTrain,
                    method = "svmRadialCost", # Method to execute
                    metric = "RMSE",
                    preProc = c("center", "scale"), 
                    tuneGrid = tuneGrid, 
                    trControl = trainControl(method = "repeatedcv",
                                             number = 10, 
                                             repeats = 1)) #Repeated 10-fold cross-validation can be specified
  predictedTARGET <- predict(modelFit, expDataTest) 
  print (modelFit)
  modelParameters <- toString(as.vector(tuneGrid)) 
  caretDesc <- toString(as.vector(modelFit$finalModel@param))
  modelFileName <- paste("//Users/Fernando/Dropbox/3 Project/R/","/models/", "SVM Radial Cost", toString(Sys.time()), "RData", sep="")
  save(modelFit,file = modelFileName)
  recordExperiment("SVM Radial Cost",modelParameters, caretDesc, modelFileName,
                   fileName, expDataTest$TARGET, predictedTARGET, 
                   nrow(expDataTrain) + nrow(expDataTest), Sys.time() - totTime,
                   RMSE(predictedTARGET, expDataTest$TARGET), R2(predictedTARGET, expDataTest$TARGET), getTrainPerf(modelFit)[1,1], getTrainPerf(modelFit)[1,2]) # Metric resuls
  print(paste("Total time executeSVM", Sys.time() - totTime))
  return (modelFit)
}


#######################################################################################################################################
# Function that Execute SVML Linear kernelab Implementation
# Parameters
#  expDataTrain: train data
#  expDataTest: test data
#  fileName: name of file to use to save the model as R file
# Output
#  caret object with the model generated.
#######################################################################################################################################
executeSVML <- function (expDataTrain, expDataTest,fileName){
  print ("Start execution executeSVML")
  totTime <- Sys.time()
  sigDist <- sigest(TARGET ~ ., data = expDataTrain, frac = 1)
  tuneGrid <- data.frame(#sigma = as.vector(sigDist)[1], 
    C = 2^(-2:7)) #First Execution
    #C = c(24,32,40,48,52))# Second
  modelFit <- train(TARGET ~ .,
                    data = expDataTrain,
                    method = "svmLinear", # Method to execute
                    metric = "RMSE",
                    preProc = c("center", "scale"),
                    tuneGrid = tuneGrid, 
                    trControl = trainControl(method = "repeatedcv",
                                             number = 5, 
                                             repeats = 1)) #Repeated 10-fold cross-validation can be specified
  predictedTARGET <- predict(modelFit, expDataTest) 
  print (modelFit)
  modelParameters <- toString(as.vector(tuneGrid)) 
  caretDesc <- toString(as.vector(modelFit$finalModel@param))
  modelFileName <- paste("//Users/Fernando/Dropbox/3 Project/R/","/models/", "SVMlinear ", toString(Sys.time()), "RData", sep="")
  save(modelFit,file = modelFileName)
  recordExperiment("svmLinear",modelParameters, caretDesc, modelFileName,
                   fileName, expDataTest$TARGET, predictedTARGET, 
                   nrow(expDataTrain) + nrow(expDataTest), Sys.time() - totTime,
                   RMSE(predictedTARGET, expDataTest$TARGET), R2(predictedTARGET, expDataTest$TARGET), getTrainPerf(modelFit)[1,1], getTrainPerf(modelFit)[1,2]) # Metric resuls
  print(paste("Total time executeSVM", Sys.time() - totTime))
  return (modelFit)
}


#######################################################################################################################################
# Function that Execute SVML Linear e1071 Implementation
# Parameters
#  expDataTrain: train data
#  expDataTest: test data
#  fileName: name of file to use to save the model as R file
# Output
#  caret object with the model generated.
#######################################################################################################################################
executeSVML2 <- function (expDataTrain, expDataTest,fileName){
  print ("Start execution executeSVML2")
  totTime <- Sys.time()
  sigDist <- sigest(TARGET ~ ., data = expDataTrain, frac = 1)
  tuneGrid <- expand.grid(cost = 2^(-2:7)) # First Iteration 
  #tuneGrid = data.frame(cost = c(.25, .5, 1)) # Second Iteration
  modelFit <- train(TARGET ~ .,
                    data = expDataTrain,
                    method = "svmLinear2", # Method to execute
                    metric = "RMSE",
                    preProc = c("center", "scale"), 
                    tuneGrid = tuneGrid, 
                    
                    trControl = trainControl(method = "repeatedcv",
                                             number = 5, 
                                             repeats = 1)) #Repeated 10-fold cross-validation can be specified
  predictedTARGET <- predict(modelFit, expDataTest) 
  print (modelFit)
  modelParameters <- toString(as.vector(tuneGrid)) 
  caretDesc <- toString(as.vector(modelFit$finalModel@param))
  modelFileName <- paste("//Users/Fernando/Dropbox/3 Project/R/","/models/", "SVM Linear2 ", toString(Sys.time()), "RData", sep="")
  save(modelFit,file = modelFileName)
  recordExperiment("SVM Linear2",modelParameters, caretDesc, modelFileName,
                   fileName, expDataTest$TARGET, predictedTARGET, 
                   nrow(expDataTrain) + nrow(expDataTest), Sys.time() - totTime,
                   RMSE(predictedTARGET, expDataTest$TARGET), R2(predictedTARGET, expDataTest$TARGET), getTrainPerf(modelFit)[1,1], getTrainPerf(modelFit)[1,2]) # Metric resuls
  print(paste("Total time executeSVML2", Sys.time() - totTime))
  return (modelFit)
}


#######################################################################################################################################
# Function that Execute SVML Random forest
# Parameters
#  expDataTrain: train data
#  expDataTest: test data
#  numTrees: number of trees to grow
#  fileName: name of file to use to save the model as R file
# Output
#  caret object with the model generated.
#######################################################################################################################################
executeRF <-function(expDataTrain, expDataTest, numTrees, fileName){
  print ("Start execution executeRF")
  totTime <- Sys.time()
  # First Iteration 
  #mtry <- sqrt(ncol(expDataTrain)) #### mtry <- ncol(data) - 1 # mtry: Number of variables randomly sampled as candidates at each split.
  #bestmtry <- tuneRF(expDataTrain, expDataTrain$TARGET, stepFactor=1.5, improve=1e-5, ntree=100) # Starting with the default value of mtry, search for the optimal value (with respect to Out-of-Bag error estimate) of mtry for randomForest.
  #tuneGrid <- expand.grid(.mtry=bestmtry) 
  
  #tuneGrid <- expand.grid(.mtry=c(10,20, 40,50))  # Second Iteration 
  #tuneGrid <- expand.grid(.mtry=c(12,17, 25,37,52))  # Third Iteration 
  #tuneGrid <- expand.grid(.mtry=c(22, 25, 28,31))  # Fourth Iteration 
  tuneGrid <- expand.grid(.mtry=c( 24))  # FINAL Iteration 
  modelFit <- train(TARGET ~ .,
                    data = expDataTrain,
                    method = "rf", 
                    metric = "RMSE",
                    # preProc = c("center", "scale"),
                    ntree = numTrees,  
                    tuneGrid=tuneGrid,
                    trControl = trainControl(method ="repeatedcv", 
                                             number = 10, 
                                             repeats = 1)) 
  predictedTARGET <- predict(modelFit, expDataTest)
  print (modelFit)
  modelParameters <- paste("numTrees", numTrees, "  ",toString(as.vector(tuneGrid))) 
  caretDesc <- paste(modelFit$finalModel[1], " mtry: ", modelFit$finalModel$mtry)
  modelFileName <- paste("//Users/Fernando/Dropbox/3 Project/R/","/models/", "RF ", toString(Sys.time()), "RData", sep="")
  save(modelFit,file =modelFileName)
  recordExperiment(paste("RF", numTrees),modelParameters, caretDesc, modelFileName,                   
                   fileName, expDataTest$TARGET, predictedTARGET, 
                   nrow(expDataTrain) + nrow(expDataTest), Sys.time() - totTime,
                   RMSE(predictedTARGET, expDataTest$TARGET), R2(predictedTARGET, expDataTest$TARGET), getTrainPerf(modelFit)[1,1], getTrainPerf(modelFit)[1,2]) # Metric resuls
  print(paste("Total time executeRF", Sys.time() - totTime))
  return(modelFit)
}


#######################################################################################################################################
# Function that Execute KNN
# Parameters
#  expDataTrain: train data
#  expDataTest: test data
#  numTrees: number of trees to grow
#  fileName: name of file to use to save the model as R file
# Output
#  caret object with the model generated.
#######################################################################################################################################
executeKNN <-function(expDataTrain, expDataTest, fileName){
  totTime <- Sys.time()
  print("Start execution executeKNN")
  recomK <- round(sqrt(nrow(expDataTrain)), digits = 0)
  minK <- round(recomK*0.6, digits = 0)
  maxK <- round(recomK*1.3, digits = 0) 
  kMax <- seq(minK,maxK, by=round((maxK-minK)/10,digits=0))# Pre studi itration 
  #kMax <- c(3,4,5,6,7,8,9,10,15,20,30,40,50,60,70,80,90)  # First Iteration
  #kMax <- c(1,2,3,4,5)  # Second iteration
  kMax <- c(1, 2, 3, 4, 5, 10, 15, 20, 25 , 50) # Fourth iteration
  modelFit <- train(TARGET ~ .,
                  data = expDataTrain,
                  method = "knn", 
                  metric = "RMSE",
                  preProc = c("center", "scale"), 
                  tuneGrid = data.frame(.k = kMax), 
                  trControl = trainControl(method ="repeatedcv", 
                                           number = 5, 
                                           repeats = 1)) #Repeated 10-fold cross-validation can be specified
  print (modelFit)
  predictedTARGET <- predict(modelFit, expDataTest)
  modelParameters <- toString(as.vector(data.frame(.k = minK:maxK))) 
  caretDesc <- toString(as.vector(modelFit))
  modelFileName <- paste("//Users/Fernando/Dropbox/3 Project/R/","/models/", "KNN ", toString(Sys.time()), "RData", sep="")
  save(modelFit,file =modelFileName)
  
  recordExperiment("KNN",modelParameters, caretDesc, modelFileName,                   
                   fileName, expDataTest$TARGET, predictedTARGET, 
                   nrow(expDataTrain) + nrow(expDataTest), Sys.time() - totTime,
                   RMSE(predictedTARGET, expDataTest$TARGET), R2(predictedTARGET, expDataTest$TARGET), getTrainPerf(modelFit)[1,1], getTrainPerf(modelFit)[1,2]) # Metric resuls
  print(paste("Total time executeKNN", Sys.time() - totTime))
  return (modelFit)
}


#######################################################################################################################################
# Function that Execute weighted KNN
# Parameters
#  expDataTrain: train data
#  expDataTest: test data
#  numTrees: number of trees to grow
#  fileName: name of file to use to save the model as R file
# Output
#  caret object with the model generated.
#######################################################################################################################################
executeKKNN <-function(expDataTrain, expDataTest, fileName){
  totTime <- Sys.time()
  print("Start execution executeKKNN")
  #kMax <- c(3,4,5,6,7, 40, 50, 60)    # First Iteration
  #kMax <- c(5, 6 , 7, 30, 40, 50, 60) # Second Iteration  
  kMax <- c(4, 5, 6 , 7 )# Third Iteration
  myTuneGrid <- expand.grid( kmax = kMax,
                             #distance =c(1), # First Iteration
                             #distance =c(0.5, 1,1.5), # Second Iteraton 
                             distance =c(0.1, 0.2, 0.3, 0.4, 0.5), #Third Iteration
                             #kernel =c("triangular", "rectangular", "epanechnikov", "biweight", "triweight", "cos", "inv", "gaussian"))# First Iteration
                             #kernel =c( "epanechnikov", "biweight", "triweight", "cos")) # Second Iteration
                             kernel =c( "biweight", "triweight"))
                             
  modelFit <- train(TARGET ~ .,
                     data = expDataTrain,
                     method = "kknn", 
                     metric = "RMSE",
                     preProc = c("center", "scale"), 
                     tuneGrid = myTuneGrid,
                     trControl = trainControl(method ="repeatedcv", 
                                              number = 5, 
                                              repeats = 1)) 
  print (modelFit)
  predictedTARGET <- predict(modelFit, expDataTest)
  modelParameters <- toString(as.vector(data.frame(.k = minK:maxK))) 
  caretDesc <- toString(as.vector(modelFit))
  modelFileName <- paste("//Users/Fernando/Dropbox/3 Project/R/","/models/", "KNNN ", toString(Sys.time()), "RData", sep="")
  save(modelFit,file =modelFileName)
  recordExperiment("KKNN",modelParameters, caretDesc, modelFileName,                   
                   fileName, expDataTest$TARGET, predictedTARGET, 
                   nrow(expDataTrain) + nrow(expDataTest), Sys.time() - totTime,
                   RMSE(predictedTARGET, expDataTest$TARGET), R2(predictedTARGET, expDataTest$TARGET), getTrainPerf(modelFit)[1,1], getTrainPerf(modelFit)[1,2]) # Metric resuls
  print(paste("Total time executeKNN", Sys.time() - totTime))
  return (modelFit)
}







