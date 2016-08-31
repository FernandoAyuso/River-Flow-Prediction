# File to read the data from XML and prepare for use
# DIVIDING THE EXCEL INTO 4 FILES WITH
#   - Heathers
#   - Right data types
#   - NA shorted.


#install.packages("readxl")
#install.packages("zoo")
#install.packages("imputeTS")

library(readxl)  
library(zoo)
library(imputeTS)



dfRiver <- read_excel(filename, 1)
write.csv(dfRiver, file = "A029_raw.csv")
head(dfRiver)
colnames(dfRiver) <- c("Date","Levelm", "FlowCMS", "Temp", "Rainmm")
dfRiver <- dfRiver[-1,]
dfRiver <- dfRiver[-1,]
dfRiver <- dfRiver[-1,]
#dfRiver[1] <- NULL
#Handle data types
dfRiver$Levelm<-as.numeric(as.character(dfRiver$Levelm))
dfRiver$FlowCMS<-as.numeric(as.character(dfRiver$FlowCMS))
dfRiver$Temp<-as.numeric(as.character(dfRiver$Temp))
dfRiver$Rainmm<-as.numeric(as.character(dfRiver$Rainmm))
sapply(dfRiver, mode)
sapply(dfRiver, class)
head(dfRiver)
# Handle missing values #  interpolation using package imputeTS. Univariant interpolation for time series. using splpliner non linear reg.

dfRiver$Levelm <- na.interpolation(dfRiver$Levelm, option = "spline")
dfRiver$FlowCMS <- na.interpolation(dfRiver$FlowCMS, option = "spline")
dfRiver$Temp <- na.interpolation(dfRiver$Temp, option = "spline")
dfRiver$Rainmm <- na.interpolation(dfRiver$Rainmm, option = "spline")

dfRiver[["Date"]] <- as.POSIXct(dfRiver[["Date"]] * (60*60*24), origin="1899-12-30" , tz="GMT")
write.csv(dfRiver, file = "A029_clean.csv")





