#load packages
<<<<<<< HEAD
library("zoo", "ggplot2", "tidyverse", "data.table")
=======
library("zoo", "ggplot2", "tidy", "data.table")
>>>>>>> 251937e0300c450e36643c7ddf0b973b865d1ab6

##import data
#########
#/Users/chumasharajapakshe/Documents/GitHub/Mathematical_Stats/Data/AnnualTemp.csv
#/Users/ts/Git/Mathematical_Stats
Paths = c("/Users/ts/Git/Mathematical_Stats", "Users/chumasharajapakshe/Documents/GitHub/Mathematical_Stats")
names(Paths) = c("ts", "chumasharajapakshe")
setwd(Paths[Sys.info()[7]])

#annual
annualData <- read.csv("./Data/AnnualTemp.csv", 
                 header = TRUE, sep = ";", quote = "\"'", dec = ",", 
                 fill = TRUE, comment.char="")
colnames(annualData) <- c("Year", "Eelde", "De Bilt", "Maastricht") 
#monthly
monthlyData <- read.csv("./Data/MonthlyTemp.csv", 
                        header = TRUE, sep = ";", quote = "\"'", dec = ",", 
                        fill = TRUE, comment.char="")
#label
colnames(monthlyData) <- c("Year", "Eelde", "De Bilt", "Maastricht") 
#daily
dailyData <- read.csv("./Data/DailyTemp.csv", 
                        header = TRUE, sep = ";", quote = "\"'", dec = ",", 
                        fill = TRUE, comment.char="")
#label
colnames(dailyData) <- c("Year", "Eelde", "De Bilt", "Maastricht") 

#smoothed
monthlySmoothedData <- read.csv("./Data/SMTemp.csv", 
                                header = TRUE, sep = ";", quote = "\"'", dec = ",", 
                                fill = TRUE, comment.char="")
#label
colnames(dailyData) <- c("Year", "Eelde", "De Bilt", "Maastricht") 

#save
save.image("ProjectData.RData")

#Split the sample in a number of subsamples, and 
#compare average temperatures across the subsamples.
#non-overlapping subsamples
subsample_1 <- subset(annualData, Year >= 1907 & Year < 1958)

#overlapping samples
zoo::rollapply(annualData$Eelde, 10, mean)
zoo::rollmean(annualData$'De Bilt', 10)


