#load packages
library("zoo", "ggplot2", "tidy")

##import data

#annual
annualData <- read.csv("~/R projects/Mathematical_Stats/Data/AnnualTemp.csv", 
                 header = TRUE, sep = ";", quote = "\"'", dec = ",", 
                 fill = TRUE, comment.char="")
colnames(annualData) <- c("Year", "Eelde", "De Bilt", "Maastricht") 
#monthly
monthlyData <- read.csv("~/R projects/Mathematical_Stats/Data/MonthlyTemp.csv", 
                        header = TRUE, sep = ";", quote = "\"'", dec = ",", 
                        fill = TRUE, comment.char="")
#label
colnames(monthlyData) <- c("Year", "Eelde", "De Bilt", "Maastricht") 
#daily
dailyData <- read.csv("/Users/ts/R projects/Mathematical_Stats/Data/DailyTemp.csv", 
                        header = TRUE, sep = ";", quote = "\"'", dec = ",", 
                        fill = TRUE, comment.char="")
#label
colnames(dailyData) <- c("Year", "Eelde", "De Bilt", "Maastricht") 

#smoothed
monthlySmoothedData <- read.csv("/Users/ts/R projects/Mathematical_Stats/Data/SmoothedMonthlyTemp.csv", 
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

#hgjhvh
