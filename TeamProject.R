#############################################################
#########################HOUSEKEEPING########################
#############################################################
# Package names
packages <- c("dplyr", "zoo", "tidyr", "ggplot2", "tidyverse", "xtable", "knitr")

# Install packages not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

#load packages
invisible(lapply(packages, library, character.only = TRUE))

#/Users/chumasharajapakshe/Documents/GitHub/Mathematical_Stats/Data/AnnualTemp.csv
#/Users/ts/Git/Mathematical_Stats
Paths = c("/Users/ts/Git/Mathematical_Stats", "Users/chumasharajapakshe/Documents/GitHub/Mathematical_Stats")
names(Paths) = c("ts", "chumasharajapakshe")
setwd(Paths[Sys.info()[7]])

#############################################################
#########################DATA IMPORT#########################
#############################################################

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

#set df as data tables
setDT(annualData)
setDT(monthlyData)
setDT(monthlySmoothedData)
setDT(dailyData)

#save
save.image("ProjectData.RData")

#############################################################
########################DATA ANALYSIS########################
#############################################################

#Split the sample in a number of subsamples, and 
#compare average temperatures across the subsamples.
#non-overlapping subsamples
subsample_1 <- subset(annualData, Year >= 1907 & Year < 1958)

#overlapping samples
#zoo::rollapply(annualData$Eelde, 10, mean)



