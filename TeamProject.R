#########################HOUSEKEEPING########################

rm(list = ls(all = TRUE)) ###CLEAR ALL
# Package names
packages <- c("data.table", "dplyr", "zoo", "tidyr", "ggplot2", "tidyverse", "xtable", "knitr", "stargazer")

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

#########################DATA IMPORT#########################

#annual
da <- fread('./Data/AnnualTemp.csv', dec=",")
colnames(da) <- c('year', 'de_bilt', 'eelde', 'maastricht')
#monthly
dm <- fread('./Data/MonthlyTemp.csv', dec=",")
colnames(dm) <- c('month', 'de_bilt', 'eelde', 'maastricht')

#smoothed
dms <- fread('./Data/SMTemp.csv', dec=",")
colnames(dms) <- c('month', 'de_bilt', 'eelde', 'maastricht')

#daily
dd <- fread('./Data/DailyTemp.csv', dec=",")
colnames(dd) <- c('date', 'de_bilt', 'eelde', 'maastricht')
########################DATA ANALYSIS########################

#original code then turned into function
# rollmeans <- frollmean(da[, .(de_bilt, eelde, maastricht)], 10)
# setDT(rollmeans)
# keepYears <- seq(from = 10, to = 120, by = 10)
# rollmeans[, ID := .I ] #data starts in 1907 (first year from which onwards we can computer a decade, ID starts at 1)
# meantable <- rollmeans[ID %%10 == 0 , .(Year = ID + 1897, Eelde = V1, 'De Bilt' = V2, Maastricht = V3)]

xYearMean <- function(x){
  #generate data.table with rolling means, window=x
  xYrm <- as.data.table(frollmean(da[, .(de_bilt, eelde, maastricht)], x))[, ID := .I ]
  keepYearsX <- seq(from = 1, to = 120, by = x) #used to drop all other rows
  
  #data starts in 1907, first 10 years of rollmean are NA, years in this table indicate start of window
  xYmt <- xYrm[ID %%x == 0 , .(Year = ID + 1897, Eelde = V1, 'De Bilt' = V2, Maastricht = V3)]
  return(xYmt)
}

xYearMean <- function(x, FNAME){
  #generate data.table with rolling means, window=x
  frollapply(da[, .(de_bilt, eelde, maastricht)], 20, FUN = FNAME)
  xYrm <- as.data.table(frollmean(da[, .(de_bilt, eelde, maastricht)], x))[, ID := .I ]
  keepYearsX <- seq(from = 1, to = 120, by = x) #used to drop all other rows
  
  #data starts in 1907, first 10 years of rollmean are NA, years in this table indicate start of window
  xYmt <- xYrm[ID %%x == 0 , .(Year = ID + 1897, Eelde = V1, 'De Bilt' = V2, Maastricht = V3)]
  return(xYmt)
}

meanTable10 <- xYearMean(10, mean)
meanTable5 <- xYearMean(5, mean)
meanTable20 <- xYearMean(20, mean)
meanTable50 <- xYearMean(50, mean)

medianTable5 <- xYearMean(5, median)
