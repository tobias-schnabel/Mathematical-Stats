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
# the following functions all work the same way: 
# 1. They first subset to a new data.table the annual/monthly/monthly (smoothed)/daily 
# data by applying a user-specified function (mean, median, sum, etc.) in a 
# rolling window specified by the user
# 2. They then subset the original data to a second new table and generate row numbers
# and keep Years/months etc. and bind these 2 "new" tables together
# 3. They then drop all rows outside the specified time window so that we are left with 
# a subset of data with e.g. 10 year means or 6 day medians
# We use these to experiment with different subsets 

xYearStat <- function(x, STATNAME){
  #generate data.table with rolling means, window=x
  xYrS <- as.data.table(frollapply(da[, .(de_bilt, eelde, maastricht)], x, FUN = STATNAME))[, ID := .I ]
  dateSet <- da[, ID := .I][, .(year, ID)]
  xYrStatInclYr <- cbind(xYrS, dateSet)
  xYST <- xYrStatInclYr[ID %% x == 0 , .(Year = year, Eelde = V1, 'De Bilt' = V2, Maastricht = V3)]
  return(xYST)
}

xMonthStat <- function(x, STATNAME){
    xMosRS <- as.data.table(frollapply(dm[, .(de_bilt, eelde, maastricht)], x, FUN = STATNAME))[, ID := .I ]
    dateSet <- dm[, ID := .I][, .(month, ID)]
    xMosRSInclMonth <- cbind(xMosRS, dateSet)
    xMosStat <- xMosRSInclMonth[(ID) %% x == 0 , .(Month = month , Eelde = V1, 'De Bilt' = V2, Maastricht = V3)]
    return(xMosStat)
}

xMonthSmoothedStat <- function(x, STATNAME){
  xMosSRS <- as.data.table(frollapply(dms[, .(de_bilt, eelde, maastricht)], x, FUN = STATNAME))[, ID := .I ]
  dateSet <- dms[, ID := .I][, .(month, ID)]
  xMosSRSInclMonth <- cbind(xMosSRS, dateSet)
   xMosSStat <- xMosSRSInclMonth[(ID) %% x == 0 , .(Month = month , Eelde = V1, 'De Bilt' = V2, Maastricht = V3)]
  return(xMosSStat)
}

xDayStat <- function(x, STATNAME){
  xDayRS <- as.data.table(frollapply(dd[, .(de_bilt, eelde, maastricht)], x, FUN = STATNAME))[, ID := .I ]
  dateSet <- dd[, ID := .I][, .(date, ID)]
  xDayRSInclDate <- cbind(xDayRS, dateSet)
  xMosSStat <- xDayRSInclDate[(ID) %% x == 0 , .(Date = date, Eelde = V1, 'De Bilt' = V2, Maastricht = V3)]
  return(xMosSStat)
}

xDateComparisonSet <- function(monthAndDate){
  inputdate <- as.Date(as.character(monthAndDate), format("%m%d"))
  compSet <- dd[, daterows := as.Date(as.character(date), format = "%Y%m%d")][daterows == inputdate]

return(compSet)
}
mar15 <- xDateComparisonSet(0315)


meanTable10y <- xYearStat(10, mean)
meanTable5y <- xYearStat(5, mean)
meanTable50y <- xYearStat(50, mean)
medianTable10y <- xYearStat(10, median)

meanTable10mo <- xMonthStat(10, mean)
meanTable10mo <- xMonthStat(10, mean)
medianTable5mo <- xMonthStat(5, median)

meanTable20d <- xDayStat(20, mean)


