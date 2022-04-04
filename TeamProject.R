#########################HOUSEKEEPING########################

rm(list = ls(all = TRUE)) ###CLEAR ALL
# Package names
packages <- c("data.table", "dplyr", "zoo", "tidyr", "ggplot2", "ggthemes", "scales", "strucchange",
              "tidyverse", "xtable", "knitr", "stargazer", "patchwork", "remotes", "broom", "purrr")
# package grateful must be installed by hand# install.packages("remotes")
remotes::install_github("Pakillo/grateful")
# Install packages not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

#load packages
invisible(lapply(packages, library, character.only = TRUE))


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
#functions
{
  xYearStat <- function(x, STATNAME){
    #generate data.table with rolling means, window=x
    xYrS <- as.data.table(frollapply(da[, .(de_bilt, eelde, maastricht)], x, FUN = STATNAME))[, ID := .I ]
    dateSet <- da[, ID := .I][, .(year, ID)]
    xYrStatInclYr <- cbind(xYrS, dateSet)
    xYST <- xYrStatInclYr[ID %% x == 0 , .(Year = year, Eelde = V1, 'De Bilt' = V2, Maastricht = V3)]
    return(xYST)
  }
  
  xYearYoverlapStat <- function(x, y, STATNAME){
    #generate data.table with rolling means, window=x
    xYrS <- as.data.table(frollapply(da[, .(de_bilt, eelde, maastricht)], x, FUN = STATNAME))[, ID := .I ]
    dateSet <- da[, ID := .I][, .(year, ID)]
    xYrStatInclYr <- cbind(xYrS, dateSet)
    z = x - y
    xYST <- xYrStatInclYr[ID %% z == 0 , .(Year = year, Eelde = V1, 'De.Bilt' = V2, Maastricht = V3)]
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
  
  subsetMonth <- function(mm){
    mm <- str_pad(as.character(mm), 2, side = "left", pad = '0')
    
    subset <- dd[, DateCol := as.Date(as.character(date), format = "%Y%m%d")
    ][, month := format(as.Date(DateCol), "%m")]
    compSet <- subset[month == mm, .(DateCol, eelde, de_bilt, maastricht)]
    return(compSet)
  }
  
  subsetMonthLong <- function(mm){
    mm <- str_pad(as.character(mm), 2, side = "left", pad = '0')
    
    subset <- dd[, DateCol := as.Date(as.character(date), format = "%Y%m%d")
    ][, month := format(as.Date(DateCol), "%m")]
    compSet <- subset[month == mm, .(Month = month, Eelde = eelde, De.Bilt = de_bilt, Maastricht = maastricht)]
    ret <- melt(compSet, id.vars = "Month", measure.vars = c("Maastricht", "Eelde", "De.Bilt"),
                variable.factor = T, variable.name = "City", value.name = "Temperature")[, Citymean := mean(Temperature), by = City]
    return(ret)
  }
  
  subsetDate <- function(mmdd){
    mmdd <- str_pad(as.character(mmdd), 4, side = "left", pad = '0')
    
    subset <- dd[, DateCol := as.Date(as.character(date), format = "%Y%m%d")
    ][, md := format(as.Date(DateCol), "%m%d")][, yd := format(as.Date(DateCol), "%Y%m%d")]
    
    compSet <- subset[mmdd == md, .(Date = md, eelde, de_bilt, maastricht, yd)]
    return(compSet)
  }
  
  subsetDateLong <- function(mmdd){
    mmdd <- str_pad(as.character(mmdd), 4, side = "left", pad = '0')
    
    subset <- dd[, DateCol := as.Date(as.character(date), format = "%Y%m%d")
    ][, md := format(as.Date(DateCol), "%m%d")]
    
    compSet <- subset[mmdd == md, .(Date = DateCol, Eelde = eelde, De.Bilt = de_bilt, Maastricht = maastricht)]
    ret <- melt(compSet, id.vars = "Date", measure.vars = c("Maastricht", "Eelde", "De.Bilt"),
                variable.factor = T, variable.name = "City", value.name = "Temperature")[, Citymean := mean(Temperature)]
    
    return(ret)
  } 
}

#gen datasets needed
{
  #rolling window plots
  rollingMean10_5 <- xYearYoverlapStat(10, 5, mean)
  rollingMean20_10 <- xYearYoverlapStat(20, 10, mean)
  
  # february <- subsetMonth(2)

  meanTable10y <- xYearStat(10, mean)
  meanTable5y <- xYearStat(5, mean)
  meanTable50y <- xYearStat(50, mean)
  medianTable10y <- xYearStat(10, median)
  meanTable10mo <- xMonthStat(10, mean)
  medianTable5mo <- xMonthStat(5, median)
  meanTable20d <- xDayStat(20, mean)
  varTable5y <- xYearStat(5, var)
}

#test for structural change
{
  structmat1 <- matrix(nrow = 3, ncol=2)
  rownames(structmat1) <- c('De Bilt', 'Eelde', 'Maastricht')
  colnames(structmat1) <- c('F-Statistic', 'p-value')
  
  scyM <- sctest(da$year ~ da$maastricht, type = "Chow")
  scyE <- sctest(da$year ~ da$eelde, type = "Chow")
  scyD <- sctest(da$year ~ da$de_bilt, type = "Chow")
  
  structmat1[3,1] <- scyM$statistic
  structmat1[3,2] <- scyM$p.value
  structmat1[2,1] <- scyE$statistic
  structmat1[2,2] <- scyE$p.value
  structmat1[1,1] <- scyD$statistic
  structmat1[1,2] <- scyD$p.value
  
  structtabY <- as.data.table(structmat1, keep.rownames = T)
  setnames(structtabY, "rn", "City")
  rm('scyM','scyE', 'scyD', 'structmat1')
  
  structmat2 <- matrix(nrow = 3, ncol=2)
  rownames(structmat2) <- c('De Bilt', 'Eelde', 'Maastricht')
  colnames(structmat2) <- c('F-Statistic', 'p-value')
  
  scmM <- sctest(dm$month ~ dm$maastricht, type = "Chow")
  scmE <- sctest(dm$month ~ dm$eelde, type = "Chow")
  scmD <- sctest(dm$month ~ dm$de_bilt, type = "Chow")
  
  structmat2[3,1] <- scmM$statistic
  structmat2[3,2] <- scmM$p.value
  structmat2[2,1] <- scmE$statistic
  structmat2[2,2] <- scmE$p.value
  structmat2[1,1] <- scmD$statistic
  structmat2[1,2] <- scmD$p.value
  
  structtabM <- as.data.table(structmat2, keep.rownames = T)
  setnames(structtabM, "rn", "City")
  
  rm('scmM','scmE', 'scmD', 'structmat2')
  
  #find breakpoints
  structmat3 <- matrix(nrow = 3, ncol=2)
  rownames(structmat3) <- c('De Bilt', 'Eelde', 'Maastricht')
  colnames(structmat3) <- c('Yearly Data Breakpoint', 'Monthly Data Breakpoint')
  
  ybpm <-  breakpoints(da$year ~ da$maastricht, h = 0.35, breaks = 1)
  structmat3[3,1] <- da[ID == ybpm$breakpoints, year]
  ybpe <- breakpoints(da$year ~ da$eelde, h = 0.35, breaks = 1)
  structmat3[2,1] <- da[ID == ybpe$breakpoints, year]
  ybpd <- breakpoints(da$year ~ da$de_bilt, h = 0.35, breaks = 1)
  structmat3[1,1] <- da[ID == ybpd$breakpoints, year]
  
  mbpm <-  breakpoints(dm$month ~ dm$maastricht, h = 0.35, breaks = 1)
  structmat3[3,2] <- dm[ID == mbpm$breakpoints, month]/100
  mbpe <- breakpoints(dm$month ~ dm$eelde, h = 0.35, breaks = 1)
  structmat3[2,2] <- dm[ID == mbpe$breakpoints, month]/100
  mbpd <- breakpoints(dm$month ~ dm$de_bilt, h = 0.35, breaks = 1)
  structmat3[1,2] <- dm[ID == mbpd$breakpoints, month]/100
  
  structtabBP <- as.data.table(structmat3, keep.rownames = T)
  setnames(structtabBP, "rn", "City")
  rm('ybpm','ybpe', 'ybpd', 'mbpd', 'mbpe', 'structmat3')
}

#subset according to breakpoint results
prebreakY <- da[year <= 1961]
postbreakY <- da[year > 1961 & year < 2017] #ensure equal sample size
prebreakM <- dm[month <= 196210]
postbreakM <- dm[month > 196210 & month < (max(month)-203)] #ensure equal sample size

#subset according to climate results
preCBY <- da[year <= 1975 & year > 1930] #ensure equal sample size
postCBY <- da[year > 1975]
preCBM <- dm[month <= 197501 & month > 192902] #ensure equal sample size
postCBM <- dm[month > 197501]

#test for differences in means (annual)
{
  testmat1 <- matrix(nrow = 3, ncol=4)
  rownames(testmat1) <- c('De Bilt', 'Eelde', 'Maastricht')
  colnames(testmat1) <- c('t-Statistic', 'p-value', 'C.I. Lower', 'C.I. Upper')
  
  ttmAM <- t.test(prebreakY$maastricht, postbreakY$maastricht, paired = T)
  ttmAE <- t.test(prebreakY$eelde, postbreakY$eelde, paired = T)
  ttmAD <- t.test(prebreakY$de_bilt, postbreakY$de_bilt, paired = T)
  
  testmat1[1,1] <- ttmAD$statistic
  testmat1[2,1] <- ttmAE$statistic
  testmat1[3,1] <- ttmAM$statistic
  
  testmat1[1,2] <- ttmAD$p.value
  testmat1[2,2] <- ttmAE$p.value
  testmat1[3,2] <- ttmAM$p.value
  
  testmat1[1,3] <- ttmAD$conf.int[1:1]
  testmat1[2,3] <- ttmAE$conf.int[1:1]
  testmat1[3,3] <- ttmAM$conf.int[1:1]
  
  testmat1[1,4] <- ttmAD$conf.int[2:2]
  testmat1[2,4] <- ttmAE$conf.int[2:2]
  testmat1[3,4] <- ttmAM$conf.int[2:2]
  
  testmat21 <- matrix(nrow = 3, ncol=4)
  rownames(testmat21) <- c('De Bilt', 'Eelde', 'Maastricht')
  colnames(testmat21) <- c('t-Statistic', 'p-value', 'C.I. Lower', 'C.I. Upper')
  
  cbttmAM <- t.test(preCBY$maastricht, postCBY$maastricht, paired = T)
  cbttmAE <- t.test(preCBY$eelde, postCBY$eelde, paired = T)
  cbttmAD <- t.test(preCBY$de_bilt, postCBY$de_bilt, paired = T)
  
  testmat21[1,1] <- cbttmAD$statistic
  testmat21[2,1] <- cbttmAE$statistic
  testmat21[3,1] <- cbttmAM$statistic
  
  testmat21[1,2] <- cbttmAD$p.value
  testmat21[2,2] <- cbttmAE$p.value
  testmat21[3,2] <- cbttmAM$p.value
  
  testmat21[1,3] <- cbttmAD$conf.int[1:1]
  testmat21[2,3] <- cbttmAE$conf.int[1:1]
  testmat21[3,3] <- cbttmAM$conf.int[1:1]
  
  testmat21[1,4] <- cbttmAD$conf.int[2:2]
  testmat21[2,4] <- cbttmAE$conf.int[2:2]
  testmat21[3,4] <- cbttmAM$conf.int[2:2]
}

  #test for differences in means (monthly)
{
  testmat2 <- matrix(nrow = 3, ncol=4)
  rownames(testmat2) <- c('De Bilt', 'Eelde', 'Maastricht')
  colnames(testmat2) <- c('t-Statistic', 'p-value', 'C.I. Lower', 'C.I. Upper')
  
  ttmMM <- t.test(prebreakM$maastricht, postbreakM$maastricht, paired = T)
  ttmME <- t.test(prebreakM$eelde, postbreakM$eelde, paired = T)
  ttmMD <- t.test(prebreakM$de_bilt, postbreakM$de_bilt, paired = T)
  
  testmat2[1,1] <- ttmAD$statistic
  testmat2[2,1] <- ttmAE$statistic
  testmat2[3,1] <- ttmAM$statistic
  
  testmat2[1,2] <- ttmAD$p.value
  testmat2[2,2] <- ttmAE$p.value
  testmat2[3,2] <- ttmAM$p.value
  
  testmat2[1,3] <- ttmAD$conf.int[1:1]
  testmat2[2,3] <- ttmAE$conf.int[1:1]
  testmat2[3,3] <- ttmAM$conf.int[1:1]
  
  testmat2[1,4] <- ttmAD$conf.int[2:2]
  testmat2[2,4] <- ttmAE$conf.int[2:2]
  testmat2[3,4] <- ttmAM$conf.int[2:2]
  
  testmat23 <- matrix(nrow = 3, ncol=4)
  rownames(testmat23) <- c('De Bilt', 'Eelde', 'Maastricht')
  colnames(testmat23) <- c('t-Statistic', 'p-value', 'C.I. Lower', 'C.I. Upper')
  
  CBttmMM <- t.test(preCBM$maastricht, postCBM$maastricht, paired = T)
  CBttmME <- t.test(preCBM$eelde, postCBM$eelde, paired = T)
  CBttmMD <- t.test(preCBM$de_bilt, postCBM$de_bilt, paired = T)
  
  testmat23[1,1] <- CBttmMD$statistic
  testmat23[2,1] <- CBttmME$statistic
  testmat23[3,1] <- CBttmMM$statistic
  
  testmat23[1,2] <- CBttmMD$p.value
  testmat23[2,2] <- CBttmME$p.value
  testmat23[3,2] <- CBttmMM$p.value
  
  testmat23[1,3] <- CBttmMD$conf.int[1:1]
  testmat23[2,3] <- CBttmME$conf.int[1:1]
  testmat23[3,3] <- CBttmMM$conf.int[1:1]
  
  testmat23[1,4] <- CBttmMD$conf.int[2:2]
  testmat23[2,4] <- CBttmME$conf.int[2:2]
  testmat23[3,4] <- CBttmMM$conf.int[2:2]
  
}
  
  #use subsamples set earlier
{  #meanTable10y
  testmat3 <- matrix(nrow = 10, ncol=6)
  rownames(testmat3) <- 1:10
  colnames(testmat3) <- c('Base Year', 'Comparison Year', 't-Statistic', 'p-value', 'C.I. Lower', 'C.I. Upper')
  
  j = 1
  for (i in 2:11) {
    testmat3[j,1] <- meanTable10y[1,Year]
    testmat3[j,2] <- meanTable10y[i,Year]
    testmat3[j,3] <- t.test(meanTable10y[1,2:4], meanTable10y[i,2:4])$statistic
    testmat3[j,4] <- t.test(meanTable10y[1,2:4], meanTable10y[i,2:4])$p.value
    testmat3[j,5] <- t.test(meanTable10y[1,2:4], meanTable10y[i,2:4])$conf.int[1:1]
    testmat3[j,6] <- t.test(meanTable10y[1,2:4], meanTable10y[i,2:4])$conf.int[2:2]
    j <- j + 1
  }
  
  #meanTable5y
  testmat4 <- matrix(nrow = 21, ncol=6)
  rownames(testmat4) <- 1:21
  colnames(testmat4) <- c('Base Year', 'Comparison Year', 't-Statistic', 'p-value', 'C.I. Lower', 'C.I. Upper')
  
  j = 1
  for (i in 2:22) {
    testmat4[j,1] <- meanTable5y[1,Year]
    testmat4[j,2] <- meanTable5y[i,Year]
    testmat4[j,3] <- t.test(meanTable5y[1,2:4], meanTable5y[i,2:4])$statistic
    testmat4[j,4] <- t.test(meanTable5y[1,2:4], meanTable5y[i,2:4])$p.value
    testmat4[j,5] <- t.test(meanTable5y[1,2:4], meanTable5y[i,2:4])$conf.int[1:1]
    testmat4[j,6] <- t.test(meanTable5y[1,2:4], meanTable5y[i,2:4])$conf.int[2:2]
    j <- j + 1
  }
  
  
  #medianTable10y
  testmat5 <- matrix(nrow = 10, ncol=6)
  rownames(testmat5) <- 1:10
  colnames(testmat5) <- c('Base Year', 'Comparison Year', 't-Statistic', 'p-value', 'C.I. Lower', 'C.I. Upper')
  
  j = 1
  for (i in 2:11) {
    testmat5[j,1] <- medianTable10y[1,Year]
    testmat5[j,2] <- medianTable10y[i,Year]
    testmat5[j,3] <- t.test(medianTable10y[1,2:4], medianTable10y[i,2:4])$statistic
    testmat5[j,4] <- t.test(medianTable10y[1,2:4], medianTable10y[i,2:4])$p.value
    testmat5[j,5] <- t.test(medianTable10y[1,2:4], medianTable10y[i,2:4])$conf.int[1:1]
    testmat5[j,6] <- t.test(medianTable10y[1,2:4], medianTable10y[i,2:4])$conf.int[2:2]
    j <- j + 1
  } 
  
}

#test for homogeneity of variance (annual)
{
  testmat6 <- matrix(nrow = 3, ncol=5)
  rownames(testmat6) <- c('De Bilt', 'Eelde', 'Maastricht')
  colnames(testmat6) <- c('F-Statistic', 'p-value', 'C.I. Lower', 'C.I. Upper', 'Point Est.')
  
  FtmAM <- var.test(prebreakY$maastricht, postbreakY$maastricht)
  FtmAE <- var.test(prebreakY$eelde, postbreakY$eelde)
  FtmAD <- var.test(prebreakY$de_bilt, postbreakY$de_bilt)
  
  testmat6[1,1] <- FtmAD$statistic
  testmat6[2,1] <- FtmAE$statistic
  testmat6[3,1] <- FtmAM$statistic
  
  testmat6[1,5] <- FtmAD$estimate
  testmat6[2,5] <- FtmAE$estimate
  testmat6[3,5] <- FtmAM$estimate
  
  testmat6[1,2] <- FtmAD$p.value
  testmat6[2,2] <- FtmAE$p.value
  testmat6[3,2] <- FtmAM$p.value
  
  testmat6[1,3] <- FtmAD$conf.int[1:1]
  testmat6[2,3] <- FtmAE$conf.int[1:1]
  testmat6[3,3] <- FtmAM$conf.int[1:1]
  
  testmat6[1,4] <- FtmAD$conf.int[2:2]
  testmat6[2,4] <- FtmAE$conf.int[2:2]
  testmat6[3,4] <- FtmAM$conf.int[2:2]
  
  testmat22 <- matrix(nrow = 3, ncol=4)
  rownames(testmat22) <- c('De Bilt', 'Eelde', 'Maastricht')
  colnames(testmat22) <- c('t-Statistic', 'p-value', 'C.I. Lower', 'C.I. Upper')
  
  cbFtmAM <- var.test(preCBY$maastricht, postCBY$maastricht)
  cbFtmAE <- var.test(preCBY$eelde, postCBY$eelde)
  cbFtmAD <- var.test(preCBY$de_bilt, postCBY$de_bilt)
  
  testmat22[1,1] <- cbFtmAD$statistic
  testmat22[2,1] <- cbFtmAE$statistic
  testmat22[3,1] <- cbFtmAM$statistic
  
  testmat22[1,2] <- cbFtmAD$p.value
  testmat22[2,2] <- cbFtmAE$p.value
  testmat22[3,2] <- cbFtmAM$p.value
  
  testmat22[1,3] <- cbFtmAD$conf.int[1:1]
  testmat22[2,3] <- cbFtmAE$conf.int[1:1]
  testmat22[3,3] <- cbFtmAM$conf.int[1:1]
  
  testmat22[1,4] <- cbFtmAD$conf.int[2:2]
  testmat22[2,4] <- cbFtmAE$conf.int[2:2]
  testmat22[3,4] <- cbFtmAM$conf.int[2:2]
  
}


#test for homogeneity of variance (monthly)
{
  testmat7 <- matrix(nrow = 3, ncol=5)
  rownames(testmat7) <- c('De Bilt', 'Eelde', 'Maastricht')
  colnames(testmat7) <- c('F-Statistic', 'p-value', 'C.I. Lower', 'C.I. Upper', 'Point Est.')
  
  FtmMM <- var.test(prebreakM$maastricht, postbreakM$maastricht)
  FtmME <- var.test(prebreakM$eelde, postbreakM$eelde)
  FtmMD <- var.test(prebreakM$de_bilt, postbreakM$de_bilt)
  
  testmat7[1,1] <- FtmMD$statistic
  testmat7[2,1] <- FtmME$statistic
  testmat7[3,1] <- FtmMM$statistic
  
  testmat7[1,5] <- FtmMD$estimate
  testmat7[2,5] <- FtmME$estimate
  testmat7[3,5] <- FtmMM$estimate
  
  testmat7[1,2] <- FtmMD$p.value
  testmat7[2,2] <- FtmME$p.value
  testmat7[3,2] <- FtmMM$p.value
  
  testmat7[1,3] <- FtmMD$conf.int[1:1]
  testmat7[2,3] <- FtmME$conf.int[1:1]
  testmat7[3,3] <- FtmMM$conf.int[1:1]
  
  testmat7[1,4] <- FtmMD$conf.int[2:2]
  testmat7[2,4] <- FtmME$conf.int[2:2]
  testmat7[3,4] <- FtmMM$conf.int[2:2]
  
  testmat24 <- matrix(nrow = 3, ncol=5)
  rownames(testmat24) <- c('De Bilt', 'Eelde', 'Maastricht')
  colnames(testmat24) <- c('F-Statistic', 'p-value', 'C.I. Lower', 'C.I. Upper', 'Point Est.')
  
  CBFtmMM <- var.test(preCBM$maastricht, postCBM$maastricht)
  CBFtmME <- var.test(preCBM$eelde, postCBM$eelde)
  CBFtmMD <- var.test(preCBM$de_bilt, postCBM$de_bilt)
  
  testmat24[1,1] <- CBFtmMD$statistic
  testmat24[2,1] <- CBFtmME$statistic
  testmat24[3,1] <- CBFtmMM$statistic
  
  testmat24[1,5] <- CBFtmMD$estimate
  testmat24[2,5] <- CBFtmME$estimate
  testmat24[3,5] <- CBFtmMM$estimate
  
  testmat24[1,2] <- CBFtmMD$p.value
  testmat24[2,2] <- CBFtmME$p.value
  testmat24[3,2] <- CBFtmMM$p.value
  
  testmat24[1,3] <- CBFtmMD$conf.int[1:1]
  testmat24[2,3] <- CBFtmME$conf.int[1:1]
  testmat24[3,3] <- CBFtmMM$conf.int[1:1]
  
  testmat24[1,4] <- CBFtmMD$conf.int[2:2]
  testmat24[2,4] <- CBFtmME$conf.int[2:2]
  testmat24[3,4] <- CBFtmMM$conf.int[2:2]
  
}

#manual tests
{
  #manual Break test
  
  #manual CB
  
}


#simple OLS
OLS <- function(x,y){
  beta <- t(x - mean(x)) %*% (y - mean(y)) / crossprod(x - mean(x))
  alpha <- mean(y) - beta * mean(x)
  return(c(alpha, beta))
}
testmat6

OLS(da$Maastricht, da$DeBilt)

#confidence interval 95%
CI <- function(n, x, m, sd){
  upperbound <- (qnorm(0.975)*sd)/sqrt(n)+x
  lowerbound <- -(qnorm(0.975)*sd)/sqrt(n)+x
  return(c(lowerbound, upperbound))
}

if (Sys.info()[7] == "ts") {
  #credit OSS authors
  knitr::write_bib(c(.packages()),
 "/Users/ts/Dropbox/Apps/Overleaf/Project Mathematical Statistics/packages.bib")

  grateful::cite_packages(output = "paragraph", dependencies = T, include.RStudio = T, 
                out.dir = "/Users/ts/Dropbox/Apps/Overleaf/Project Mathematical Statistics/",
                bib.file = "grateful.bib")

########################Do Plots & Tables################################
invisible(source("Tidy.R")) 
invisible(source("Plots.R"))
invisible(source("Tables.R"))
########################R File########################
file.copy('TeamProject.R', '/Users/ts/Dropbox/Apps/Overleaf/Project Mathematical Statistics/', overwrite = T)
}
