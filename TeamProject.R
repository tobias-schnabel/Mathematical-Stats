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
  meanTable10mo <- xMonthStat(10, mean)
  medianTable5mo <- xMonthStat(5, median)
  meanTable20d <- xDayStat(20, mean)
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
postbreakY <- da[year > 1961]
prebreakM <- dm[month <= 196210]
postbreakM <- dm[month > 196210]

#test for differences in means/medians/vars
testmat1 <- matrix(nrow = 3, ncol=2)
rownames(testmat1) <- c('De Bilt', 'Eelde', 'Maastricht')
colnames(testmat1) <- c('t-Statistic', 'p-value')



#simple OLS
OLS <- function(x,y){
  beta <- t(x - mean(x)) %*% (y - mean(y)) / crossprod(x - mean(x))
  alpha <- mean(y) - beta * mean(x)
  return(c(alpha, beta))
}

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
