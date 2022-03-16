#########################HOUSEKEEPING########################

rm(list = ls(all = TRUE)) ###CLEAR ALL
# Package names
packages <- c("data.table", "dplyr", "zoo", "tidyr", "ggplot2", "ggthemes", 
              "tidyverse", "xtable", "knitr", "stargazer", "remotes")
# package grateful must be installed by hand# install.packages("remotes")
remotes::install_github("Pakillo/grateful")
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
  ][, md := format(as.Date(DateCol), "%m%d")]
  
  compSet <- subset[mmdd == md, .(Date = md, eelde, de_bilt, maastricht)]
  return(compSet)
}

subsetDateLong <- function(mmdd){
  mmdd <- str_pad(as.character(mmdd), 4, side = "left", pad = '0')
  
  subset <- dd[, DateCol := as.Date(as.character(date), format = "%Y%m%d")
  ][, md := format(as.Date(DateCol), "%m%d")]
  
  compSet <- subset[mmdd == md, .(Date = md, Eelde = eelde, De.Bilt = de_bilt, Maastricht = maastricht)]
  ret <- melt(compSet, id.vars = "Date", measure.vars = c("Maastricht", "Eelde", "De.Bilt"),
              variable.factor = T, variable.name = "City", value.name = "Temperature")[, Citymean := mean(Temperature)]
  return(ret)
}

march15 <- subsetDate(315)
april <- subsetMonthLong(4)
september <- subsetMonthLong(9)
february <- subsetMonth(2)

rollingMean10_5 <- xYearYoverlapStat(10, 5, mean)

meanTable10y <- xYearStat(10, mean)
meanTable5y <- xYearStat(5, mean)
meanTable50y <- xYearStat(50, mean)
medianTable10y <- xYearStat(10, median)

meanTable10mo <- xMonthStat(10, mean)
meanTable10mo <- xMonthStat(10, mean)
medianTable5mo <- xMonthStat(5, median)

meanTable20d <- xDayStat(20, mean)

if (Sys.info()[7] == "ts") {
  #credit OSS authors
  knitr::write_bib(c(.packages()),
 "/Users/ts/Dropbox/Apps/Overleaf/Project Mathematical Statistics/packages.bib")

  grateful::cite_packages(output = "paragraph", dependencies = T, include.RStudio = T, 
                out.dir = "/Users/ts/Dropbox/Apps/Overleaf/Project Mathematical Statistics/",
                bib.file = "grateful.bib")

###############################################################
##############################tidy#############################
daC <- da[, .(Maastricht = maastricht, Eelde = eelde, De.Bilt = de_bilt, Year = year)]
daLong <- melt(daC, id.vars = c("Year"), measure.vars = c("Maastricht", "Eelde", "De.Bilt"),
               variable.factor = T, variable.name = "City", value.name = "Temperature")
citymeanA <- daLong[, Citymean := mean(Temperature), by = City]

dmsC <- dms[, .(Maastricht = maastricht, Eelde = eelde, De.Bilt = de_bilt, Month = month )]
dmsLong <- melt(dmsC, id.vars = c("Month"), measure.vars = c("Maastricht", "Eelde", "De.Bilt"),
               variable.factor = T, variable.name = "City", value.name = "Temperature")
citymeanMS <- dmsLong[, Citymean := mean(Temperature), by = City]

ddC <- dd[, .(Maastricht = maastricht, Eelde = eelde, De.Bilt = de_bilt, Date = date )]
ddLong <- melt(ddC, id.vars = c("Date"), measure.vars = c("Maastricht", "Eelde", "De.Bilt"),
                variable.factor = T, variable.name = "City", value.name = "Temperature")
citymeanD <- ddLong[, Citymean := mean(Temperature), by = City]

rollC <- rollingMean10_5
rollingL <- melt(rollC, id.vars = c("Year"), measure.vars = c("Maastricht", "Eelde", "De.Bilt"),
                 variable.factor = T, variable.name = "City", value.name = "Temperature")


########################Plots########################
#densities

densplotyears <- ggplot(daLong, aes(x = Temperature, color = City)) + geom_density() +
  geom_vline(data=citymeanA, aes(xintercept = Citymean, color = City), linetype = "dashed") +
            theme_minimal() + ylab("Density") + ggtitle("Annual Mean Temperatures") +
  theme( panel.grid.minor = element_blank(), plot.title = element_text(hjust = 0.5)) 


  ggsave("AD.png",  bg = "white", dpi = "retina", width = 20, height = 10, units = "cm",
  path = "/Users/ts/Dropbox/Apps/Overleaf/Project Mathematical Statistics/Figures")

aprilPlot <- ggplot(april, aes(x = Temperature, color = City)) + geom_density() +
  geom_vline(aes(xintercept = Citymean, color = City), linetype = "dashed") +
  theme_minimal() + ylab("Density")+ ggtitle("Mean Temperatures in April") +
  theme( panel.grid.minor = element_blank(), plot.title = element_text(hjust = 0.5)) 

ggsave("aprilD.png",  bg = "white", dpi = "retina", width = 20, height = 10, units = "cm",
       path = "/Users/ts/Dropbox/Apps/Overleaf/Project Mathematical Statistics/Figures")


septemberplot <- ggplot(september, aes(x = Temperature, color = City)) + geom_density() +
  geom_vline(aes(xintercept = Citymean, color = City), linetype = "dashed") +
  theme_minimal() + ylab("Density")+ ggtitle("Mean Temperatures in September") +
  theme( panel.grid.minor = element_blank(), plot.title = element_text(hjust = 0.5))

ggsave("septemberD.png",  bg = "white", dpi = "retina", width = 20, height = 10, units = "cm",
       path = "/Users/ts/Dropbox/Apps/Overleaf/Project Mathematical Statistics/Figures")

densplotmonthsS <- ggplot(dmsLong, aes(x = Temperature, color = City)) + geom_density() +
  geom_vline(data=citymeanMS, aes(xintercept = Citymean, color = City), linetype = "dashed") +
  theme_minimal() + ylab("Density")+ ggtitle("Monthly Mean Temperatures") +
  theme( panel.grid.minor = element_blank(), plot.title = element_text(hjust = 0.5)) 

  ggsave("MD.png",  bg = "white", dpi = "retina", width = 20, height = 10, units = "cm",
   path = "/Users/ts/Dropbox/Apps/Overleaf/Project Mathematical Statistics/Figures")

densplotDays <- ggplot(ddLong, aes(x = Temperature, color = City)) + geom_density() +
    geom_vline(data=citymeanD, aes(xintercept = Citymean, color = City), linetype = "dashed") +
    theme_minimal() + ylab("Density")+ ggtitle("Daily Mean Temperatures") +
    theme( panel.grid.minor = element_blank(), plot.title = element_text(hjust = 0.5)) 
  
  ggsave("DD.png",  bg = "white", dpi = "retina", width = 20, height = 10, units = "cm",
         path = "/Users/ts/Dropbox/Apps/Overleaf/Project Mathematical Statistics/Figures")
  
densplotRoll <- ggplot(rollingL, aes(x = Temperature, color = City))+ geom_density() +
  theme_minimal() + ylab("Density")+ ggtitle("Rolling 5 Year Window 10 Year Mean Temperatures") +
  theme( panel.grid.minor = element_blank(), plot.title = element_text(hjust = 0.5))

ggsave("10_5D.png",  bg = "white", dpi = "retina", width = 20, height = 10, units = "cm",
       path = "/Users/ts/Dropbox/Apps/Overleaf/Project Mathematical Statistics/Figures")


########################Tables########################
daSS <- daC[,.("De Bilt" = De.Bilt, Eelde, Maastricht)]
stargazer(daSS, out.header = F, title = "Annual Data",
                    out = "/Users/ts/Dropbox/Apps/Overleaf/Project Mathematical Statistics/Tables/AS" )


dmsSS <- dmsC[,.("De Bilt" = De.Bilt, Eelde, Maastricht)]
stargazer(dmsSS, out.header = F, title = "Smoothed Monthly Data",
          out = "/Users/ts/Dropbox/Apps/Overleaf/Project Mathematical Statistics/Tables/MS" )

ddSS <- ddC[,.("De Bilt" = De.Bilt, Eelde, Maastricht)]
stargazer(ddSS, out.header = F, title = "Daily Data",
          out = "/Users/ts/Dropbox/Apps/Overleaf/Project Mathematical Statistics/Tables/DS" )

}
                    