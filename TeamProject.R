#########################HOUSEKEEPING########################

rm(list = ls(all = TRUE)) ###CLEAR ALL
# Package names
packages <- c("data.table", "dplyr", "zoo", "tidyr", "ggplot2", "tidyverse", "xtable", "knitr")

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
da <- fread('./Data/AnnualTemp.csv')
colnames(da) <- c('year', 'de_bilt', 'eelde', 'maastricht')
#monthly
dm <- fread('./Data/MonthlyTemp.csv')
colnames(dm) <- c('month', 'de_bilt', 'eelde', 'maastricht')

#smoothed
dms <- fread('./Data/SMTemp.csv')
colnames(dms) <- c('month', 'de_bilt', 'eelde', 'maastricht')

#daily
dd <- fread('./Data/DailyTemp.csv')
colnames(dd) <- c('date', 'de_bilt', 'eelde', 'maastricht')
########################DATA ANALYSIS########################

#Split the sample in a number of subsamples, and 
#compare average temperatures across the subsamples.
#non-overlapping subsamples

#tenyearmeans <- da[, .(meanMST = mean(maastricht) )]




