#####################
#Exercise 3.5
#import csv
data <- read.csv("~/R projects/Mathematical_Stats/Data/data.csv", header = TRUE, sep = ";", quote = "\"'", dec = ",", fill = TRUE, comment.char="")
save(data, file = "TempData.Rdata")
colnames(data) <- c("Year", "Eelde", "De Bilt", "Maastricht") 
# For each of the three series, calculate the overall sample mean and ten-year averages.
# Analyse your outcomes. Under which conditions are the calculated quantities equal to maximum likelihood estimators? Explain.

mean(data$Eelde) #col3
mean(data$De.Bilt) #col2
mean(data$Maastricht) #col4
mean(data[1:10, 2])

tenYearMeans <- function(colnum, startrow) {
  i <- startrow
  j <- as.numeric(colnum)
  
  return(mean(data[i:i+10, j]))
}


decadeIndex <- seq(from =0, to=120, by=10)
meanMat <- matrix(0, length(decadeIndex), 3)


tenYearMeans(2, 20)
for(i in 2:4) {
  for (j in 1:length(decadeIndex)) {
    meanMat[j,i-1] <-tenYearMeans(i, j)
  }
}

for(i in c(1906, 1916,1926,1936,1946,1956,1966,1976,1986,1996,2006)){
  print(mean(data$De.Bilt[data$Date > i & data$Date < i+10]))
}

for(i in c(1906, 1916,1926,1936,1946,1956,1966,1976,1986,1996,2006)){
  print(mean(data$Eelde[data$Date > i & data$Date < i+10]))
}

for(i in c(1906, 1916,1926,1936,1946,1956,1966,1976,1986,1996,2006)){
  print(mean(data$Maastricht[data$Date > i & data$Date < i+10]))
}


zoo::rollapply(data$De.Bilt, 10, mean)
zoo::rollmean(data$De.Bilt, 10)

#3.5 b)

median(data$De.Bilt)
sd(data$De.Bilt)
var(data$De.Bilt)

median(data$Eelde)
sd(data$Eelde)
var(data$Eelde)

median(data$Maastricht)
sd(data$Maastricht)
var(data$Maastricht)

#3.5 c)

plot(data$Maastricht, data$De.Bilt)
cor(data$Maastricht, data$De.Bilt)
fit = lm(data$Maastricht ~ data$De.Bilt)
fit

plot(data$De.Bilt, data$Maastricht)
cor(data$De.Bilt, data$Maastricht)
fit = lm(data$De.Bilt ~ data$Maastricht)
fit

#3.5 d)

x = data$De.Bilt
y = data$Maastricht

beta_estimate1 = cov(x,y)/var(x)
beta_estimate1

alpha_estimate1 = mean(y) - beta_estimate1*mean(x)
alpha_estimate1

beta_estimate2 = cov(y,x)/var(y)
beta_estimate2

alpha_estimate2 = mean(x) - beta_estimate2*mean(y)
alpha_estimate2

#3.5 e)

plot(data$Maastricht, data$Eelde)
cor(data$Maastricht, data$Eelde)
fit = lm(data$Maastricht ~ data$Eelde)
fit

plot(data$Eelde, data$Maastricht)
cor(data$Maastricht, data$Eelde)
fit = lm(data$Eelde ~ data$Maastricht)
fit 

plot(data$De.Bilt, data$Eelde)
cor(data$De.Bilt, data$Eelde)
fit = lm(data$De.Bilt ~ data$Eelde)
fit

plot(data$Eelde, data$De.Bilt)
cor(data$De.Bilt, data$Eelde)
fit = lm(data$Eelde ~ data$De.Bilt)
fit
