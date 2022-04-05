##gert required subsets
#4-way plot subsets
march <- subsetMonthLong(3)
june <- subsetMonthLong(6)
september <- subsetMonthLong(9)
december <- subsetMonthLong(12)

#rolling window plots
rollingMean10_5 <- xYearYoverlapStat(10, 5, mean)
rollingMean20_10 <- xYearYoverlapStat(20, 10, mean)

#date plot
april7 <- subsetDateLong(407)
########################Plots########################
#raw data
TSA <- ggplot(daLong, aes(x = Year, y = Temperature)) +
  geom_line(aes(color = City)) + labs(y = 'Temperature', x = 'Year') +
  theme_minimal() + scale_color_tableau() + 
  theme( panel.grid.minor = element_blank(), plot.title = element_text(hjust = 0.5)) +
  xlim(1905, 2025) + ggtitle("Annual Data")

ggsave("TSA.png",  bg = "white", dpi = "retina", width = 20, height = 10, units = "cm",
       path = "/Users/ts/Dropbox/Apps/Overleaf/Project Mathematical Statistics/Figures/Raw")

TSABP <- ggplot(daLong, aes(x = Year, y = Temperature)) +
  geom_line(aes(color = City)) + labs(y = 'Temperature', x = 'Year') +
  theme_minimal() + scale_color_tableau() + geom_vline(xintercept = 1961, linetype = 'dashed', colour = '#76B7B2') +
  theme( panel.grid.minor = element_blank(), plot.title = element_text(hjust = 0.5)) +
  xlim(1945, 1975) + ggtitle("Annual Data: Breakpoint")

ggsave("TSA_BP.png",  bg = "white", dpi = "retina", width = 20, height = 10, units = "cm",
       path = "/Users/ts/Dropbox/Apps/Overleaf/Project Mathematical Statistics/Figures/Raw")

TS4_7 <- ggplot(april7, aes(x = Date, y = Temperature, xmin = as.Date("1907-01-01", "%Y-%m-%d"), xmax = as.Date("2023-01-01", "%Y-%m-%d"))) +
  geom_line(aes(color = City)) + labs(y = 'Temperature', x = 'Year') +
  theme_minimal() + scale_color_tableau() +
  theme( panel.grid.minor = element_blank(), panel.grid.major = element_blank(), plot.title = element_text(hjust = 0.5), legend.position = "bottom") +
  ggtitle("Temperatures on April 7") +
  geom_text(aes(as.Date("2022-04-07"), 11), label=10, colour = '#4E79A7') + #maastricht
  geom_text(aes(as.Date("2022-04-07"), 9), label = 9,  colour = '#F28E2B') + #eelde
  geom_text(aes(as.Date("2022-04-07"), 10), label = 9,  colour = '#E15759')  #debilt

ggsave("TS4_7.png",  bg = "white", dpi = "retina", width = 20, height = 12, units = "cm",
       path = "/Users/ts/Dropbox/Apps/Overleaf/Project Mathematical Statistics/Figures/Raw")

#red E15759
#blue 4E79A7
#orange F28E2B

# dat <- dmLong[, Month := yearmon(Month)]
TSM <- ggplot(dmLong, aes(x = Month, y = Temperature)) +
  geom_line(aes(color = City)) + labs(y = 'Temperature', x = 'Year') +
  theme_minimal() + scale_color_tableau() + 
  theme( panel.grid.minor = element_blank(), plot.title = element_text(hjust = 0.5)) +
  xlim(190501, 202501) + ggtitle("Monthly Data")

ggsave("TSM.png",  bg = "white", dpi = "retina", width = 20, height = 10, units = "cm",
       path = "/Users/ts/Dropbox/Apps/Overleaf/Project Mathematical Statistics/Figures/Raw")

TSMS <- ggplot(dmsLong, aes(x = Month, y = Temperature)) +
  geom_line(aes(color = City)) + labs(y = 'Temperature', x = 'Year') +
  theme_minimal() + scale_color_tableau() + 
  theme( panel.grid.minor = element_blank(), plot.title = element_text(hjust = 0.5)) +
  xlim(190501, 202501) + ggtitle("Smoothed Monthly Data")

ggsave("TSMS.png",  bg = "white", dpi = "retina", width = 20, height = 10, units = "cm",
       path = "/Users/ts/Dropbox/Apps/Overleaf/Project Mathematical Statistics/Figures/Raw")


#normality
qqY <-  ggplot(daLong, aes(sample = Temperature)) +
  stat_qq(aes(color = City)) +
  labs(y = "Weight", x = "Theoretical") +theme_minimal() + ggtitle("Annual Data Q-Q Plot") +
  theme( panel.grid.minor = element_blank(), plot.title = element_text(hjust = 0.5)) +
  scale_color_tableau()


ggsave("qqY.png",  bg = "white", dpi = "retina", width = 20, height = 10, units = "cm",
       path = "/Users/ts/Dropbox/Apps/Overleaf/Project Mathematical Statistics/Figures/Raw")

qqM <-  ggplot(dmLong, aes(sample = Temperature)) +
  stat_qq(aes(color = City)) +
  labs(y = "Weight", x = "Theoretical") +theme_minimal() + ggtitle("Monthly Data Q-Q Plot") +
  theme( panel.grid.minor = element_blank(), plot.title = element_text(hjust = 0.5)) +
  scale_color_tableau()


ggsave("qqM.png",  bg = "white", dpi = "retina", width = 20, height = 10, units = "cm",
       path = "/Users/ts/Dropbox/Apps/Overleaf/Project Mathematical Statistics/Figures/Raw")

qqMs <-  ggplot(dmsLong, aes(sample = Temperature)) +
  stat_qq(aes(color = City)) +
  labs(y = "Weight", x = "Theoretical") +theme_minimal() + ggtitle("Smoothed Monthly Data Q-Q Plot") +
  theme( panel.grid.minor = element_blank(), plot.title = element_text(hjust = 0.5)) +
  scale_color_tableau()


ggsave("qqMs.png",  bg = "white", dpi = "retina", width = 20, height = 10, units = "cm",
       path = "/Users/ts/Dropbox/Apps/Overleaf/Project Mathematical Statistics/Figures/Raw")

qqD <-  ggplot(ddLong, aes(sample = Temperature)) +
  stat_qq(aes(color = City)) +
  labs(y = "Weight", x = "Theoretical") +theme_minimal() + ggtitle("Daily Data Q-Q Plot") +
  theme( panel.grid.minor = element_blank(), plot.title = element_text(hjust = 0.5)) +
  scale_color_tableau()


ggsave("qqD.png",  bg = "white", dpi = "retina", width = 20, height = 10, units = "cm",
       path = "/Users/ts/Dropbox/Apps/Overleaf/Project Mathematical Statistics/Figures/Raw")

#histograms
histY <- ggplot(daLong, aes(x = Temperature, color = City)) + 
  geom_histogram(fill="white", alpha=0.5, position="dodge", bins=45) +
  geom_vline(data=citymeanA, aes(xintercept = Citymean, color = City), linetype = "dashed") +
  theme_minimal() + ylab("Density") + ggtitle("Annual Mean Temperatures") +
  theme( panel.grid.minor = element_blank(), plot.title = element_text(hjust = 0.5)) +
  scale_color_tableau()

ggsave("Ahist.png",  bg = "white", dpi = "retina", width = 20, height = 10, units = "cm",
       path = "/Users/ts/Dropbox/Apps/Overleaf/Project Mathematical Statistics/Figures/Raw")


histM <- ggplot(dmLong, aes(x = Temperature, color = City)) + 
  geom_histogram(fill="white", alpha=0.5, position="dodge", bins=45) +
  geom_vline(data=citymeanM, aes(xintercept = Citymean, color = City), linetype = "dashed") +
  theme_minimal() + ylab("Density") + ggtitle("Monthly Mean Temperatures") +
  theme( panel.grid.minor = element_blank(), plot.title = element_text(hjust = 0.5)) +
  scale_color_tableau()

ggsave("Mhist.png",  bg = "white", dpi = "retina", width = 20, height = 10, units = "cm",
       path = "/Users/ts/Dropbox/Apps/Overleaf/Project Mathematical Statistics/Figures/Raw")


#densities

densplotyears <- ggplot(daLong, aes(x = Temperature, color = City)) + geom_density() +
  geom_vline(data=citymeanA, aes(xintercept = Citymean, color = City), linetype = "dashed") +
  theme_minimal() + ylab("Density") + ggtitle("Annual Mean Temperatures") +
  theme( panel.grid.minor = element_blank(), plot.title = element_text(hjust = 0.5)) +
  scale_color_tableau()


ggsave("AD.png",  bg = "white", dpi = "retina", width = 20, height = 10, units = "cm",
       path = "/Users/ts/Dropbox/Apps/Overleaf/Project Mathematical Statistics/Figures/Raw")

densplotmonths <- ggplot(dmLong, aes(x = Temperature, color = City)) + geom_density() +
  geom_vline(data=citymeanM, aes(xintercept = Citymean, color = City), linetype = "dashed") +
  theme_minimal() + ylab("Density")+ ggtitle("Monthly Mean Temperatures") +
  theme( panel.grid.minor = element_blank(), plot.title = element_text(hjust = 0.5)) +
  scale_color_tableau()

ggsave("MD.png",  bg = "white", dpi = "retina", width = 20, height = 10, units = "cm",
       path = "/Users/ts/Dropbox/Apps/Overleaf/Project Mathematical Statistics/Figures/Raw")


densplotmonthsS <- ggplot(dmsLong, aes(x = Temperature, color = City)) + geom_density() +
  geom_vline(data=citymeanMS, aes(xintercept = Citymean, color = City), linetype = "dashed") +
  theme_minimal() + ylab("Density")+ ggtitle("Smoothed Monthly Mean Temperatures") +
  theme( panel.grid.minor = element_blank(), plot.title = element_text(hjust = 0.5)) +
  scale_color_tableau()

ggsave("MSD.png",  bg = "white", dpi = "retina", width = 20, height = 10, units = "cm",
       path = "/Users/ts/Dropbox/Apps/Overleaf/Project Mathematical Statistics/Figures/Raw")

densplotDays <- ggplot(ddLong, aes(x = Temperature, color = City)) + geom_density() +
  geom_vline(data=citymeanD, aes(xintercept = Citymean, color = City), linetype = "dashed") +
  theme_minimal() + ylab("Density")+ ggtitle("Daily Mean Temperatures") +
  theme( panel.grid.minor = element_blank(), plot.title = element_text(hjust = 0.5)) +
  scale_color_tableau()

ggsave("DD.png",  bg = "white", dpi = "retina", width = 20, height = 10, units = "cm",
       path = "/Users/ts/Dropbox/Apps/Overleaf/Project Mathematical Statistics/Figures")

densplotRoll10_5 <- ggplot(rolling10_5L, aes(x = Temperature, color = City))+ geom_density() +
  theme_minimal() + ylab("Density")+ ggtitle("Rolling 5 Year Window 10 Year Mean Temperatures") +
  theme( panel.grid.minor = element_blank(), plot.title = element_text(hjust = 0.5))+
  scale_color_tableau()

ggsave("10_5D.png",  bg = "white", dpi = "retina", width = 20, height = 10, units = "cm",
       path = "/Users/ts/Dropbox/Apps/Overleaf/Project Mathematical Statistics/Figures/Raw")

densplotRoll20_10 <- ggplot(rolling20_10L, aes(x = Temperature, color = City))+ geom_density() +
  theme_minimal() + ylab("Density")+ ggtitle("Rolling 10 Year Window 20 Year Mean Temperatures") +
  theme( panel.grid.minor = element_blank(), plot.title = element_text(hjust = 0.5))+
  scale_color_tableau()

ggsave("20_10D.png",  bg = "white", dpi = "retina", width = 20, height = 10, units = "cm",
       path = "/Users/ts/Dropbox/Apps/Overleaf/Project Mathematical Statistics/Figures/Raw")


marchD2 <- ggplot(march, aes(x = Temperature, color = City)) + geom_density() +
  geom_vline(aes(xintercept = Citymean, color = City), linetype = "dashed") +
  theme_minimal() + ylab("Density")+ ggtitle("March") +
  theme( panel.grid.minor = element_blank(), plot.title = element_text(hjust = 0.5, size = 12), legend.position="none") +
  scale_color_tableau()

juneD2 <- ggplot(june, aes(x = Temperature, color = City)) + geom_density() +
  geom_vline(aes(xintercept = Citymean, color = City), linetype = "dashed") +
  theme_minimal() + ylab("Density")+ ggtitle("June") +
  theme( panel.grid.minor = element_blank(), plot.title = element_text(hjust = 0.5, size = 12), legend.position="none") +
  scale_color_tableau()

septemberD2 <- ggplot(september, aes(x = Temperature, color = City)) + geom_density() +
  geom_vline(aes(xintercept = Citymean, color = City), linetype = "dashed") +
  theme_minimal() + ylab("Density")+ ggtitle("September") +
  theme( panel.grid.minor = element_blank(), plot.title = element_text(hjust = 0.5, size = 12), legend.position="none") +
  scale_color_tableau()

decemberD2 <- ggplot(december, aes(x = Temperature, color = City)) + geom_density() +
  geom_vline(aes(xintercept = Citymean, color = City), linetype = "dashed") +
  theme_minimal() + ylab("Density")+ ggtitle("December") +
  theme( panel.grid.minor = element_blank(), plot.title = element_text(hjust = 0.5, size = 12), legend.position="none") +
  scale_color_tableau()

fourwayplot <- marchD2 + juneD2 + septemberD2 + decemberD2 + 
  plot_layout(guides = "collect") & theme(legend.position = "bottom") 

fourwayplot <-  fourwayplot +  plot_annotation(title = 'Mean Temperatures in Different Months',
                                               caption = 'Means computed individually',
                                               theme = theme(plot.title = element_text(hjust = 0.5))
) 
#, family="Times New Roman"

ggsave("4wayD.png",  bg = "white", dpi = "retina", width = 20, height = 15, units = "cm",
       path = "/Users/ts/Dropbox/Apps/Overleaf/Project Mathematical Statistics/Figures/Raw")


#test stats for 5y mean tables
#graph building blocks

hyptestdatw <- as.data.table(cbind(seq(from=1916, to=2016, by = 5), testmat4[,4])) 
colnames(hyptestdatw) <- c("Year", "p-values")

hyptestdat <- melt(hyptestdatw, id.vars = c("Year"), measure.vars = "p-values", variable.factor = F)

hyptestplot <- ggplot(hyptestdat, aes(x= hyptestdat$Year, y= hyptestdat$value)) + 
  geom_line(colour = '#E15759') +
  geom_hline(yintercept = 0.05, colour = '#4E79A7') + 
  geom_text(aes(2000, 0.05, label = 0.05, vjust = -0.5, colour = '#4E79A7')) +
  geom_hline(yintercept = 0.1, colour = '#F28E2B') + 
  geom_text(aes(2000, 0.1, label = 0.1, vjust = -0.5, colour = '#F28E2B')) +
  labs(y = 'p-values', x = 'Year') +
  theme_minimal() + scale_color_tableau() +
  theme( panel.grid.minor = element_blank(), plot.title = element_text(hjust = 0.5), legend.position = "none") +
  ggtitle("t-test Significance Levels, t-tests on 5-year Means, Base Year 1911")

ggsave("hyptestplot.png",  bg = "white", dpi = "retina", width = 20, height = 15, units = "cm",
       path = "/Users/ts/Dropbox/Apps/Overleaf/Project Mathematical Statistics/Figures")

##regression plots
#red E15759
#blue 4E79A7
#orange F28E2B
regPlotAllY <- ggplot(daLong, aes(x = Year, y = Temperature)) +
  geom_point(aes(colour = City)) + labs(y = 'Temperature', x = 'Year') +
  theme_minimal() + scale_color_tableau() + 
  theme( panel.grid.minor = element_blank(), plot.title = element_text(hjust = 0.5)) +
  xlim(1905, 2025) + ggtitle("Yearly Data") 

ggsave("regYD.png",  bg = "white", dpi = "retina", width = 20, height = 15, units = "cm",
       path = "/Users/ts/Dropbox/Apps/Overleaf/Project Mathematical Statistics/Figures/Regs")

regYE <- ggplot(filter(daLong, City == 'Eelde'), aes(x = Year, y = Temperature)) +
  geom_point(colour = '#F28E2B') + labs(y = 'Temperature', x = 'Year') +
  theme_minimal() + scale_color_tableau() + 
  theme( panel.grid.minor = element_blank(), plot.title = element_text(hjust = 0.5)) +
  xlim(1905, 2025) + ggtitle("Regression of Year on Temperature in Eelde") +
  geom_smooth(method='lm', color = '#499894', fill = '#86BCB6')

ggsave("regYE.png",  bg = "white", dpi = "retina", width = 20, height = 15, units = "cm",
       path = "/Users/ts/Dropbox/Apps/Overleaf/Project Mathematical Statistics/Figures/Regs")

regYD <- ggplot(filter(daLong, City == 'De.Bilt'), aes(x = Year, y = Temperature)) +
  geom_point(colour = '#E15759') + labs(y = 'Temperature', x = 'Year') +
  theme_minimal() + scale_color_tableau() + 
  theme( panel.grid.minor = element_blank(), plot.title = element_text(hjust = 0.5)) +
  xlim(1905, 2025) + ggtitle("Regression of Year on Temperature in De Bilt") +
  geom_smooth(method='lm', color = '#499894', fill = '#86BCB6')

ggsave("regYD.png",  bg = "white", dpi = "retina", width = 20, height = 15, units = "cm",
       path = "/Users/ts/Dropbox/Apps/Overleaf/Project Mathematical Statistics/Figures/Regs")

regYM <- ggplot(filter(daLong, City == 'Maastricht'), aes(x = Year, y = Temperature)) +
  geom_point(colour = '#4E79A7') + labs(y = 'Temperature', x = 'Year') +
  theme_minimal() + scale_color_tableau() + 
  theme( panel.grid.minor = element_blank(), plot.title = element_text(hjust = 0.5)) +
  xlim(1905, 2025) + ggtitle("Regression of Year on Temperature in Maastricht") +
  geom_smooth(method='lm', color = '#499894', fill = '#86BCB6')

ggsave("regYM.png",  bg = "white", dpi = "retina", width = 20, height = 15, units = "cm",
       path = "/Users/ts/Dropbox/Apps/Overleaf/Project Mathematical Statistics/Figures/Regs")

allRegsY <- ggplot(daLong, aes(x = Year, y = Temperature)) +
  geom_point(aes(colour = City)) + facet_wrap(vars(City), nrow = 3) + 
  theme_minimal() + scale_color_tableau() + 
  theme(panel.grid.minor = element_blank(), plot.title = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0.5), legend.position = 'none') +
  geom_smooth(method='lm', color = '#499894', fill = '#86BCB6') +
  labs(y = 'Temperature', x = 'Year', title = "Regressions using Yearly Data",
       caption = "95% C.I. shown around fitted regression lines")

ggsave("AllRegsY.png",  bg = "white", dpi = "retina", width = 20, height = 15, units = "cm",
       path = "/Users/ts/Dropbox/Apps/Overleaf/Project Mathematical Statistics/Figures/Regs")

allRegsM <- ggplot(dmLong, aes(x = Month, y = Temperature)) +
  geom_point(aes(colour = City)) + facet_wrap(vars(City), nrow = 3) + 
  theme_minimal() + scale_color_tableau() + 
  theme(panel.grid.minor = element_blank(), plot.title = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0.5), legend.position = 'none') +
  geom_smooth(method='lm', color = '#499894', fill = '#86BCB6') +
  labs(y = 'Temperature', x = 'Year', title = "Regressions using Mothly Data",
       caption = "95% C.I. shown around fitted regression lines")

ggsave("AllRegsM.png",  bg = "white", dpi = "retina", width = 20, height = 15, units = "cm",
       path = "/Users/ts/Dropbox/Apps/Overleaf/Project Mathematical Statistics/Figures/Regs")

allRegsYCB <- ggplot(daLong, aes(x = Year, y = Temperature)) +
  geom_point(aes(colour = City)) + facet_wrap(vars(City), nrow = 3) + 
  theme_minimal() + scale_color_tableau() + 
  theme(panel.grid.minor = element_blank(), plot.title = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0.5), legend.position = 'none') +
  geom_smooth(data = subset(daLong, Year < 1975),  method='lm', fullrange = F, color = '#499894', fill = '#86BCB6') +
  geom_smooth(data = subset(daLong, Year >= 1975),method='lm', fullrange = F, color = '#B07AA1', fill = '#D4A6C8') +
  labs(y = 'Temperature', x = 'Year', title = "Regressions using Yearly Data, 1975 Break",
       caption = "95% C.I. shown around fitted regression lines")

ggsave("AllRegsYCB.png",  bg = "white", dpi = "retina", width = 20, height = 15, units = "cm",
       path = "/Users/ts/Dropbox/Apps/Overleaf/Project Mathematical Statistics/Figures/Regs")

allRegsYB <- ggplot(daLong, aes(x = Year, y = Temperature)) +
  geom_point(aes(colour = City)) + facet_wrap(vars(City), nrow = 3) + 
  theme_minimal() + scale_color_tableau() + 
  theme(panel.grid.minor = element_blank(), plot.title = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0.5), legend.position = 'none') +
  geom_smooth(data = subset(daLong, Year < 1961),  method='lm', fullrange = F, color = '#499894', fill = '#86BCB6') +
  geom_smooth(data = subset(daLong, Year >= 1961),method='lm', fullrange = F, color = '#B07AA1', fill = '#D4A6C8') +
  labs(y = 'Temperature', x = 'Year', title = "Regressions using Yearly Data, 1961 Break",
       caption = "95% C.I. shown around fitted regression lines")

ggsave("AllRegsYB.png",  bg = "white", dpi = "retina", width = 20, height = 15, units = "cm",
       path = "/Users/ts/Dropbox/Apps/Overleaf/Project Mathematical Statistics/Figures/Regs")

allRegsMCB <- ggplot(dmLong, aes(x = Month, y = Temperature)) +
  geom_point(aes(colour = City)) + facet_wrap(vars(City), nrow = 3) + 
  theme_minimal() + scale_color_tableau() + 
  theme(panel.grid.minor = element_blank(), plot.title = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0.5), legend.position = 'none') +
  geom_smooth(data = subset(dmLong, Month < 197501),  method='lm', fullrange = F, color = '#499894', fill = '#86BCB6') +
  geom_smooth(data = subset(dmLong, Month >= 197501),method='lm', fullrange = F, color = '#B07AA1', fill = '#D4A6C8') +
  labs(y = 'Temperature', x = 'Month', title = "Regressions using Monthly Data, 1975 Break",
       caption = "95% C.I. shown around fitted regression lines")

ggsave("AllRegsMCB.png",  bg = "white", dpi = "retina", width = 20, height = 15, units = "cm",
       path = "/Users/ts/Dropbox/Apps/Overleaf/Project Mathematical Statistics/Figures/Regs")

allRegsMB <- ggplot(dmLong, aes(x = Month, y = Temperature)) +
  geom_point(aes(colour = City)) + facet_wrap(vars(City), nrow = 3) + 
  theme_minimal() + scale_color_tableau() + 
  theme(panel.grid.minor = element_blank(), plot.title = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0.5), legend.position = 'none') +
  geom_smooth(data = subset(dmLong, Month < 196102),  method='lm', fullrange = F, color = '#499894', fill = '#86BCB6') +
  geom_smooth(data = subset(dmLong, Month >= 196102),method='lm', fullrange = F, color = '#B07AA1', fill = '#D4A6C8') +
  labs(y = 'Temperature', x = 'Month', title = "Regressions using Monthly Data, 1961 Break",
       caption = "95% C.I. shown around fitted regression lines")

ggsave("AllRegsMB.png",  bg = "white", dpi = "retina", width = 20, height = 15, units = "cm",
       path = "/Users/ts/Dropbox/Apps/Overleaf/Project Mathematical Statistics/Figures/Regs")


                                              