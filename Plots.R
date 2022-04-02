########################Plots########################
#raw data
TSA <- ggplot(daLong, aes(x = Year, y = Temperature)) +
  geom_line(aes(color = City)) + labs(y = 'Temperature', x = 'Year') +
  theme_minimal() + scale_color_tableau() + 
  theme( panel.grid.minor = element_blank(), plot.title = element_text(hjust = 0.5)) +
  xlim(1905, 2025) + ggtitle("Annual Data")

ggsave("TSA.png",  bg = "white", dpi = "retina", width = 20, height = 10, units = "cm",
       path = "/Users/ts/Dropbox/Apps/Overleaf/Project Mathematical Statistics/Figures")


# dat <- dmLong[, Month := yearmon(Month)]
TSM <- ggplot(dmLong, aes(x = Month, y = Temperature)) +
  geom_line(aes(color = City)) + labs(y = 'Temperature', x = 'Year') +
  theme_minimal() + scale_color_tableau() + 
  theme( panel.grid.minor = element_blank(), plot.title = element_text(hjust = 0.5)) +
  xlim(190501, 202501) + ggtitle("Monthly Data")

ggsave("TSM.png",  bg = "white", dpi = "retina", width = 20, height = 10, units = "cm",
       path = "/Users/ts/Dropbox/Apps/Overleaf/Project Mathematical Statistics/Figures")

TSMS <- ggplot(dmsLong, aes(x = Month, y = Temperature)) +
  geom_line(aes(color = City)) + labs(y = 'Temperature', x = 'Year') +
  theme_minimal() + scale_color_tableau() + 
  theme( panel.grid.minor = element_blank(), plot.title = element_text(hjust = 0.5)) +
  xlim(190501, 202501) + ggtitle("Smoothed Monthly Data")

ggsave("TSMS.png",  bg = "white", dpi = "retina", width = 20, height = 10, units = "cm",
       path = "/Users/ts/Dropbox/Apps/Overleaf/Project Mathematical Statistics/Figures")


#normality
qqY <-  ggplot(daLong, aes(sample = Temperature)) +
  stat_qq(aes(color = City)) +
  labs(y = "Weight", x = "Theoretical") +theme_minimal() + ggtitle("Annual Data Q-Q Plot") +
  theme( panel.grid.minor = element_blank(), plot.title = element_text(hjust = 0.5)) +
  scale_color_tableau()


ggsave("qqY.png",  bg = "white", dpi = "retina", width = 20, height = 10, units = "cm",
       path = "/Users/ts/Dropbox/Apps/Overleaf/Project Mathematical Statistics/Figures")

qqM <-  ggplot(dmLong, aes(sample = Temperature)) +
  stat_qq(aes(color = City)) +
  labs(y = "Weight", x = "Theoretical") +theme_minimal() + ggtitle("Monthly Data Q-Q Plot") +
  theme( panel.grid.minor = element_blank(), plot.title = element_text(hjust = 0.5)) +
  scale_color_tableau()


ggsave("qqM.png",  bg = "white", dpi = "retina", width = 20, height = 10, units = "cm",
       path = "/Users/ts/Dropbox/Apps/Overleaf/Project Mathematical Statistics/Figures")

qqMs <-  ggplot(dmsLong, aes(sample = Temperature)) +
  stat_qq(aes(color = City)) +
  labs(y = "Weight", x = "Theoretical") +theme_minimal() + ggtitle("Smoothed Monthly Data Q-Q Plot") +
  theme( panel.grid.minor = element_blank(), plot.title = element_text(hjust = 0.5)) +
  scale_color_tableau()


ggsave("qqMs.png",  bg = "white", dpi = "retina", width = 20, height = 10, units = "cm",
       path = "/Users/ts/Dropbox/Apps/Overleaf/Project Mathematical Statistics/Figures")

qqD <-  ggplot(ddLong, aes(sample = Temperature)) +
  stat_qq(aes(color = City)) +
  labs(y = "Weight", x = "Theoretical") +theme_minimal() + ggtitle("Daily Data Q-Q Plot") +
  theme( panel.grid.minor = element_blank(), plot.title = element_text(hjust = 0.5)) +
  scale_color_tableau()


ggsave("qqD.png",  bg = "white", dpi = "retina", width = 20, height = 10, units = "cm",
       path = "/Users/ts/Dropbox/Apps/Overleaf/Project Mathematical Statistics/Figures")

#histograms
histY <- ggplot(daLong, aes(x = Temperature, color = City)) + 
  geom_histogram(fill="white", alpha=0.5, position="dodge", bins=45) +
  geom_vline(data=citymeanA, aes(xintercept = Citymean, color = City), linetype = "dashed") +
  theme_minimal() + ylab("Density") + ggtitle("Annual Mean Temperatures") +
  theme( panel.grid.minor = element_blank(), plot.title = element_text(hjust = 0.5)) +
  scale_color_tableau()

ggsave("Ahist.png",  bg = "white", dpi = "retina", width = 20, height = 10, units = "cm",
       path = "/Users/ts/Dropbox/Apps/Overleaf/Project Mathematical Statistics/Figures")


histM <- ggplot(dmLong, aes(x = Temperature, color = City)) + 
  geom_histogram(fill="white", alpha=0.5, position="dodge", bins=45) +
  geom_vline(data=citymeanM, aes(xintercept = Citymean, color = City), linetype = "dashed") +
  theme_minimal() + ylab("Density") + ggtitle("Monthly Mean Temperatures") +
  theme( panel.grid.minor = element_blank(), plot.title = element_text(hjust = 0.5)) +
  scale_color_tableau()

ggsave("Mhist.png",  bg = "white", dpi = "retina", width = 20, height = 10, units = "cm",
       path = "/Users/ts/Dropbox/Apps/Overleaf/Project Mathematical Statistics/Figures")


#densities

densplotyears <- ggplot(daLong, aes(x = Temperature, color = City)) + geom_density() +
  geom_vline(data=citymeanA, aes(xintercept = Citymean, color = City), linetype = "dashed") +
  theme_minimal() + ylab("Density") + ggtitle("Annual Mean Temperatures") +
  theme( panel.grid.minor = element_blank(), plot.title = element_text(hjust = 0.5)) +
  scale_color_tableau()


ggsave("AD.png",  bg = "white", dpi = "retina", width = 20, height = 10, units = "cm",
       path = "/Users/ts/Dropbox/Apps/Overleaf/Project Mathematical Statistics/Figures")

densplotmonths <- ggplot(dmLong, aes(x = Temperature, color = City)) + geom_density() +
  geom_vline(data=citymeanM, aes(xintercept = Citymean, color = City), linetype = "dashed") +
  theme_minimal() + ylab("Density")+ ggtitle("Monthly Mean Temperatures") +
  theme( panel.grid.minor = element_blank(), plot.title = element_text(hjust = 0.5)) +
  scale_color_tableau()

ggsave("MD.png",  bg = "white", dpi = "retina", width = 20, height = 10, units = "cm",
       path = "/Users/ts/Dropbox/Apps/Overleaf/Project Mathematical Statistics/Figures")


densplotmonthsS <- ggplot(dmsLong, aes(x = Temperature, color = City)) + geom_density() +
  geom_vline(data=citymeanMS, aes(xintercept = Citymean, color = City), linetype = "dashed") +
  theme_minimal() + ylab("Density")+ ggtitle("Smoothed Monthly Mean Temperatures") +
  theme( panel.grid.minor = element_blank(), plot.title = element_text(hjust = 0.5)) +
  scale_color_tableau()

ggsave("MSD.png",  bg = "white", dpi = "retina", width = 20, height = 10, units = "cm",
       path = "/Users/ts/Dropbox/Apps/Overleaf/Project Mathematical Statistics/Figures")

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
       path = "/Users/ts/Dropbox/Apps/Overleaf/Project Mathematical Statistics/Figures")

densplotRoll20_10 <- ggplot(rolling20_10L, aes(x = Temperature, color = City))+ geom_density() +
  theme_minimal() + ylab("Density")+ ggtitle("Rolling 10 Year Window 20 Year Mean Temperatures") +
  theme( panel.grid.minor = element_blank(), plot.title = element_text(hjust = 0.5))+
  scale_color_tableau()

ggsave("20_10D.png",  bg = "white", dpi = "retina", width = 20, height = 10, units = "cm",
       path = "/Users/ts/Dropbox/Apps/Overleaf/Project Mathematical Statistics/Figures")


marchD <- ggplot(march, aes(x = Temperature, color = City)) + geom_density() +
  geom_vline(aes(xintercept = Citymean, color = City), linetype = "dashed") +
  theme_minimal() + ylab("Density")+ ggtitle("Mean Temperatures in March") +
  theme( panel.grid.minor = element_blank(), plot.title = element_text(hjust = 0.5)) +
  scale_color_tableau()

ggsave("marchD.png",  bg = "white", dpi = "retina", width = 20, height = 10, units = "cm",
       path = "/Users/ts/Dropbox/Apps/Overleaf/Project Mathematical Statistics/Figures")

juneD <- ggplot(june, aes(x = Temperature, color = City)) + geom_density() +
  geom_vline(aes(xintercept = Citymean, color = City), linetype = "dashed") +
  theme_minimal() + ylab("Density")+ ggtitle("Mean Temperatures in June") +
  theme( panel.grid.minor = element_blank(), plot.title = element_text(hjust = 0.5)) +
  scale_color_tableau()

ggsave("juneD.png",  bg = "white", dpi = "retina", width = 20, height = 10, units = "cm",
       path = "/Users/ts/Dropbox/Apps/Overleaf/Project Mathematical Statistics/Figures")

septemberD <- ggplot(september, aes(x = Temperature, color = City)) + geom_density() +
  geom_vline(aes(xintercept = Citymean, color = City), linetype = "dashed") +
  theme_minimal() + ylab("Density")+ ggtitle("Mean Temperatures in September") +
  theme( panel.grid.minor = element_blank(), plot.title = element_text(hjust = 0.5))+
  scale_color_tableau()

ggsave("septemberD.png",  bg = "white", dpi = "retina", width = 20, height = 10, units = "cm",
       path = "/Users/ts/Dropbox/Apps/Overleaf/Project Mathematical Statistics/Figures")

decemberD <- ggplot(december, aes(x = Temperature, color = City)) + geom_density() +
  geom_vline(aes(xintercept = Citymean, color = City), linetype = "dashed") +
  theme_minimal() + ylab("Density")+ ggtitle("Mean Temperatures in December") +
  theme( panel.grid.minor = element_blank(), plot.title = element_text(hjust = 0.5))+
  scale_color_tableau()

ggsave("decemberD.png",  bg = "white", dpi = "retina", width = 20, height = 10, units = "cm",
       path = "/Users/ts/Dropbox/Apps/Overleaf/Project Mathematical Statistics/Figures")

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
       path = "/Users/ts/Dropbox/Apps/Overleaf/Project Mathematical Statistics/Figures")

