########################Tables########################
daSS <- daC[,.("De Bilt" = De.Bilt, Eelde, Maastricht)]
stargazer(daSS, out.header = F, title = "Annual Data",
          out = "/Users/ts/Dropbox/Apps/Overleaf/Project Mathematical Statistics/Tables/AS" )


dmSS <- dmC[,.("De Bilt" = De.Bilt, Eelde, Maastricht)]
stargazer(dmSS, out.header = F, title = "Smoothed Monthly Data",
          out = "/Users/ts/Dropbox/Apps/Overleaf/Project Mathematical Statistics/Tables/MS" )

dmsSS <- dmsC[,.("De Bilt" = De.Bilt, Eelde, Maastricht)]
stargazer(dmsSS, out.header = F, title = "Smoothed Monthly Data",
          out = "/Users/ts/Dropbox/Apps/Overleaf/Project Mathematical Statistics/Tables/MSS" )

ddSS <- ddC[,.("De Bilt" = De.Bilt, Eelde, Maastricht)]
stargazer(ddSS, out.header = F, title = "Daily Data",
          out = "/Users/ts/Dropbox/Apps/Overleaf/Project Mathematical Statistics/Tables/DS" )


#time series break tests
stargazer(structtabY, out.header = F, summary = F,  title = "Structural Break in Yearly Data",
          out = "/Users/ts/Dropbox/Apps/Overleaf/Project Mathematical Statistics/Tables/SBY")

stargazer(structtabM, out.header = F, summary = F,  title = "Structural Break in Yearly Data",
          out = "/Users/ts/Dropbox/Apps/Overleaf/Project Mathematical Statistics/Tables/SBM")

stargazer(structtabBP, out.header = F, summary = F, digit.separate = 0 ,
          title = "Structural Break Breakpoints",
          out = "/Users/ts/Dropbox/Apps/Overleaf/Project Mathematical Statistics/Tables/SBBP")
