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


