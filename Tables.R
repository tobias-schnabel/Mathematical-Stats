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
print(xtable(structtabY, align = "llll", caption = "Structural Break in Yearly Data", digits = 5), caption.placement = 'top',
      type = "latex", file = "/Users/ts/Dropbox/Apps/Overleaf/Project Mathematical Statistics/Tables/SBY")

print(xtable(structtabM, align = "llll", caption = "Structural Break in Monthly Data", digits = 5), caption.placement = 'top',
      type = "latex", file = "/Users/ts/Dropbox/Apps/Overleaf/Project Mathematical Statistics/Tables/SBM")

print(xtable(structtabBP, align = "llcc", caption = "Structural Break Breakpoints", digits = c(0,0,0,2)), caption.placement = 'top',
      type = "latex", file = "/Users/ts/Dropbox/Apps/Overleaf/Project Mathematical Statistics/Tables/SBBP")

#paired t tests
print(xtable(testmat1, align = "lcccc", caption = "Paired t-tests, Yearly Data", digits = c(5,3,7,4,4)), caption.placement = 'top',
      type = "latex", file = "/Users/ts/Dropbox/Apps/Overleaf/Project Mathematical Statistics/Tables/Paired-t-Y")

print(xtable(testmat2, align = "lcccc", caption = "Paired t-tests, Monthly Data", digits = c(5,3,7,4,4)), caption.placement = 'top',
      type = "latex", file = "/Users/ts/Dropbox/Apps/Overleaf/Project Mathematical Statistics/Tables/Paired-t-M")


