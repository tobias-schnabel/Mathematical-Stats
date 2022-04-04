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
print(xtable(testmat1, align = "lcccc", caption = "Paired t-tests, Yearly Data, 1961 Break", digits = c(5,3,7,4,4)), caption.placement = 'top',
      type = "latex", file = "/Users/ts/Dropbox/Apps/Overleaf/Project Mathematical Statistics/Tables/Paired-t-Y")

print(xtable(testmat2, align = "lcccc", caption = "Paired t-tests, Monthly Data, 1961 Break", digits = c(5,3,7,4,4)), caption.placement = 'top',
      type = "latex", file = "/Users/ts/Dropbox/Apps/Overleaf/Project Mathematical Statistics/Tables/Paired-t-M")

print(xtable(testmat21, align = "lcccc", caption = "Paired t-tests, Yearly Data, 1975 Break", digits = c(5,3,7,4,4)), caption.placement = 'top',
      type = "latex", file = "/Users/ts/Dropbox/Apps/Overleaf/Project Mathematical Statistics/Tables/Paired-t-CBY")

print(xtable(testmat23, align = "lcccc", caption = "Paired t-tests, Monthly Data, 1975 Break", digits = c(5,3,7,4,4)), caption.placement = 'top',
      type = "latex", file = "/Users/ts/Dropbox/Apps/Overleaf/Project Mathematical Statistics/Tables/Paired-t-CBM")

#t test loops
print(xtable(testmat3, align = "lccc|c|c|c", caption = "t-tests, 10-Year Means", digits = c(1,0,0,4,5,4,4)), caption.placement = 'top',
      type = "latex", file = "/Users/ts/Dropbox/Apps/Overleaf/Project Mathematical Statistics/Tables/tT10Ymean")

print(xtable(testmat4, align = "lccc|c|c|c", caption = "t-tests, 5-Year Means", digits = c(1,0,0,4,5,4,4)), caption.placement = 'top',
      type = "latex", file = "/Users/ts/Dropbox/Apps/Overleaf/Project Mathematical Statistics/Tables/tT5Ymean")

print(xtable(testmat5, align = "lccc|c|c|c", caption = "t-tests, 10-Year Medians", digits = c(1,0,0,4,5,4,4)), caption.placement = 'top',
      type = "latex", file = "/Users/ts/Dropbox/Apps/Overleaf/Project Mathematical Statistics/Tables/tT10Ymedian")

#F-tests

print(xtable(testmat6, align = "lccccc", caption = "F-tests, Yearly Data, 1961 Break", digits = c(5,5,5,7,4,4)), caption.placement = 'top',
      type = "latex", file = "/Users/ts/Dropbox/Apps/Overleaf/Project Mathematical Statistics/Tables/F-test-Y")

print(xtable(testmat7, align = "lccccc", caption = "F-tests, Monthly Data, 1961 Break", digits = c(5,5,5,7,4,4)), caption.placement = 'top',
      type = "latex", file = "/Users/ts/Dropbox/Apps/Overleaf/Project Mathematical Statistics/Tables/F-test-M")

print(xtable(testmat22, align = "lcccc", caption = "F-tests, Yearly Data, 1975 Break", digits = c(5,5,7,4,4)), caption.placement = 'top',
      type = "latex", file = "/Users/ts/Dropbox/Apps/Overleaf/Project Mathematical Statistics/Tables/F-test-CBY")

print(xtable(testmat24, align = "lccccc", caption = "F-tests, Monthly Data, 1975 Break", digits = c(5,5,5,7,4,4)), caption.placement = 'top',
      type = "latex", file = "/Users/ts/Dropbox/Apps/Overleaf/Project Mathematical Statistics/Tables/F-test-CBM")

print(xtable(testmat6, align = "lcccc", caption = "F-tests, Yearly Data, 1961 Break", digits = c(5,3,7,4,4)), caption.placement = 'top',
      type = "latex", file = "/Users/ts/Dropbox/Apps/Overleaf/Project Mathematical Statistics/Tables/F-test-Y")

print(xtable(testmat7, align = "lcccc", caption = "F-tests, Monthly Data, 1961 Break", digits = c(5,3,7,4,4)), caption.placement = 'top',
      type = "latex", file = "/Users/ts/Dropbox/Apps/Overleaf/Project Mathematical Statistics/Tables/F-test-M")

print(xtable(testmat22, align = "lcccc", caption = "F-tests, Yearly Data, 1975 Break", digits = c(5,3,7,4,4)), caption.placement = 'top',
      type = "latex", file = "/Users/ts/Dropbox/Apps/Overleaf/Project Mathematical Statistics/Tables/F-test-CBY")

print(xtable(testmat24, align = "lcccc", caption = "F-tests, Monthly Data, 1975 Break", digits = c(5,3,7,4,4)), caption.placement = 'top',
      type = "latex", file = "/Users/ts/Dropbox/Apps/Overleaf/Project Mathematical Statistics/Tables/F-test-CBM")

print(xtable(testmatMan, align = "lcccc", caption = "t-tests, 1975 Break", digits = c(4,4,7,4,4)), caption.placement = 'top',
      type = "latex", file = "/Users/ts/Dropbox/Apps/Overleaf/Project Mathematical Statistics/Tables/t-t-man")

#print manual reg results
print(xtable(regMat, align = "lccc", caption = "Results of Manual Computation of Regression Coefficients", digits = c(4,6,6,8)), caption.placement = 'top',
        type = "latex", file = "/Users/ts/Dropbox/Apps/Overleaf/Project Mathematical Statistics/Tables/regMat")

print(xtable(regMat2, align = "lccc", caption = "Results of Manual Computation of Regression Coefficients", digits = c(4,6,6,8)), caption.placement = 'top',
      type = "latex", file = "/Users/ts/Dropbox/Apps/Overleaf/Project Mathematical Statistics/Tables/regMat2")

print(xtable(regMat3, align = "lccc", caption = "Results of Manual Computation of Regression Coefficients", digits = c(4,6,6,8)), caption.placement = 'top',
      type = "latex", file = "/Users/ts/Dropbox/Apps/Overleaf/Project Mathematical Statistics/Tables/regMat3")

print(xtable(regMatM, align = "lccc", caption = "Results of Manual Computation of Regression Coefficients", digits = c(4,6,6,8)), caption.placement = 'top',
      type = "latex", file = "/Users/ts/Dropbox/Apps/Overleaf/Project Mathematical Statistics/Tables/regMatM")

print(xtable(regMatM2, align = "lccc", caption = "Results of Manual Computation of Regression Coefficients", digits = c(4,6,6,8)), caption.placement = 'top',
      type = "latex", file = "/Users/ts/Dropbox/Apps/Overleaf/Project Mathematical Statistics/Tables/regMatM2")

print(xtable(regMatM3, align = "lccc", caption = "Results of Manual Computation of Regression Coefficients", digits = c(4,6,6,8)), caption.placement = 'top',
      type = "latex", file = "/Users/ts/Dropbox/Apps/Overleaf/Project Mathematical Statistics/Tables/regMatM3")




