########################Tables########################
daSS <- daC[,.("De Bilt" = De.Bilt, Eelde, Maastricht)]
stargazer(daSS, out.header = F, title = "Annual Data", table.placement = "H",
          label = "BAS", out = "/Users/ts/Dropbox/Apps/Overleaf/Project Mathematical Statistics/Tables/Descriptive/AS" )


dmSS <- dmC[,.("De Bilt" = De.Bilt, Eelde, Maastricht)]
stargazer(dmSS, out.header = F, title = "Monthly Data", table.placement = "H",
          label = "BMS",out = "/Users/ts/Dropbox/Apps/Overleaf/Project Mathematical Statistics/Tables/Descriptive/MS" )

dmsSS <- dmsC[,.("De Bilt" = De.Bilt, Eelde, Maastricht)]
stargazer(dmsSS, out.header = F, title = "Smoothed Monthly Data", table.placement = "H",
          label = "BMSS",out = "/Users/ts/Dropbox/Apps/Overleaf/Project Mathematical Statistics/Tables/Descriptive/MSS" )

ddSS <- ddC[,.("De Bilt" = De.Bilt, Eelde, Maastricht)]
stargazer(ddSS, out.header = F, title = "Daily Data", table.placement = "H",
          label = "BDS",out = "/Users/ts/Dropbox/Apps/Overleaf/Project Mathematical Statistics/Tables/Descriptive/DS" )


#time series break tests
print(xtable(structtabY, align = "llll", caption = "Structural Break in Yearly Data", digits = 5, label = "BSBY"), caption.placement = 'top', table.placement = "H",
      type = "latex", file = "/Users/ts/Dropbox/Apps/Overleaf/Project Mathematical Statistics/Tables/Tests/SBY")

print(xtable(structtabM, align = "llll", caption = "Structural Break in Monthly Data", digits = 5, label = "BSBM"), caption.placement = 'top', table.placement = "H",
      type = "latex", file = "/Users/ts/Dropbox/Apps/Overleaf/Project Mathematical Statistics/Tables/Tests/SBM")

print(xtable(structtabBP, align = "llcc", caption = "Structural Break Breakpoints", digits = c(0,0,0,2), label = "BSBBP"), caption.placement = 'top', table.placement = "H",
      type = "latex", file = "/Users/ts/Dropbox/Apps/Overleaf/Project Mathematical Statistics/Tables/Tests/SBBP")

#paired t tests
print(xtable(testmat1, align = "lcccc", caption = "Paired t-tests, Yearly Data, 1961 Break", digits = c(5,3,7,4,4), label = "BPaired-t-Y"), caption.placement = 'top', table.placement = "H",
      type = "latex", file = "/Users/ts/Dropbox/Apps/Overleaf/Project Mathematical Statistics/Tables/Tests/Paired-t-Y")

print(xtable(testmat2, align = "lcccc", caption = "Paired t-tests, Monthly Data, 1961 Break", digits = c(5,3,7,4,4), label = "BPaired-t-M"), caption.placement = 'top', table.placement = "H",
      type = "latex", file = "/Users/ts/Dropbox/Apps/Overleaf/Project Mathematical Statistics/Tables/Tests/Paired-t-M")

print(xtable(testmat21, align = "lcccc", caption = "Paired t-tests, Yearly Data, 1975 Break", digits = c(5,3,7,4,4), label = "BPaired-t-CBY"), caption.placement = 'top', table.placement = "H",
      type = "latex", file = "/Users/ts/Dropbox/Apps/Overleaf/Project Mathematical Statistics/Tables/Tests/Paired-t-CBY")

print(xtable(testmat23, align = "lcccc", caption = "Paired t-tests, Monthly Data, 1975 Break", digits = c(5,3,7,4,4), label = "BPaired-t-CBM"), caption.placement = 'top', table.placement = "H",
      type = "latex", file = "/Users/ts/Dropbox/Apps/Overleaf/Project Mathematical Statistics/Tables/Tests/Paired-t-CBM")

#t test loops
print(xtable(testmat3, align = "lccc|c|c|c", caption = "t-tests, 10-Year Means", digits = c(1,0,0,4,5,4,4), label = "BtT10Ymean"), caption.placement = 'top', table.placement = "H",
      type = "latex", file = "/Users/ts/Dropbox/Apps/Overleaf/Project Mathematical Statistics/Tables/Tests/tT10Ymean")

print(xtable(testmat4, align = "lccc|c|c|c", caption = "t-tests, 5-Year Means", digits = c(1,0,0,4,5,4,4), label = "BtT5Ymean"), caption.placement = 'top', table.placement = "H",
      type = "latex", file = "/Users/ts/Dropbox/Apps/Overleaf/Project Mathematical Statistics/Tables/Tests/tT5Ymean")

print(xtable(testmat5, align = "lccc|c|c|c", caption = "t-tests, 10-Year Medians", digits = c(1,0,0,4,5,4,4), label = "BtT10Ymedian"), caption.placement = 'top', table.placement = "H",
      type = "latex", file = "/Users/ts/Dropbox/Apps/Overleaf/Project Mathematical Statistics/Tables/Tests/tT10Ymedian")

#F-tests

print(xtable(testmat6, align = "lccccc", caption = "F-tests, Yearly Data, 1961 Break", digits = c(5,5,5,7,4,4), label = "BF-test-Y"), caption.placement = 'top', table.placement = "H",
      type = "latex", file = "/Users/ts/Dropbox/Apps/Overleaf/Project Mathematical Statistics/Tables/Tests/F-test-Y")

print(xtable(testmat7, align = "lccccc", caption = "F-tests, Monthly Data, 1961 Break", digits = c(5,5,5,7,4,4), label = "BF-test-M"), caption.placement = 'top', table.placement = "H",
      type = "latex", file = "/Users/ts/Dropbox/Apps/Overleaf/Project Mathematical Statistics/Tables/Tests/F-test-M")

print(xtable(testmat22, align = "lccccc", caption = "F-tests, Yearly Data, 1975 Break", digits = c(5,5,7,4,4,4), label = "BF-test-CBY"), caption.placement = 'top', table.placement = "H",
      type = "latex", file = "/Users/ts/Dropbox/Apps/Overleaf/Project Mathematical Statistics/Tables/Tests/F-test-CBY")

print(xtable(testmat24, align = "lccccc", caption = "F-tests, Monthly Data, 1975 Break", digits = c(5,5,5,7,4,4), label = "BF-test-CBM"), caption.placement = 'top', table.placement = "H",
      type = "latex", file = "/Users/ts/Dropbox/Apps/Overleaf/Project Mathematical Statistics/Tables/Tests/F-test-CBM")

print(xtable(testmat6, align = "lccccc", caption = "F-tests, Yearly Data, 1961 Break", digits = c(5,3,7,4,4,4), label = "BF-test-YB"), caption.placement = 'top', table.placement = "H",
      type = "latex", file = "/Users/ts/Dropbox/Apps/Overleaf/Project Mathematical Statistics/Tables/Tests/F-test-YB")

print(xtable(testmat7, align = "lccccc", caption = "F-tests, Monthly Data, 1961 Break", digits = c(5,3,7,4,4,4), label = "BF-test-MB"), caption.placement = 'top', table.placement = "H",
      type = "latex", file = "/Users/ts/Dropbox/Apps/Overleaf/Project Mathematical Statistics/Tables/Tests/F-test-MB")

print(xtable(testmat22, align = "lccccc", caption = "F-tests, Yearly Data, 1975 Break", digits = c(5,3,7,4,4,4), label = "BF-test-CBY"), caption.placement = 'top', table.placement = "H",
      type = "latex", file = "/Users/ts/Dropbox/Apps/Overleaf/Project Mathematical Statistics/Tables/Tests/F-test-CBY")

print(xtable(testmat24, align = "lccccc", caption = "F-tests, Monthly Data, 1975 Break", digits = c(5,3,7,4,4,4), label = "BF-test-CBM"), caption.placement = 'top', table.placement = "H",
      type = "latex", file = "/Users/ts/Dropbox/Apps/Overleaf/Project Mathematical Statistics/Tables/Tests/F-test-CBM")

print(xtable(testmatMan, align = "lcccc", caption = "t-tests, 1975 Break", digits = c(4,4,7,4,4), label = "Bt-t-man"), caption.placement = 'top', table.placement = "H",
      type = "latex", file = "/Users/ts/Dropbox/Apps/Overleaf/Project Mathematical Statistics/Tables/Tests/t-t-man")

#print white test results
print(xtable(testmatHsced, align = "lcc", caption = "White Tests for Heteroskedasticity", digits = c(5,5,5), label = "Bwhite"), caption.placement = 'top', 
      table.placement = "H", hline.after = c(-1,0,nrow(testmatHsced),3), type = "latex", file = "/Users/ts/Dropbox/Apps/Overleaf/Project Mathematical Statistics/Tables/Regressions/white")

print(xtable(testmatHsced2, align = "lcc", caption = "White Tests for Heteroskedasticity", digits = c(5,5,5), label = "BwhiteBreak"), caption.placement = 'top', 
      table.placement = "H", hline.after = c(-1,0,nrow(testmatHsced),3), type = "latex", file = "/Users/ts/Dropbox/Apps/Overleaf/Project Mathematical Statistics/Tables/Regressions/whiteBreak")


#print manual reg results
print(xtable(regMat, align = "lccc", caption = "Manually Computed Regression Coefficients, Maastricht, Yearly Data", digits = c(4,6,6,8), label = "BregMat"), caption.placement = 'top', table.placement = "H",
        type = "latex", file = "/Users/ts/Dropbox/Apps/Overleaf/Project Mathematical Statistics/Tables/Regressions/regMat")

print(xtable(regMat2, align = "lccc", caption = "Manually Computed Regression Coefficients, De Bilt, Yearly Data", digits = c(4,6,6,8), label = "BregMat2"), caption.placement = 'top', table.placement = "H",
      type = "latex", file = "/Users/ts/Dropbox/Apps/Overleaf/Project Mathematical Statistics/Tables/Regressions/regMat2")

print(xtable(regMat3, align = "lccc", caption = "Manually Computed Regression Coefficients, Eelde, Yearly Data", digits = c(4,6,6,8), label = "BregMat3"), caption.placement = 'top', table.placement = "H",
      type = "latex", file = "/Users/ts/Dropbox/Apps/Overleaf/Project Mathematical Statistics/Tables/Regressions/regMat3")

print(xtable(regMatM, align = "lccc", caption = "Manually Computed Regression Coefficients, Maastricht, Monthly Data", digits = c(4,6,6,8), label = "BregMatM"), caption.placement = 'top', table.placement = "H",
      type = "latex", file = "/Users/ts/Dropbox/Apps/Overleaf/Project Mathematical Statistics/Tables/Regressions/regMatM")

print(xtable(regMatM2, align = "lccc", caption = "Manually Computed Regression Coefficients, De Bilt, Monthly Data", digits = c(4,6,6,8), label = "BregMatM2"), caption.placement = 'top', table.placement = "H",
      type = "latex", file = "/Users/ts/Dropbox/Apps/Overleaf/Project Mathematical Statistics/Tables/Regressions/regMatM2")

print(xtable(regMatM3, align = "lccc", caption = "Manually Computed Regression Coefficients, Eelde, Monthly Data", digits = c(4,6,6,8), label = "BregMatM3"), caption.placement = 'top', table.placement = "H",
      type = "latex", file = "/Users/ts/Dropbox/Apps/Overleaf/Project Mathematical Statistics/Tables/Regressions/regMatM3")

#print full regression table

##yearly data
#full
stargazer(regYD, regYE, regYM, out.header = F, title = "Regressions, Yearly Data", table.placement = "H",
          label = "BRegY", out = "/Users/ts/Dropbox/Apps/Overleaf/Project Mathematical Statistics/Tables/Regressions/RegY")
#restricted
stargazer(regPreBYD, regPreBYE, regPreBYM, out.header = F, title = "Regressions, Yearly Data, Before 1961 Break", table.placement = "H",
          label = "BRegYRBPre",  out = "/Users/ts/Dropbox/Apps/Overleaf/Project Mathematical Statistics/Tables/Regressions/RegYRBPre")

stargazer(regPostBYD, regPostBYE, regPostBYM, out.header = F, title = "Regressions, Yearly Data, After 1961 Break", table.placement = "H",
          label = "BRegYRBPost",  out = "/Users/ts/Dropbox/Apps/Overleaf/Project Mathematical Statistics/Tables/Regressions/RegYRBPost")

stargazer(regPreCBYD, regPreCBYE, regPreCBYM, out.header = F, title = "Regressions, Yearly Data, Before 1975 Break", table.placement = "H",
          label = "BRegYRCBPre",  out = "/Users/ts/Dropbox/Apps/Overleaf/Project Mathematical Statistics/Tables/Regressions/RegYRCBPre")

stargazer(regPostCBYD, regPostCBYE, regPostCBYM, out.header = F, title = "Regressions, Yearly Data, After 1975 Break", table.placement = "H",
          label = "BRegYRCBPost",  out = "/Users/ts/Dropbox/Apps/Overleaf/Project Mathematical Statistics/Tables/Regressions/RegYRCBPost")

#demo
stargazer(regPreCBYM, regPostCBYM, out.header = F, title = "Regressions, Yearly Data, Before and After 1975 Break", table.placement = "H",
          column.labels = c("Year < 1975", "Year > 1975"), label = "BRegDemo",  out = "/Users/ts/Dropbox/Apps/Overleaf/Project Mathematical Statistics/Tables/Regressions/RegDemo")

##monthly data
#full
stargazer(regMD, regME, regMM, out.header = F, title = "Regressions, Monthly Data", table.placement = "H", label = "BRegM",
          out = "/Users/ts/Dropbox/Apps/Overleaf/Project Mathematical Statistics/Tables/Regressions/RegM")
#restricted
stargazer(regPreBMD, regPreBME, regPreBMM, out.header = F, title = "Regressions, Monthly Data, Before 1961 Break", table.placement = "H",
          label = "BRegMRBPre" ,no.space = TRUE, out = "/Users/ts/Dropbox/Apps/Overleaf/Project Mathematical Statistics/Tables/Regressions/RegMRBPre")

stargazer(regPostBMD, regPostBME, regPostBMM, out.header = F, title = "Regressions, Monthly Data, After 1961 Break", table.placement = "H",
          label = "BRegMRBPost" ,no.space = TRUE, out = "/Users/ts/Dropbox/Apps/Overleaf/Project Mathematical Statistics/Tables/Regressions/RegMRBPost")

stargazer(regPreCBMD, regPreCBME, regPreCBMM, out.header = F, title = "Regressions, Monthly Data, Before 1975 Break", table.placement = "H",
          label = "BRegMRCBPre", out = "/Users/ts/Dropbox/Apps/Overleaf/Project Mathematical Statistics/Tables/Regressions/RegMRCBPre")

stargazer(regPostCBMD, regPostCBME, regPostCBMM, out.header = F, title = "Regressions, Monthly Data, After 1975 Break", table.placement = "H",
          label = "BRegMRCBPost", out = "/Users/ts/Dropbox/Apps/Overleaf/Project Mathematical Statistics/Tables/Regressions/RegMRCBPost")

##Bootstrap
print(xtable(BSmat1, align = "lccc", caption = "Bootstrap: t-test for Regression Coefficients", digits = c(4,4,6,6), 
             label = "BBSmat1"), caption.placement = 'top', table.placement = "H",
      type = "latex", sanitize.text.function = function(x) {x},
      file = "/Users/ts/Dropbox/Apps/Overleaf/Project Mathematical Statistics/Tables/BS/BSmat1")

print(xtable(BSmat2, align = "lcccc", caption = "Bootstrap: t-test for Regression Coefficients", digits = c(4,4,6,6,6), 
             label = "BBSmat1"), caption.placement = 'top', table.placement = "H",
      type = "latex", sanitize.text.function = function(x) {x},
      file = "/Users/ts/Dropbox/Apps/Overleaf/Project Mathematical Statistics/Tables/BS/BSmat2")

