##############################tidy#############################

daC <- da[, .(Maastricht = maastricht, Eelde = eelde, De.Bilt = de_bilt, Year = year)]
daLong <- melt(daC, id.vars = c("Year"), measure.vars = c("Maastricht", "Eelde", "De.Bilt"),
               variable.factor = T, variable.name = "City", value.name = "Temperature")
citymeanA <- daLong[, Citymean := mean(Temperature), by = City]

dmsC <- dms[, .(Maastricht = maastricht, Eelde = eelde, De.Bilt = de_bilt, Month = month )]
dmsLong <- melt(dmsC, id.vars = c("Month"), measure.vars = c("Maastricht", "Eelde", "De.Bilt"),
                variable.factor = T, variable.name = "City", value.name = "Temperature")
citymeanMS <- dmsLong[, Citymean := mean(Temperature), by = City]

dmC <- dm[, .(Maastricht = maastricht, Eelde = eelde, De.Bilt = de_bilt, Month = month )]
dmLong <- melt(dmC, id.vars = c("Month"), measure.vars = c("Maastricht", "Eelde", "De.Bilt"),
               variable.factor = T, variable.name = "City", value.name = "Temperature")
citymeanM <- dmLong[, Citymean := mean(Temperature), by = City]

ddC <- dd[, .(Maastricht = maastricht, Eelde = eelde, De.Bilt = de_bilt, Date = date )]
ddLong <- melt(ddC, id.vars = c("Date"), measure.vars = c("Maastricht", "Eelde", "De.Bilt"),
               variable.factor = T, variable.name = "City", value.name = "Temperature")
citymeanD <- ddLong[, Citymean := mean(Temperature), by = City]


rolling10_5L <- melt(rollingMean10_5, id.vars = c("Year"), measure.vars = c("Maastricht", "Eelde", "De.Bilt"),
                     variable.factor = T, variable.name = "City", value.name = "Temperature")

rolling20_10L <- melt(rollingMean20_10, id.vars = c("Year"), measure.vars = c("Maastricht", "Eelde", "De.Bilt"),
                      variable.factor = T, variable.name = "City", value.name = "Temperature")
