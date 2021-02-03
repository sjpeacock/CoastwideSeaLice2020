
library(gplots)

bootLice <- function(x, n = 1000, na.rm = FALSE){
	
	bootMeans <- apply(matrix(sample(x, size = length(x)*n, replace = TRUE), nrow = length(x), ncol = n), 2, mean)
	
	return(c(mean(bootMeans, na.rm = na.rm), quantile(bootMeans, c(0.025, 0.975), , na.rm = na.rm)))
}

###############################################################################
# Look at coastwide sea lice data from Clayoquot Sound, Broughton Archipelago
# and the Discovery Islands
###############################################################################

# Read in data
dat <- read.csv("data/coastwideSeaLice.csv")

dat$date <- as.Date(paste(dat$year, dat$month, dat$day, sep ="-"))

# Relable Johnstone as Discovery
dat$region[dat$region == "Johnstone"] <- "Discovery"

# Relable QueenCharlotte to PortHardy
dat$region[dat$region == "QueenCharlotte"] <- "PortHardy"


regions <- c("Discovery", "Broughton", "PortHardy", "Clayoquot", "Nootka")

# Fish numbers and species over time
unique(dat$species)
species <- c("chum", "pink", "Chinook", "sockeye", "coho")

# Combine Chinook and hatchery chinook for now
dat$species[dat$species == "Chinook-hatchery"] <- "Chinook"

colRegion <- c(Discovery = 4, Broughton = 3, PortHardy = 5, Clayoquot  = 2, Nootka = 6)
pchRegion <- c(Discovery = 0, Broughton = 5, PortHardy = 1, Clayoquot  = 2, Nootka = 6)


###############################################################################
# Number of salmon sampled
###############################################################################
nFish <- list(); length(nFish) <- length(regions)

for(i in 1:length(regions)){
	nFish[[i]] <- array(NA, dim = c(length(unique(dat$species)), length(unique(dat$date[dat$region == regions[i]]))), dimnames = list(unique(dat$species), unique(dat$date[dat$region == regions[i]])))
	for(j in 1:length(unique(dat$species))){
		dum <- tapply(dat$region[dat$region == regions[i] & dat$species == unique(dat$species)[j]], dat$date[dat$region == regions[i] & dat$species == unique(dat$species)[j]], length)
		nFish[[i]][j, match(as.character(names(dum)), as.character(unique(dat$date[dat$region == regions[i]])))] <- dum
	}
}

# quartz(width = 6.3, height = 3, pointsize = 10)
pdf("figures/FishNumbers.pdf", width = 6.3, height = 3, pointsize = 10)
par(mar = c(3, 4, 3, 12))
plot(dat$date, rep(1, length(dat$date)), "n", ylim= c(0, 180), las = 1, bty = "l", ylab = "Number of fish sampled", xlab = "", xlim = as.Date(c("2020-04-01", "2020-09-12")))#range(spdates))
for(i in 1:length(regions)){
	for(j in 1:length(unique(dat$species))){
		points(unique(dat$date[dat$region == regions[i]]), nFish[[i]][j, ], col = colRegion[i], pch = j)
	}
	segments(x0 = min(dat$date[dat$region == regions[i]]), x1 = max(dat$date[dat$region == regions[i]]), y0 = 180 + (i-1)*6, y1 = 180 + (i-1)*6, col = colRegion[i], lwd = 4, xpd = NA)
}

legend(as.Date("2020-10-10"), 220, pch = c(1:length(unique(dat$species))), col = grey(0.6), legend = unique(dat$species), title = "Species", xpd =NA)
legend(as.Date("2020-10-10"), 100, fill = colRegion, legend = regions, title = "Region", xpd =NA, border = NA)
dev.off()

# Fish size

colSpp <- c("#FF0000", "#FF33CC", "#666633", "#003399", "#FF9933")
hist(dat$length, xlab = "Fish length (mm)", las = 1, col = NA, border = NA, main = "", breaks= seq(5, 160, 5), ylim = c(0, 300), xlim = c(20, 160))
for(i in length(species)){
	h <- hist(dat$length[dat$species == species[i]], col = paste(colSpp[i], "75", sep = ""), add = TRUE, border = NA, breaks= seq(5, 160, 5))
	dens <- density(dat$length[dat$species == species[i]], na.rm = TRUE)
  lines(dens[[1]], dens[[2]]/max(h$density)*max(h$counts), col = colSpp[i], lwd = 1.5)
}

legend("topright", col = colSpp, lwd = 1.2, unique(dat$species))

# Table: for each region/species: number sampled, mean forklength, mean juveniel lice (cops and chals, both spp), mean adult lep and mean adult caligus abundance
datSumm <- data.frame(
	region = rep(regions, each = length(species)),
	species = rep(species, length(regions)), 
	nFish = rep(NA, length(species)*length(regions)),
	forklength = rep(NA, length(species)*length(regions)),
	juvenileLice = rep(NA, length(species)*length(regions)),
	adultLep = rep(NA, length(species)*length(regions)),
	adultCaligus = rep(NA, length(species)*length(regions)))

for(i in 1:length(regions)){
	for(j in 1:length(species)){
		
		dat.ij <- dat[dat$region == regions[i] & dat$species == species[j], ]
		
		if(dim(dat.ij)[1] > 0){
			
			ind <- which(datSumm$region == regions[i] & datSumm$species == species[j])
			
			datSumm[ind, 'nFish'] <- dim(dat.ij)[1]
			
			dumLength <- bootLice(dat.ij$length, na.rm = TRUE)
			datSumm[ind, 'forklength'] <- paste(sprintf("%.1f", dumLength[1]), " (", sprintf("%.1f", dumLength[2]), ", ", sprintf("%.1f", dumLength[3]), ")", sep = "")
			
			dumL1 <- bootLice(apply(dat.ij[, c("Lep_cope", "Cal_cope", "chalA", "chalB", "unid_cope", "unid_chal")], 1, sum, na.rm = TRUE))
			datSumm[ind, 'juvenileLice'] <- paste(sprintf("%.2f", dumL1[1]), " (", sprintf("%.2f", dumL1[2]), ", ", sprintf("%.2f", dumL1[3]), ")", sep = "")
			
			dumL2 <- bootLice(apply(dat.ij[, c("Lep_PAfemale", "Lep_PAmale", "Lep_nongravid", "Lep_gravid", "Lep_unid")], 1, sum, na.rm = TRUE))
			datSumm[ind, 'adultLep'] <- paste(sprintf("%.2f", dumL2[1]), " (", sprintf("%.2f", dumL2[2]), ", ", sprintf("%.2f", dumL2[3]), ")", sep = "")		
			
			dumL3 <- bootLice(apply(dat.ij[, c("Caligus_mot", "Caligus_gravid")], 1, sum, na.rm = TRUE))
			datSumm[ind, 'adultCaligus'] <- paste(sprintf("%.2f", dumL3[1]), " (", sprintf("%.2f", dumL3[2]), ", ", sprintf("%.2f", dumL3[3]), ")", sep = "")	
		}
}}

write.csv(datSumm, file = "dataSummary.csv")
	
###############################################################################
# Location of samples
###############################################################################

library(PBSmapping)
gshhg <- "~/Google Drive/Mapping/gshhg-bin-2.3.7/"
xlim <- c(-128, -124) + 360
ylim <- c(48.6, 51.4)
land <- importGSHHS(paste0(gshhg,"gshhs_i.b"), xlim = xlim, ylim = ylim, maxLevel = 2, useWest = TRUE)
rivers <- importGSHHS(paste0(gshhg,"wdb_rivers_i.b"), xlim = xlim, ylim = ylim, useWest = TRUE)
borders <- importGSHHS(paste0(gshhg,"wdb_borders_i.b"), xlim = xlim, ylim = ylim, useWest = TRUE, maxLevel = 1)

#------------------------------------------------------------------------------
# Sampling site locations
siteDat <- data.frame(uniqueSiteID = unique(paste(dat$region, dat$location, dat$date, sep = "--")))
siteDat$EID <- 1:length(siteDat$uniqueSiteID)
siteDat$nFish <- rep(NA, dim(siteDat)[1])
siteDat$X <- rep(NA, dim(siteDat)[1])
siteDat$Y <- rep(NA, dim(siteDat)[1])
siteDat$region <- rep(NA, dim(siteDat)[1])
siteDat$location <- rep(NA, dim(siteDat)[1])

for(i in 1:dim(siteDat)[1]){
  dum <- which(paste(dat$region, dat$location, dat$date, sep = "--") == siteDat$uniqueSiteID[i])
  siteDat$nFish[i] <- length(dum)
  siteDat$X[i] <- - dat$lon[dum[1]]
  siteDat$Y[i] <- dat$lat[dum[1]]
  siteDat$region[i] <- dat$region[dum[1]]
  siteDat$location[i] <- dat$location[dum[1]]
}

siteDat1 <- as.EventData(siteDat[which(is.na(siteDat$X) == FALSE), ], projection = "LL")


#------------------------------------------------------------------------------
# Farm locations
datFarm <- read.csv('data/lice-audit-verif-pou-2011-ongoing-rpt-pac-dfo-mpo-aquaculture-eng.csv')
datFarm$Audit.Date <- as.Date(datFarm$Audit.Date, format = "%d-%b-%y")
datFarm <- subset(datFarm, datFarm$Audit.Date >= "2019-01-01")	

farmLoc <- data.frame(farmName = unique(datFarm$Site.Common.Name))
farmLoc$EID <- 1:length(farmLoc$farmName)
farmLoc$X <- datFarm$Longitude[match(farmLoc$farmName, datFarm$Site.Common.Name)]
farmLoc$Y <- datFarm$Latitude[match(farmLoc$farmName, datFarm$Site.Common.Name)]
farmLoc$FishHealthZone <- datFarm$Fish.Health.Zone[match(farmLoc$farmName, datFarm$Site.Common.Name)]
farmLoc$Company <- datFarm$Licence.Holder[match(farmLoc$farmName, datFarm$Site.Common.Name)]

farmLoc <- as.EventData(farmLoc, projection = "LL")

#------------------------------------------------------------------------------
# Plot map
quartz(width = 5, height = 6)
plotMap(land, xlim = xlim - 360, ylim = ylim,	col = grey(0.8), bg = "aliceblue", las = 1, lwd = 0.5, border = grey(0.6))
addPoints(farmLoc, pch = 14 + as.numeric(as.factor(c(farmLoc$Company))), cex = 0.6)
addPoints(siteDat1, pch = pchRegion[siteDat1$region], col = colRegion[siteDat1$region])#, col = c(3, 2, 4)[as.numeric(as.factor(siteDat1$region))])
legend("bottomleft", pch = c(15:17, pchRegion), pt.lwd = 1.5, col = c(rep(1, 3), colRegion), legend = c("Farm (Cermaq)", "Farm (Greig)", "Farm (MOWI)", regions), bg = "white", pt.cex = c(rep(0.6, 3), rep(1, length(regions))), cex = 0.8)


###############################################################################
# Overall prevalence, by region
###############################################################################

prevalence <- list(); length(prevalence) <- length(regions)
overallAbundance <- list(); length(prevalence) <- length(regions)
weeks <- seq(as.Date("2020-03-29"), max(dat$date), 7)

for(i in 1:length(regions)){
	weeks.i <- unique(findInterval(unique(dat$date[dat$region == regions[i]]), weeks))
	dateWeekInd <- findInterval(unique(dat$date[dat$region == regions[i]]), weeks)
	
	prevalence[[i]] <- array(NA, dim = c(length(weeks.i), 3), dimnames = list(as.character(weeks[weeks.i]), c("mean", "LCI", "UCI")))
	overallAbundance[[i]] <- array(NA, dim = c(length(weeks.i), 3), dimnames = list(as.character(weeks[weeks.i]), c("mean", "LCI", "UCI")))
	
	for(j in 1:length(weeks.i)){ # for each day
		
		allLice <- as.numeric(apply(dat[dat$region == regions[i] & is.element(dat$date, unique(dat$date[dat$region == regions[i]])[which(dateWeekInd == dateWeekInd[j])]), which(names(dat) =="Lep_cope") : which(names(dat) == "unid_adult")], 1, sum))
		
		overallAbundance[[i]][j, ] <- bootLice(allLice)
		
		prevLice <- as.numeric(allLice > 0)
		prevalence[[i]][j, ] <- bootLice(prevLice)
		
		
	}}

#------------------------------------------------------------------------------
# Plot
# quartz(width = 5, height = 5, pointsize = 10)
pdf("figures/OverallPrevAbund.pdf", width = 5, height = 5, pointsize = 10)
par(mfrow = c(2, 1), mar= c(4, 4, 2, 1))

# Prevalence
plot(unique(dat$date), rep(1, length(unique(dat$date))), "n", ylim = c(0, 1), las = 1, bty = "l", ylab = "Prevalence", xlab = "", yaxs="i")
mtext(side = 3, line = 0.5, adj = 0, "a)")
abline(v = weeks, col= grey(0.8), lwd = 0.8)
for(i in 1:length(regions)){
	plotCI(as.Date(rownames(prevalence[[i]])) + i, prevalence[[i]][, 'mean'], li = prevalence[[i]][, 'LCI'], ui = prevalence[[i]][, 'UCI'], col = colRegion[i], pch = pchRegion[i], gap = 0.3,  add=TRUE, sfrac = 0, xpd = NA, lwd = 1.2)
	}
legend("bottomright", bg = "white", pch = pchRegion, col = colRegion, pt.lwd = 1.2, regions)

# Overall  abundance
ymax <- 35
plot(unique(dat$date), rep(1, length(unique(dat$date))), "n", ylim = c(0, ymax), las = 1, bty = "l", ylab = "Abundnace", xlab = "")
mtext(side = 3, line = 0.5, adj = 0, "b)")
abline(v = weeks, col= grey(0.8), lwd = 0.8)
for(i in 1:length(regions)){
	plotCI(as.Date(rownames(overallAbundance[[i]])) + i, overallAbundance[[i]][, 'mean'], li = overallAbundance[[i]][, 'LCI'], ui = overallAbundance[[i]][, 'UCI'], col = colRegion[i], pch = pchRegion[i], gap = 0.3,  add=TRUE, sfrac = 0, xpd = NA, lwd = 1.2)
	
	if(sum(overallAbundance[[i]][, 'UCI'] > ymax) > 0){
		ind <- which(overallAbundance[[i]][, 'UCI'] > ymax)
		for(a in 1:length(ind)){
			arrows(x0 = as.Date(rownames(overallAbundance[[i]]))[ind[a]] + i, x1 = as.Date(rownames(overallAbundance[[i]]))[ind[a]] + i, y0 = ymax, y1 = ymax*1.05, length = 0.08, xpd = NA, col = colRegion[i])
		}
	}
	
}

dev.off()
###############################################################################
# Lice by stage, any species, all regions separated
###############################################################################


liceByStage <- list(); length(liceByStage) <- length(regions)*3; dim(liceByStage) <- c(length(regions), 3)
dimnames(liceByStage) <- list(regions, c("Cope", "Chal", "Mot"))

weeks <- seq(as.Date("2020-03-29"), max(dat$date), 7)


for(i in 1:length(regions)){
	
	weeks.i <- unique(findInterval(unique(dat$date[dat$region == regions[i]]), weeks))
	
	liceByStage[[i, 1]] <- array(NA, dim = c(length(weeks.i), 3), dimnames = list(as.character(weeks[weeks.i]), c("mean", "LCI", "UCI")))
	liceByStage[[i, 2]] <- array(NA, dim = c(length(weeks.i), 3), dimnames = list(as.character(weeks[weeks.i]), c("mean", "LCI", "UCI")))
	liceByStage[[i, 3]] <- array(NA, dim = c(length(weeks.i), 3), dimnames = list(as.character(weeks[weeks.i]), c("mean", "LCI", "UCI")))
	
	dateWeekInd <- findInterval(unique(dat$date[dat$region == regions[i]]), weeks)
	
	for(j in 1:length(weeks.i)){ # for each day
		
		
		L <- as.numeric(apply(dat[dat$region == regions[i] & is.element(dat$date, unique(dat$date[dat$region == regions[i]])[which(dateWeekInd == dateWeekInd[j])]), c('Lep_cope', 'Cal_cope', 'unid_cope')], 1, sum))
		liceByStage[[i, 1]][j, ] <- bootLice(L)
		
		C <- apply(dat[dat$region == regions[i] & is.element(dat$date, unique(dat$date[dat$region == regions[i]])[which(dateWeekInd == dateWeekInd[j])]), c('chalA', 'chalB', 'unid_chal')], 1, sum)
		liceByStage[[i, 2]][j, ] <- bootLice(C)
		
		M <- apply(dat[dat$region == regions[i] & is.element(dat$date, unique(dat$date[dat$region == regions[i]])[which(dateWeekInd == dateWeekInd[j])]), c('Lep_PAfemale', 'Lep_PAmale', 'Lep_nongravid', 'Lep_gravid', 'Lep_unid', 'Caligus_mot', 'Caligus_gravid', 'unid_PA', 'unid_adult')], 1, sum)
		liceByStage[[i, 3]][j, ] <- bootLice(M)
		
	} # end day j
} # end region i



#------------------------------------------------------------------------------
# Plot

ylims <- c(6, 25, 4)
# quartz(width = 5, height = 6, pointsize = 10)
pdf("figures/LiceByStage.pdf", width = 5, height = 6, pointsize = 10)
par(mfrow = c(3, 1), mar= c(4, 4, 2, 1))

for(s in 1:3){
	plot(unique(dat$date), rep(1, length(unique(dat$date))), "n", ylim = c(0, ylims[s]), las = 1, bty = "l", ylab = "Abundance per fish", xlab = "")
	mtext(side = 3, line = 0.5, adj = 0, paste(letters[s], c("Copepodid", "Chalimus", "Motile")[s], sep=") "))
	abline(v = weeks, col= grey(0.8), lwd = 0.8)
	for(i in 1:length(regions)){
		plotCI(as.Date(rownames(liceByStage[[i, s]])) + i, liceByStage[[i, s]][, 'mean'], li = liceByStage[[i, s]][, 'LCI'], ui = liceByStage[[i, s]][, 'UCI'], col = colRegion[i], pch = pchRegion[i], gap = 0.5,  add=TRUE, sfrac = 0, xpd = NA, lwd = 1.2)
		if(sum(liceByStage[[i, s]][, 'UCI'] > ylims[s]) > 0){
			ind <- which(liceByStage[[i, s]][, 'UCI'] > ylims[s])
			for(a in 1:length(ind)){
				arrows(x0 = as.Date(rownames(liceByStage[[i, s]]))[ind[a]] + i, x1 = as.Date(rownames(liceByStage[[i, s]]))[ind[a]] + i, y0 = ylims[s], y1 = ylims[s]*1.05, length = 0.08, xpd = NA, col = colRegion[i])
			}
		}
	}
	if(s == 1) legend("topright", pch = pchRegion, col = colRegion, regions, bg = 'white', pt.lwd = 1.2)
}

dev.off()
###############################################################################
# Adult lice by species
###############################################################################


motsBySpp <- list(); length(motsBySpp) <- length(regions)*2; dim(motsBySpp) <- c(length(regions), 2)
dimnames(motsBySpp) <- list(regions, c("Lep", "Cal"))

weeks <- seq(as.Date("2020-03-29"), max(dat$date), 7)

for(i in 1:length(regions)){
	
	weeks.i <- unique(findInterval(unique(dat$date[dat$region == regions[i]]), weeks))
	
	motsBySpp[[i, 1]] <- array(NA, dim = c(length(weeks.i), 3), dimnames = list(as.character(weeks[weeks.i]), c("mean", "LCI", "UCI")))
	motsBySpp[[i, 2]] <- array(NA, dim = c(length(weeks.i), 3), dimnames = list(as.character(weeks[weeks.i]), c("mean", "LCI", "UCI")))
	
	dateWeekInd <- findInterval(unique(dat$date[dat$region == regions[i]]), weeks)
	
	for(j in 1:length(weeks.i)){ # for each day
		
		
		Lep <- as.numeric(apply(dat[dat$region == regions[i] & is.element(dat$date, unique(dat$date[dat$region == regions[i]])[which(dateWeekInd == dateWeekInd[j])]), c('Lep_PAfemale', 'Lep_PAmale', 'Lep_nongravid', 'Lep_gravid', 'Lep_unid')], 1, sum))
		motsBySpp[[i, 1]][j, ] <- bootLice(Lep)
		
		Cal <- as.numeric(apply(dat[dat$region == regions[i] & is.element(dat$date, unique(dat$date[dat$region == regions[i]])[which(dateWeekInd == dateWeekInd[j])]), c('Caligus_mot', 'Caligus_gravid')], 1, sum))
		motsBySpp[[i, 2]][j, ] <- bootLice(Cal)
		
	} # end day j
} # end region i


#------------------------------------------------------------------------------
# Plot

ylims <- c(3, 1)
# quartz(width = 5, height = 5, pointsize = 10)
pdf(file = "figures/LiceBySpecies.pdf", width = 5, height = 5, pointsize = 10)
par(mfrow = c(2, 1), mar= c(4, 4, 2, 1))

for(s in 1:2){
	plot(unique(dat$date), rep(1, length(unique(dat$date))), "n", ylim = c(0, ylims[s]), las = 1, bty = "l", ylab = "Abundance per fish", xlab = "")
	mtext(side = 3, line = 0.5, adj = 0, paste(letters[s], c("Lepeoptheirus", "Caligus")[s], sep=") "))
	abline(v = weeks, col= grey(0.8), lwd = 0.8)
	for(i in 1:length(regions)){
    pch.i <- c(pchRegion[i], NA)[as.numeric(motsBySpp[[i, s]][, 'mean'] > ylims[s]) + 1]
		plotCI(as.Date(rownames(motsBySpp[[i, s]])) + i, motsBySpp[[i, s]][, 'mean'], li = motsBySpp[[i, s]][, 'LCI'], ui = motsBySpp[[i, s]][, 'UCI'], col = colRegion[i], gap = 0.3,  add=TRUE, sfrac = 0, xpd = NA, pch = pch.i)
		if(sum(motsBySpp[[i, s]][, 'UCI'] > ylims[s]) > 0){
			ind <- which(motsBySpp[[i, s]][, 'UCI'] > ylims[s])
			for(a in 1:length(ind)){
				arrows(x0 = as.Date(rownames(motsBySpp[[i, s]]))[ind[a]] + i, x1 = as.Date(rownames(motsBySpp[[i, s]]))[ind[a]] + i, y0 = ylims[s], y1 = ylims[s]*1.05, length = 0.08, xpd = NA, col = colRegion[i])
			}
		}
	}
	if(s == 2) legend("bottomright",  pch = pchRegion, col = colRegion, regions, bg = 'white')
}

dev.off()