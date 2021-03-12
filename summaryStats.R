
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


# # How many total samples by species for each region?
# nFish2020 <- array(NA, dim = c(5, 5), dimnames = list(regions, names(apply(nFish[[1]], 1, sum, na.rm=TRUE))))
# for(i in 1:length(regions)){
# 	nFish2020[i, ] <- apply(nFish[[i]], 1, sum, na.rm=TRUE)
# }
# 
# bp <- barplot(nFish2020, beside = TRUE, col = colRegion, las =1, ylim = c(0, 1200), main = "Juvenile salmon sampled for sea lice in 2020", ylab = "Number of fish")
# text(bp, nFish2020, pos = 3, srt = 2, nFish2020, cex = 0.8, xpd =NA)
# abline(h = 1)
# legend("topright", fill = colRegion, regions)

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
			
			dumL2 <- bootLice(apply(dat.ij[, c("Lep_PAfemale", "Lep_PAmale", "Lep_nongravid", "Lep_gravid", "Lep_unid", "Lep_male")], 1, sum, na.rm = TRUE))
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
			text(as.Date(rownames(overallAbundance[[i]]))[ind[a]] + i, ymax, round(overallAbundance[[i]][ ind[a], 'UCI'], 1), pos = 3, cex = 0.8, xpd = NA)
		}
	}
	
}

text()

dev.off()
###############################################################################
# Lice by stage, any species, all regions separated
###############################################################################

sp <- "both"
# Set up list to store bootstrapped means
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
		
		# Copes
		if(sp == "L"){
			L <- as.numeric(dat[dat$region == regions[i] & is.element(dat$date, unique(dat$date[dat$region == regions[i]])[which(dateWeekInd == dateWeekInd[j])]), c('Lep_cope')])
		} else if(sp == "C"){
			L <- as.numeric(dat[dat$region == regions[i] & is.element(dat$date, unique(dat$date[dat$region == regions[i]])[which(dateWeekInd == dateWeekInd[j])]), c('Cal_cope')])
		} else {
			L <- as.numeric(apply(dat[dat$region == regions[i] & is.element(dat$date, unique(dat$date[dat$region == regions[i]])[which(dateWeekInd == dateWeekInd[j])]), c('Lep_cope', 'Cal_cope', 'unid_cope')], 1, sum))
		}
		
		liceByStage[[i, 1]][j, ] <- bootLice(L)
		
		# Chalimus - no split between species
		C <- apply(dat[dat$region == regions[i] & is.element(dat$date, unique(dat$date[dat$region == regions[i]])[which(dateWeekInd == dateWeekInd[j])]), c('chalA', 'chalB', 'unid_chal')], 1, sum)
		
		liceByStage[[i, 2]][j, ] <- bootLice(C)
		
		# Motiles
		
		if(sp == "L"){
			M <- apply(dat[dat$region == regions[i] & is.element(dat$date, unique(dat$date[dat$region == regions[i]])[which(dateWeekInd == dateWeekInd[j])]), c('Lep_PAfemale', 'Lep_PAmale', 'Lep_male', 'Lep_nongravid', 'Lep_gravid', 'Lep_unid')], 1, sum)
		} else if(sp == "C"){
			M <- apply(dat[dat$region == regions[i] & is.element(dat$date, unique(dat$date[dat$region == regions[i]])[which(dateWeekInd == dateWeekInd[j])]), c('Caligus_mot', 'Caligus_gravid')], 1, sum)
		} else {
			M <- apply(dat[dat$region == regions[i] & is.element(dat$date, unique(dat$date[dat$region == regions[i]])[which(dateWeekInd == dateWeekInd[j])]), c('Lep_PAfemale', 'Lep_PAmale', 'Lep_nongravid', 'Lep_gravid', 'Lep_unid', 'Caligus_mot', 'Caligus_gravid', 'Lep_male', 'unid_PA', 'unid_adult')], 1, sum)
		}
		liceByStage[[i, 3]][j, ] <- bootLice(M)
		
	} # end day j
} # end region i



#------------------------------------------------------------------------------
# Plot

ylims <- c(6, 25, 5)
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
		
		
		Lep <- as.numeric(apply(dat[dat$region == regions[i] & is.element(dat$date, unique(dat$date[dat$region == regions[i]])[which(dateWeekInd == dateWeekInd[j])]), c('Lep_PAfemale', 'Lep_PAmale', 'Lep_nongravid', 'Lep_gravid', 'Lep_male', 'Lep_unid')], 1, sum))
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

###############################################################################
# Plots for Alex Feb 25, 2021
###############################################################################

#------------------------------------------------------------------------------
# Check Fig. 5c - mots vs copes in PortHardy samples
#------------------------------------------------------------------------------
quartz(pointsize =10)
par(mar = c(4,4,2,1))
hist(dat$Caligus_mot[dat$date == as.Date("2020-07-13")], breaks = seq(-0.5, 7.5, 1), col = "#0000FF70", border =NA, las = 1, xlab = "Number of lice per fish", main = "July 13 at Port Hardy")
hist(dat$Caligus_gravid[dat$date == as.Date("2020-07-13")], add = TRUE, col = "#FF000070", border = NA_character_, breaks = seq(-0.5, 7.5, 1))
legend("topright", fill = c("#0000FF70", "#FF000070"), c("adult Caligus", "gravid Caligus"))

#------------------------------------------------------------------------------
# SIngle abundance and prevalence for each region
#------------------------------------------------------------------------------

prevalence1 <- array(NA, dim = c(length(regions), 3), dimnames = list(regions, c("mean", "li", "ui")))
abundance1 <- array(NA, dim = c(length(regions), 3), dimnames = list(regions, c("mean", "li", "ui")))

for(i in 1:length(regions)){
	
	allLice <- as.numeric(apply(dat[dat$region == regions[i], which(names(dat) =="Lep_cope") : which(names(dat) == "unid_adult")], 1, sum))
	
	abundance1[i, ] <- bootLice(allLice)
	
	prevLice <- as.numeric(allLice > 0)
	prevalence1[i, ] <- bootLice(prevLice)
}

quartz(width = 5, height = 5, pointsize = 10)
par(mfrow = c(2,1), mar = c(4,4,2,1))
barplot2(prevalence1[, 1], plot.ci = TRUE, ci.l = prevalence1[, 2], ci.u = prevalence1[, 3], names.arg = , ylim = c(0,1), las = 1, ylab = "Prevalence", srt = 90)#, col=colRegion
abline(h = 0)
mtext(side = 3, adj= 0, line = 0.5, "a)")

barplot2(abundance1[, 1], plot.ci = TRUE, ci.l = abundance1[, 2], ci.u = abundance1[, 3], names.arg = regions, las = 1, ylab  = "Abundance")#, col=colRegion
abline(h = 0)
mtext(side = 3, adj= 0, line = 0.5, "b)")

#------------------------------------------------------------------------------
# SOckeye abundance and prevalence for each region
#------------------------------------------------------------------------------
prevalenceSK <- array(0, dim = c(length(regions), 3), dimnames = list(regions, c("mean", "li", "ui")))
abundanceSK <- array(0, dim = c(length(regions), 3), dimnames = list(regions, c("mean", "li", "ui")))
nFishSK <- numeric(length(regions))

for(i in 1:length(regions)){
	
	dat.i <- dat[dat$region == regions[i] & dat$species == "sockeye", which(names(dat) =="Lep_cope") : which(names(dat) == "unid_adult")]
	
	nFishSK[i] <- dim(dat.i)[1]
	
	if(nFishSK[i] > 0){
		allLice <- as.numeric(apply(dat.i, 1, sum))
		
		abundanceSK[i, ] <- bootLice(allLice)
		
		prevLice <- as.numeric(allLice > 0)
		prevalenceSK[i, ] <- bootLice(prevLice)
	}
}

quartz(width = 5, height = 5, pointsize = 10)
par(mfrow = c(2,1), mar = c(4,4,2,1))
barplot2(prevalenceSK[, 1], plot.ci = TRUE, ci.l = prevalenceSK[, 2], ci.u = prevalenceSK[, 3], names.arg = , col=colRegion, ylim = c(0,1), las = 1, ylab = "Prevalence", srt = 90)
abline(h = 0)
mtext(side = 3, adj= 0, line = 0.5, "a)")

bp <- barplot2(abundanceSK[, 1], plot.ci = TRUE, ci.l = abundanceSK[, 2], ci.u = abundanceSK[, 3], names.arg = regions, col=colRegion, las = 1, ylab  = "Abundance")
abline(h = 0)
mtext(side = 3, adj= 0, line = 0.5, "b)")

# text(bp, abundanceSK[, 3], nFishSK, pos = 3, xpd = NA)

write.csv(cbind(prevalenceSK, abundanceSK), file = "sockeyeAbundPrev.csv")


#------------------------------------------------------------------------------
# Port Hardy
#------------------------------------------------------------------------------
apply(dat[dat$region == "PortHardy", which(names(dat) =="Lep_cope") : which(names(dat) == "unid_adult")], 2, sum, na.rm = TRUE) #There are no unid, so don't worry about that

PHdates <- unique(dat$date[dat$region == "PortHardy"])
stages <- c("LepCope", "CalCope", "ChalA", "ChalB", "LepPA", "LepA",  "CalA")
stagesCol <- list(
	"Lep_cope" = which(names(dat) == "Lep_cope"),
  "Cal_cope" = which(names(dat) == "Cal_cope"),
  "chalA" = which(names(dat) == "chalA"),
  "chalB" = which(names(dat) == "chalB"),
  "Lep_PA" = which(names(dat) %in% c("Lep_PAfemale", "Lep_PAmale")),
	"Lep_adult" = which(names(dat) %in% c("Lep_nongravid", "Lep_gravid", "Lep_male")),
	"Caligus_adult" = which(names(dat) %in% c("Caligus_mot", "Caligus_gravid")))
		
# Set up list to store bootstrapped means
liceByStagePH <- array(NA, dim = c(3, length(stages), 3), dimnames = list(PHdates, stages, c("mean", "li", "ui")))

for(j in 1:length(PHdates)){
	for(i in 1:length(stages)){
		dat.ij <- dat[which(dat$region == "PortHardy" & dat$date == PHdates[j]), stagesCol[[i]]]
		if(length(stagesCol[[i]]) > 1) L <- apply(dat.ij, 1, sum) else L <- dat.ij
		liceByStagePH[j, i, ] <- bootLice(L)
	}	
}

# Plot
x <- c(1:4, 5.5, 6.5, 7.5)
colx <- c(2,4,1)[c(1,2,3,3,1,1,2)]

quartz(width = 5, height = 6, pointsize = 10)
par(mfrow = c(3, 1), mar= c(4, 4, 2, 1))

for(j in 1:length(PHdates)){
	plotCI(x, liceByStagePH[j, ,1], li = liceByStagePH[j, ,2], ui = liceByStagePH[j, ,3], pch = 21, pt.bg = colx, xaxt = "n", xlim = c(0.5, 8), cex = 1.5, gap = 0, las = 1, bty = "l", xlab = "", ylab = "Abundance", ylim = range(liceByStagePH))
	mtext(side = 3, adj = 0, line = 0.5, paste(letters[j], ") ", c("July 13, 2020", "July 30, 2020", "September 12, 2020")[j], sep = ""))
	axis(side = 1, at = x, labels = stages)
}


# Plot differently
quartz(width = 5, height = 6, pointsize = 10)
layout(mat = matrix(c(1,1,2,3,3,4,5,5,6), nrow = 3, byrow = TRUE))
par(mar= c(3,2, 2, 1), oma = c(2,2.5,0,0))

for(j in 1:length(PHdates)){
	plotCI(1:4, liceByStagePH[j, 1:4,1], li = liceByStagePH[j, 1:4,2], ui = liceByStagePH[j, 1:4,3], pch = 21, pt.bg = colx[1:4], xaxt = "n", xlim = c(0.5, 4.5), cex = 1.5, gap = 0, las = 1, bty = "l", xlab = "", ylab = "", ylim = range(liceByStagePH[,1:4,]))
	axis(side = 1, at = 1:4, labels = stages[1:4])
	
	mtext(side = 3, adj = 0, line = 0.5, paste(letters[j], ") ", c("July 13, 2020", "July 30, 2020", "September 12, 2020")[j], sep = ""))
	
	if(j == 1) legend("topleft", pch = 21, pt.bg = c(2,4,1), legend = c("Lep", "Cal", "Unknown"), pt.cex = 1.5, bty = "n")
	
	plotCI(1:3, liceByStagePH[j, 5:7,1], li = liceByStagePH[j, 5:7,2], ui = liceByStagePH[j, 5:7,3], pch = 21, pt.bg = colx[5:7], xaxt = "n", xlim = c(0.5, 3.5), cex = 1.5, gap = 0, las = 1, bty = "l", xlab = "", ylab = "", ylim = range(liceByStagePH[,5:7,]))
	
	axis(side = 1, at = 1:3, labels = stages[5:7])
}
mtext(side =1, outer = TRUE, "Stage of sea lice")
mtext(side =2, outer = TRUE, "Abundance", line = 1)

# Condensed dates
quartz(width = 5, height = 3.5, pointsize = 10)
layout(mat = matrix(c(1,1,1,1,2,2,2), nrow = 1, byrow = TRUE))
par(mar= c(3, 2, 2, 1), oma = c(2,2.5,0,0))

j <- 1
plotCI(1:4 + c(-0.12, 0, 0.12)[j], liceByStagePH[j, 1:4,1], li = liceByStagePH[j, 1:4,2], ui = liceByStagePH[j, 1:4,3], pch = 21, pt.bg = "white", col = colx[1:4], xaxt = "n", xlim = c(0.5, 4.5), cex = 1.5, gap = 0, las = 1, bty = "l", xlab = "", ylab = "", ylim = c(0,10))#range(liceByStagePH[,1:4,]))
for(j in 2:3) plotCI(1:4+ c(-0.12, 0, 0.12)[j], liceByStagePH[j, 1:4,1], li = liceByStagePH[j, 1:4,2], ui = liceByStagePH[j, 1:4,3], pch = c(21, 22, 23)[j], pt.bg = list("white", colx[1:4], c("#FF000050", "#0000FF50", "#00000050", "#00000050"))[[j]], col = colx[1:4], cex = 1.5, gap = 0, add = TRUE)

axis(side = 1, at = 1:4, labels = stages[1:4])

legend("topleft", pch = c(21:23), pt.bg = c("white", 1, "#00000050"), c("Jul 13", "Jul 30", "Sep 12"), bty = "n")

if(j == 1) legend("topleft", pch = 21, pt.bg = c(2,4,1), legend = c("Lep", "Cal", "Unknown"), pt.cex = 1.5, bty = "n")

j <- 1
plotCI(1:3+ c(-0.12, 0, 0.12)[j], liceByStagePH[j, 5:7,1], li = liceByStagePH[j, 5:7,2], ui = liceByStagePH[j, 5:7,3], pch = 21, pt.bg = "white", col = colx[1:4], xaxt = "n", xlim = c(0.5, 3.5), cex = 1.5, gap = 0, las = 1, bty = "l", xlab = "", ylab = "", ylim = range(liceByStagePH[,5:7,]))
for(j in 2:3) plotCI(1:3+ c(-0.12, 0, 0.12)[j], liceByStagePH[j, 5:7,1], li = liceByStagePH[j, 5:7,2], ui = liceByStagePH[j, 5:7,3], pch = c(21, 22, 23)[j], pt.bg = list("white", colx[1:4], c("#FF000050", "#0000FF50", "#00000050", "#00000050"))[[j]], col = colx[1:4], cex = 1.5, gap = 0, add = TRUE)
axis(side = 1, at = 1:3, labels = stages[5:7])
mtext(side =1, outer = TRUE, "Stage of sea lice")
mtext(side =2, outer = TRUE, "Abundance", line = 1)


#------------------------------------------------------------------------------
# Por Hardy try # 2

#------------------------------------------------------------------------------
# Port Hardy
#------------------------------------------------------------------------------

PHdates <- unique(dat$date[dat$region == "PortHardy"])
stages <- c("Cope", "ChalA", "ChalB", "Mot")
stagesCol <- list(
	"Cope" = which(names(dat) %in% c("Lep_cope", "Cal_cope")),
	"chalA" = which(names(dat) == "chalA"),
	"chalB" = which(names(dat) == "chalB"),
	"Mot" = which(names(dat) %in% c("Lep_PAfemale", "Lep_PAmale", "Lep_nongravid", "Lep_gravid", "Lep_male","Caligus_mot", "Caligus_gravid")))

# Set up list to store bootstrapped means
liceByStagePH <- array(NA, dim = c(3, length(stages), 3), dimnames = list(PHdates, stages, c("mean", "li", "ui")))

for(j in 1:length(PHdates)){
	for(i in 1:length(stages)){
		dat.ij <- dat[which(dat$region == "PortHardy" & dat$date == PHdates[j]), stagesCol[[i]]]
		if(length(stagesCol[[i]]) > 1) L <- apply(dat.ij, 1, sum) else L <- dat.ij
		liceByStagePH[j, i, ] <- bootLice(L)
	}	
}

par(mfrow = c(1,1))

barplot2(liceByStagePH[, , 1], plot.ci = TRUE, ci.l = liceByStagePH[, , 2], ci.u = liceByStagePH[, , 3],  names.arg = stages, col = c(2,grey(0.8),4), beside = TRUE, las = 1, ylab  = "Abundance")
legend("topright", fill =c(2,grey(0.8),4),  c("Jul 13", "Jul 30", "Sep 12"), bty = "n")

# 3-panel
quartz(width = 3.2, height = 5, pointsize = 10)
par(mfrow = c(3,1), mar = c(4,4,2,1))

for(i in 1:3){ # For each date
	barplot2(liceByStagePH[i, , 1], plot.ci = TRUE, ci.l = liceByStagePH[i, , 2], ci.u = liceByStagePH[i, , 3],  names.arg = stages, col = grey(0.8), las = 1, ylab  = "Abundance", ylim = c(0, 25))
	abline(h = 0)
	mtext(side = 3, line= 0.5, adj = 0, paste(letters[i], ") ", c("Jul 13", "Jul 30", "Sep 12")[i], sep =""))
}

mtext(side = 1, "Stage of sea lice", line = 3)


write.csv(rbind(liceByStagePH[1,,], liceByStagePH[2,,], liceByStagePH[3,,]), file = "PortHardyLice.csv")
