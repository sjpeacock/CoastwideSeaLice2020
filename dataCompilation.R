###############################################################################
# Align columns from the three datasets: Broughton, Discovery, and Clayoquot
# clean up lat/lon from site data etc.
###############################################################################

#------------------------------------------------------------------------------
# DIscovery Islands data from Alex
#------------------------------------------------------------------------------
discDat <- read.csv("data/Discovery.csv")

unique(discDat$DNA)
unique(discDat$set_id)
unique(discDat$location)

# Decimal degrees for lat/lon
unique(discDat$site_id) # These are lat/lons; need to change

# Correct one site that is wrong latitude (Nootka 3)
discDat[which(discDat$site_id == "40 49.11  126 34.67"), 'site_id'] <- "49 40.11  126 34.67"

discDat$lat <- rep(NA, dim(discDat)[1])
discDat$long <- rep(NA, dim(discDat)[1])

for(i in 1:dim(discDat)[1]){
	dum <- strsplit(discDat$site_id[i], split = " ")[[1]]
	discDat$lat[i] <- as.numeric(dum[1]) + as.numeric(dum[2])/60
	discDat$lon[i] <- as.numeric(dum[4]) + as.numeric(dum[5])/60
}

discDat$species[discDat$species == "Chinook "] <- "Chinook"
discDat$species[discDat$species == "chinook"] <- "Chinook"
discDat$species[discDat$species == "chinook-hatchery"] <- "Chinook-hatchery"

#------------------------------------------------------------------------------
#  Broughton data from Salmon Coast Field Station
#------------------------------------------------------------------------------
broFish <- read.csv("data/Broughton_fish.csv")
broSite <- read.csv("data/Broughton_site.csv")

names(broFish)

#------------------------------------------------------------------------------
# Calyoquot data from Cedar Coast Field Station
#------------------------------------------------------------------------------
clayFish <- read.csv("data/Clayoquot_fish.csv")
claySite <- read.csv("data/Clayoquot_site.csv")

claySite$lat <- rep(NA, dim(claySite)[1])
claySite$lon <- rep(NA, dim(claySite)[1])
for(i in 1:dim(claySite)[1]){
	dumLat <- strsplit(claySite$latitude[i], split ="°")[[1]]
	claySite$lat[i] <- as.numeric(dumLat[1]) + as.numeric(dumLat[2])/60
	
	dumLon <- strsplit(claySite$longitude[i], split ="°")[[1]]
	claySite$lon[i] <- as.numeric(dumLon[1]) + as.numeric(dumLon[2])/60
}

clayFish$species[clayFish$species == "chum "] <- "chum"
clayFish$species[clayFish$species == "chinook"] <- "Chinook"

#------------------------------------------------------------------------------
# Bring data sets together
#------------------------------------------------------------------------------
n <- list(disc = dim(discDat)[1], bro = dim(broFish)[1], clay = dim(clayFish)[1])

dat <- data.frame(
	region = c(
		rep("Discovery", n$disc),
		rep("Broughton", n$bro),
		rep("Clayoquot", n$clay)),
	
	source = c(
		rep("AMorton", n$disc), 
		rep("SalmonCoast", n$bro),
		rep("CedarCoast", n$clay)),
	
	# set_id = c(
	# 	discDat$set_id,
	# 	rep(NA, n$bro),
	# 	rep(NA, n$clay)),
	
	year = c(
		discDat$year,
		broFish$year,
		clayFish$year),
	
	month = c(
		discDat$month,
		broFish$month,
		clayFish$month),
	
	day = c(
		discDat$day,
		broFish$day,
		clayFish$day),
	
	site_id = c(
		discDat$site_id,
		broFish$site_id,
		clayFish$site_id),
	
	location = c(
		discDat$location,
		broFish$location,
		clayFish$location),
	
	salt = c(
		discDat$salt,
		broSite$salt[match(broFish$site_id, broSite$site_id)],
		claySite$salt_surf[match(clayFish$site_id, claySite$site_id)]),
	
	temp = c(
		discDat$temp,
		broSite$temp[match(broFish$site_id, broSite$site_id)],
		claySite$temp_surf[match(clayFish$site_id, claySite$site_id)]),
	
	lat = c(
		discDat$lat,
		c(50.789212, 50.847516, 50.786129)[as.numeric(as.factor(broSite$location[match(broFish$site_id, broSite$site_id)]))],
		claySite$lat[match(clayFish$site_id, claySite$site_id)]),
	
	lon = c(
		discDat$lon,
		c(126.492472, 126.309342, 126.688889)[as.numeric(as.factor(broSite$location[match(broFish$site_id, broSite$site_id)]))],
		claySite$lon[match(clayFish$site_id, claySite$site_id)]),
	
	fish_id = c(
		discDat$fish_id,
		broFish$fish_id,
		clayFish$fish_id),
	
	species = c(
		discDat$species,
		broFish$species,
		clayFish$species),
	
	length = c(
		as.numeric(discDat$length),
		broFish$length,
		clayFish$length),
	
	height = c(
		as.numeric(discDat$height),
		broFish$height,
		clayFish$height),
	
	Lep_cope = c(
		discDat$Lep_cope,
		broFish$Lep_cope,
		clayFish$Lep_cope),
	
	Cal_cope = c(
		discDat$Cal_cope,
		broFish$Caligus_cope,
		clayFish$Caligus_cope),
	
	chalA = c(
		discDat$chalA,
		broFish$chalA,
		clayFish$chalA),
	
	chalB = c(
		discDat$chalB,
		broFish$chalB,
		clayFish$chalB),
	
	Lep_PAfemale = c(
		as.numeric(discDat$Lep_PAfemale),
		broFish$Lep_PAfemale,
		clayFish$Lep_PAfemale),
	
	Lep_PAmale = c(
		as.numeric(discDat$Lep_PAmale) + as.numeric(discDat$novel.male),
		broFish$Lep_PAmale,
		clayFish$Lep_PAmale),
		
	Lep_nongravid = c(
		discDat$Lep_nongravid,
		broFish$Lep_nongravid,
		clayFish$Lep_nongravid),
	
	Lep_gravid = c(
		discDat$Lep_gravid,
		broFish$Lep_gravid,
		clayFish$Lep_gravid),
	#**
	Lep_male = c(
		discDat$Lep_male,
		broFish$Lep_male,
		clayFish$Lep_male),
	
	Lep_unid = c(
		discDat$Lep_unid,
		rep(NA, n$bro),
		rep(NA, n$clay)),

	Caligus_mot = c(
		as.numeric(discDat$Caligus_mot),
		broFish$Caligus_mot,
		clayFish$Caligus_mot),
	
	Caligus_gravid = c(
		as.numeric(discDat$Caligus_gravid),
		broFish$Caligus_gravid,
		clayFish$Caligus_gravid),
	
	unid_cope = c(
		discDat$unid_cope,
	  broFish$unid_cope,
	  clayFish$unid_cope),
	
	unid_chal = c(
		discDat$chal_unid,
		broFish$chal_unid,
		clayFish$chal_unid),
	
	unid_PA = c(
		discDat$unid_PA,
		broFish$unid_PA,
		clayFish$unid_PA),
	
	unid_adult = c(
		discDat$unid_adult,
		broFish$unid_adult,
		clayFish$unid_adult)
)


# Replace NAs with zeroes for lice counts
NAs <- which(is.na(dat[,16:31]) == TRUE, arr.ind = TRUE)
dat[cbind(NAs[, 1], c(16:31)[NAs[, 2]])] <- 0

unique(dat$location[dat$region == "Discovery"])

# Assign proper regions to Alex's samples outside of Discovery
dat$region[dat$location == "Blenkinsop Bay - Johnstone Strait"] <- "Johnstone"
dat$region[dat$location == "Knox Bay, Johnstone Strait"] <- "Johnstone"

dat$region[dat$location == "Port Hardy"] <- "QueenCharlotte"
dat$region[dat$location == "Port Alexander"] <- "QueenCharlotte"

dat$region[dat$location == "Jewitt Cove, Strange Island"] <- "Nootka"
dat$region[dat$location == "N. end Cook Channel"] <- "Nootka"
dat$region[dat$location == "Villaverda Is, Cook Channel"] <- "Nootka"
dat$region[dat$location == "N. Tahsis Inlet below Mozino Pt"] <- "Nootka"
dat$region[dat$location == "Hois Pt"] <- "Nootka"



#------------------------------------------------------------------------------
write.csv(dat, "data/coastwideSeaLice.csv")
