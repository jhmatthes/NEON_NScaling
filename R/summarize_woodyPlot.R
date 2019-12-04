# This code uses the most recent NEON woody vegetation inventory plot data per individual
# and calculates (per plot): 
#   1. Canopy composition: total crown area per taxonID for canopyPosition == "Shaded" | "Partially shaded"
#   2a. Plot basal area: total basal area per plot
#   2b. Plot composition: total fraction basal area per taxonID

library(magrittr)

# Plot locations table
plot_loc <- readr::read_csv("data/filesToStack10098/stackedFiles/vst_perplotperyear.csv") %>%
  dplyr::select(namedLocation, domainID, siteID, plotID, plotType, nlcdClass, 
                decimalLatitude, decimalLongitude, elevation, totalSampledAreaTrees) %>%
  dplyr::arrange(plotID)

plot_lat_lon <- plot_loc %>%
  dplyr::group_by(plotID, plotType) %>%
  dplyr::summarize(lat = round(mean(decimalLatitude),6),
            lon = round(mean(decimalLongitude),6))

# Mapped tree location and taxonID table
loc_spp <- readr::read_csv("data/filesToStack10098/stackedFiles/vst_mappingandtagging.csv",
                    na = c("", "NA"), guess_max = 100000) %>%
  dplyr::select(namedLocation, domainID, siteID, plotID, subplotID, eventID, 
                individualID, taxonID, scientificName)

taxon_sciName_table <- loc_spp %>%
  dplyr::select(siteID, taxonID, scientificName) %>%
  dplyr::distinct()

# Individual measurements table
woody_indiv <- readr::read_csv("data/filesToStack10098/stackedFiles/vst_apparentindividual.csv",
                                na = c("", "NA"), guess_max = 100000) %>%
  dplyr::select(-c("uid", "subplotID","tempShrubStemID","tagStatus","shape","basalStemDiameter",
                   "basalStemDiameterMsrmntHeight","remarks","recordedBy","measuredBy","dataQF")) %>%
  dplyr::left_join(loc_spp, by = c("namedLocation","domainID", "siteID", "plotID",
                                   "eventID","individualID")) %>%
  dplyr::left_join(plot_loc, by = c("namedLocation","domainID", "siteID", "plotID")) %>%
  dplyr::mutate(year = lubridate::year(date),
         indiv_date = paste(individualID, year))

# Find most recent measurement per individual
woody_indiv_lastMeas <- woody_indiv %>%
  dplyr::group_by(plotID, individualID) %>%
  dplyr::summarize(last_date = max(year)) %>%
  dplyr::mutate(indiv_date = paste(individualID, last_date))

woody_indiv_lastMeas_plot <- woody_indiv_lastMeas %>%
  dplyr::group_by(plotID, last_date) %>%
  dplyr::summarize(count = dplyr::n())

# Filter woody individual data to most recent measurement
woody_indiv_recent <- woody_indiv %>%
  dplyr::filter(indiv_date %in% woody_indiv_lastMeas$indiv_date) 

# Calculate crown area per species per plot
crownArea_byTaxa <- woody_indiv_recent %>%
  dplyr::filter(plantStatus == "Live",
         canopyPosition == "Full sun" | canopyPosition == "Partially shaded") %>%
  dplyr::group_by(plotID, eventID, taxonID, totalSampledAreaTrees) %>%
  dplyr::summarize(DBH_mean_cm = mean(stemDiameter, na.rm=TRUE),
            BA_cm2perm2 = sum(pi*(stemDiameter/2)^2, na.rm=TRUE)/mean(totalSampledAreaTrees),
            crownArea_m2 = sum(pi*(ninetyCrownDiameter/2)*(maxCrownDiameter/2), na.rm=TRUE),
            crownArea_m2perm2 = crownArea_m2/mean(totalSampledAreaTrees)) 

# Plot total basal area in canopy, total crown area, sum taxonID == NA basal area
crownArea_plotStats <- crownArea_byTaxa %>%
  dplyr::group_by(plotID) %>%
  dplyr::summarize(BA_cm2perm2 = sum(BA_cm2perm2),
                   crownArea_m2perm2 = sum(crownArea_m2perm2),
                   taxonID_NA_BA = sum(BA_cm2perm2[is.na(taxonID)]),
                   crownArea_NA = sum(BA_cm2perm2[is.na(crownArea_m2perm2)]))

# Live total basal area per taxon per plot
basalArea_byTaxa <- woody_indiv_recent %>%
  dplyr::filter(plantStatus == "Live") %>%
  dplyr::group_by(plotID, taxonID, totalSampledAreaTrees) %>%
  dplyr::summarize(DBH_mean_cm = mean(stemDiameter, na.rm=TRUE),
                   DBH_sd_cm = sd(stemDiameter, na.rm=TRUE),
                   BA_cm2perm2 = sum(pi*(stemDiameter/2)^2, na.rm=TRUE)/mean(totalSampledAreaTrees)) 

basalArea_byPlot <- woody_indiv_recent %>%
  dplyr::filter(plantStatus == "Live") %>%
  dplyr::group_by(plotID, totalSampledAreaTrees) %>%
  dplyr::summarize(DBH_mean_cm = mean(stemDiameter, na.rm=TRUE),
                   DBH_sd_cm = sd(stemDiameter, na.rm=TRUE),
                   BA_cm2perm2 = sum(pi*(stemDiameter/2)^2, na.rm=TRUE)/mean(totalSampledAreaTrees)) 





