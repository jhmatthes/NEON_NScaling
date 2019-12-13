# Preliminary processing
#
# This code does data wrangling for the NEON N Across Scales manuscript.
# It should be run after the download_data.R code, where downloaded and stacked
# data products should be stored within a local data/ folder in the path of 
# this script file.
#
# This code requires the packages: readr, dplyr, magrittr, stringr,
# tidyr, and lubridate, which can all be downloaded as the tidyverse package.

library(magrittr)
library(dplyr)

# Load soil inorganic N transformations flux function
source("R/def.calc.ntrans.R")

# Calculate soil N mineralization rates
N_external <- readr::read_csv("data/filesToStack10080/stackedFiles/ntr_externalLab.csv", guess_max = 10000)
N_internal <- readr::read_csv("data/filesToStack10080/stackedFiles/ntr_internalLab.csv", guess_max = 10000)
kclIntBlank <- readr::read_csv("data/filesToStack10080/stackedFiles/ntr_internalLabBlanks.csv", guess_max = 10000)
soilMoist <- readr::read_csv("data/filesToStack10080/stackedFiles/sls_soilMoisture.csv", guess_max = 10000)

soil_Ninorg_rates <- def.calc.ntrans(kclInt = N_internal, kclIntBlank = kclIntBlank,
                           kclExt = N_external, soilMoist=soilMoist,
                           dropConditions = c("deprecatedMethod", "other"), dropFlagged = T)

# Separate out initial inorganic N pool sizes
Ninorganic_initial <- soil_Ninorg_rates %>%
  dplyr::filter(nTransBoutType == "tInitial") %>%
  dplyr::select(incubationPairID, soilAmmoniumNugPerGram, soilNitrateNitriteNugPerGram)

# Separate sampleID into horizon and min/max depth, make siteID, month, year columns
# Final joining units: plotID, month_year
soil_Ninorg_rates <- soil_Ninorg_rates %>%
  dplyr::filter(!is.na(netNminugPerGramPerDay)) %>% #remove NA flux values 
  dplyr::select(-soilAmmoniumNugPerGram, -soilNitrateNitriteNugPerGram) %>% #remove tFinal inorg N pools
  dplyr::left_join(Ninorganic_initial, by = "incubationPairID") %>% #add initial inrog N pools
  tidyr::separate(sampleID, into = c("siteplot","horizon","min_depth","max_depth","colDate"),
           sep = "-") %>%
  tidyr::separate(siteplot, into = c("siteID","justPlot"), sep = "_") %>%
  dplyr::mutate(month = lubridate::month(collectDate), 
                year = lubridate::year(collectDate),
         month_year = stringr::str_pad(paste(month,year,sep="-"),
                                       width=7,pad="0",side="left")) %>%
  dplyr::select(siteID, plotID, collectDate, month_year, horizon,
         soilAmmoniumNugPerGram, soilNitrateNitriteNugPerGram, 
         netNminugPerGramPerDay, netNitugPerGramPerDay) 

# Load and organize soil C & N pools
# Final joining units: plotID, month_year
soil_CNplots <- readr::read_csv("data/filesToStack10078/stackedFiles/sls_soilChemistry.csv") %>%
  tidyr::separate(sampleID, into = c("siteplot","horizon","min_depth","max_depth","colDate"),
           sep = "-") %>%
  tidyr::separate(siteplot, into = c("siteID","justPlot"), sep = "_") %>%
  dplyr::mutate(year = lubridate::year(collectDate), 
                month = lubridate::month(collectDate),
         month_year = stringr::str_pad(paste(month,year,sep="-"),
                                       width=7,pad="0",side="left"),
         soilNPercent = nitrogenPercent, soilCPercent = organicCPercent,
         soilCNRatio = CNratio) %>%
  dplyr::filter(is.na(dataQF), analyticalRepNumber == 1) %>%
  dplyr::select(siteID, plotID, plotType, month_year, horizon, 
                collectDate, soilNPercent, soilCPercent, soilCNRatio)

# Load and oraganize the leaf litter data (needles & leaves only)
# Final joining units: plotID, month_year
litter_CN <- readr::read_csv("data/filesToStack10031/stackedFiles/ltr_litterCarbonNitrogen.csv") %>%
  dplyr::mutate(year = lubridate::year(setDate), 
                month = lubridate::month(setDate), 
         month_year = stringr::str_pad(paste(month,year,sep="-"),
                                       width=7,pad="0",side="left"),
         litterNPercent = nitrogenPercent, 
         litterCPercent = carbonPercent,
         litterCNRatio = CNratio) %>%
  dplyr::select(siteID, plotID, plotType, month, year, month_year, collectDate, 
                litterNPercent, litterCPercent, litterCNRatio)

# Foliar CN samples
# Final joining units: individualID, month_year
foliar_CN_trees <- readr::read_csv("data/filesToStack10026/stackedFiles/cfc_fieldData.csv") %>%
  dplyr::mutate(year = lubridate::year(collectDate),
                month_year = stringr::str_pad(paste(lubridate::month(collectDate),
                                                    lubridate::year(collectDate),
                                                    sep="-"),width=7,pad="0",side="left")) %>%
  dplyr::select(month_year, year, sampleID, tagID, individualID, taxonID, plantStatus)

foliar_CN <- readr::read_csv("data/filesToStack10026/stackedFiles/cfc_carbonNitrogen.csv") %>%
  dplyr::mutate(foliarNPercent = nitrogenPercent, foliarCPercent = carbonPercent,
         foliarCNRatio = CNratio) %>%
  dplyr::select(siteID, plotID, plotType, sampleID, foliarNPercent, foliarCPercent, 
                foliarCNRatio) %>%
  dplyr::left_join(foliar_CN_trees, by = "sampleID") %>%
  dplyr::select(-sampleID)

# Read in root chemistry data
root_chem <- readr::read_csv("non_raw_data/rootchem_NEON.csv") %>%
  mutate(year = lubridate::year(collectDate),
         coarseRoot_percentN = `root_2-10_nitrogenPercent`) 

root_chem$fineRoot_percentN <- rowMeans(root_chem[,c(7,10,13)], na.rm=TRUE)

root_chem <- dplyr::select(root_chem, siteID, plotID, plotType, year, 
                           coarseRoot_percentN, fineRoot_percentN)

# Combine together all "slow" variables by averaging across year within plotID
# 1. Aggregate each dataset to plotID level
rootN_plotID <- root_chem %>%
  dplyr::group_by(siteID, plotID, plotType) %>%
  dplyr::summarize(coarseRootNPercent = mean(coarseRoot_percentN, na.rm=TRUE),
                   fineRootNPercent = mean(fineRoot_percentN, na.rm=TRUE))

foliarN_plotID <- foliar_CN %>%
  dplyr::group_by(siteID, plotID, plotType, taxonID) %>%
  dplyr::summarize(foliarNPercent = mean(foliarNPercent, na.rm=TRUE))

foliarN_plotID_noTaxonID <- foliar_CN %>%
  dplyr::group_by(siteID, plotID, plotType) %>%
  dplyr::summarize(foliarNPercent = mean(foliarNPercent, na.rm=TRUE))

litterN_plotID <- litter_CN %>%
  dplyr::group_by(siteID, plotID, plotType) %>%
  dplyr::summarize(litterNPercent = mean(litterNPercent, na.rm=TRUE))

soilN_plotID <- soil_CNplots %>%
  dplyr::group_by(siteID, plotID, plotType, horizon) %>%
  dplyr::summarize(soilNPercent = mean(soilNPercent, na.rm=TRUE)) %>%
  tidyr::spread(key = horizon, soilNPercent) %>%
  dplyr::mutate(soilNPercentOHoriz = O,
                soilNPercentMHoriz = M) %>%
  dplyr::select(siteID, plotID, plotType, soilNPercentOHoriz, 
                soilNPercentMHoriz)

# 2. Join together all datasets
dataN_plotID <- dplyr::full_join(rootN_plotID, litterN_plotID, 
                          by = c("siteID", "plotID", "plotType")) %>%
  dplyr::full_join(foliarN_plotID, by = c("siteID", "plotID", "plotType")) %>%
  dplyr::full_join(soilN_plotID, by = c("siteID", "plotID", "plotType"))

readr::write_csv(dataN_plotID,"N_plotID.csv")

dataN_plotID_noTaxonID <- dplyr::full_join(rootN_plotID, litterN_plotID, 
                                 by = c("siteID", "plotID", "plotType")) %>%
  dplyr::full_join(foliarN_plotID_noTaxonID, by = c("siteID", "plotID", "plotType")) %>%
  dplyr::full_join(soilN_plotID, by = c("siteID", "plotID", "plotType"))
readr::write_csv(dataN_plotID_noTaxonID,"N_plotID_noTaxonID.csv")



# SOME EXTRA STUFF
# # Compare sample number per plot within sites (smallest unit of analysis)
# foliar_CN_n <- foliar_CN %>%
#   group_by(siteID, plotID, plotType) %>%
#   summarize(count = n())
# 
# # Example where date (maybe) doesn't matter: joining litterCN and soilCN
# litter_plotID <- litter_CN %>%
#   dplyr::group_by(siteID, plotID) %>%
#   dplyr::summarize(litterN_mean = mean(litterNPercent, na.rm=TRUE),
#                    litterN_sd = sd(litterNPercent, na.rm=TRUE),
#                    litterN_n = n())
# 
# soilCN_plotID <- soil_CNplots %>%
#   dplyr::group_by(siteID, plotID, horizon) %>%
#   dplyr::summarize(soilN_mean = mean(soilNPercent, na.rm=TRUE),
#                    soilN_sd = sd(soilNPercent, na.rm=TRUE),
#                    soilN_n = n())
#  
# litter_soil_plotID <- dplyr::full_join(litter_plotID, soilCN_plotID, 
#                                 by = c("siteID","plotID")) %>%
#   dplyr::arrange(plotID) %>%
#   filter(!is.na(litterN_mean), !is.na(soilN_mean))
# 
# ggplot(dplyr::filter(litter_soil_plotID, horizon == "O")) +
#   geom_point(aes(x = litterN_mean, y = soilN_mean, color = siteID)) +
#   geom_smooth(aes(x = litterN_mean, y = soilN_mean, color = siteID), method = "lm")

