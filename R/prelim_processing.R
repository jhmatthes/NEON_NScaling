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
N_external <- readr::read_csv("data/new/filesToStack10080/stackedFiles/ntr_externalLab.csv", guess_max = 10000)
N_internal <- readr::read_csv("data/new/filesToStack10080/stackedFiles/ntr_internalLab.csv", guess_max = 10000)
kclIntBlank <- readr::read_csv("data/new/filesToStack10080/stackedFiles/ntr_internalLabBlanks.csv", guess_max = 10000)
soilMoist <- readr::read_csv("data/new/filesToStack10080/stackedFiles/sls_soilMoisture.csv", guess_max = 10000)

soil_Ninorg_rates <- def.calc.ntrans(kclInt = N_internal, kclIntBlank = kclIntBlank,
                           kclExt = N_external, soilMoist=soilMoist,
                           dropConditions = c("deprecatedMethod", "other"), dropFlagged = T)

# Separate out initial inorganic N pool sizes
soil_NinorgPools_initial <- dplyr::filter(soil_Ninorg_rates, nTransBoutType == "tInitial") %>%
  dplyr::select(plotID, sampleID, incubationPairID, soilAmmoniumNugPerGram, soilNitrateNitriteNugPerGram) %>%
  dplyr::mutate(soilAmmoniumNugPerGram_initial = soilAmmoniumNugPerGram,
         soilNitrateNitriteNugPerGram_initial = soilNitrateNitriteNugPerGram) %>%
  dplyr::select(-soilAmmoniumNugPerGram, -soilNitrateNitriteNugPerGram) %>%
  dplyr::group_by(plotID, sampleID, incubationPairID) %>%
  dplyr::summarize(soilAmmoniumNugPerGram_initial = mean(soilAmmoniumNugPerGram_initial, na.rm=TRUE),
            soilNitrateNitriteNugPerGram_initial = mean(soilNitrateNitriteNugPerGram_initial, na.rm=TRUE))

# Separate sampleID into horizon and min/max depth, make siteID, month, year columns
# Final joining units: plotID, month_year
soil_Ninorg_rates <- soil_Ninorg_rates %>%
  dplyr::filter(nTransBoutType == "tFinal") %>% #remove NA flux values 
  dplyr::select(plotID, collectDate, incubationPairID, netNminugPerGramPerDay, netNitugPerGramPerDay) %>% #remove tFinal inorg N pools
  dplyr::left_join(soil_NinorgPools_initial, by = c("plotID", "incubationPairID")) %>% #add initial inrog N pools
  tidyr::separate(sampleID, into = c("siteplot","horizon","min_depth","max_depth","colDate"),
           sep = "-") %>%
  tidyr::separate(siteplot, into = c("siteID","justPlot"), sep = "_") %>%
  dplyr::mutate(month = lubridate::month(collectDate), 
                year = lubridate::year(collectDate),
         month_year = stringr::str_pad(paste(month,year,sep="-"),
                                       width=7,pad="0",side="left")) %>%
  dplyr::select(siteID, plotID, collectDate, month_year, horizon,
         soilAmmoniumNugPerGram_initial, soilNitrateNitriteNugPerGram_initial, 
         netNminugPerGramPerDay, netNitugPerGramPerDay) 

# # Plot Nmin rates by site & season
 # ggplot2::ggplot(soil_Ninorg_rates) +
 #   ggplot2::geom_jitter(ggplot2::aes(x = collectDate, y = netNminugPerGramPerDay, color = horizon)) +
 #   ggplot2::facet_wrap(~siteID) +
 #   cowplot::theme_cowplot()+ 
 #   ggplot2::labs(y = "Net Nmineralization (ug day^-1)") +
 #   ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)) 

# Load and organize soil C & N pools
# Final joining units: plotID, month_year
soil_CNplots <- readr::read_csv("data/new/filesToStack10078/stackedFiles/sls_soilChemistry.csv") %>%
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
                soilNPercent, soilCPercent, soilCNRatio) 

soil_CNplot_Ohorizon <- filter(soil_CNplots, horizon == "O") %>%
  mutate(soilNPercent_OHoriz = soilNPercent,
         soilCPercent_OHoriz = soilCPercent,
         soilCNRatio_OHoriz = soilCNRatio) %>%
  select(-soilNPercent, -soilCPercent, -soilCNRatio) %>%
  group_by(siteID, plotID) %>%
  summarize(soilNPercent_OHoriz = mean(soilNPercent_OHoriz, na.rm=TRUE),
            soilCPercent_OHoriz = mean(soilCPercent_OHoriz, na.rm=TRUE),
            soilCNRatio_OHoriz = mean(soilCNRatio_OHoriz, na.rm=TRUE))

soil_CNplot_Mhorizon <- filter(soil_CNplots, horizon == "M") %>%
  mutate(soilNPercent_MHoriz = soilNPercent,
         soilCPercent_MHoriz = soilCPercent,
         soilCNRatio_MHoriz = soilCNRatio) %>%
  select(-soilNPercent, -soilCPercent, -soilCNRatio) %>%
  group_by(siteID, plotID) %>%
  summarize(soilNPercent_MHoriz = mean(soilNPercent_MHoriz, na.rm=TRUE),
            soilCPercent_MHoriz = mean(soilCPercent_MHoriz, na.rm=TRUE),
            soilCNRatio_MHoriz = mean(soilCNRatio_MHoriz, na.rm=TRUE))

soilCN_plotID <- full_join(soil_CNplot_Ohorizon, soil_CNplot_Mhorizon)

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
foliar_CN_trees <- readr::read_csv("data/new/filesToStack10026/stackedFiles/cfc_fieldData.csv") %>%
  dplyr::mutate(year = lubridate::year(collectDate),
                month_year = stringr::str_pad(paste(lubridate::month(collectDate),
                                                    lubridate::year(collectDate),
                                                    sep="-"),width=7,pad="0",side="left")) %>%
  dplyr::select(month_year, year, sampleID, tagID, individualID, taxonID, plantStatus)

foliarCN_plotID <- readr::read_csv("data/new/filesToStack10026/stackedFiles/cfc_carbonNitrogen.csv") %>%
  dplyr::mutate(foliarNPercent = nitrogenPercent, foliarCPercent = carbonPercent,
         foliarCNRatio = CNratio) %>%
  dplyr::select(domainID, siteID, plotID, plotType, sampleID, foliarNPercent, foliarCPercent, 
                foliarCNRatio) %>%
  dplyr::left_join(foliar_CN_trees, by = "sampleID") %>%
  dplyr::select(-sampleID) %>%
  group_by(domainID, siteID, plotID) %>%
  summarize(foliarNPercent = mean(foliarNPercent, na.rm=TRUE),
            foliarCPercent = mean(foliarCPercent, na.rm=TRUE),
            foliarCNRatio = mean(foliarCNRatio, na.rm=TRUE))

# Read in root chemistry data
# Format "BBC" roots
root_chem_BBC <- readr::read_csv("data/new/filesToStack10102/stackedFiles/bbc_rootChemistry.csv") %>%
  filter(grepl("BBC",cnSampleID)) %>%
  select(domainID, siteID, plotID, cnSampleID, 
         nitrogenPercent, carbonPercent, CNratio) %>%
  group_by(domainID, siteID, plotID) %>%
  summarize(rootNPercent = mean(nitrogenPercent, na.rm=TRUE),
            rootCPercent = mean(carbonPercent, na.rm=TRUE),
            rootCNratio = mean(CNratio, na.rm=TRUE),
            rootSample_n = sum(!is.na(nitrogenPercent)))

# Format Megapit roots
root_chem_megapit <- readr::read_csv("data/new/filesToStack10102/stackedFiles/bbc_rootChemistry.csv") %>%
  filter(grepl("MEGAPT",namedLocation)) %>%
  tidyr::separate(cnSampleID, into = c("site","plot","depth","status","size")) %>%
  filter(status == "LIVE") %>%
  select(domainID, siteID, collectDate, size, nitrogenPercent, carbonPercent, CNratio) %>%
  mutate(plotID = paste(siteID,"megapit",sep="_")) %>%
  group_by(domainID, siteID, plotID) %>%
  summarize(rootNPercent = mean(nitrogenPercent, na.rm=TRUE),
            rootCPercent = mean(carbonPercent, na.rm=TRUE),
            rootCNratio = mean(CNratio, na.rm=TRUE),
            rootSample_n = sum(!is.na(nitrogenPercent)))

# Join whole root table
rootCN_plotID <- full_join(root_chem_BBC, root_chem_megapit)

# Plot characteristics
plot_info <- readr::read_csv("data/new/filesToStack10098/stackedFiles/vst_perplotperyear.csv") %>%
  select(domainID, siteID, plotID, nlcdClass, decimalLatitude, decimalLongitude, 
         easting, northing, utmZone, elevation)
plot_info <- unique(plot_info)

# Combine together all "slow" variables by averaging across year within plotID
# 1. Aggregate each dataset to plotID level

litterN_plotID <- litter_CN %>%
  dplyr::group_by(siteID, plotID, plotType) %>%
  dplyr::summarize(litterNPercent = mean(litterNPercent, na.rm=TRUE))

# 2. Join together all datasets
dataCN_plotID <- dplyr::full_join(rootCN_plotID, litterN_plotID, 
                          by = c("siteID", "plotID")) %>%
  dplyr::full_join(foliarCN_plotID, by = c("domainID","siteID", "plotID")) %>%
  dplyr::full_join(soilCN_plotID, by = c("siteID", "plotID")) %>%
  dplyr::left_join(plot_info, by = c("domainID", "siteID", "plotID"))

readr::write_csv(dataCN_plotID,"CN_plotID.csv")

dataN_plotID_noTaxonID <- dplyr::full_join(rootN_plotID, litterN_plotID, 
                                 by = c("siteID", "plotID", "plotType")) %>%
  dplyr::full_join(foliarCN_plotID_noTaxonID, by = c("siteID", "plotID", "plotType")) %>%
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

