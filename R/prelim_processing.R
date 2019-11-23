# Preliminary processing
#
# This code does data wrangling for the NEON N Across Scales manuscript.
# It should be run after the download_data.R code, where downloaded and stacked
# data products should be stored within a local data/ folder in the path of 
# this script file.

# Load soil inorganic N transformations flux function
source("def.calc.ntrans.R")

# Calculate soil N mineralization rates
N_external <- read_csv("data/filesToStack10080/stackedFiles/ntr_externalLab.csv", guess_max = 10000)
N_internal <- read_csv("data/filesToStack10080/stackedFiles/ntr_internalLab.csv", guess_max = 10000)
kclIntBlank <- read_csv("data/filesToStack10080/stackedFiles/ntr_internalLabBlanks.csv", guess_max = 10000)
soilMoist <- read_csv("data/filesToStack10080/stackedFiles/sls_soilMoisture.csv", guess_max = 10000)

soil_Ninorg_rates <- def.calc.ntrans(kclInt = N_internal, kclIntBlank = kclIntBlank,
                           kclExt = N_external, soilMoist=soilMoist,
                           dropConditions = c("deprecatedMethod", "other"), dropFlagged = T)

# Separate out initial inorganic N pool sizes
Ninorganic_initial <- soil_Ninorg_rates %>%
  filter(nTransBoutType == "tInitial") %>%
  select(incubationPairID, soilAmmoniumNugPerGram, soilNitrateNitriteNugPerGram)

# Separate sampleID into horizon and min/max depth, make siteID, month, year columns
soil_Ninorg_rates <- soil_Ninorg_rates %>%
  filter(!is.na(netNminugPerGramPerDay)) %>% #remove NA flux values 
  select(-soilAmmoniumNugPerGram, -soilNitrateNitriteNugPerGram) %>% #remove tFinal inorg N pools
  left_join(Ninorganic_initial, by = "incubationPairID") %>% #add initial inrog N pools
  separate(sampleID, into = c("siteplot","horizon","min_depth","max_depth","colDate"),
           sep = "-") %>%
  separate(siteplot, into = c("siteID","justPlot"), sep = "_") %>%
  mutate(month = month(collectDate), year = year(collectDate),
         month_year = str_pad(paste(month,year,sep="-"),width=7,pad="0",side="left")) %>%
  select(siteID, plotID, collectDate, month_year, horizon,
         soilAmmoniumNugPerGram, soilNitrateNitriteNugPerGram, 
         netNminugPerGramPerDay, netNitugPerGramPerDay) 

# Load and organize soil C & N pools
soil_CNplots <- read_csv("data/filesToStack10078/stackedFiles/sls_soilChemistry.csv") %>%
  separate(sampleID, into = c("siteplot","horizon","min_depth","max_depth","colDate"),
           sep = "-") %>%
  separate(siteplot, into = c("siteID","justPlot"), sep = "_") %>%
  mutate(year = year(collectDate), month = month(collectDate),
         month_year = str_pad(paste(month,year,sep="-"),width=7,pad="0",side="left"),
         soilNPercent = nitrogenPercent, soilCPercent = organicCPercent,
         soilCNRatio = CNratio) %>%
  filter(is.na(dataQF), analyticalRepNumber == 1) %>%
  dplyr::select(siteID, plotID, plotType, month_year, horizon, collectDate, soilNPercent,
         soilCPercent, soilCNRatio)

soilCN_HARV <- filter(soil_CNplots, siteID == "HARV") %>%
  left_join(plot_lat_lon)

write_csv(soilCN_HARV, "soilCN_HARV.csv")

# Load and oraganize the leaf litter data (needles & leaves only)
litter_CN <- readr::read_csv("data/filesToStack10031/stackedFiles/ltr_litterCarbonNitrogen.csv") %>%
  dplyr::mutate(year = lubridate::year(setDate), month = lubridate::month(setDate), 
         month_year = stringr::str_pad(paste(month,year,sep="-"),width=7,pad="0",side="left"),
         litterNPercent = nitrogenPercent, litterCPercent = carbonPercent,
         litterCNRatio = CNratio) %>%
  dplyr::select(siteID, plotID, plotType, month_year, collectDate, litterNPercent,
         litterCPercent, litterCNRatio)

# Foliar CN samples
foliar_CN_trees <- readr::read_csv("data/filesToStack10026/stackedFiles/cfc_fieldData.csv") %>%
  dplyr::mutate(month_year = stringr::str_pad(paste(month(collectDate),year(collectDate),sep="-"),width=7,pad="0",side="left")) %>%
  dplyr::select(month_year, sampleID, tagID, individualID, taxonID, plantStatus)

foliar_CN <- readr::read_csv("data/filesToStack10026/stackedFiles/cfc_carbonNitrogen.csv") %>%
  dplyr::mutate(foliarNPercent = nitrogenPercent, foliarCPercent = carbonPercent,
         foliarCNRatio = CNratio) %>%
  dplyr::select(siteID, plotID, plotType, sampleID, foliarNPercent, foliarCPercent, foliarCNRatio) %>%
  dplyr::left_join(foliar_CN_trees, by = "sampleID")

# Compare sample number per plot within sites (smallest unit of analysis)
foliar_CN_n <- foliar_CN %>%
  group_by(siteID, plotID, plotType) %>%
  summarize(count = n())


# # Work with soil bulk density to get total N pool sizes
# soil_bulkdens <- read_csv("data/filesToStack10047/stackedFiles/spc_bulkdensity.csv")
# soil_horizons <- read_csv("data/filesToStack10047/stackedFiles/spc_perhorizon.csv")
# soil_plots <- read_csv("data/filesToStack10047/stackedFiles/spc_perplot.csv")
# 
# tmp <- left_join(soil_bulkdens, soil_horizons,
#                  by = c("namedLocation", "domainID", "siteID", "plotID", "nrcsDescriptionID", "horizonID", 
#                         "horizonName"))
