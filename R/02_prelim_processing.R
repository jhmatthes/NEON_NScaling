# Preliminary processing
#
# This code does data wrangling for the NEON N Across Scales manuscript.
# It should be run after the download_data.R code, where downloaded and stacked
# data products should be stored within a local data/ folder in the path of 
# this script file.

# This code requires the packages: readr, dplyr, magrittr, stringr,
# tidyr, and lubridate, which can all be downloaded as the tidyverse package.

library(magrittr)
library(dplyr)

# Load and organize soil percentage C & N pools
soil_CNplots <- sls_soilChemistry %>%
  tidyr::separate(sampleID, into = c("siteplot","horizon","plot_a","plot_b","colDate"),
           sep = "-") %>%
  tidyr::separate(siteplot, into = c("siteID","justPlot"), sep = "_") %>%
  dplyr::mutate(year = lubridate::year(collectDate), 
                month = lubridate::month(collectDate),
         month_year = stringr::str_pad(paste(month,year,sep="-"),
                                       width=7,pad="0",side="left"),
         soilNPercent = nitrogenPercent, soilCPercent = organicCPercent,
         soilCNRatio = CNratio) %>%
  dplyr::filter(cnPercentQF == "OK", analyticalRepNumber == 1) %>%
  dplyr::select(domainID, siteID, plotID, plotType, month_year, horizon, 
                soilNPercent, soilCPercent, soilCNRatio) 

soil_CNplot_Ohorizon <- filter(soil_CNplots, horizon == "O") %>%
  mutate(soilNPercent_OHoriz = soilNPercent,
         soilCPercent_OHoriz = soilCPercent,
         soilCNRatio_OHoriz = soilCNRatio) %>%
  select(-soilNPercent, -soilCPercent, -soilCNRatio) %>%
  group_by(domainID, siteID, plotID, plotType) %>%
  summarize(soilNPercent_OHoriz_mean = mean(soilNPercent_OHoriz, na.rm=TRUE),
            soilNPercent_OHoriz_n = sum(!is.na(soilNPercent_OHoriz)),
            soilCPercent_OHoriz_mean = mean(soilCPercent_OHoriz, na.rm=TRUE),
            soilCPercent_OHoriz_n = sum(!is.na(soilCPercent_OHoriz)),
            soilCNRatio_OHoriz_mean = mean(soilCNRatio_OHoriz, na.rm=TRUE),
            soilCNRatio_OHoriz_n = sum(!is.na(soilCNRatio_OHoriz)))

soil_CNplot_Mhorizon <- filter(soil_CNplots, horizon == "M") %>%
  mutate(soilNPercent_MHoriz = soilNPercent,
         soilCPercent_MHoriz = soilCPercent,
         soilCNRatio_MHoriz = soilCNRatio) %>%
  select(-soilNPercent, -soilCPercent, -soilCNRatio) %>%
  group_by(domainID, siteID, plotID, plotType) %>%
  summarize(soilNPercent_MHoriz_mean = mean(soilNPercent_MHoriz, na.rm=TRUE),
            soilNPercent_MHoriz_n = sum(!is.na(soilNPercent_MHoriz)),
            soilCPercent_MHoriz_mean = mean(soilCPercent_MHoriz, na.rm=TRUE),
            soilCPercent_MHoriz_n = sum(!is.na(soilCPercent_MHoriz)),
            soilCNRatio_MHoriz_mean = mean(soilCNRatio_MHoriz, na.rm=TRUE),
            soilCNRatio_MHoriz_n = sum(!is.na(soilCNRatio_MHoriz)))

soilCN_plotID <- full_join(soil_CNplot_Ohorizon, soil_CNplot_Mhorizon)

# Summarize plots and count replicates
soilCN_plotIDReps <- soilCN_plotID[,grepl("_n",colnames(soilCN_plotID))]
soilCN_plotIDReps <- data.frame(siteID = soilCN_plotID$siteID, 
                                plotID = soilCN_plotID$plotID,
                                soilCN_plotIDReps)

# Summarize sites, plot, and replicate counts
soilCN_siteReps <- soilCN_plotID %>%
  group_by(domainID, siteID) %>%
  summarize(NPercent_OHoriz_totalreps = sum(soilNPercent_OHoriz_n, na.rm=TRUE),
            NPercent_OHoriz_plotreps = sum(!is.na(soilNPercent_OHoriz_n)),
            NPercent_MinHoriz_totalreps = sum(soilNPercent_MHoriz_n, na.rm=TRUE),
            NPercent_MinHoriz_plotreps = sum(!is.na(soilNPercent_MHoriz_n)),
            CPercent_OHoriz_totalreps = sum(soilCPercent_OHoriz_n, na.rm=TRUE),
            CPercent_OHoriz_plotreps = sum(!is.na(soilCPercent_OHoriz_n)),
            CPercent_MinHoriz_totalreps = sum(soilCPercent_MHoriz_n, na.rm=TRUE),
            CPercent_MinHoriz_plotreps = sum(!is.na(soilCPercent_MHoriz_n)),
            CNRatio_OHoriz_totalreps = sum(soilCNRatio_OHoriz_n, na.rm=TRUE),
            CNRatio_OHoriz_plotreps = sum(!is.na(soilCNRatio_OHoriz_n)),
            CNRatio_MinHoriz_totalreps = sum(soilCNRatio_MHoriz_n, na.rm=TRUE),
            CNRatio_MinHoriz_plotreps = sum(!is.na(soilCNRatio_MHoriz_n)))

# Load and oraganize the leaf litter data (needles & leaves only)
# Final joining units: plotID, month_year 
litterCN_plotID <- ltr_litterCarbonNitrogen %>%
  dplyr::mutate(year = lubridate::year(setDate), 
                month = lubridate::month(setDate), 
         month_year = stringr::str_pad(paste(month,year,sep="-"),
                                       width=7,pad="0",side="left"),
         litterNPercent = nitrogenPercent, 
         litterCPercent = carbonPercent,
         litterCNRatio = CNratio) %>%
  dplyr::filter(analyticalRepNumber == 1, cnPercentQF == "OK") %>%
  dplyr::select(domainID, siteID, plotID, plotType, month, year, month_year, collectDate, 
                litterNPercent, litterCPercent, litterCNRatio) %>%
  dplyr::group_by(domainID, siteID, plotID, plotType) %>%
  dplyr::summarize(litterNPercent_mean = mean(litterNPercent, na.rm=TRUE),
                   litterNPercent_n = sum(!is.na(litterNPercent)),
                   litterCPercent_mean = mean(litterCPercent, na.rm=TRUE),
                   litterCPercent_n = sum(!is.na(litterCPercent)),
                   litterCNRatio_mean = mean(litterCNRatio, na.rm=TRUE),
                   litterCNRatio_n = sum(!is.na(litterCNRatio)))

# Summarize sites, plot, and replicate counts
litterCN_siteReps <- litterCN_plotID %>%
  group_by(domainID, siteID) %>%
  summarize(litterN_totalreps = sum(litterNPercent_n, na.rm=TRUE),
            litterN_plotreps = sum(!is.na(litterNPercent_n)),
            litterC_totalreps = sum(litterCPercent_n, na.rm=TRUE),
            litterC_plotreps = sum(!is.na(litterCPercent_n)),
            litterCNR_totalreps = sum(litterCNRatio_n, na.rm=TRUE),
            litterCNR_plotreps = sum(!is.na(litterCNRatio_n)))

# Foliar CN samples
foliarCN_plotID <- cfc_carbonNitrogen %>%
  dplyr::mutate(foliarNPercent = nitrogenPercent, foliarCPercent = carbonPercent,
         foliarCNRatio = CNratio) %>%
  dplyr::filter(analyticalRepNumber == 1, cnPercentQF == "OK") %>%
  dplyr::select(domainID, siteID, plotID, plotType, sampleID, foliarNPercent, foliarCPercent, 
                foliarCNRatio) %>%
  group_by(domainID, siteID, plotID, plotType) %>%
  summarize(foliarNPercent_mean = mean(foliarNPercent, na.rm=TRUE),
            foliarNPercent_n = sum(!is.na(foliarNPercent)),
            foliarCPercent_mean = mean(foliarCPercent, na.rm=TRUE),
            foliarCPercent_n = sum(!is.na(foliarCPercent)),
            foliarCNRatio_mean = mean(foliarCNRatio, na.rm=TRUE),
            foliarCNRatio_n = sum(!is.na(foliarCNRatio)))

# Summarize sites, plot, and replicate counts
foliarCN_siteReps <- foliarCN_plotID %>%
  group_by(domainID, siteID) %>%
  summarize(foliarN_totalreps = sum(foliarNPercent_n, na.rm=TRUE),
            foliarN_plotreps = sum(!is.na(foliarNPercent_n)),
            foliarC_totalreps = sum(foliarCPercent_n, na.rm=TRUE),
            foliarC_plotreps = sum(!is.na(foliarCPercent_n)),
            foliarCNR_totalreps = sum(foliarCNRatio_n, na.rm=TRUE),
            foliarCNR_plotreps = sum(!is.na(foliarCNRatio_n)))

# Read in root chemistry data
# Format "BBC" roots - average across all 3 size classes (< 1mm, 1-2mm, 2-10mm)
# we may want to look at just "fine" roots < 2mm down the road...
# we may also want to take weighted average of root chemistry based on root mass per size class
rootCN_plotID <- bbc_rootChemistry %>%
  filter(grepl("BBC",cnSampleID)) %>%
  dplyr::filter(analyticalRepNumber == 1, cnPercentQF == "OK") %>%
  select(domainID, siteID, plotID, plotType, cnSampleID, 
         nitrogenPercent, carbonPercent, CNratio) %>%
  group_by(domainID, siteID, plotID, plotType) %>%
  summarize(rootNPercent = mean(nitrogenPercent, na.rm=TRUE),
            rootNPercent_n = sum(!is.na(nitrogenPercent)),
            rootCPercent = mean(carbonPercent, na.rm=TRUE),
            rootCPercent_n = sum(!is.na(carbonPercent)),
            rootCNratio = mean(CNratio, na.rm=TRUE),
            rootCNratio_n = sum(!is.na(CNratio)))

# Summarize sites, plot, and replicate counts
rootCN_siteReps <- rootCN_plotID %>%
  group_by(domainID, siteID) %>%
  summarize(rootN_totalreps = sum(rootNPercent_n, na.rm=TRUE),
            rootN_plotreps = sum(!is.na(rootNPercent_n)),
            rootC_totalreps = sum(rootCPercent_n, na.rm=TRUE),
            rootC_plotreps = sum(!is.na(rootCPercent_n)),
            rootCNR_totalreps = sum(rootCNratio_n, na.rm=TRUE),
            rootCNR_plotreps = sum(!is.na(rootCNratio_n)))

# Read in soil texture data
soiltexture_plotID <- spc_particlesize %>%
  select(domainID, siteID, plotID, sandTotal, siltTotal, clayTotal, carbonateClay,
         clayFineContent, siltFineContent) %>%
  group_by(domainID, siteID, plotID) %>%
  summarize(pctSand = mean(sandTotal, na.rm=TRUE),
            pctSilt = mean(siltTotal, na.rm=TRUE),
            pctClay = mean(clayTotal, na.rm=TRUE))


# Read in inogranic N data

#https://data.neonscience.org/data-products/DP1.10086.001

inorganic.df<-as.data.frame(inorganicN[1])
#inorganic.df$

# get ammonium

ammonium_plotID <- inorganic.df  %>%
  dplyr::filter(ntr_externalLab.ammoniumNRepNum == 1, ntr_externalLab.ammoniumNQF == "OK") %>%
  dplyr::select(ntr_externalLab.domainID, ntr_externalLab.siteID, ntr_externalLab.plotID, 
                ntr_externalLab.kclSampleID, ntr_externalLab.kclAmmoniumNConc) %>%
  group_by(ntr_externalLab.domainID, ntr_externalLab.siteID, ntr_externalLab.plotID) %>%
  summarize(soil_ammonium_mean = mean(ntr_externalLab.kclAmmoniumNConc, na.rm=TRUE),
            soil_ammonium_n = sum(!is.na(ntr_externalLab.kclAmmoniumNConc)))

ammonium_plotID <-data.frame(ammonium_plotID)
#head(ammonium_plotID)    

# now get nitrate

nitrate_plotID <- inorganic.df  %>%
  dplyr::filter(ntr_externalLab.nitrateNitriteNRepNum == 1, ntr_externalLab.nitrateNitriteNQF == "OK") %>%
  dplyr::select(ntr_externalLab.domainID, ntr_externalLab.siteID, ntr_externalLab.plotID, 
                ntr_externalLab.kclSampleID, ntr_externalLab.kclNitrateNitriteNConc) %>%
  group_by(ntr_externalLab.domainID, ntr_externalLab.siteID, ntr_externalLab.plotID) %>%
  summarize(soil_nitrate_mean = mean(ntr_externalLab.kclNitrateNitriteNConc,na.rm=TRUE),
            soil_nitrate_n = sum(!is.na(ntr_externalLab.kclNitrateNitriteNConc)))

nitrate_plotID <- data.frame(nitrate_plotID)

inorganicNmerged <- merge(nitrate_plotID,ammonium_plotID,by=c('ntr_externalLab.domainID',
                                                              'ntr_externalLab.siteID',
                                                              'ntr_externalLab.plotID'))

#sum to get total inorganic N
inorganicNmerged$inorganicN<-inorganicNmerged$soil_nitrate_mean + inorganicNmerged$soil_ammonium_mean
#head(inorganicNmerged)

colnames(inorganicNmerged) <- c('domainID','siteID','plotID','soil_nitrate_mean','soil_nitrate_n',
                                'soil_ammonium_mean','soil_ammonium_n','inorganicN')


# tidy up net N mineralization data

# Read in soil texture data
min.df <- min.df %>%
  select(all_data.domainID, all_data.siteID, all_data.plotID, all_data.netNminugPerGramPerDay) %>%
  group_by(all_data.domainID, all_data.siteID, all_data.plotID) %>%
  summarize(netNminugPerGramPerDay = mean(all_data.netNminugPerGramPerDay, na.rm=TRUE)) %>%
  filter(!netNminugPerGramPerDay=='NaN')

colnames(min.df) <-c('domainID','siteID','plotID','netNminugPerGramPerDay')
min.df$netNminugPerGramPerDay<-round(min.df$netNminugPerGramPerDay,2)

# Combine together all variables to the plotID level
dataCN_plotID <- dplyr::full_join(rootCN_plotID, litterCN_plotID, 
                          by = c("domainID", "siteID", "plotID", "plotType")) %>%
  dplyr::full_join(foliarCN_plotID, by = c("domainID","siteID", "plotID", "plotType")) %>%
  dplyr::full_join(soilCN_plotID, by = c("domainID", "siteID", "plotID", "plotType")) %>%
  dplyr::full_join(soiltexture_plotID, by = c("domainID", "siteID", "plotID")) %>%
  dplyr::full_join(inorganicNmerged, by = c("domainID", "siteID", "plotID")) %>%
  dplyr::full_join(min.df, by = c("domainID", "siteID", "plotID")) 

readr::write_csv(dataCN_plotID,"CN_plotID.csv")

# compile table of replication by site, plot, and total number
dataCN_reps <- dplyr::full_join(rootCN_siteReps, litterCN_siteReps, 
                                by = c("domainID","siteID")) %>%
  dplyr::full_join(foliarCN_siteReps,by = c("domainID","siteID")) %>%
  dplyr::full_join(soilCN_siteReps, by = c("domainID","siteID"))

readr::write_csv(dataCN_reps,"CN_siteReps.csv")

# Summarize sites, plot, and replicate counts
rootCN_siteReps <- rootCN_plotID %>%
  group_by(domainID, siteID) %>%
  summarize(rootN_totalreps = sum(rootNPercent_n, na.rm=TRUE),
            rootN_plotreps = sum(!is.na(rootNPercent_n)),
            rootC_totalreps = sum(rootCPercent_n, na.rm=TRUE),
            rootC_plotreps = sum(!is.na(rootCPercent_n)),
            rootCNR_totalreps = sum(rootCNratio_n, na.rm=TRUE),
            rootCNR_plotreps = sum(!is.na(rootCNratio_n)))

#### Investigate summaries of reps by year 
# Summarize by site x plot x year
litterCN_plotID_year  <- ltr_litterCarbonNitrogen %>%
  dplyr::mutate(year = lubridate::year(setDate), 
                month = lubridate::month(setDate), 
                month_year = stringr::str_pad(paste(month,year,sep="-"),
                                              width=7,pad="0",side="left"),
                litterNPercent = nitrogenPercent, 
                litterCPercent = carbonPercent,
                litterCNRatio = CNratio) %>%
  dplyr::select(year, domainID, siteID, plotID, plotType, month, year, month_year, collectDate, 
                litterNPercent, litterCPercent, litterCNRatio) %>%
  dplyr::group_by(year, domainID, siteID, plotID, plotType) %>%
  dplyr::summarize(litterNPercent_mean = mean(litterNPercent, na.rm=TRUE),
                   litterNPercent_n = sum(!is.na(litterNPercent)),
                   litterCPercent_mean = mean(litterCPercent, na.rm=TRUE),
                   litterCPercent_n = sum(!is.na(litterCPercent)),
                   litterCNRatio_mean = mean(litterCNRatio, na.rm=TRUE),
                   litterCNRatio_n = sum(!is.na(litterCNRatio)))

litterCN_siteYearReps <- litterCN_plotID_year %>%
  group_by(domainID, siteID, year) %>%
  summarize(litterN_totalreps = sum(litterNPercent_n, na.rm=TRUE),
            litterN_plotreps = sum(!is.na(litterNPercent_n)),
            litterC_totalreps = sum(litterCPercent_n, na.rm=TRUE),
            litterC_plotreps = sum(!is.na(litterCPercent_n)),
            litterCNR_totalreps = sum(litterCNRatio_n, na.rm=TRUE),
            litterCNR_plotreps = sum(!is.na(litterCNRatio_n)))

# Foliar CN samples: site x plot x year
foliarCN_plotID_year <- cfc_carbonNitrogen %>%
  dplyr::mutate(foliarNPercent = nitrogenPercent, foliarCPercent = carbonPercent,
                foliarCNRatio = CNratio,
                year = lubridate::year(collectDate)) %>%
  dplyr::filter(analyticalRepNumber == 1, cnPercentQF == "OK") %>%
  dplyr::select(year, domainID, siteID, plotID, plotType, sampleID, foliarNPercent, foliarCPercent, 
                foliarCNRatio) %>%
  group_by(year, domainID, siteID, plotID, plotType) %>%
  summarize(foliarNPercent_mean = mean(foliarNPercent, na.rm=TRUE),
            foliarNPercent_n = sum(!is.na(foliarNPercent)),
            foliarCPercent_mean = mean(foliarCPercent, na.rm=TRUE),
            foliarCPercent_n = sum(!is.na(foliarCPercent)),
            foliarCNRatio_mean = mean(foliarCNRatio, na.rm=TRUE),
            foliarCNRatio_n = sum(!is.na(foliarCNRatio)))

# Summarize sites, plot, and replicate counts
foliarCN_siteYearReps <- foliarCN_plotID %>%
  group_by(domainID, siteID) %>%
  summarize(foliarN_totalreps = sum(foliarNPercent_n, na.rm=TRUE),
            foliarN_plotreps = sum(!is.na(foliarNPercent_n)),
            foliarC_totalreps = sum(foliarCPercent_n, na.rm=TRUE),
            foliarC_plotreps = sum(!is.na(foliarCPercent_n)),
            foliarCNR_totalreps = sum(foliarCNRatio_n, na.rm=TRUE),
            foliarCNR_plotreps = sum(!is.na(foliarCNRatio_n)))

# Investigate why so many root reps?
rootCN_reps <- bbc_rootChemistry %>%
  filter(grepl("BBC",cnSampleID)) %>%
  mutate(year = lubridate::year(collectDate)) %>%
  select(year, domainID, siteID, plotID, plotType, cnSampleID, 
         nitrogenPercent, carbonPercent, CNratio) %>%
  group_by(year, domainID, siteID, plotID, plotType) %>%
  summarize(rootNPercent = mean(nitrogenPercent, na.rm=TRUE),
            rootNPercent_n = sum(!is.na(nitrogenPercent)))

# Investigate soil CN repeat sampling
# Load and organize soil percentage C & N pools
soil_CNplots_year <- sls_soilChemistry %>%
  tidyr::separate(sampleID, into = c("siteplot","horizon","plot_a","plot_b","colDate"),
                  sep = "-") %>%
  tidyr::separate(siteplot, into = c("siteID","justPlot"), sep = "_") %>%
  dplyr::mutate(year = lubridate::year(collectDate), 
                month = lubridate::month(collectDate),
                month_year = stringr::str_pad(paste(month,year,sep="-"),
                                              width=7,pad="0",side="left"),
                soilNPercent = nitrogenPercent, soilCPercent = organicCPercent,
                soilCNRatio = CNratio) %>%
  dplyr::filter(cnPercentQF == "OK", analyticalRepNumber == 1) %>%
  dplyr::select(year, domainID, siteID, plotID, plotType, horizon, 
                soilNPercent, soilCPercent, soilCNRatio) 

soil_CNplot_Ohorizon_year <- filter(soil_CNplots_year, horizon == "O") %>%
  mutate(soilNPercent_OHoriz = soilNPercent,
         soilCPercent_OHoriz = soilCPercent,
         soilCNRatio_OHoriz = soilCNRatio) %>%
  select(-soilNPercent, -soilCPercent, -soilCNRatio) %>%
  group_by(domainID, siteID, plotID, plotType, year) %>%
  summarize(soilNPercent_OHoriz_mean = mean(soilNPercent_OHoriz, na.rm=TRUE),
            soilNPercent_OHoriz_n = sum(!is.na(soilNPercent_OHoriz)),
            soilCPercent_OHoriz_mean = mean(soilCPercent_OHoriz, na.rm=TRUE),
            soilCPercent_OHoriz_n = sum(!is.na(soilCPercent_OHoriz)),
            soilCNRatio_OHoriz_mean = mean(soilCNRatio_OHoriz, na.rm=TRUE),
            soilCNRatio_OHoriz_n = sum(!is.na(soilCNRatio_OHoriz)))

soil_CNplot_Mhorizon_year <- filter(soil_CNplots_year, horizon == "M") %>%
  mutate(soilNPercent_MHoriz = soilNPercent,
         soilCPercent_MHoriz = soilCPercent,
         soilCNRatio_MHoriz = soilCNRatio) %>%
  select(-soilNPercent, -soilCPercent, -soilCNRatio) %>%
  group_by(domainID, siteID, plotID, plotType, year) %>%
  summarize(soilNPercent_MHoriz_mean = mean(soilNPercent_MHoriz, na.rm=TRUE),
            soilNPercent_MHoriz_n = sum(!is.na(soilNPercent_MHoriz)),
            soilCPercent_MHoriz_mean = mean(soilCPercent_MHoriz, na.rm=TRUE),
            soilCPercent_MHoriz_n = sum(!is.na(soilCPercent_MHoriz)),
            soilCNRatio_MHoriz_mean = mean(soilCNRatio_MHoriz, na.rm=TRUE),
            soilCNRatio_MHoriz_n = sum(!is.na(soilCNRatio_MHoriz)))

soilCN_plotID_year <- full_join(soil_CNplot_Ohorizon_year, 
                                soil_CNplot_Mhorizon_year)

