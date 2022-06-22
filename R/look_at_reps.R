

#look at data:

cn_full <- read.csv('data/CN_full_dataset.csv')
head(cn_full)

subset_soil_look <- cn_full %>%
  dplyr::select(soilNPercent_MHoriz_mean, plotType, siteID)

subset_soil_look <- na.omit(subset_soil_look)

aggregate(soilNPercent_MHoriz_mean ~ plotType + siteID,length,data=cn_full)

duplicated(cn_full)

cn_site_reps <- read.csv('data/CN_site_replicates.csv')
head(cn_site_reps,1)

yearly <- read.csv('data/CN_full_data_by_year.csv')

yearly_soil <- yearly %>%
  dplyr::select(year, plotType, siteID,soilNPercent_MHoriz_mean)

aggregate(soilNPercent_MHoriz_mean ~ plotType + siteID + year,length,data=yearly_soil)



#
#

#post downloading of soil N data:
sls_soilChemistry$eve

soil_CNplots_2 <- sls_soilChemistry %>%
  tidyr::separate(sampleID, into = c("siteplot","horizon","plot_a","plot_b","colDate"),
                  sep = "-") %>%
  tidyr::separate(siteplot, into = c("siteID","justPlot"), sep = "_") %>%
  dplyr::mutate(year = lubridate::year(collectDate), 
                month = lubridate::month(collectDate),
                month_year = stringr::str_pad(paste(month,year,sep="-"),
                                              width=7,pad="0",side="left"),
                soilNPercent = nitrogenPercent, soilCPercent = organicCPercent,
                soilCNRatio = CNratio) %>%
  dplyr::filter(cnPercentQF == "OK") %>%
  dplyr::select(domainID, siteID, plotID, plotType, year, horizon, 
                soilNPercent, soilCPercent, soilCNRatio) 

soil_CNplot_Ohorizon_2 <- filter(soil_CNplots_2, horizon == "O") %>%
  mutate(soilNPercent_OHoriz = soilNPercent,
         soilCPercent_OHoriz = soilCPercent,
         soilCNRatio_OHoriz = soilCNRatio) %>%
  dplyr::select(-soilNPercent, -soilCPercent, -soilCNRatio) %>%
  group_by(domainID, siteID, plotID, plotType) %>%
  summarise(soilNPercent_OHoriz_mean = mean(soilNPercent_OHoriz, na.rm=TRUE),
            soilNPercent_OHoriz_n = sum(!is.na(soilNPercent_OHoriz)),
            soilCPercent_OHoriz_mean = mean(soilCPercent_OHoriz, na.rm=TRUE),
            soilCPercent_OHoriz_n = sum(!is.na(soilCPercent_OHoriz)),
            soilCNRatio_OHoriz_mean = mean(soilCNRatio_OHoriz, na.rm=TRUE),
            soilCNRatio_OHoriz_n = sum(!is.na(soilCNRatio_OHoriz)))

soil_CNplot_Mhorizon_2 <- filter(soil_CNplots_2, horizon == "M") %>%
  mutate(soilNPercent_MHoriz = soilNPercent,
         soilCPercent_MHoriz = soilCPercent,
         soilCNRatio_MHoriz = soilCNRatio) %>%
  dplyr::select(-soilNPercent, -soilCPercent, -soilCNRatio) %>%
  group_by(year,domainID, siteID, plotID, plotType) %>%
  summarise(soilNPercent_MHoriz_mean = mean(soilNPercent_MHoriz, na.rm=TRUE),
            soilNPercent_MHoriz_n = sum(!is.na(soilNPercent_MHoriz)),
            soilCPercent_MHoriz_mean = mean(soilCPercent_MHoriz, na.rm=TRUE),
            soilCPercent_MHoriz_n = sum(!is.na(soilCPercent_MHoriz)),
            soilCNRatio_MHoriz_mean = mean(soilCNRatio_MHoriz, na.rm=TRUE),
            soilCNRatio_MHoriz_n = sum(!is.na(soilCNRatio_MHoriz)))

soilCN_plotID_2 <- full_join(soil_CNplot_Ohorizon_2, soil_CNplot_Mhorizon_2)

rm(soil_CNplots,soil_CNplot_Ohorizon, soil_CNplot_Mhorizon)

# Summarize plots and count replicates
soilCN_plotIDReps <- soilCN_plotID_2[,grepl("_n",colnames(soilCN_plotID_2))]
soilCN_plotIDReps <- data.frame(siteID = soilCN_plotID$siteID, 
                                plotID = soilCN_plotID$plotID,
                                soilCN_plotIDReps)

sample_sizes <- aggregate(soilNPercent_MHoriz_mean ~ siteID + plotType + year,length,data=soilCN_plotID_2)
aggregate(soilNPercent_MHoriz_mean ~ plotType,median,data=sample_sizes)
unique(soilCN_plotID_2$year)

rep_look <- ggplot(soilCN_site_reps_year,aes(x=year,y=soil_n_reps,color=plotType)) +
  stat_summary(fun='mean',geom='line') +
  stat_summary(fun='mean',geom='point') +
  ylab('Averge # of % Soil N samples') 
  #geom_point()
  
  png('rep_look.png')

rep_look 

dev.off()

  
