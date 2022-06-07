# last updated: 2022-6-6
# author: Andrew Felton
# project: NEON N scaling
# notes: This code downloads and calculates soil inorganic N data from the NEON data portal

################################################################################
library(neonNTrans)

#get corrected inorganic soil N (nitrate + ammonium) data
soil_inorganic_Data <- loadByProduct(dpID = "DP1.10086.001", package = "basic", 
                          site = "all",check.size = F,token = neonToken)

#get mineral horizon
horizon_check <- soil_inorganic_Data$sls_bgcSubsampling
horizon_check <- horizon_check %>%
  dplyr::select(sampleID,horizon) %>%
  filter(horizon == 'M')

kclInt <- soil_inorganic_Data$ntr_internalLab
kclInt <- merge(kclInt,horizon_check,by=c('sampleID'))

out <- def.calc.ntrans(kclInt = kclInt, 
                       kclIntBlank = soil_inorganic_Data$ntr_internalLabBlanks, 
                       kclExt = soil_inorganic_Data$ntr_externalLab, 
                       soilMoist = soil_inorganic_Data$sls_soilMoisture, 
                       dropAmmoniumFlags = "blanks exceed sample value", 
                       dropNitrateFlags = "blanks exceed sample value" )

out_2 <- data.frame(out[1])
head(out_2)

out_2 <- out_2 %>%
  dplyr::select(all_data.sampleID,all_data.siteID,all_data.plotID,all_data.extractionEndDate,
         all_data.soilInorganicNugPerGram)

head(out_2)

out_2 <- out_2 %>%
  rename('sampleID' = 'all_data.sampleID',
    'siteID' = 'all_data.siteID',
         'plotID' = 'all_data.plotID',
         'extractionEndDate' = 'all_data.extractionEndDate',
         'soilInorganicNugPerGram' = 'all_data.soilInorganicNugPerGram')

#merge with only M horizon samples
out_2 <- merge(out_2,horizon_check,by = c('sampleID'))
out_2$

#get plot average and reps
out_2 <- out_2 %>%
  group_by(siteID,plotID) %>%
summarise(soilInorganicNugPerGram_mean = mean(soilInorganicNugPerGram,na.rm=TRUE), #averaging across years
          soilInorganicNugPerGram_n = sum(!is.na(soilInorganicNugPerGram))) %>%
filter(soilInorganicNugPerGram_n > 0) #get rid o NA plots

#save to file for later addition to larger dataset
write.csv(out_2,'data/inorganic_soil_n_per_gram.csv')

rm(out,out_2,soil_inorganic_Data)

#next would be to do this by year



