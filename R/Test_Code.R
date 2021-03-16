#neon N transformations


library(neonUtilities)

soilData <- loadByProduct(site = "all", dpID = "DP1.10086.001", package = "basic", check.size = F)
library(devtools) 
#install_github("NEONScience/NEON-Nitrogen-Transformations/neonNTrans", dependencies=TRUE)  
library(neonNTrans) 
out <- def.calc.ntrans(kclInt = soilData$ntr_internalLab,
                       kclIntBlank = soilData$ntr_internalLabBlanks,
                       kclExt = soilData$ntr_externalLab,
                       soilMoist = soilData$sls_soilMoisture,
                       dropAmmoniumFlags = "blanks exceed sample value",
                       dropNitrateFlags = "blanks exceed sample value" )

# Read in inogranic N data
min.df<-as.data.frame(out[1])
# Read in soil texture data
min.df_plotID <- min.df %>%
  select(all_data.domainID, all_data.siteID, all_data.plotID, all_data.netNminugPerGramPerDay) %>%
  group_by(all_data.domainID, all_data.siteID, all_data.plotID) %>%
  summarize(netNminugPerGramPerDay = mean(all_data.netNminugPerGramPerDay, na.rm=TRUE)) %>%
  filter(!netNminugPerGramPerDay=='NaN')

colnames(min.df_plotID) <-c('domainID','siteID','plotID','netNminugPerGramPerDay')
min.df_plotID$netNminugPerGramPerDay<-round(min.df_plotID$netNminugPerGramPerDay,2)
head(min.df_plotID)
hist(min.df_plotID$netNminugPerGramPerDay)
