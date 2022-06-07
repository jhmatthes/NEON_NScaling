# getting NPP data

# data reference:

#https://lpdaac.usgs.gov/products/myd17a3hgfv006/
library(MODISTools)

climate.df_2<-read.csv('./../data_pre-processed/bioclim_data.csv', #file path is relative
                     header = T, stringsAsFactors = F)
head(climate.df_2)
climate.df_2=climate.df_2[c(1,2,3)]
climate.df_2=climate.df_2[c(3,2,1)]
colnames(climate.df_2) <-c('site_name', 'lat','lon')

#NPP bands
mt_bands(product='MYD17A3HGF')
?mt_batch_subset

#get data
NEON_NPP<-mt_batch_subset(
  df=climate.df_2,
  product='MYD17A3HGF',
  band='Npp_500m',
  start = "2003-01-01",
  end = "2021-12-31",
  km_lr = 0,
  km_ab = 0,
  out_dir = tempdir(),
  internal = TRUE,
  ncores = "auto"
)

head(NEON_NPP)
summary(NEON_NPP)
#units are kg/m^2

#
neon_npp_means<- aggregate(value~site,mean,data=NEON_NPP)
head(neon_npp_means)  

#re-scale by 0.0001
neon_npp_means$value <- neon_npp_means$value*0.0001
#convert from kg to g
neon_npp_means$value <- neon_npp_means$value*1000
#round
neon_npp_means$value <- round(neon_npp_means$value,2)
#rename columns
colnames(neon_npp_means)  = c('siteID','npp_g_m2')

#save
write.csv(neon_npp_means,'./../data/NPP_data.csv')

#see if these data are correlated with MAP
# neon_npp_means <- merge(neon_npp_means,climate.df_2,by=c('siteID'))
# head(neon_npp_means)
# plot(npp_g_m2~Bio.12,data=neon_npp_means)
