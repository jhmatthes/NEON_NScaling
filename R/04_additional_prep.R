# key preps for analyses or for figure creation

# get aridity and veg data, subset data into just veg and herb veg types -----

#unique(plot.df$siteID)

# get aridity data
vpd <- read.csv('./../data_pre-processed/scaled_vpd.csv')
head(vpd)

#cleanup
vpd<-vpd[c(2,3)]
colnames(vpd) <- c('siteID','vpd')

# merge with vpd data frame
plot.df.2 <- merge(vpd,plot.df,by=c('siteID'))

#unique(plot.df$Lcclass)

#filter to just woody and herb (not NAs)
plot.df.2 <- plot.df.2 %>%
  dplyr::filter(!Lcclass=='NA')
length(unique(plot.df.2$siteID)) # 35 sites
head(plot.df.2)
#unique(plot.df.2$Lcclass)
unique(plot.df.2$siteID)

#combine with site coordinate df
# mat_map <- read.csv('./../data_pre-processed/MAT_MAP_Allsites.csv')
# sites <- data.frame(unique(plot.df.2$siteID))
# colnames(sites) <- c('siteID')
# sites <- merge(mat_map[c(1,2,3)],sites,by='siteID')
# write.csv(sites,'./../data_pre-processed/sites_in_analyses.csv')

# Get mean values for each plot-site combination for each pool -----
mean_foliar<-aggregate(foliarNPercent_mean~siteID + plotID,mean,data=plot.df.2)
mean_root <- aggregate(rootNPercent ~ siteID + plotID, mean, data = plot.df.2)
mean_soil<-aggregate(soilNPercent_MHoriz_mean~siteID + plotID,mean,data=plot.df.2)
mean_soil_inorganic<-aggregate(inorganicN~siteID + plotID,mean,data=plot.df.2)
mean_soil_mineralization<-aggregate(netNminugPerGramPerDay~siteID + plotID,mean,data=plot.df.2)
mean_litter<-aggregate(litterNPercent_mean~siteID + plotID,mean,data=plot.df.2)
mean_resorp<-aggregate(resorpN~siteID + plotID,mean,data=plot.df.2)