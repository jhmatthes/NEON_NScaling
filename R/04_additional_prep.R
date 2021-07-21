# key preps for analyses and figure creation

# Get mean values for each plot-site combination for each pool 
mean_foliar<-aggregate(foliarNPercent_mean~siteID + plotID,mean,data=plot.df)
mean_root <- aggregate(rootNPercent ~ siteID + plotID, mean, data = plot.df)
mean_soil<-aggregate(soilNPercent_MHoriz_mean~siteID + plotID,mean,data=plot.df)
mean_soil_inorganic<-aggregate(inorganicN~siteID + plotID,mean,data=plot.df)
mean_soil_mineralization<-aggregate(netNminugPerGramPerDay~siteID + plotID,mean,data=plot.df)
mean_litter<-aggregate(litterNPercent_mean~siteID + plotID,mean,data=plot.df)
mean_resorp<-aggregate(resorpN~siteID + plotID,mean,data=plot.df)
mean_foliar_cn<-aggregate(foliarCNRatio_mean~siteID + plotID,mean,data=plot.df)
mean_root_cn <- aggregate(rootCNratio ~ siteID + plotID, mean, data = plot.df)
mean_soil_cn<-aggregate(soilCNRatio_MHoriz_mean~siteID + plotID,mean,data=plot.df)
