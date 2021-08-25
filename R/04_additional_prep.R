# key preps for analyses and figure creation

# Get mean values for each plot-site combination for each pool 

#N
mean_foliar<-aggregate(foliarNPercent_mean~siteID + plotID,mean,data=plot.df)
mean_root <- aggregate(rootNPercent ~ siteID + plotID, mean, data = plot.df)
mean_soil<-aggregate(soilNPercent_MHoriz_mean~siteID + plotID,mean,data=plot.df)
mean_soil_inorganic<-aggregate(inorganicN~siteID + plotID,mean,data=plot.df)
mean_soil_mineralization<-aggregate(netNminugPerGramPerDay~siteID + plotID,mean,data=plot.df)
mean_litter<-aggregate(litterNPercent_mean~siteID + plotID,mean,data=plot.df)
mean_resorp<-aggregate(resorpN~siteID + plotID,mean,data=plot.df)

#C:N
mean_foliar_cn<-aggregate(foliarCNRatio_mean~siteID + plotID,mean,data=plot.df)
mean_root_cn <- aggregate(rootCNratio ~ siteID + plotID, mean, data = plot.df)
mean_soil_cn<-aggregate(soilCNRatio_MHoriz_mean~siteID + plotID,mean,data=plot.df)
mean_litter_cn<-aggregate(litterCNRatio_mean~siteID + plotID,mean,data=plot.df)

#add site-means of soil texture to plot.df

coarse_fine_class <- aggregate(pctSand~siteID,mean,data=plot.df)
coarse_fine_class$pctSand_mean = round(coarse_fine_class$pctSand,2)
plot.df <- merge(plot.df,coarse_fine_class[c(1,3)],by=c('siteID'))

#add classification of soil texture
coarse_fine_class <- aggregate(pctSand~siteID,mean,data=plot.df)
coarse_fine_class$pctSand = round(coarse_fine_class$pctSand,2)

coarse_fine_class_coarse<-coarse_fine_class %>%
  dplyr::filter(pctSand < 50)
coarse_fine_class_coarse$texture <- 'coarse'

coarse_fine_class_fine<-coarse_fine_class %>%
  dplyr::filter(pctSand > 50)
coarse_fine_class_fine$texture <- 'fine'

coarse_fine_class_fine_course <- rbind(coarse_fine_class_fine,coarse_fine_class_coarse)

plot.df <- merge(plot.df,coarse_fine_class_fine_course[c(1,3)],by=c('siteID'))

#add classification of climate

map.site <- aggregate(MAP~siteID,mean,data=plot.df)

map.site_dry <- map.site %>%
  dplyr::filter(MAP < 1000)
map.site_dry$climate <- 'dry'
map.site_wet <- map.site %>%
  dplyr::filter(MAP > 1000)
map.site_wet$climate <- 'wet'
map.site_dry_wet <- rbind(map.site_wet,map.site_dry)
plot.df<-merge(map.site_dry_wet[c(1,3)],plot.df,by=c('siteID'))


