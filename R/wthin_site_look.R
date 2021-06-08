
#Within-site look between foliar and soil N

#foliar N and total soil N
# the cross-site relationship was not significant 

# re-run this bit of code:
merge_foliar_soil_means <-filter_reps(mean_foliar, mean_soil)

length(merge_foliar_soil_means$siteID) 
#23 with the limit as 4

#round to 2 decimal points
merge_foliar_soil_means$foliarNPercent_mean<-round(merge_foliar_soil_means$foliarNPercent_mean,2)
merge_foliar_soil_means$soilNPercent_MHoriz_mean<-round(merge_foliar_soil_means$soilNPercent_MHoriz_mean,2)

#add veg type
merge_foliar_soil_means <- merge(merge_foliar_soil_means,vegtype.df,by='siteID')

within_site_foliar_total <- merge_foliar_soil_means %>%
  dplyr::filter(reps > 9)



within_site_foliar_total <- merge(plot.df.2,within_site_foliar_total[c(1,2)],by='siteID')

#visualize

ggplot(within_site_foliar_total,aes(soilNPercent_MHoriz_mean,foliarNPercent_mean)) +
  facet_wrap(~siteID,scales = 'free') +
  geom_point()

#does not appear related within sites

#
#

# foliar and soil inorganic N

merge_foliar_soil_inorganic <- filter_reps(mean_foliar, mean_soil_inorganic)

length(merge_foliar_soil_inorganic$siteID) 
# 21 sites with the 4 rep thresholds

#round to two decimal points
merge_foliar_soil_inorganic$foliarNPercent_mean <- round(merge_foliar_soil_inorganic$foliarNPercent_mean,2)
merge_foliar_soil_inorganic$inorganicN<- round(merge_foliar_soil_inorganic$inorganicN,2)

#add veg type
merge_foliar_soil_inorganic <- merge(merge_foliar_soil_inorganic,vegtype.df,by='siteID')

within_site_foliar_inorganic <- merge_foliar_soil_inorganic %>%
  dplyr::filter(reps > 9)

within_site_foliar_inorganic <- merge(plot.df.2,within_site_foliar_inorganic[c(1,2)],by='siteID')

ggplot(within_site_foliar_inorganic,aes(inorganicN,foliarNPercent_mean)) +
  facet_wrap(~siteID,scales = 'free') +
  geom_point()

#more sites show relationships for inorganic soil N

#
#

# foliar and soil C:N

#re-run this code:

merge_mean_soil_foliar_cn <- filter_reps(mean_soil_cn, mean_foliar_cn)
length(merge_mean_soil_foliar_cn$siteID) 

#round to two decimal places
merge_mean_soil_foliar_cn$foliarCNRatio_mean <- round(merge_mean_soil_foliar_cn$foliarCNRatio_mean,2)
merge_mean_soil_foliar_cn$soilCNRatio_MHoriz_mean <- round(merge_mean_soil_foliar_cn$soilCNRatio_MHoriz_mean,2)

#add veg type
merge_mean_soil_foliar_cn <- merge(merge_mean_soil_foliar_cn,vegtype.df,by='siteID')

within_site_foliar_soil_cn <- merge_mean_soil_foliar_cn %>%
  dplyr::filter(reps > 9)

within_site_foliar_soil_cn  <- merge(plot.df.2,within_site_foliar_soil_cn[c(1,2)],by='siteID')

#visualize 
ggplot(within_site_foliar_soil_cn,aes(soilCNRatio_MHoriz_mean,foliarCNRatio_mean)) +
  facet_wrap(~siteID,scales = 'free') +
  geom_point()

#not many strong relationships!

# stopped here 



