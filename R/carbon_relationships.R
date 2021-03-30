# look at cross-site C relationships
head(plot.df)

#get plot means and sample sizes ------
mean_foliar_c<-aggregate(foliarCPercent_mean~siteID + plotID,mean,data=plot.df)
mean_root_c <- aggregate(rootCPercent ~ siteID + plotID, mean, data = plot.df)
mean_soil_c<-aggregate(soilCPercent_MHoriz_mean~siteID + plotID,mean,data=plot.df)

#look at sample sizes
mean_foliar_c_n<-aggregate(foliarCPercent_mean~siteID,length,data=plot.df)
mean_root_c_n <- aggregate(rootCPercent ~ siteID , length, data = plot.df)
mean_soil_c_n<-aggregate(soilCPercent_MHoriz_mean~siteID ,length,data=plot.df)

# look at foliar and soil C relationship ------

merge_foliar_c_means <- filter_reps(mean_foliar_c, mean_soil_c)

length(merge_foliar_c_means$siteID) # 29 sites

#look at relationships
plot(foliarCPercent_mean ~ soilCPercent_MHoriz_mean,data=merge_foliar_c_means)
outlierTest(lm(foliarCPercent_mean ~ soilCPercent_MHoriz_mean,data=merge_foliar_c_means)) #no outlier
summary(lm(foliarCPercent_mean ~ soilCPercent_MHoriz_mean,data=merge_foliar_c_means))
# Adjusted R-squared:  0.02184 
# p-value: 0.2132

plot(foliarCPercent_mean ~ soilCPercent_MHoriz_mean,data=merge_foliar_c_means)

#no clear relationship. What about without high value?
summary(lm(foliarCPercent_mean ~ soilCPercent_MHoriz_mean,data=merge_foliar_c_means[-7,]))
plot(foliarCPercent_mean ~ soilCPercent_MHoriz_mean,data=merge_foliar_c_means[-7,])
# Stronger relationship without high value
#Adjusted R-squared:  0.1791,p-value: 0.01432

#-------------------------------------------------------------

#now do root and soil C relationships----


merge_root_c_means <- filter_reps(mean_root_c, mean_soil_c)

length(merge_root_c_means$siteID) # 28 sites

# round
merge_root_c_means$rootCPercent <- round(merge_root_c_means$rootCPercent,2)
merge_root_c_means$soilCPercent_MHoriz_mean <- round(merge_root_c_means$soilCPercent_MHoriz_mean,2)

#look at relationships
plot(rootCPercent ~ soilCPercent_MHoriz_mean,data=merge_root_c_means)
outlierTest(lm(rootCPercent ~ soilCPercent_MHoriz_mean,data=merge_root_c_means)) #no outlier
summary(lm(rootCPercent ~ soilCPercent_MHoriz_mean,data=merge_root_c_means))
#Adjusted R-squared:  0.3812, p-value: 0.0002774 



