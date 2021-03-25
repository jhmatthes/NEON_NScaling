# look at cross-site C relationships
head(plot.df)

#get plot means
mean_foliar_c<-aggregate(foliarCPercent_mean~siteID + plotID,mean,data=plot.df)
mean_root_c <- aggregate(rootCPercent ~ siteID + plotID, mean, data = plot.df)
mean_soil_c<-aggregate(soilCPercent_MHoriz_mean~siteID + plotID,mean,data=plot.df)
# mean_soil_inorganic<-aggregate(inorganicN~siteID + plotID,mean,data=plot.df)
# mean_soil_mineralization<-aggregate(netNminugPerGramPerDay~siteID + plotID,mean,data=plot.df)

#look at sample sizes
mean_foliar_c_n<-aggregate(foliarCPercent_mean~siteID,length,data=plot.df)
mean_root_c_n <- aggregate(rootCPercent ~ siteID , length, data = plot.df)
mean_soil_c_n<-aggregate(soilCPercent_MHoriz_mean~siteID ,length,data=plot.df)

# look at folar and soil N relationship ------
mean_foliar_soil_c <- merge(mean_foliar_c, mean_soil_c, by = c('siteID', 'plotID'))
length_mean_foliar_soil_c <- aggregate(plotID ~ siteID, length, data = mean_foliar_soil_c )
colnames(length_mean_foliar_soil_c) <- c('siteID','reps')

#remove sites with less than 4 replicates
length_mean_foliar_soil_c_reps <- length_mean_foliar_soil_c %>%
  dplyr::filter(reps > 3)

#merge so sites with < 4 reps are removed
merge_foliar_soil_c <- merge(length_mean_foliar_soil_c_reps,mean_foliar_soil_c,by=c('siteID'))

#get site means
merge_foliar_c_means<- merge_foliar_soil_c %>%
  group_by(siteID) %>%
  summarize(foliarC = mean(foliarCPercent_mean),
            soilC = mean(soilCPercent_MHoriz_mean))

length(merge_foliar_c_means$siteID) # 29 sites

#look at relationships
plot(foliarC ~ soilC,data=merge_foliar_c_means)
outlierTest(lm(foliarC ~ soilC,data=merge_foliar_c_means)) #no outlier
summary(lm(foliarC ~ soilC,data=merge_foliar_c_means))
# Adjusted R-squared:  0.02184 p-value: 0.2132

head(merge_foliar_c_means)
plot(foliarCPercent_mean~soilCPercent_MHoriz_mean,data=merge_foliar_soil_c)

#no clear relationship. What about without high value?
summary(lm(foliarC ~ soilC,data=merge_foliar_c_means[-7,]))
plot(foliarC ~ soilC,data=merge_foliar_c_means[-7,])
# Stronger relationship without high value
#Adjusted R-squared:  0.1791,p-value: 0.01432

#-------------------------------------------------------------

#now do root and soil C relationships----

mean_root_soil_c <- merge(mean_root_c, mean_soil_c, by = c('siteID', 'plotID'))
length_mean_root_soil_c <- aggregate(plotID ~ siteID, length, data = mean_root_soil_c )
colnames(length_mean_root_soil_c) <- c('siteID','reps')

#remove sites with less than 4 replicates
length_mean_root_soil_c_reps <- length_mean_root_soil_c %>%
  dplyr::filter(reps > 3)

#merge so sites with < 4 reps are removed
merge_root_soil_c <- merge(length_mean_root_soil_c_reps,mean_root_soil_c,by=c('siteID'))

#get site means
merge_root_c_means <- merge_root_soil_c %>%
  group_by(siteID) %>%
  summarize(rootC = mean(rootCPercent),
            soilC = mean(soilCPercent_MHoriz_mean))

length(merge_root_c_means$siteID) # 28 sites

#look at relationships
plot(rootC ~ soilC,data=merge_root_c_means)
outlierTest(lm(rootC ~ soilC,data=merge_root_c_means)) #no outlier
summary(lm(rootC ~ soilC,data=merge_root_c_means))
#Adjusted R-squared:  0.3812, p-value: 0.0002774 



