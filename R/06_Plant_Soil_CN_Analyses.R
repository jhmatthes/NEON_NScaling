#C:N analyses

#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#---------------   Soil C:N effects on plant C:N     ---------------------------
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#  Root C:N and soil C:N spatial gradient  -----

merge_mean_soil_root_cn<-filter_reps(mean_soil_cn, mean_root_cn)

#check # of sites
length(merge_mean_soil_root_cn$siteID)

#round to two decimal places
merge_mean_soil_root_cn$rootCNratio <- round(merge_mean_soil_root_cn$rootCNratio,2)
merge_mean_soil_root_cn$soilCNRatio_MHoriz_mean <- round(merge_mean_soil_root_cn$soilCNRatio_MHoriz_mean,2)

#add veg type
merge_mean_soil_root_cn <- merge(merge_mean_soil_root_cn,vegtype.df,by='siteID')
aggregate(siteID~Lcclass,length,data=merge_mean_soil_root_cn)

#check for outliers
outlierTest(lm(rootCNratio~soilCNRatio_MHoriz_mean,data=merge_mean_soil_root_cn))
#no outliers

root_soil_cn_lm<-lm(rootCNratio~soilCNRatio_MHoriz_mean,data=merge_mean_soil_root_cn)
#summary(root_soil_cn_lm) 
#tab_model(root_soil_cn_lm)

#-------------------------------------------------------------------------------
# Foliar C:N and soil C:N spatial gradient  ---------

merge_mean_soil_foliar_cn <- filter_reps(mean_soil_cn, mean_foliar_cn)

#check # of sites
length(merge_mean_soil_foliar_cn$siteID) 

#round to two decimal places
merge_mean_soil_foliar_cn$foliarCNRatio_mean <- round(merge_mean_soil_foliar_cn$foliarCNRatio_mean,2)
merge_mean_soil_foliar_cn$soilCNRatio_MHoriz_mean <- round(merge_mean_soil_foliar_cn$soilCNRatio_MHoriz_mean,2)

#add veg type
merge_mean_soil_foliar_cn <- merge(merge_mean_soil_foliar_cn,vegtype.df,by='siteID')
aggregate(siteID~Lcclass,length,data=merge_mean_soil_foliar_cn)

#check for outliers
outlierTest(lm(foliarCNRatio_mean~soilCNRatio_MHoriz_mean,data=merge_mean_soil_foliar_cn))
#one outlier (1)

foliar_soil_cn_lm<-lm(foliarCNRatio_mean~soilCNRatio_MHoriz_mean,data=merge_mean_soil_foliar_cn[-1,])
#tab_model(foliar_soil_cn_lm)

#-------------------------------------------------------------------------------
# Root C:N and foliar C:N spatial gradient --------

merge_mean_root_foliar_cn <- filter_reps(mean_root_cn, mean_foliar_cn)

#check # of sites
length(merge_mean_root_foliar_cn$siteID) 

#round to two decimal places
merge_mean_root_foliar_cn$foliarCNRatio_mean <- round(merge_mean_root_foliar_cn$foliarCNRatio_mean,2)
merge_mean_root_foliar_cn$rootCNratio <- round(merge_mean_root_foliar_cn$rootCNratio,2)

#add veg type
merge_mean_root_foliar_cn <- merge(merge_mean_root_foliar_cn,vegtype.df,by='siteID')
aggregate(siteID~Lcclass,length,data=merge_mean_root_foliar_cn)

outlierTest(lm(foliarCNRatio_mean~rootCNratio,data=merge_mean_root_foliar_cn))
#no outliers

foliar_root_cn_lm<-lm(foliarCNRatio_mean~rootCNratio,data=merge_mean_root_foliar_cn)
tab_model(foliar_root_cn_lm)

#-------------------------------------------------------------------------------
# Mixed effect model: soil C:N effects on foliar C:N ---------------------------

foliar_cn_lme <- select(plot.df,c('siteID','MAP','Lcclass','soilCNRatio_MHoriz_mean','foliarCNRatio_mean'))

#remove NAs
foliar_cn_lme <- foliar_cn_lme %>%
  dplyr::filter(!soilCNRatio_MHoriz_mean=='NA') %>%
  dplyr::filter(!foliarCNRatio_mean =='NA') 

#check sample sizes
aggregate(foliarCNRatio_mean~siteID,length,data=foliar_cn_lme)

#remove site with only one rep
foliar_cn_lme <- foliar_cn_lme %>%
  dplyr::filter(!(siteID=="WREF")) %>%
  dplyr::filter(!(siteID=="BONA"))

#check herb sample size
herb.count<-subset(foliar_cn_lme,Lcclass=='herb')
length(herb.count$siteID) #59 obs
length(unique(herb.count$siteID)) #6 herb sites

#check woody sample size
woody.count<-subset(foliar_cn_lme,Lcclass=='woody')
length(woody.count$siteID)
length(unique(woody.count$siteID))


#Mixedd effects model:

leaf_cn_lme.1<-lme(foliarCNRatio_mean~ soilCNRatio_MHoriz_mean + MAP + Lcclass, random= ~1|siteID,data=foliar_cn_lme)
summary(leaf_cn_lme.1) #only significant factor is soil C:N
#r.squaredGLMM(leaf_cn_lme.1)


#-------------------------------------------------------------------------------
# Mixed effect model: soil C:N effects on foliar C:N ---------------------------

# Do mixed effects analysis for root N, same work flow

root_cn_lme <- select(plot.df,c('siteID','MAP','Lcclass','rootCNratio','soilCNRatio_MHoriz_mean'))
head(root_cn_lme)

root_cn_lme  <- root_cn_lme  %>%
  dplyr::filter(!rootCNratio=='NA') %>%
  dplyr::filter(!soilCNRatio_MHoriz_mean =='NA') 

#check sample sizes
aggregate(rootCNratio~siteID,length,data=root_cn_lme)

#all have at least 4 reps

#check herb sample size
herb.count<-subset(root_cn_lme,Lcclass=='herb')
length(herb.count$siteID)

#check woody sample size
woody.count<-subset(root_cn_lme,Lcclass=='woody')
length(woody.count$siteID)
length(unique(woody.count$siteID))

#mixed effects model:

root_cn_lme.1<-lme(rootCNratio~ soilCNRatio_MHoriz_mean + MAP + Lcclass , random= ~1|siteID,data=root_cn_lme)
summary(root_cn_lme.1) # soil C:N only significant 
# r.squaredGLMM(root_cn_lme.1)


#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#---------------   plant C:N feedbacks to soil C:N        ----------------------
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
# soil C:N and plant N resorption-----------------------------------------------

mean_resorp_soil_cn_2 <- filter_reps(mean_resorp, mean_soil_cn)
length(mean_resorp_soil_cn_2$plotID)
# N = 9 sites

#round to two decimal places
mean_resorp_soil_cn_2$resorpN <- round(mean_resorp_soil_cn_2$resorpN ,2)
mean_resorp_soil_cn_2$soilCNRatio_MHoriz_mean <- round(mean_resorp_soil_cn_2$soilCNRatio_MHoriz_mean,2)

#add veg type
mean_resorp_soil_cn_2 <- merge(mean_resorp_soil_cn_2,vegtype.df,by='siteID')
aggregate(siteID~Lcclass,length,data=mean_resorp_soil_cn_2)

#take a look
plot(soilCNRatio_MHoriz_mean~resorpN,data=mean_resorp_soil_cn_2)

#look at  outliers
#outlierTest(lm(soilCNRatio_MHoriz_mean~resorpN,data=mean_resorp_soil_cn_2))
#no outliers

#summary(lm(soilCNRatio_MHoriz_mean~resorpN,data=mean_resorp_soil_cn_2))
#not significant 



#-------------------------------------------------------------------------------
# soil C:N and litter C:N-----------------------------------------------

mean_litter_soil_cn_2 <- filter_reps(mean_litter_cn,mean_soil_cn)
length(mean_litter_soil_cn_2$plotID)
# 13 sites

#round to two decimal places
mean_litter_soil_cn_2$litterCNRatio_mean <- round(mean_litter_soil_cn_2$litterCNRatio_mean ,2)
mean_litter_soil_cn_2$soilCNRatio_MHoriz_mean <- round(mean_litter_soil_cn_2$soilCNRatio_MHoriz_mean,2)

#add veg type
mean_litter_soil_cn_2 <- merge(mean_litter_soil_cn_2,vegtype.df,by='siteID')
aggregate(siteID~Lcclass,length,data=mean_litter_soil_cn_2)

#take a look
plot(soilCNRatio_MHoriz_mean~litterCNRatio_mean,data=mean_litter_soil_cn_2)

#look at  outliers
#outlierTest(lm(soilCNRatio_MHoriz_mean~litterCNRatio_mean,data=mean_litter_soil_cn_2))
#no outliers

litter_soil_cn_lm<-lm(soilCNRatio_MHoriz_mean~litterCNRatio_mean,data=mean_litter_soil_cn_2)
tab_model(litter_soil_cn_lm)
#significant



#-------------------------------------------------------------------------------
# mixed effects models for plant effects on soil C:N (STOPPED HERE) ------------

soil_cn_lme <- select(plot.df,c('siteID','MAP','Lcclass','soilCNRatio_MHoriz_mean','litterCNRatio_mean'))
head(soil_cn_lme)

soil_cn_lme <- soil_cn_lme %>%
  dplyr::filter(!soilCNRatio_MHoriz_mean=='NA') %>%
  dplyr::filter(!litterCNRatio_mean =='NA') 

#check sample sizes
aggregate(soilCNRatio_MHoriz_mean~siteID,length,data=soil_cn_lme)

#remove site with only one rep
soil_cn_lme<- soil_cn_lme %>%
  dplyr::filter(!(siteID=="SJER")) %>%
  dplyr::filter(!(siteID=="DEJU"))

#check herb
herb.count<-subset(soil_cn_lme,Lcclass=='herb')
length(herb.count$siteID)
length(unique(herb.count$siteID))

#check woody
woody.count<-subset(soil_cn_lme,Lcclass=='woody')
length(woody.count$siteID)
length(unique(woody.count$siteID))

# lme functions lets you see P values in summary output
soil_cn_lme.1<-lme(soilCNRatio_MHoriz_mean~ litterCNRatio_mean + MAP , random= ~1|siteID,data=soil_cn_lme)
summary(soil_cn_lme.1)
# r.squaredGLMM(soil_cn_lme.1)

#note: nothing significant here in LMEs, most variance attributed to random effects




#-------------------------------------------------------------------------------