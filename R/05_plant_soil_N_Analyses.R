# last updated: 2021-31-6
# author: Adrienne Keller/Andrew Felton
# project: NEON N scaling
# notes:

#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
# ----------------Soil impacts on plant N --------------------------------------
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
# % Foliar N and % total soil N spatial gradient ----

#make data frame where each combination has at least 4 reps per site
merge_foliar_soil_means <-filter_reps(mean_foliar, mean_soil)

length(merge_foliar_soil_means$siteID) 
#26 sites with the limit of 4 replicates

#round to 2 decimal points
merge_foliar_soil_means$foliarNPercent_mean<-round(merge_foliar_soil_means$foliarNPercent_mean,2)
merge_foliar_soil_means$soilNPercent_MHoriz_mean<-round(merge_foliar_soil_means$soilNPercent_MHoriz_mean,2)

#add veg type
merge_foliar_soil_means <- merge(merge_foliar_soil_means,vegtype.df,by='siteID')
aggregate(siteID~Lcclass,length,data=merge_foliar_soil_means)
# 6 herb, 20 woody

#look at  outliers
outlierTest(lm(foliarNPercent_mean ~ soilNPercent_MHoriz_mean,data=merge_foliar_soil_means))
#no outliers

#take a look
#plot(foliarNPercent_mean ~ soilNPercent_MHoriz_mean,data=merge_foliar_soil_means)

#look at LM
foliar_soil_total_lm<-lm(foliarNPercent_mean ~ soilNPercent_MHoriz_mean,data=merge_foliar_soil_means)
summary(foliar_soil_total_lm)
# not significant 

#see if inferences change when we remove GUAN (the site with very high soil N)

#plot(foliarNPercent_mean ~ soilNPercent_MHoriz_mean,data=merge_foliar_soil_means[-6,])
foliar_soil_total_noguan_lm<-lm(foliarNPercent_mean ~ soilNPercent_MHoriz_mean,data=merge_foliar_soil_means[-6,])
summary(foliar_soil_total_lm)
# Still not significant 


#-------------------------------------------------------------------------------
# % Root N and % total soil N spatial gradient -----

#make data frame where each combination has at least 4 reps per site
merge_soil_root <- filter_reps(mean_soil,mean_root)

#count to two decimal points
merge_soil_root$rootNPercent <- round(merge_soil_root$rootNPercent,2)
merge_soil_root$soilNPercent_MHoriz_mean <- round(merge_soil_root$soilNPercent_MHoriz_mean,2)

# check # of sites
length(merge_soil_root$siteID) 
#22 sites with the 4 rep limit

#add veg type
merge_soil_root <- merge(merge_soil_root,vegtype.df,by='siteID')
aggregate(siteID~Lcclass,length,data=merge_soil_root)

# take a look
outlierTest(lm(rootNPercent~soilNPercent_MHoriz_mean,data=merge_soil_root))
#one outlier (# 20)

#take a look
#plot(rootNPercent~soilNPercent_MHoriz_mean,data=merge_soil_root[-20,])

root_soil_total_lm<-lm(rootNPercent~soilNPercent_MHoriz_mean,data=merge_soil_root[-20,]) 
summary(root_soil_total_lm)
# not significant (but close)

#-------------------------------------------------------------------------------
# % Foliar N and soil inorganic N (ammonium + nitrate) spatial gradient ------------

#make data frame where each combination has at least 4 reps per site
merge_foliar_soil_inorganic <- filter_reps(mean_foliar, mean_soil_inorganic)

# check # of sites
length(merge_foliar_soil_inorganic$siteID) 
# 25 sites with the 4 rep thresholds

#round to two decimal points
merge_foliar_soil_inorganic$foliarNPercent_mean <- round(merge_foliar_soil_inorganic$foliarNPercent_mean,2)
merge_foliar_soil_inorganic$inorganicN<- round(merge_foliar_soil_inorganic$inorganicN,2)

#add veg type
merge_foliar_soil_inorganic <- merge(merge_foliar_soil_inorganic,vegtype.df,by='siteID')
aggregate(siteID~Lcclass,length,data=merge_foliar_soil_inorganic)

#take a look
plot(foliarNPercent_mean~inorganicN,data=merge_foliar_soil_inorganic)

# outlierTest(lm(foliarNPercent_mean~inorganicN,data=merge_foliar_soil_inorganic))
#no outliers

#take a look
#plot(foliarNPercent_mean~inorganicN,data=merge_foliar_soil_inorganic)

leaf_soil_inorganic_lm<-lm(foliarNPercent_mean~inorganicN,data=merge_foliar_soil_inorganic)
summary(leaf_soil_inorganic_lm)
# not significant 

#-------------------------------------------------------------------------------

# % Root N and  soil inorganic N (ammonium + nitrate) spatial gradient ------

#make data frame where each combination has at least 4 reps per site
merge_soil_root_inorganic <- filter_reps(mean_soil_inorganic, mean_root)

#check # of sites
length(merge_soil_root_inorganic$siteID) 
# 25 sites with minimum of 4 reps

#round to two decimal points
merge_soil_root_inorganic$rootNPercent<-round(merge_soil_root_inorganic$rootNPercent,2)
merge_soil_root_inorganic$inorganicN<-round(merge_soil_root_inorganic$inorganicN,2)

#add veg type
merge_soil_root_inorganic <- merge(merge_soil_root_inorganic,vegtype.df,by='siteID')
aggregate(siteID~Lcclass,length,data=merge_soil_root_inorganic)

#outlierTest(lm(rootNPercent~inorganicN,data=merge_soil_root_inorganic))
#no outliers

#take a look
#plot(rootNPercent~inorganicN,data=merge_soil_root_inorganic)

root_soil_inorganic_lm<-lm(rootNPercent~inorganicN,data=merge_soil_root_inorganic)
summary(root_soil_inorganic_lm)
#tab_model(root_soil_inorganic_lm)
#significant 

#-------------------------------------------------------------------------------
# Mixed effects models of % Foliar N using inorganic soil N --------------------

# select columns
foliar_lme <- select(plot.df,c('siteID','MAP','foliarNPercent_mean','inorganicN','Lcclass'))
#head(foliar_lme)

# eliminate NAs
foliar_lme <- foliar_lme %>%
  dplyr::filter(!foliarNPercent_mean=='NA') %>%
  dplyr::filter(!inorganicN =='NA') 

#check sample sizes
length_foliar_lme<-aggregate(foliarNPercent_mean~siteID,length,data=foliar_lme)
length_inorganic_lme<-aggregate(inorganicN~siteID,length,data=foliar_lme)

#get rid of WREF. Doesn't have enough replicates.

#remove sites with less than 4 replicates
foliar_lme <- foliar_lme %>%
  dplyr::filter(!siteID  =='WREF')

#check herb reps
herb.count<-subset(foliar_lme,Lcclass=='herb')
length(herb.count$siteID) #52 obs
length(unique(herb.count$siteID)) #6 herb sites

#check woody reps
woody.count<-subset(foliar_lme,Lcclass=='woody')
length(woody.count$siteID) #173 obs
length(unique(woody.count$siteID)) #20 herb sites

# mixed effects model: 

# Chose lme functions because it lets you see P values in summary output
leaf_lme.1<-lme(foliarNPercent_mean~ inorganicN + MAP + Lcclass, random= ~1|siteID,data=foliar_lme)
summary(leaf_lme.1) # inorganic N shows as significant
r.squaredGLMM(leaf_lme.1)


#-------------------------------------------------------------------------------
# Mixed effects models of % Root N using inorganic soil N ---------------------------------------------------------

# Do mixed effects analysis for root N, same work flow

root_lme <- select(plot.df,c('siteID','MAP','rootNPercent','inorganicN','Lcclass'))
#head(root_lme)

# Remove NAs
root_lme <- root_lme %>%
  dplyr::filter(!rootNPercent=='NA') %>%
  dplyr::filter(!inorganicN =='NA') 

#check sample sizes
length_root_lme<-aggregate(rootNPercent~siteID,length,data=root_lme)
length_inorganic_lme<-aggregate(inorganicN~siteID,length,data=root_lme)

#remove sites with less than 4 replicates
root_lme <- root_lme %>%
  dplyr::filter(!siteID  =='GUAN') %>%
  dplyr::filter(!siteID  =='PUUM')

#check herb sample size
herb.count<-subset(root_lme,Lcclass=='herb')
length(herb.count$siteID) #28 obs
length(unique(herb.count$siteID)) #6 herb sites

#check woody sample size
woody.count<-subset(root_lme,Lcclass=='woody')
length(woody.count$siteID)
length(unique(woody.count$siteID))

# Mixed effects model:

root_lme.1<-lme(rootNPercent~ inorganicN + MAP + Lcclass , random= ~1|siteID,data=root_lme)
summary(root_lme.1) 
r.squaredGLMM(root_lme.1)


#-------------------------------------------------------------------------------
# Mixed effects models of soil effects on % foliar N using % total soil N ------

foliar_lme_total <- select(plot.df,c('siteID','MAP','foliarNPercent_mean','soilNPercent_MHoriz_mean','Lcclass'))
#head(foliar_lme_total)

foliar_lme_total <- foliar_lme_total %>%
  dplyr::filter(!foliarNPercent_mean=='NA') %>%
  dplyr::filter(!soilNPercent_MHoriz_mean=='NA') 

#check sample sizes
length_foliar_lme_total<-aggregate(foliarNPercent_mean~siteID,length,data=foliar_lme_total)

#get rid of WREF, PUUM, and BONA

#remove sites with less than 4 replicates
foliar_lme_total <- foliar_lme_total %>%
  dplyr::filter(!siteID  =='WREF') %>%
  dplyr::filter(!siteID  =='PUUM') %>%
  dplyr::filter(!siteID  =='BONA')

#check herb sample size
herb.count<-subset(foliar_lme_total,Lcclass=='herb')
length(herb.count$siteID) #55 obs
length(unique(herb.count$siteID)) #6 herb sites

#check woody sample size
woody.count<-subset(foliar_lme_total,Lcclass=='woody')
length(woody.count$siteID) #152
length(unique(woody.count$siteID)) #17 woody sites

# lme functions (lme lets you see P values in summary output)
leaf_total_lme.1<-lme(foliarNPercent_mean~ soilNPercent_MHoriz_mean + MAP + Lcclass, random= ~1|siteID,data=foliar_lme_total)
summary(leaf_total_lme.1) # inorganic N shows as significant
r.squaredGLMM(leaf_total_lme.1)


#-------------------------------------------------------------------------------

# Mixed effects models of soil effects on % root N using % total soil N ------

root_lme_total <- select(plot.df,c('siteID','MAP','rootNPercent','soilNPercent_MHoriz_mean','Lcclass'))
#head(root_lme)

root_lme_total <- root_lme_total %>%
  dplyr::filter(!rootNPercent=='NA') %>%
  dplyr::filter(!soilNPercent_MHoriz_mean =='NA') 

#check sample sizes
length_root_lme<-aggregate(rootNPercent~siteID,length,data=root_lme_total)

#remove sites with less than 4 replicates
root_lme_total <- root_lme_total %>%
  dplyr::filter(!siteID  =='PUUM') %>%
  dplyr::filter(!siteID  =='BONA') %>%
  dplyr::filter(!siteID  =='JORN') %>%
  dplyr::filter(!siteID  =='MOAB') %>%
  dplyr::filter(!siteID  =='GUAN')

#check herb sample size
herb.count<-subset(root_lme_total,Lcclass=='herb')
length(herb.count$siteID) #28 obs
length(unique(herb.count$siteID)) #7 herb sites

#check woody sample sizes
woody.count<-subset(root_lme_total,Lcclass=='woody')
length(woody.count$siteID) #73 obs
length(unique(woody.count$siteID)) #17 sites

#Mixed effects model:

root_total_lme.1<-lme(rootNPercent~ soilNPercent_MHoriz_mean + MAP + Lcclass , random= ~1|siteID,data=root_lme_total)
summary(root_total_lme.1) 
r.squaredGLMM(root_total_lme.1)

#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
# ----------------Plant feedbacks to soil N ------------------------------------
#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------

# Litter and soil inorganic N -----
merge_litter_soil_inorganic <- filter_reps(mean_litter, mean_soil_inorganic)

length(merge_litter_soil_inorganic$siteID) 
# 12 sites with the 4 rep thresholds

merge_litter_soil_inorganic <- merge(merge_litter_soil_inorganic,vegtype.df,by='siteID')

# round to two decimal points
merge_litter_soil_inorganic$litterNPercent_mean <- round(merge_litter_soil_inorganic$litterNPercent_mean,2)
merge_litter_soil_inorganic$inorganicN <- round(merge_litter_soil_inorganic$inorganicN,2)

# take a look
outlierTest(lm(inorganicN~litterNPercent_mean,data=merge_litter_soil_inorganic))
#one outlier (3)

#plot(inorganicN~litterNPercent_mean,data=merge_litter_soil_inorganic[-3,])

#remove outlier
summary(lm(inorganicN~litterNPercent_mean,data=merge_litter_soil_inorganic[-3,]))

#not significant

#-------------------------------------------------------------------------------
# Resorption and inorganic soil N ------


# Resorption and inorganic soil N
mean_resorp_inorganic_soil <- filter_reps(mean_resorp, mean_soil_inorganic)

#compare woody versus herb representation
mean_resorp_inorganic_soil <- merge(mean_resorp_inorganic_soil,vegtype.df,by='siteID')

# round to two decimal points
mean_resorp_inorganic_soil$resorpN <- round(mean_resorp_inorganic_soil$resorpN,2)
mean_resorp_inorganic_soil$inorganicN <- round(mean_resorp_inorganic_soil$inorganicN,2)

# take a look
#plot(inorganicN~resorpN,data=mean_resorp_inorganic_soil) 
outlierTest(lm(inorganicN~resorpN,data=mean_resorp_inorganic_soil))


# one outlier (1)#try without outlier
plot(inorganicN~resorpN,data=mean_resorp_inorganic_soil[-1,])
summary(lm(inorganicN~resorpN,data=mean_resorp_inorganic_soil[-1,]))

#not significant 

#-------------------------------------------------------------------------------
# % Total soil N and % litter N spatial gradient -------------------------------

merge_litter_soil_total <- filter_reps(mean_litter, mean_soil)

#check number of sites
length(merge_litter_soil_total$siteID) 
# 13 sites with the 4 rep thresholds

merge_litter_soil_total <- merge(merge_litter_soil_total,vegtype.df,by='siteID')

# take a look
outlierTest(lm(soilNPercent_MHoriz_mean~litterNPercent_mean,data=merge_litter_soil_total))
#one outlier (4)

summary(lm(soilNPercent_MHoriz_mean~litterNPercent_mean,data=merge_litter_soil_total[-4,]))

#not significant 

#-------------------------------------------------------------------------------
# % Total soil N and N resorption spatial gradient -----------------------------

mean_resorp_total_soil <- filter_reps(mean_resorp, mean_soil)

#compare woody versus herb representation
mean_resorp_total_soil <- merge(mean_resorp_total_soil,vegtype.df,by='siteID')

# round to two decimal points
mean_resorp_total_soil$resorpN <- round(mean_resorp_total_soil$resorpN,2)
mean_resorp_total_soil$soilNPercent_MHoriz_mean <- round(mean_resorp_total_soil$soilNPercent_MHoriz_mean,2)

# take a look
outlierTest(lm(soilNPercent_MHoriz_mean~resorpN,data=mean_resorp_total_soil))
#one outlier (1)

summary(lm(soilNPercent_MHoriz_mean~resorpN,data=mean_resorp_total_soil[-1,]))

#not significant 

#-------------------------------------------------------------------------------
# Mixed effects models for plant effects on soil N ---------


# look at both total and inorganic soil N

# total soil N
total_soil_lme <- select(plot.df,c('siteID','MAP','soilNPercent_MHoriz_mean','litterNPercent_mean','Lcclass','resorpN'))

#remove NAs
total_soil_lme <- total_soil_lme %>%
  dplyr::filter(!soilNPercent_MHoriz_mean=='NA') %>%
  dplyr::filter(!litterNPercent_mean =='NA') %>% 
  dplyr::filter(!resorpN =='NA')

#check sample sizes
length_total_soil_lme<-aggregate(soilNPercent_MHoriz_mean~siteID,length,data=total_soil_lme)

#remove sites with less than 4 replicates
total_soil_lme <- total_soil_lme %>%
  dplyr::filter(!siteID  =='DELA') %>%
  dplyr::filter(!siteID  =='SCBI') %>%
  dplyr::filter(!siteID  =='SJER') %>%
  dplyr::filter(!siteID  =='UNDE') %>%
  dplyr::filter(!siteID  =='BONA') %>%
  dplyr::filter(!siteID  =='DEJU') %>%
  dplyr::filter(!siteID  =='PUUM') %>%
  dplyr::filter(!siteID  =='UNDE') 

#head(mean_foliar_lme)
unique(total_soil_lme$Lcclass)
length(unique(total_soil_lme$siteID))
#these are all woody

# lme functions lets you see P values in summary output
total_soil_lme.1<-lme(soilNPercent_MHoriz_mean~ litterNPercent_mean + MAP, random= ~1|siteID,data=total_soil_lme)
summary(total_soil_lme.1) 
r.squaredGLMM(total_soil_lme.1)


#inorganic soil N

inorganic_soil_lme <- select(plot.df,c('siteID','MAP','inorganicN','litterNPercent_mean','Lcclass','resorpN'))

#remove NAs
inorganic_soil_lme <- inorganic_soil_lme  %>%
  dplyr::filter(!inorganicN=='NA') %>%
  dplyr::filter(!litterNPercent_mean =='NA') %>% 
  dplyr::filter(!resorpN =='NA')

#check sample sizes
length_inorganic_soil_lme<-aggregate(inorganicN~siteID,length,data=inorganic_soil_lme)

#remove sites with less than 4 replicates
inorganic_soil_lme <- inorganic_soil_lme %>%
  dplyr::filter(!siteID  =='DELA') %>%
  dplyr::filter(!siteID  =='SCBI') %>%
  dplyr::filter(!siteID  =='SJER') %>%
  dplyr::filter(!siteID  =='UNDE') %>%
  dplyr::filter(!siteID  =='BONA') %>%
  dplyr::filter(!siteID  =='DEJU') %>%
  dplyr::filter(!siteID  =='PUUM')

unique(inorganic_soil_lme$Lcclass)
length(unique(inorganic_soil_lme$siteID))

# lme functions lets you see P values in summary output
inorganic_soil_lme.1<-lme(inorganicN~ litterNPercent_mean + MAP, random= ~1|siteID,data=inorganic_soil_lme)
summary(inorganic_soil_lme.1) #nothing significant 
r.squaredGLMM(inorganic_soil_lme.1)


#-------------------------------------------------------------------------------
# % Root and % foliar N relationship --------------------------------------------------

merge_mean_foliar_root <- filter_reps(mean_foliar, mean_root)

length(merge_mean_foliar_root$siteID) 
# N = 20 sites

#round to two decimal points
merge_mean_foliar_root$rootNPercent<-round(merge_mean_foliar_root$rootNPercent,2)
merge_mean_foliar_root$foliarNPercent_mean<-round(merge_mean_foliar_root$foliarNPercent_mean,2)

length(merge_mean_foliar_root$siteID)

#add veg type
merge_mean_foliar_root <- merge(merge_mean_foliar_root,vegtype.df,by='siteID')
aggregate(siteID~Lcclass,length,data=merge_mean_foliar_root)

#take a loon
#plot(foliarNPercent_mean~rootNPercent,data=merge_mean_foliar_root)

#see if there are outliers
outlierTest(lm(foliarNPercent_mean~rootNPercent,data=merge_mean_foliar_root))
#no outliers

foliar_root_lm<-lm(foliarNPercent_mean~rootNPercent,data=merge_mean_foliar_root)
tab_model(foliar_root_lm)



#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
# ----------------Plant-Soil C:N relationships ------------------------------------
#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
