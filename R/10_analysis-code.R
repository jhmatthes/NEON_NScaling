# last updated: 2020-10-26
# author: Adrienne Keller
# project: NEON N scaling
# notes:

# get aridity and veg data, subset data into just veg and herb veg types -----

# get aridity data
vpd <- read.csv('./../data_pre-processed/scaled_vpd.csv')
head(vpd)

#cleanup
vpd<-vpd[c(2,3)]
colnames(vpd) <- c('siteID','vpd')

# merge with vpd data frame
plot.df <- merge(vpd,plot.df,by=c('siteID'))

#unique(plot.df$Lcclass)

#filter to just woody and herb (not NAs)
plot.df.2 <- plot.df %>%
  dplyr::filter(!Lcclass=='NA')
length(unique(plot.df.2$siteID)) # 35 sites
head(plot.df.2)
#unique(plot.df.2$Lcclass)

# Get mean values for each plot-site combination for each pool -----
mean_foliar<-aggregate(foliarNPercent_mean~siteID + plotID,mean,data=plot.df.2)
mean_root <- aggregate(rootNPercent ~ siteID + plotID, mean, data = plot.df.2)
mean_soil<-aggregate(soilNPercent_MHoriz_mean~siteID + plotID,mean,data=plot.df.2)
mean_soil_inorganic<-aggregate(inorganicN~siteID + plotID,mean,data=plot.df.2)
mean_soil_mineralization<-aggregate(netNminugPerGramPerDay~siteID + plotID,mean,data=plot.df.2)
mean_litter<-aggregate(litterNPercent_mean~siteID + plotID,mean,data=plot.df.2)
mean_resorp<-aggregate(resorpN~siteID + plotID,mean,data=plot.df.2)

# compare variability of pools within versus across sites ----------------------

# you'll have to run the previous to sections of code first

# % Foliar N
cross_site_foliar_sd<-round(sd(mean_site_foliar$`%Foliar`),2)

#average within-site foliar SD
# first filter out sites with less than 10 replicates
within_site_foliar_sd <- merge(mean_foliar,mean_site_foliar,by=c('siteID'))
within_site_foliar_sd <- within_site_foliar_sd %>%
  dplyr::filter(N > 9)
within_site_foliar_sd <- mean(sd(round(within_site_foliar_sd$foliarNPercent_mean),2))
within_site_foliar_sd<-round(within_site_foliar_sd,2)

scale <- c('within','across')
sd<-c(within_site_foliar_sd,cross_site_foliar_sd)
leaf_scale<-data.frame(sd,scale)
leaf_scale$pool <- 'foliar'

# % Root N
cross_site_root_sd<-round(sd(mean_site_root$`%Root`),2)

#average within-site root SD
within_site_root_sd <- merge(mean_root,mean_site_root,by=c('siteID'))
within_site_root_sd <- within_site_root_sd %>%
  dplyr::filter(N > 9)
within_site_root_sd <- mean(sd(round(within_site_root_sd$rootNPercent),2))
within_site_root_sd<-round(within_site_root_sd,2)

scale <- c('within','across')
sd<-c(within_site_root_sd,cross_site_root_sd)
root_scale<-data.frame(sd,scale)
root_scale$pool <- 'root'
  
# % Total soil N
cross_site_soil_sd<-round(sd(mean_site_soil$`%TotalSoil`),2)

#average within-site total soil N
within_site_soil_sd <- merge(mean_soil,mean_site_soil,by=c('siteID'))
within_site_soil_sd <- within_site_soil_sd %>%
  dplyr::filter(N > 9)
within_site_soil_sd <- mean(sd(round(within_site_soil_sd$soilNPercent_MHoriz_mean),2))
within_site_soil_sd<-round(within_site_soil_sd,2)

scale <- c('within','across')
sd<-c(within_site_soil_sd,cross_site_soil_sd)
soil_scale<-data.frame(sd,scale)
soil_scale$pool <- 'soil'
  
# % Inorganic soil N
cross_site_soil_inorganic_sd<-round(sd(mean_site_soil_inorganic$`%InorganicSoil`),2)

#average within-site inorganic soil N
within_site_soil_inorganic_sd <- merge(mean_soil_inorganic,mean_site_soil_inorganic,by=c('siteID'))
within_site_soil_inorganic_sd <- within_site_soil_inorganic_sd %>%
  dplyr::filter(N > 9)
within_site_soil_inorganic_sd <- mean(sd(round(within_site_soil_inorganic_sd$`%InorganicSoil`),2))
within_site_soil_inorganic_sd<-round(within_site_soil_inorganic_sd,2)

scale <- c('within','across')
sd<-c(within_site_soil_inorganic_sd,cross_site_soil_inorganic_sd)
soil_inorganic_scale<-data.frame(sd,scale)
soil_inorganic_scale$pool <- 'soil_inorganic'

# Combine the datasets
scale_sd<-rbind(soil_inorganic_scale,soil_scale,root_scale,leaf_scale)

pdf(file='./../output/analyses_April_2021/within_versus_across_variation.pdf',
    width=8,height=8)

#plot it out
ggplot(scale_sd,aes(reorder(pool,-sd),sd,fill=scale)) +
  geom_bar(position="dodge", stat="identity") +
  xlab('Pool') +
  ylab('Standard deviation')

dev.off()

  
#-------------------------------------------------------------------------------
# foliar and total soil N bivariate relationship ----

#make data frame where each comination has at least 4 reps per site
merge_foliar_soil_means <-filter_reps(mean_foliar, mean_soil)

length(merge_foliar_soil_means$siteID) 
#23 with the limit as 4

#round to 2 decimal points
merge_foliar_soil_means$foliarNPercent_mean<-round(merge_foliar_soil_means$foliarNPercent_mean,2)
merge_foliar_soil_means$soilNPercent_MHoriz_mean<-round(merge_foliar_soil_means$soilNPercent_MHoriz_mean,2)

#add veg type
merge_foliar_soil_means <- merge(merge_foliar_soil_means,vegtype.df,by='siteID')
aggregate(siteID~Lcclass,length,data=merge_foliar_soil_means)

#look at  outliers
outlierTest(lm(foliarNPercent_mean ~ soilNPercent_MHoriz_mean,data=merge_foliar_soil_means))
#no outliers

#take a look
#plot(foliarNPercent_mean ~ soilNPercent_MHoriz_mean,data=merge_foliar_soil_means)

#look at LM
foliar_soil_total_lm<-lm(foliarNPercent_mean ~ soilNPercent_MHoriz_mean,data=merge_foliar_soil_means)
summary(foliar_soil_total_lm)
#NS

#-------------------------------------------------------------------------------
# root and total soil N -----

#make data frame where each combination has at least 4 reps per site
merge_soil_root <- filter_reps(mean_soil,mean_root)

#count to two decimal points
merge_soil_root$rootNPercent <- round(merge_soil_root$rootNPercent,2)
merge_soil_root$soilNPercent_MHoriz_mean <- round(merge_soil_root$soilNPercent_MHoriz_mean,2)

length(merge_soil_root$siteID) 
#22 sites with the 4 rep limit

#add veg type
merge_soil_root <- merge(merge_soil_root,vegtype.df,by='siteID')
aggregate(siteID~Lcclass,length,data=merge_soil_root)

# take a look
#plot(rootNPercent~soilNPercent_MHoriz_mean,data=merge_soil_root)
outlierTest(lm(rootNPercent~soilNPercent_MHoriz_mean,data=merge_soil_root))
#one outlier

root_soil_total_lm<-lm(rootNPercent~soilNPercent_MHoriz_mean,data=merge_soil_root[-18,]) 
summary(root_soil_total_lm)
#NS, but close

#-------------------------------------------------------------------------------
# inorganic N and leaf N ------------

merge_foliar_soil_inorganic <- filter_reps(mean_foliar, mean_soil_inorganic)

length(merge_foliar_soil_inorganic$siteID) 
# 21 sites with the 4 rep thresholds

#round to two decimal points
merge_foliar_soil_inorganic$foliarNPercent_mean <- round(merge_foliar_soil_inorganic$foliarNPercent_mean,2)
merge_foliar_soil_inorganic$inorganicN<- round(merge_foliar_soil_inorganic$inorganicN,2)

#add veg type
merge_foliar_soil_inorganic <- merge(merge_foliar_soil_inorganic,vegtype.df,by='siteID')
aggregate(siteID~Lcclass,length,data=merge_foliar_soil_inorganic)

#take a look
# outlierTest(lm(foliarNPercent_mean~inorganicN,data=merge_foliar_soil_inorganic))
#no outliers

leaf_soil_inorganic_lm<-lm(foliarNPercent_mean~inorganicN,data=merge_foliar_soil_inorganic)
summary(leaf_soil_inorganic_lm)

#NS

#-------------------------------------------------------------------------------

# root N and  soil inorganic N ------

merge_soil_root_inorganic <- filter_reps(mean_soil_inorganic, mean_root)

length(merge_soil_root_inorganic$siteID) 
# 22 sites with minimum of 4 reps

#round to two decimal points
merge_soil_root_inorganic$rootNPercent<-round(merge_soil_root_inorganic$rootNPercent,2)
merge_soil_root_inorganic$inorganicN<-round(merge_soil_root_inorganic$inorganicN,2)

#add veg type
merge_soil_root_inorganic <- merge(merge_soil_root_inorganic,vegtype.df,by='siteID')
aggregate(siteID~Lcclass,length,data=merge_soil_root_inorganic)

#take a look
#outlierTest(lm(rootNPercent~inorganicN,data=merge_soil_root_inorganic))
#no outliers

root_soil_inorganic_lm<-lm(rootNPercent~inorganicN,data=merge_soil_root_inorganic)
summary(root_soil_inorganic_lm)
tab_model(root_soil_inorganic_lm)
#moderately significant 

#-------------------------------------------------------------------------------
# Mixed effects models of soil effects on plant N using inorganic soil N ---------------------------------------------------------

# since we have established inorganic N is 'better', 
# we will use this in the % N models models to represent soil N

foliar_lme <- select(plot.df.2,c('siteID','vpd','foliarNPercent_mean','inorganicN','Lcclass'))
head(foliar_lme)

foliar_lme <- foliar_lme %>%
  dplyr::filter(!foliarNPercent_mean=='NA') %>%
  dplyr::filter(!inorganicN =='NA') 

#check sample sizes
length_foliar_lme<-aggregate(foliarNPercent_mean~siteID,length,data=foliar_lme)
length_inorganic_lme<-aggregate(inorganicN~siteID,length,data=foliar_lme)

#get rid of WREF

#remove sites with less than 4 replicates
foliar_lme <- foliar_lme %>%
  dplyr::filter(!siteID  =='WREF')

#check
herb.count<-subset(foliar_lme,Lcclass=='herb')
length(herb.count$siteID) #59 obs
unique(herb.count$siteID) #6 herb sites

#
woody.count<-subset(foliar_lme,Lcclass='woody')
length(woody.count$siteID)
unique(woody.count$siteID)

# lme functions (lme lets you see P values in summary output)
leaf_lme.1<-lme(foliarNPercent_mean~ inorganicN + vpd + Lcclass, random= ~1|siteID,data=foliar_lme)
summary(leaf_lme.1) # inorganic N shows as significant
r.squaredGLMM(leaf_lme.1)


#
#

# Do mixed effects analysis for root N, same work flow

root_lme <- select(plot.df.2,c('siteID','vpd','rootNPercent','inorganicN','Lcclass'))
#head(root_lme)

root_lme <- root_lme %>%
  dplyr::filter(!rootNPercent=='NA') %>%
  dplyr::filter(!inorganicN =='NA') 

#check sample sizes
length_root_lme<-aggregate(rootNPercent~siteID,length,data=root_lme)
length_inorganic_lme<-aggregate(inorganicN~siteID,length,data=root_lme)

#check
herb.count<-subset(root_lme,Lcclass=='herb')
length(herb.count$siteID) #28 obs
unique(herb.count$siteID) #6 herb sites

#
woody.count<-subset(root_lme,Lcclass='woody')
length(woody.count$siteID)
unique(woody.count$siteID)

#now do lmes
root_lme.1<-lme(rootNPercent~ inorganicN + vpd + Lcclass , random= ~1|siteID,data=root_lme)
summary(root_lme.1) # inorganic N barely significant in this case
r.squaredGLMM(root_lme.1)


#-------------------------------------------------------------------------------

#plant feedbacks to soil  inorganic N --------------------

merge_litter_soil_inorganic <- filter_reps(mean_litter, mean_soil_inorganic)

length(merge_litter_soil_inorganic$siteID) 
# 12 sites with the 4 rep thresholds

merge_litter_soil_inorganic <- merge(merge_litter_soil_inorganic,vegtype.df,by='siteID')

# round to two decimal points
merge_litter_soil_inorganic$litterNPercent_mean <- round(merge_litter_soil_inorganic$litterNPercent_mean,2)
merge_litter_soil_inorganic$inorganicN <- round(merge_litter_soil_inorganic$inorganicN,2)

# take a look
outlierTest(lm(inorganicN~litterNPercent_mean,data=merge_litter_soil_inorganic))

#remove outlier
#plot(inorganicN~litterNPercent_mean,data=merge_litter_soil_inorganic[-6,])
summary(lm(inorganicN~litterNPercent_mean,data=merge_litter_soil_inorganic[-6,]))
tab_model(lm(inorganicN~litterNPercent_mean,data=merge_litter_soil_inorganic[-6,]))
#significant

##


# Resorption and inorganic soil N
mean_resorp_inorganic_soil <- filter_reps(mean_resorp, mean_soil_inorganic)

#compare woody versus herb representation
mean_resorp_inorganic_soil <- merge(mean_resorp_inorganic_soil,vegtype.df,by='siteID')

# round to two decimal points
mean_resorp_inorganic_soil$resorpN <- round(mean_resorp_inorganic_soil$resorpN,2)
mean_resorp_inorganic_soil$inorganicN <- round(mean_resorp_inorganic_soil$inorganicN,2)

# take a look
plot(inorganicN~resorpN,data=mean_resorp_inorganic_soil) # no relationship
outlierTest(lm(inorganicN~resorpN,data=mean_resorp_inorganic_soil))

#try without outlier
plot(inorganicN~resorpN,data=mean_resorp_inorganic_soil[-5,])
summary(lm(inorganicN~resorpN,data=mean_resorp_inorganic_soil[-5,]))

#not significant 

#-------------------------------------------------------------------------------
# plant feedbacks to total soil N ------

merge_litter_soil_total <- filter_reps(mean_litter, mean_soil)

length(merge_litter_soil_total$siteID) 
# 13 sites with the 4 rep thresholds

merge_litter_soil_total <- merge(merge_litter_soil_total,vegtype.df,by='siteID')

# take a look
outlierTest(lm(soilNPercent_MHoriz_mean~litterNPercent_mean,data=merge_litter_soil_total))
#no outlier

#remove outlier
#plot(soilNPercent_MHoriz_mean~litterNPercent_mean,data=merge_litter_soil_total)
summary(lm(soilNPercent_MHoriz_mean~litterNPercent_mean,data=merge_litter_soil_total))

#not significant 

##

# Resorption and total soil N
mean_resorp_total_soil <- filter_reps(mean_resorp, mean_soil)

#compare woody versus herb representation
mean_resorp_total_soil <- merge(mean_resorp_total_soil,vegtype.df,by='siteID')

# round to two decimal points
mean_resorp_total_soil$resorpN <- round(mean_resorp_total_soil$resorpN,2)
mean_resorp_total_soil$soilNPercent_MHoriz_mean <- round(mean_resorp_total_soil$soilNPercent_MHoriz_mean,2)

# take a look
#plot(soilNPercent_MHoriz_mean~resorpN,data=mean_resorp_total_soil) # no relationship
outlierTest(lm(soilNPercent_MHoriz_mean~resorpN,data=mean_resorp_total_soil))
#no outliers

summary(lm(soilNPercent_MHoriz_mean~resorpN,data=mean_resorp_total_soil))

#not significant 

#-------------------------------------------------------------------------------
# mixed effects models for plant effects on soil N ---------

# get aridity data
# vpd <- read.csv('./../data_pre-processed/scaled_vpd.csv')
# head(vpd)
# #cleanup
# vpd<-vpd[c(2,3)]
# colnames(vpd) <- c('siteID','vpd')

# look at both total and inroganic soil N

# total soil N

mean_total_soil_lme<-aggregate(soilNPercent_MHoriz_mean~siteID + plotID
                           + litterNPercent_mean + resorpN + Lcclass,mean,data=plot.df)
#head(mean_foliar_lme)
unique(mean_total_soil_lme$Lcclass)

#rename to veg type to herb versus woody. 'Croplands' are deemed herbaceous
mean_total_soil_lme <- rename_lcc(mean_total_soil_lme,crop = T)
# head(mean_foliar_lme)
# unique(mean_foliar_lme$Lcclass)

# merge with vpd data frame
mean_total_soil_lme <- merge(vpd,mean_total_soil_lme,by=c('siteID'))
#head(mean_foliar_lme)

#check sample sizes
length_mean_total_soil_lme <-aggregate(soilNPercent_MHoriz_mean~siteID,length,data=mean_total_soil_lme)

#remove site with only one two reps (less than 4 reps. This does not leave much to work with...)
mean_total_soil_lme <- mean_total_soil_lme %>%
  dplyr::filter(!(siteID=="SCBI")) %>%
  dplyr::filter(!(siteID=="UNDE")) 
unique(mean_total_soil_lme$siteID) # works
# N=5 sites

# lme functions lets you see P values in summary output
total_soil_lme.1<-lme(soilNPercent_MHoriz_mean~ litterNPercent_mean + resorpN  + Lcclass + vpd, random= ~1|siteID,data=mean_total_soil_lme)
summary(total_soil_lme.1) #vpd is slightly significant
r.squaredGLMM(total_soil_lme.1)


# R2m       R2c
# [1,] 0.8213875 0.8213875

#notes

# vpd shows up as a significant main effect. no variance attributable to random effects.

#inorganic soil N

mean_inorganic_soil_lme<-aggregate(inorganicN ~ plotID +siteID
                               + litterNPercent_mean + resorpN + Lcclass,mean,data=plot.df)
#head(plot.df)
unique(mean_inorganic_soil_lme$Lcclass)

#rename to veg type to herb versus woody. 'Croplands' are deemed herbaceous
mean_inorganic_soil_lme <- rename_lcc(mean_inorganic_soil_lme,crop = T)
# head(mean_inorganic_soil_lme)
# unique(mean_inorganic_soil_lme$Lcclass)

# get aridity data
# vpd <- read.csv('./../data_pre-processed/scaled_vpd.csv')
# head(vpd)

#cleanup
# vpd<-vpd[c(2,3)]
# colnames(vpd) <- c('siteID','vpd')

# merge with vpd data frame
mean_inorganic_soil_lme <- merge(vpd,mean_inorganic_soil_lme,by=c('siteID'))
#head(mean_foliar_lme)

#check sample sizes
length_mean_inorganic_soil_lme <-aggregate(inorganicN~siteID,length,data=mean_inorganic_soil_lme)

#remove site with only one two reps (less than 4)
mean_inorganic_soil_lme <- mean_inorganic_soil_lme %>%
  dplyr::filter(!(siteID=="SCBI")) %>%
  dplyr::filter(!(siteID=="UNDE"))
unique(mean_inorganic_soil_lme$siteID) # works
# N = 5 sites

# lme functions lets you see P values in summary output
inorganic_soil_lme.1<-lme(inorganicN~ litterNPercent_mean + resorpN  + Lcclass + vpd, random= ~1|siteID,data=mean_inorganic_soil_lme)
summary(inorganic_soil_lme.1) #nothing significant 
r.squaredGLMM(inorganic_soil_lme.1)

# R2m       R2c
# [1,] 0.07002065 0.9835765

#notes: interesting that when using inroganic N, all variance goes into random effects

#done


#-------------------------------------------------------------------------------
# mineralization-leaf N relationships ------

# merge foliar and mineralization data by plot ID
merge_mean_foliar_soil_mineralization <- filter_reps(mean_foliar, mean_soil_mineralization)

#round to two decimal points
merge_mean_foliar_soil_mineralization$foliarNPercent_mean<-round(merge_mean_foliar_soil_mineralization$foliarNPercent_mean,2)
merge_mean_foliar_soil_mineralization$netNminugPerGramPerDay<-round(merge_mean_foliar_soil_mineralization$netNminugPerGramPerDay,2)
  
# take a look  
plot(foliarNPercent_mean~netNminugPerGramPerDay,data=merge_mean_foliar_soil_mineralization)
outlierTest(lm(foliarNPercent_mean~netNminugPerGramPerDay,data=merge_mean_foliar_soil_mineralization))
#no outliers
summary(lm(foliarNPercent_mean~netNminugPerGramPerDay,data=merge_mean_foliar_soil_mineralization))

# Adjusted R-squared:  0.1092
# p-value: 0.05915
  
#-------------------------------------------------------------------------------
# mineralization-root relationships ------

# merge root and soil data by plot ID
merge_mean_mineralization_root <- filter_reps(mean_soil_mineralization, mean_root)

length(merge_mean_mineralization_root$siteID) 
# N = 23

#take a look
#plot(rootNPercent~netNminugPerGramPerDay,data=merge_mean_mineralization_root)
outlierTest(lm(rootNPercent~netNminugPerGramPerDay,data=merge_mean_mineralization_root))
# no outliers
summary(lm(rootNPercent~netNminugPerGramPerDay,data=merge_mean_mineralization_root))

# p-value: 0.6851
# Adjusted R-squared:  -0.03925

#-------------------------------------------------------------------------------
# Root and foliar N relationship --------------------------------------------------

merge_mean_foliar_root <- filter_reps(mean_foliar, mean_root)

length(merge_mean_foliar_root$siteID) 
# N = 24 sites

#plot(foliarNPercent_mean~rootNPercent,data=merge_mean_foliar_root)

#see if there are outliers
outlierTest(lm(foliarNPercent_mean~rootNPercent,data=merge_mean_foliar_root))
#no outliers
summary(lm(foliarNPercent_mean~rootNPercent,data=merge_mean_foliar_root))
# Adjusted R-squared:  0.2987
# p-value: 0.003376


# Get a sense if a linear or nonlinear fit is better
root_leaf_linear<-lm(foliarNPercent_mean~rootNPercent,data=merge_mean_foliar_root)
#summary(root_leaf_linear) #significant R-square = 0.15

pdf(file='./../output/bivar_root_leaf.pdf',
    width=6,height=6)

layout(matrix(1:1, ncol=1))
par(oma=c(6, 5, 6, 5), mar=c(0, 0, 0, 0),pty='s')

# merge root and foliar N
#mean_foliar_root <- merge(mean_soil_root_2, mean_foliar_soil_2, by = c('siteID'))
plot(foliarNPercent_mean~rootNPercent,xlab='',ylab='', data=merge_mean_foliar_root,cex=1.25)
mtext('% Leaf N',side=2,line=3,cex=1.5)
mtext('% Root N',side=1,line=3,cex=1.5,outer=T)
abline(root_leaf_linear, col="red",lwd=2)
text(1.2, 01.5, 'R-squared = 0.30',cex=1)

dev.off()
