# last updated: 2020-10-26
# author: Adrienne Keller
# project: NEON N scaling
# notes:
# Add veg type and VPD to the main data set -----

head(plot.df)
length(unique(plot.df$siteID))# 38
length(unique(vegtype.df$siteID)) #33

# get aridity data
vpd <- read.csv('./../data_pre-processed/scaled_vpd.csv')
head(vpd)

#cleanup
vpd<-vpd[c(2,3)]
colnames(vpd) <- c('siteID','vpd')

#simplify to woody versus herb vegetation types
plot.df <- rename_lcc(plot.df,crop = T)

# merge with vpd data frame
plot.df <- merge(vpd,plot.df,by=c('siteID'))

#filter to just woody and herb (not NAs)
plot.df.2 <- plot.df %>%
  #group_by(Lcclass) %>%
  dplyr::filter(!Lcclass=='NA')
unique(plot.df.2$Lcclass)

# Distributions of N pools -------------------------------------------
pdf(file='./../output/univar-hist.pdf',
    width=8,height=8)
# mar.default <- c(6,3,5,2) + 0.1
# par(mar = mar.default + c(2, 2, 0, 0),mfrow=c(1,4))

# Set up multi-panel
layout(matrix(1:4, ncol=2))
par(oma=c(6, 5, 6, 5), mar=c(5, 0, 0, 0),pty='s')
#?par
# Panel label setup
line = 0.75 
cex = 1.25
side = 3
adj= - 0.15

# A
hist(plot.df$soilNPercent_MHoriz_mean,main='',xlab='',ylab='',cex.lab=1.75)
abline(v = mean(plot.df$soilNPercent_MHoriz_mean, na.rm = T), col = "red", lwd = 3)
mtext('% Total soil N (mineral horizon)',side=1,line=2.2,cex=0.75)
mtext("A", side=side, line=line, cex=cex, adj=adj)

# C
hist(plot.df$foliarNPercent_mean,main='',xlab='',ylab='',cex.lab=1.75)
abline(v = mean(plot.df$foliarNPercent_mean, na.rm = T), col = "red", lwd = 3)
mtext('% Foliar N',side=1,line=2.2,cex=0.75)
mtext("C", side=side, line=line, cex=cex, adj=adj)

# B
hist(plot.df$inorganicN,main='',xlab='',ylab='',cex.lab=1.75)
abline(v = mean(plot.df$litterNPercent, na.rm = T), col = "red", lwd = 3)
mtext('% Inorganic soil N',side=1,line=2.2,cex=0.75)
mtext("B", side=side, line=line, cex=cex, adj=adj)


# D
hist(plot.df$rootNPercent, main = '', xlab = "", ylab='',cex.lab = 1.75)
abline(v = mean(plot.df$rootNPercent, na.rm = T), col = "red", lwd = 3)
mtext('% Root N',side=1,line=2.2,cex=0.75)
mtext("D", side=side, line=line, cex=cex, adj=adj)
mtext('Frequency',side=2,line=0.75,cex=1.5,outer=TRUE)

dev.off()

# note: soil N mineral horizon is heavily right-skewed (even when excluding GUAN);
# log-transform
# hist(log(plot.df$soilNPercent_MHoriz_mean))
# plot.df %>% filter(siteID != "GUAN") %>% 
#   ggplot() +
#   geom_histogram(aes(log(soilNPercent_MHoriz_mean), fill = siteID))

# Distribution of N pools by vegetation type----


#filter to just woody and herb (not NAs)
plot.df.2 <- plot.df %>%
  #group_by(Lcclass) %>%
  dplyr::filter(!Lcclass=='NA')
unique(plot.df.2$Lcclass)

#plot them

pdf(file='./../output/analyses_April_2021/veg_foliarN_distrobutions.pdf',
    width=8,height=8)

#ggplot for foliar N
ggplot(plot.df.2,aes(foliarNPercent_mean,fill=Lcclass),na.rm=TRUE) +
  geom_histogram(color='black') +
  ylab('Count') + 
  xlab('% Foliar N')

dev.off()

pdf(file='./../output/analyses_April_2021/veg_rootN_distrobutions.pdf',
    width=8,height=8)

#ggplot for root N
ggplot(plot.df.2,aes(rootNPercent,fill=Lcclass),na.rm=TRUE) +
  geom_histogram(color='black') +
  ylab('Count') + 
  xlab('% Root N')

dev.off()

#ggplot for total soil N
ggplot(plot.df.2,aes(soilNPercent_MHoriz_mean,fill=Lcclass),na.rm=TRUE) +
  geom_histogram(color='black') +
  ylab('Count') + 
  xlab('% Total soil N')

pdf(file='./../output/analyses_April_2021/veg_litterN_distrobutions.pdf',
    width=8,height=8)

#ggplot for litter N
ggplot(plot.df.2,aes(litterNPercent_mean,fill=Lcclass),na.rm=TRUE) +
  geom_histogram(color='black') +
  ylab('Count') + 
  xlab('% Litter N')

dev.off()

pdf(file='./../output/analyses_April_2021/veg_inorganicN_distrobutions.pdf',
    width=8,height=8)

#inorganic soil N
ggplot(plot.df.2,aes(inorganicN ,fill=Lcclass),na.rm=TRUE) +
  geom_histogram(color='black') +
  ylab('Count') + 
  xlab('% Inorganic soil N')

dev.off()

# relationship between VPD and N pools for each vegetation type-----
head(plot.df.2)
plot(foliarNPercent_mean~vpd,data=plot.df.2)

#VPD and % foliar N veg
summary(lm(foliarNPercent_mean~vpd*Lcclass,data=plot.df,na.rm=TRUE))
# significant interaction betwen veg and VPD effects on % leaf N
# negative effects for herb, no effects for woody

pdf(file='./../output/analyses_April_2021/foliar_vpd_veg.pdf',
    width=8,height=8)

ggplot(plot.df.2,aes(vpd,foliarNPercent_mean,color=Lcclass),na.rm=TRUE) +
  geom_point() +
  stat_smooth(method = "lm") +
  ylab('% Foliar N') + 
  xlab('Mean 30-year VPD')

dev.off()

#VPD and % root N veg
summary(lm(rootNPercent~vpd*Lcclass,data=plot.df,na.rm=TRUE))
#positive for herb nothing for woody, weak effects all around

pdf(file='./../output/analyses_April_2021/root_vpd_veg.pdf',
    width=8,height=8)

ggplot(plot.df.2,aes(vpd,rootNPercent ,color=Lcclass),na.rm=TRUE) +
  geom_point() +
  stat_smooth(method = "lm") +
  ylab('% Root N') + 
  xlab('Mean 30-year VPD')

dev.off()

# VPD and % Litter N

pdf(file='./../output/analyses_April_2021/litter_vpd_veg.pdf',
    width=8,height=8)

ggplot(plot.df.2,aes(vpd,litterNPercent_mean ,color=Lcclass),na.rm=TRUE) +
  geom_point() +
  stat_smooth(method = "lm") +
  ylab('% Litter N') + 
  xlab('Mean 30-year VPD')

dev.off()

# VPD and % Litter N

pdf(file='./../output/analyses_April_2021/soil_N_vpd_veg.pdf',
    width=8,height=8)

ggplot(plot.df.2,aes(vpd,soilNPercent_MHoriz_mean,color=Lcclass),na.rm=TRUE) +
  geom_point() +
  stat_smooth(method = "lm") +
  ylab('% Total soil N') + 
  xlab('Mean 30-year VPD')

dev.off()

#VPD inorganic soil N

pdf(file='./../output/analyses_April_2021/inroganic_N_vpd_veg.pdf',
    width=8,height=8)

ggplot(plot.df.2,aes(vpd,inorganicN,color=Lcclass),na.rm=TRUE) +
  geom_point() +
  stat_smooth(method = "lm") +
  ylab('% Inorganic soil N') + 
  xlab('Mean 30-year VPD')

dev.off()

# Bivariate relationships between N pools - cross-site with each point = site mean (Figure 3) ----

# first do this for total soil N (organic + inorganic)

# Get sample sizes 
sample_size_foliar<-aggregate(foliarNPercent_mean~siteID,length,data=plot.df)
sample_size_litter<-aggregate(litterNPercent_mean~siteID,length,data=plot.df) # KONZ only 4
sample_size_soil<-aggregate(soilNPercent_MHoriz_mean~siteID,length,data=plot.df) # HEAL only 1
sample_size_root <- aggregate(rootNPercent ~ siteID, length, data = plot.df)
sample_size_soil_inorganic <- aggregate(inorganicN ~ siteID, length, data = plot.df)

# HEAL has sample size of 1 for total soil N, this gets removed from analysis/merging
# anyways because there are no data from any of the other three pools

# Get mean values for each plot-site combination for each pool -----
mean_foliar<-aggregate(foliarNPercent_mean~siteID + plotID,mean,data=plot.df)
mean_root <- aggregate(rootNPercent ~ siteID + plotID, mean, data = plot.df)
mean_soil<-aggregate(soilNPercent_MHoriz_mean~siteID + plotID,mean,data=plot.df)
mean_soil_inorganic<-aggregate(inorganicN~siteID + plotID,mean,data=plot.df)
mean_soil_mineralization<-aggregate(netNminugPerGramPerDay~siteID + plotID,mean,data=plot.df)

#-------------------------------------------------------------------------------
# make a table of sites, mean values, and number of replicates -----

#foliar
mean_site_foliar <-aggregate(foliarNPercent_mean~siteID,mean,data=plot.df)
mean_site_foliar$Foliar<-round(mean_site_foliar$foliarNPercent_mean,2)
mean_site_foliar <- mean_site_foliar[c(1,3)]
mean_site_foliar<-merge(sample_size_foliar,mean_site_foliar ,by=c('siteID'))
colnames(mean_site_foliar) <-c('siteID','N','%Foliar')

#root
mean_site_root <-aggregate(rootNPercent~siteID,mean,data=plot.df)
mean_site_root$Root<-round(mean_site_root$rootNPercent,2)
mean_site_root <- mean_site_root[c(1,3)]
mean_site_root <-merge(sample_size_root,mean_site_root,by=c('siteID'))
colnames(mean_site_root) <-c('siteID','N','%Root')

#total soil N
mean_site_soil <-aggregate(soilNPercent_MHoriz_mean~siteID,mean,data=plot.df)
mean_site_soil$TotalSoil<-round(mean_site_soil$soilNPercent_MHoriz_mean,2)
mean_site_soil  <- mean_site_soil[c(1,3)]
mean_site_soil  <-merge(sample_size_soil,mean_site_soil ,by=c('siteID'))
colnames(mean_site_soil) <-c('siteID','N','%TotalSoil')

#inorganic soil N
mean_site_soil_inorganic <-aggregate(inorganicN~siteID,mean,data=plot.df)
mean_site_soil_inorganic$InorganicSoil<-round(mean_site_soil_inorganic$inorganicN,2)
mean_site_soil_inorganic  <- mean_site_soil_inorganic[c(1,3)]
mean_site_soil_inorganic  <-merge(sample_size_soil_inorganic,mean_site_soil_inorganic,by=c('siteID'))
colnames(mean_site_soil_inorganic) <-c('siteID','N','%InorganicSoil')

#root and leaf merge
root_leaf<- left_join(mean_site_root,mean_site_foliar,by=c('siteID'),na.rm=F)
soil_soil<-left_join(mean_site_soil,mean_site_soil_inorganic ,by=c('siteID'),na.rm=F)

#join all and save as table
# root_leaf_soil<-left_join(soil_soil,root_leaf,by=c('siteID'),na.rm=F)
# write.csv(root_leaf_soil,file='./../output/means_replicates_N_Pools.csv')

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
#29 with the limit as 4

#round to 2 decimal points
merge_foliar_soil_means$foliarNPercent_mean<-round(merge_foliar_soil_means$foliarNPercent_mean,2)
merge_foliar_soil_means$soilNPercent_MHoriz_mean<-round(merge_foliar_soil_means$soilNPercent_MHoriz_mean,2)

#look at  outliers
outlierTest(lm(foliarNPercent_mean ~ soilNPercent_MHoriz_mean,data=merge_foliar_soil_means))
#no outliers

#take a look
plot(foliarNPercent_mean ~ soilNPercent_MHoriz_mean,data=merge_foliar_soil_means)

#look at LM
foliar_soil_total_lm<-lm(foliarNPercent_mean ~ soilNPercent_MHoriz_mean,data=merge_foliar_soil_means)
tab_model(foliar_soil_total_lm)
#Adjusted R-squared:  -0.01506
#p-value: 0.4512

#-------------------------------------------------------------------------------
# root and total soil N -----

#make data frame where each combination has at least 4 reps per site
merge_soil_root <- filter_reps(mean_soil,mean_root)

#count to two decimal points
merge_soil_root$rootNPercent <- round(merge_soil_root$rootNPercent,2)
merge_soil_root$soilNPercent_MHoriz_mean <- round(merge_soil_root$soilNPercent_MHoriz_mean,2)

length(merge_soil_root$siteID) 
#26 sites with the 4 rep limit

# take a look
#plot(rootNPercent~soilNPercent_MHoriz_mean,data=merge_soil_root)
outlierTest(lm(rootNPercent~soilNPercent_MHoriz_mean,data=merge_soil_root))
#one outlier 

#remove outlier
#plot(rootNPercent~soilNPercent_MHoriz_mean,data=merge_soil_root[-21,])
root_soil_total_lm<-lm(rootNPercent~soilNPercent_MHoriz_mean,data=merge_soil_root[-21,])
tab_model(root_soil_total_lm)
#Adjusted R-squared:  0.04297 
#p-value: 0.163

#done
#-------------------------------------------------------------------------------
# inorganic N and leaf N ------------

merge_foliar_soil_inorganic <- filter_reps(mean_foliar, mean_soil_inorganic)

length(merge_foliar_soil_inorganic$siteID) 
# 27 sites with the 4 rep thresholds

#round to two decimal points
merge_foliar_soil_inorganic$foliarNPercent_mean <- round(merge_foliar_soil_inorganic$foliarNPercent_mean,2)
merge_foliar_soil_inorganic$inorganicN<- round(merge_foliar_soil_inorganic$inorganicN,2)

#take a look
# plot(foliarNPercent_mean~inorganicN,data=merge_foliar_soil_inorganic)
# outlierTest(lm(foliarNPercent_mean~inorganicN,data=merge_foliar_soil_inorganic))
#no outliers

leaf_soil_inorganic_lm<-lm(foliarNPercent_mean~inorganicN,data=merge_foliar_soil_inorganic)
tab_model(leaf_soil_inorganic_lm)

#Adjusted R-squared:  0.05556
#p-value: 0.1243

# done
#-------------------------------------------------------------------------------

# root N and  soil inorganic N ------

merge_soil_root_inorganic <- filter_reps(mean_soil_inorganic, mean_root)

length(merge_soil_root_inorganic$siteID) 
# 26 sites with minimum of 4 reps

#round to two decimal points
merge_soil_root_inorganic$rootNPercent<-round(merge_soil_root_inorganic$rootNPercent,2)
merge_soil_root_inorganic$inorganicN<-round(merge_soil_root_inorganic$inorganicN,2)

#take a look
#plot(rootNPercent~inorganicN,data=merge_soil_root_inorganic)
#outlierTest(lm(rootNPercent~inorganicN,data=merge_soil_root_inorganic))
#no outliers

root_soil_inorganic_lm<-lm(rootNPercent~inorganicN,data=merge_soil_root_inorganic)
tab_model(root_soil_inorganic_lm)
# p-value: 0.02617
# Adjusted R-squared:  0.1559 

#give this ID for plotting in the multi-panel figure
sensitivity.soil.root.lm<-lm(rootNPercent~inorganicN,data=merge_soil_root_inorganic)

#done

#-------------------------------------------------------------------------------
# plot comparison of total versus total soil N relationships -----

pdf(file='./../output/bivar_soil_leaf_root_four_panel.pdf',
    width=8,height=8)
# mar.default <- c(6,3,5,2) + 0.1
# par(mar = mar.default + c(2, 2, 0, 0),mfrow=c(1,4))


layout(matrix(1:4, ncol=2))
par(oma=c(6, 5, 6, 5), mar=c(4, 0, 0, 0),pty='s')

# Panel label setup
line = 0.75 
cex = 1.0
side = 3
adj= - 0.1

# A: total soil to root N
plot(rootNPercent~soilNPercent_MHoriz_mean,xlab='',ylab="",data=merge_soil_root[-21,],cex=1.25)
mtext('% Root N',side=2,line=2.25,cex=1.0)
mtext("A", side=side, line=line, cex=cex, adj=adj)
#mtext('% Total soil N (M Horizon)',side=1,line=2.25,cex=1.0)
#text(1, 0.75, 'N.S',cex=1)

# C: total soil leaf N
plot(foliarNPercent_mean ~ soilNPercent_MHoriz_mean,xlab='',ylab="",data=merge_foliar_soil_means,cex=1.25)
mtext("B", side=side, line=line, cex=cex, adj=adj)
mtext('% Leaf N',side=2,line=2.25,cex=1.0)
mtext('% Total soil N (M Horizon)',side=1,line=2.25,cex=1.0)
#text(1, 1, 'N.S',cex=1)

# B: inorganic soil to root N
plot(rootNPercent~inorganicN,xlab='',ylab="",data=merge_soil_root_inorganic,cex=1.25)
#mtext('% Root N',side=2,line=2.25,cex=1.0)
abline(sensitivity.soil.root.lm, col="red",lwd=2)
mtext("C", side=side, line=line, cex=cex, adj=adj)
text(1, 0.62, 'R-squared = 0.16',cex=1)

# D: inorganic soil leaf N
plot(foliarNPercent_mean ~ inorganicN,xlab='',ylab="",data=merge_foliar_soil_inorganic,cex=1.25)
mtext("D", side=side, line=line, cex=cex, adj=adj)
mtext('% Inorganic soil N (M Horizon)',side=1,line=2.25,cex=1.0)
#text(1, 1, 'N.S',cex=1)


dev.off()

#-------------------------------------------------------------------------------
# Mixed effects models of soil effects on plant N using inorganic soil N ---------------------------------------------------------

# since we have established inorganic N is 'better', 
# we will use this in the % N models models to represent soil N

head(plot.df)

# get aridity data
vpd <- read.csv('./../data_pre-processed/scaled_vpd.csv')
head(vpd)

#cleanup
vpd<-vpd[c(2,3)]
colnames(vpd) <- c('siteID','vpd')

# Do mixed effects analysis for leaf N

mean_foliar_lme<-aggregate(foliarNPercent_mean~siteID + plotID
                            + inorganicN + Lcclass,mean,data=plot.df)
#head(mean_foliar_lme)

#rename to veg type to herb versus woody. 'Croplands' are deemed herbaceous but
# ultimately don't show up so doesn't matter
mean_foliar_lme <- rename_lcc(mean_foliar_lme,crop = T)
# head(mean_foliar_lme)
# unique(mean_foliar_lme$Lcclass)

# merge with vpd data frame
mean_foliar_lme <- merge(vpd,mean_foliar_lme,by=c('siteID'))
#head(mean_foliar_lme)

#check sample sizes
length_foliar_root_lme<-aggregate(foliarNPercent_mean~siteID,length,data=mean_foliar_lme)

#remove site with only one rep
mean_foliar_lme <- mean_foliar_lme %>%
  dplyr::filter(!(siteID=="WREF"))
unique(mean_foliar_lme$siteID) # works

# lme functions (lme lets you see P values in summary output)
leaf_lme.1<-lme(foliarNPercent_mean~ inorganicN + vpd + Lcclass, random= ~1|siteID,data=mean_foliar_lme)
summary(leaf_lme.1) # inorganic N shows as significant
r.squaredGLMM(leaf_lme.1)

# R2m       R2c
# [1,] 0.4137367 0.6859931

#0.69 - 0.41

#notes 

#interesting that leaf-soil N is significant when we don't average the sites, the relationship
# more variance attributed to main effects than random effects in this model


#
#

# Do mixed effects analysis for root N, same work flow

mean_root_lme <- aggregate(rootNPercent ~ siteID + plotID + inorganicN +
                            Lcclass, mean, data = plot.df)
#head(mean_root_lme)

# *remove pctSand for now, as it made the data set too small to justify an LME

#rename veg type
mean_root_lme <- rename_lcc(mean_root_lme,crop=F)
#head(mean_root_lme)

#merge the vpd data frame
mean_root_lme <- merge(vpd,mean_root_lme,by=c('siteID'))
#head(mean_root_lme)

#check sample sizes
length_mean_root_lme<-aggregate(rootNPercent~siteID,length,data=mean_root_lme)
# lower than foliar, but looks okay, all sites have 4 replicates...

#now do lmes
root_lme.1<-lme(rootNPercent~ inorganicN + vpd + Lcclass , random= ~1|siteID,data=mean_root_lme)
summary(root_lme.1) # inorganic N barely significant in this case
r.squaredGLMM(root_lme.1)

# R2m       R2c
# [1,] 0.1055042 0.4613954

#0.46 - 0.11

# notes
 # there is about half the data in the root lme than in the foliar lme. I think 
 # this is probably why more variance gets pushed to the random effects. In this case 
 # we see that, again, soil N is the only significant main effect, but here more variance
 # is attributable to the random effects. I wonder if this would change if there was more 
# data in generally and specifically for within-site replicates

# done

#-------------------------------------------------------------------------------
# plant feedbacks to soil N ----------------------------------------------------

#get means for plot-site combinations of litter and reporption
mean_litter<-aggregate(litterNPercent_mean~siteID + plotID,mean,data=plot.df)
mean_resorp<-aggregate(resorpN~siteID + plotID,mean,data=plot.df)

merge_mean_litter_total_soil <- filter_reps(mean_litter, mean_soil)

length(merge_mean_litter_total_soil$siteID)
# 15 sites

#round to two decimal points
merge_mean_litter_total_soil$litterNPercent_mean <- round(merge_mean_litter_total_soil$litterNPercent_mean,2)
merge_mean_litter_total_soil$soilNPercent_MHoriz_mean <- round(merge_mean_litter_total_soil$soilNPercent_MHoriz_mean,2)

# take a look
plot(soilNPercent_MHoriz_mean~litterNPercent_mean,data=merge_mean_litter_total_soil)
outlierTest(lm(soilNPercent_MHoriz_mean~litterNPercent_mean,data=merge_mean_litter_total_soil)) 
# one outlier, # 4
plot(soilNPercent_MHoriz_mean~litterNPercent_mean,data=merge_mean_litter_total_soil[-4,])
soil_litter_total_lm <- lm(soilNPercent_MHoriz_mean~litterNPercent_mean,data=merge_mean_litter_total_soil[-4,])
tab_model(soil_litter_total_lm)
# Adjusted R-squared:  -0.0826 
# p-value: 0.9296

#
#

# Resorption and soil N

merge_mean_resorp_total_soil <- filter_reps(mean_resorp, mean_soil)

length(merge_mean_resorp_total_soil$siteID)
# 10 sites

#round to two decimal points
merge_mean_resorp_total_soil$resorpN <- round(merge_mean_resorp_total_soil$resorpN,2)
merge_mean_resorp_total_soil$soilNPercent_MHoriz_mean <- round(merge_mean_resorp_total_soil$soilNPercent_MHoriz_mean,2)

# take a look
plot(soilNPercent_MHoriz_mean~resorpN,data=merge_mean_resorp_total_soil) # no relationship
outlierTest(lm(soilNPercent_MHoriz_mean~resorpN,data=merge_mean_resorp_total_soil))

#look without outlier
plot(soilNPercent_MHoriz_mean~resorpN,data=merge_mean_resorp_total_soil[-1,])
soil_resorp_total_lm <- lm(soilNPercent_MHoriz_mean~resorpN,data=merge_mean_resorp_total_soil[-1,])
tab_model(soil_resorp_total_lm)
# Adjusted R-squared:  -0.1414 
# p-value: 0.9273

#-------------------------------------------------------------------------------
#create and save plot for plant feedbacks to total soil N ----

pdf(file='./../output/bivar_plant_soil_root.pdf',
    width=8,height=6)

# Set up multi-panel
layout(matrix(1:2, ncol=2))
par(oma=c(6, 5, 6, 5), mar=c(0, 4, 0, 0),pty='s')
#?par
# Panel label setup
line = 0.75 
cex = 1.25
side = 3
adj= - 0.15

# A: litter to soil N
plot(soilNPercent_MHoriz_mean~litterNPercent_mean,xlab='',ylab="% Total soil N",data=merge_mean_litter_total_soil[-4,])
#points(1.53,1.24,col='red',cex=1.3,pch=19)
mtext('% Litter N',side=1,line=2.25,cex=1.0)
mtext("A", side=side, line=line, cex=cex, adj=adj)

# B: resorp to soil N
plot(soilNPercent_MHoriz_mean~resorpN,xlab='',ylab="",data=merge_mean_resorp_total_soil[-1,])
mtext('N Resorption',side=1,line=2.25,cex=1.0)
mtext("B", side=side, line=line, cex=cex, adj=adj)
#text(35, 0.22, 'N.S',cex=1)

dev.off()
#-------------------------------------------------------------------------------

#plant feedbakcs to soil  inorganic N --------------------

#get means for plot-site combinations
# mean_litter<-aggregate(litterNPercent_mean~siteID + plotID,mean,data=plot.df)
# mean_resorp<-aggregate(resorpN~siteID + plotID,mean,data=plot.df)

# Merge Litter and total soil N data
mean_litter_inorganic_soil <- merge(mean_litter, mean_soil_inorganic, by = c('siteID', 'plotID'))

# get sample sizes per site
length_mean_litter_inorganic_soil  <- aggregate(plotID ~ siteID, length, data = mean_litter_inorganic_soil)
colnames(length_mean_litter_inorganic_soil) <- c('siteID','reps')

#remove sites with less than 4 replicates
length_mean_litter_inorganic_soil <- length_mean_litter_inorganic_soil %>%
  dplyr::filter(reps > 3)

#merge so sites with < 4 reps are removed
merge_mean_litter_inorganic_soil <- merge(length_mean_litter_inorganic_soil,
                                      mean_litter_inorganic_soil,by=c('siteID'))

#get site means
merge_mean_litter_inorganic_soil <- merge_mean_litter_inorganic_soil  %>%
  group_by(siteID) %>%
  summarize(litterNPercent_mean = mean(litterNPercent_mean),
            inorganicN = mean(inorganicN))
merge_mean_litter_inorganic_soil <- data.frame(merge_mean_litter_inorganic_soil)

length(merge_mean_litter_inorganic_soil$siteID)
#14

# round to two decimal points
merge_mean_litter_inorganic_soil$litterNPercent_mean <- round(merge_mean_litter_inorganic_soil$litterNPercent_mean,2)
merge_mean_litter_inorganic_soil$inorganicN <- round(merge_mean_litter_inorganic_soil$inorganicN,2)

# take a look
plot(inorganicN~litterNPercent_mean,data=merge_mean_litter_inorganic_soil)
outlierTest(lm(inorganicN~litterNPercent_mean,data=merge_mean_litter_inorganic_soil))

#remove outlier
plot(inorganicN~litterNPercent_mean,data=merge_mean_litter_inorganic_soil[-3,])
summary(lm(inorganicN~litterNPercent_mean,data=merge_mean_litter_inorganic_soil[-3,]))
# Adjusted R-squared:  -0.004887 
# p-value: 0.3527

##


# Resorption and inorganic soil N
mean_resorp_inorganic_soil <- merge(mean_resorp, mean_soil_inorganic, by = c('siteID', 'plotID'))

#get site count
length_mean_resorp_inorganic_soil <- aggregate(plotID ~ siteID, length, data = mean_resorp_total_soil)
colnames(length_mean_resorp_inorganic_soil) <- c('siteID','reps')

#remove sites with less than 4 replicates
length_mean_resorp_inorganic_soil <- length_mean_resorp_inorganic_soil %>%
  dplyr::filter(reps > 3)

#merge so sites with < 4 reps are removed
merge_mean_resorp_inorganic_soil <- merge(length_mean_resorp_inorganic_soil,
                                          mean_resorp_inorganic_soil,by=c('siteID'))

#get site means
merge_mean_resorp_inorganic_soil <- merge_mean_resorp_inorganic_soil  %>%
  group_by(siteID) %>%
  summarize(resorpN  = mean(resorpN),
            inorganicN = mean(inorganicN))
merge_mean_resorp_inorganic_soil <- data.frame(merge_mean_resorp_inorganic_soil)
length(merge_mean_resorp_inorganic_soil$siteID)
# N = 10

# round to two decimal points
merge_mean_resorp_inorganic_soil$resorpN <- round(merge_mean_resorp_inorganic_soil$resorpN,2)
merge_mean_resorp_inorganic_soil$inorganicN <- round(merge_mean_resorp_inorganic_soil$inorganicN,2)

# take a look
plot(inorganicN~resorpN,data=merge_mean_resorp_inorganic_soil) # no relationship
outlierTest(lm(inorganicN~resorpN,data=merge_mean_resorp_inorganic_soil))

#try without outlier
plot(inorganicN~resorpN,data=merge_mean_resorp_inorganic_soil[-1,])
summary(lm(inorganicN~resorpN,data=merge_mean_resorp_inorganic_soil[-1,]))


# create and save plot of plant feedbacks to soil inorganic N ------
pdf(file='./../output/bivar_plant_inorganic_soil_root.pdf',
    width=8,height=6)

# Set up multi-panel
layout(matrix(1:2, ncol=2))
par(oma=c(6, 5, 6, 5), mar=c(0, 4, 0, 0),pty='s')

# Panel label setup
line = 0.75 
cex = 1.25
side = 3
adj= - 0.15

# A: litter to soil N
plot(inorganicN~litterNPercent_mean,xlab='',ylab="% Inorganic soil N",data=merge_mean_litter_inorganic_soil[-3,])
mtext('% Litter N',side=1,line=2.25,cex=1.0)
mtext("A", side=side, line=line, cex=cex, adj=adj)

# B: resorp to soil N
plot(inorganicN~resorpN,xlab='',ylab="",data=merge_mean_resorp_inorganic_soil[-1,])
mtext('N Resporption',side=1,line=2.25,cex=1.0)
mtext("B", side=side, line=line, cex=cex, adj=adj)

dev.off()

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
