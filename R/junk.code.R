# junk

# Get sample sizes (# sites) of all pools ----

head(plot.df.2)

# Get sample sizes 
sample_size_foliar<-aggregate(foliarNPercent_mean~siteID,length,data=plot.df.2)
sample_size_litter<-aggregate(litterNPercent_mean~siteID,length,data=plot.df.2) # KONZ only 4
sample_size_soil<-aggregate(soilNPercent_MHoriz_mean~siteID,length,data=plot.df.2) # HEAL only 1
sample_size_root <- aggregate(rootNPercent ~ siteID, length, data = plot.df.2)
sample_size_soil_inorganic <- aggregate(inorganicN ~ siteID, length, data = plot.df.2)
sample_size_soil_mineralization<- aggregate(netNminugPerGramPerDay ~ siteID, length, data = plot.df.2)
sample_size_soil_mineralization<- aggregate(netNminugPerGramPerDay ~ siteID, length, data = plot.df.2)

# HEAL has sample size of 1 for total soil N, this gets removed from analysis/merging
# anyways because there are no data from any of the other three pools

# make a table of sites, mean values, and number of replicates -----

#foliar
mean_site_foliar <-aggregate(foliarNPercent_mean~siteID,mean,data=plot.df.2)
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

# plot comparison of total versus inorganic soil N relationships, probably a supp. figure -----

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


# compare variability of pools within versus across sites ----------------------

# you'll have to run the previous to sections of code first

# % Foliar N
cross_site_foliar_sd<-round(sd(mean_site_foliar$`%Foliar`),2)

#average within-site foliar SD #

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


# mineralization-leaf N relationships ------

# merge foliar and mineralization data by plot ID
merge_mean_foliar_soil_mineralization <- filter_reps(mean_foliar, mean_soil_mineralization)

#round to two decimal points
merge_mean_foliar_soil_mineralization$foliarNPercent_mean<-round(merge_mean_foliar_soil_mineralization$foliarNPercent_mean,2)
merge_mean_foliar_soil_mineralization$netNminugPerGramPerDay<-round(merge_mean_foliar_soil_mineralization$netNminugPerGramPerDay,2)

length(merge_mean_foliar_soil_mineralization$siteID) 

#add veg type
merge_mean_foliar_soil_mineralization <- merge(merge_mean_foliar_soil_mineralization,vegtype.df,by='siteID')
aggregate(siteID~Lcclass,length,data=merge_mean_foliar_soil_mineralization)

# take a look  
outlierTest(lm(foliarNPercent_mean~netNminugPerGramPerDay,data=merge_mean_foliar_soil_mineralization))
#no outliers

summary(lm(foliarNPercent_mean~netNminugPerGramPerDay,data=merge_mean_foliar_soil_mineralization))
#tab_model(lm(foliarNPercent_mean~netNminugPerGramPerDay,data=merge_mean_foliar_soil_mineralization))

#moderately significant 

#-------------------------------------------------------------------------------
# mineralization-root relationships ------

# merge root and soil data by plot ID
merge_mean_mineralization_root <- filter_reps(mean_soil_mineralization, mean_root)

length(merge_mean_mineralization_root$siteID) 
# 19 sites

#round to two decimal points
merge_mean_mineralization_root$rootNPercent<-round(merge_mean_mineralization_root$rootNPercent,2)
merge_mean_mineralization_root$netNminugPerGramPerDay<-round(merge_mean_mineralization_root$netNminugPerGramPerDay,2)

length(merge_mean_mineralization_root$siteID) 

#add veg type
merge_mean_mineralization_root <- merge(merge_mean_mineralization_root,vegtype.df,by='siteID')
aggregate(siteID~Lcclass,length,data=merge_mean_mineralization_root)

#take a look
#plot(rootNPercent~netNminugPerGramPerDay,data=merge_mean_mineralization_root)
outlierTest(lm(rootNPercent~netNminugPerGramPerDay,data=merge_mean_mineralization_root))
# no outliers
summary(lm(rootNPercent~netNminugPerGramPerDay,data=merge_mean_mineralization_root))

#not significant
