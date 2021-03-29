#C:N analyses

# 31 sites for root-soil total N relationships
# 31 sites for foliar-soil total N relationships

head(plot.df)

# Get sample sizes and plot means ------
sample_size_foliar_cn<-aggregate(foliarCNRatio_mean~siteID,length,data=plot.df)
#sample_size_litter<-aggregate(litterNPercent_mean~siteID,length,data=plot.df) # KONZ only 4
sample_size_soil_cn<-aggregate(soilCNRatio_MHoriz_mean~siteID,length,data=plot.df) 
sample_size_root_cn <- aggregate(rootCNratio ~ siteID, length, data = plot.df)
#sample_size_soil_inorganic <- aggregate(inorganicN ~ siteID, length, data = plot.df)

# HEAL has sample size of 1 for total soil N, this gets removed from analysis
# anyways because there are no data from any of the other three pools

# Get mean values for each plot
mean_foliar_cn<-aggregate(foliarCNRatio_mean~siteID + plotID,mean,data=plot.df)
mean_root_cn <- aggregate(rootCNratio ~ siteID + plotID, mean, data = plot.df)
mean_soil_cn<-aggregate(soilCNRatio_MHoriz_mean~siteID + plotID,mean,data=plot.df)

#-------------------------------------------------------------------------------
# soil and root C:N relationship -----

# now do root and soil N
mean_soil_root_cn <- merge(mean_soil_cn, mean_root_cn, by = c('siteID', 'plotID'))

# get site reps
length_mean_soil_root_cn <- aggregate(plotID ~ siteID, length, data = mean_soil_root_cn)
colnames(length_mean_soil_root_cn) <- c('siteID','reps')

#remove sites with less than 4 replicates
length_mean_soil_root_cn_reps <- length_mean_soil_root_cn %>%
  dplyr::filter(reps > 3)

#merge so sites with < 4 reps are removed
merge_mean_soil_root_cn <- merge(length_mean_soil_root_cn_reps,mean_soil_root_cn,by=c('siteID'))

#get site means
merge_mean_soil_root_cn   <- merge_mean_soil_root_cn    %>%
  group_by(siteID) %>%
  summarize(rootCNratio = mean(rootCNratio),
            soilCNRatio_MHoriz_mean = mean(soilCNRatio_MHoriz_mean))
merge_mean_soil_root_cn <- data.frame(merge_mean_soil_root_cn)
length(merge_mean_soil_root_cn$siteID) 
#N = 21 sites

#round to two decimal places
merge_mean_soil_root_cn$rootCNratio <- round(merge_mean_soil_root_cn$rootCNratio,2)
merge_mean_soil_root_cn$soilCNRatio_MHoriz_mean <- round(merge_mean_soil_root_cn$soilCNRatio_MHoriz_mean,2)

#plot(rootCNratio~soilCNRatio_MHoriz_mean,data=merge_mean_soil_root_cn)
outlierTest(lm(rootCNratio~soilCNRatio_MHoriz_mean,data=merge_mean_soil_root_cn))
#no outliers

#summary(lm(rootCNratio~soilCNRatio_MHoriz_mean,data=merge_mean_soil_root_cn)) 
#Adjusted R-squared:  0.4305, p-value: 0.001004

#store for linear fit
sensitiivty_soil_root_cn<-lm(rootCNratio~soilCNRatio_MHoriz_mean,data=merge_mean_soil_root_cn)

#-------------------------------------------------------------------------------
# soil and leaf C:N relationship ---------
# now do root and soil N
mean_soil_foliar_cn <- merge(mean_soil_cn, mean_foliar_cn, by = c('siteID', 'plotID'))

# get site reps
length_mean_soil_foliar_cn <- aggregate(plotID ~ siteID, length, data = mean_soil_foliar_cn)
colnames(length_mean_soil_foliar_cn) <- c('siteID','reps')

#remove sites with less than 4 replicates
length_mean_soil_foliar_cn_reps <- length_mean_soil_foliar_cn %>%
  dplyr::filter(reps > 3)

#merge so sites with < 4 reps are removed
merge_mean_soil_foliar_cn <- merge(length_mean_soil_foliar_cn_reps,mean_soil_foliar_cn,by=c('siteID'))

#get site means
merge_mean_soil_foliar_cn   <- merge_mean_soil_foliar_cn   %>%
  group_by(siteID) %>%
  summarize(soilCNRatio_MHoriz_mean = mean(soilCNRatio_MHoriz_mean),
            foliarCNRatio_mean = mean(foliarCNRatio_mean))
merge_mean_soil_foliar_cn <- data.frame(merge_mean_soil_foliar_cn)
length(merge_mean_soil_foliar_cn$siteID) 
#N = 20 sites

#round to two decimal places
merge_mean_soil_foliar_cn$foliarCNRatio_mean <- round(merge_mean_soil_foliar_cn$foliarCNRatio_mean,2)
merge_mean_soil_foliar_cn$soilCNRatio_MHoriz_mean <- round(merge_mean_soil_foliar_cn$soilCNRatio_MHoriz_mean,2)

#plot(foliarCNRatio_mean~soilCNRatio_MHoriz_mean,data=merge_mean_soil_foliar_cn)
outlierTest(lm(foliarCNRatio_mean~soilCNRatio_MHoriz_mean,data=merge_mean_soil_foliar_cn))
#no outliers

#remove outlier
#plot(foliarCNRatio_mean~soilCNRatio_MHoriz_mean,data=merge_mean_soil_foliar_cn[-1,])
#summary(lm(foliarCNRatio_mean~soilCNRatio_MHoriz_mean,data=merge_mean_soil_foliar_cn[-1,])) 
# Adjusted R-squared:  0.6695
#p-value: 1.131e-05

#store for linear fit
sensitiivty_soil_leaf_cn<-lm(foliarCNRatio_mean~soilCNRatio_MHoriz_mean,data=merge_mean_soil_foliar_cn[-1,])

#-------------------------------------------------------------------------------
# plot leaf and root - soil C:N relationship ----------------
pdf(file='./../output/bivar_soil_leaf_root_CN.pdf',
    width=8,height=6)

# Set up multi-panel
layout(matrix(1:2, ncol=2))
par(oma=c(6, 5, 6, 5), mar=c(0, 4, 0, 0),pty='s')

# Panel label setup
line = 0.75 
cex = 1.25
side = 3
adj= - 0.15

# A: soil to root N
plot(rootCNratio~soilCNRatio_MHoriz_mean,xlab='',ylab="",data=merge_mean_soil_root_cn)
mtext('Root C:N',side=2,line=2.25,cex=1.0)
abline(sensitiivty_soil_root_cn, col="red",lwd=2)
text(25, 39, 'R-squared = 0.43 ',cex=1)
mtext("A", side=side, line=line, cex=cex, adj=adj)

# B: soil leaf N
plot(foliarCNRatio_mean~soilCNRatio_MHoriz_mean,xlab='',ylab="",data=merge_mean_soil_foliar_cn[-1,])
abline(sensitiivty_soil_leaf_cn, col="red",lwd=2)
text(22.5, 19, 'R-squared = 0.67 ',cex=1)
mtext("B", side=side, line=line, cex=cex, adj=adj)
mtext('Leaf C:N',side=2,line=2.25,cex=1.0)
mtext('Total soil C:N (M Horizon)',side=1,line=-1,cex=1.25,outer=T)

dev.off()

# need to add statistics in these figure


#-------------------------------------------------------------------------------
# mixed effect models: soil effects on plant N -----------------

head(plot.df)

# get aridity data
vpd <- read.csv('./../data_pre-processed/scaled_vpd.csv')
head(vpd)

#cleanup
vpd<-vpd[c(2,3)]
colnames(vpd) <- c('siteID','vpd')

mean_foliar_cn_lme<-aggregate(foliarCNRatio_mean~siteID + plotID
                           + soilCNRatio_MHoriz_mean + Lcclass,mean,data=plot.df)
#head(mean_foliar_lme)

#rename to veg type to herb versus woody. 'Croplands' are deemed herbaceous
mean_foliar_cn_lme <- rename_lcc(mean_foliar_cn_lme,crop = T)

# merge with vpd data frame
mean_foliar_cn_lme <- merge(vpd,mean_foliar_cn_lme,by=c('siteID'))
#head(mean_foliar_lme)

#check sample sizes
length_foliar_root_cn_lme<-aggregate(foliarCNRatio_mean~siteID,length,data=mean_foliar_cn_lme)

#remove site with only one rep
mean_foliar_cn_lme <- mean_foliar_cn_lme %>%
  dplyr::filter(!(siteID=="WREF"))
unique(mean_foliar_cn_lme$siteID) # works
# N = 10 reps

# lme functions lets you see P values in summary output
leaf_cn_lme.1<-lme(foliarCNRatio_mean~ soilCNRatio_MHoriz_mean + vpd + Lcclass, random= ~1|siteID,data=mean_foliar_cn_lme)
summary(leaf_cn_lme.1) #only significant factor is soil C:N
r.squaredGLMM(leaf_cn_lme.1)
#0.57 - 0.31

# note: only soil C:N significant. Similar variance explained in main and random effects

#
#

# Do mixed effects analysis for root N, same work flow
head(plot.df)

mean_root_cn_lme <- aggregate(rootCNratio ~ siteID + plotID + soilCNRatio_MHoriz_mean +
                             Lcclass, mean, data = plot.df)
#head(mean_root_cn_lme)


#rename veg type
mean_root_cn_lme <- rename_lcc(mean_root_cn_lme,crop=F)
#head(mean_root_cn_lme)
#unique(mean_root_cn_lme$Lcclass)

#merge the vpd data frame
mean_root_cn_lme <- merge(vpd,mean_root_cn_lme,by=c('siteID'))
#head(mean_root_cn_lme)

#check sample sizes
length_mean_root_cn_lme<-aggregate(rootCNratio~siteID,length,data=mean_root_cn_lme)
# lower than foliar, but looks ok

#now do lmes
root_cn_lme.1<-lme(rootCNratio~ soilCNRatio_MHoriz_mean + vpd + Lcclass , random= ~1|siteID,data=mean_root_cn_lme)
summary(root_cn_lme.1) # soil C:N only significant 
r.squaredGLMM(root_cn_lme.1)
#0.60-0.19

#note again soil variably only significant main effect, more variance explained in random effects

#-------------------------------------------------------------------------------
# plant feedbacks to soil N----

#plot and site means
mean_litter_cn<-aggregate( litterCNRatio_mean~siteID + plotID,mean,data=plot.df)
mean_resorp<-aggregate(resorpN~siteID + plotID,mean,data=plot.df)

# Litter and total soil N
mean_litter_soil_cn <- merge(mean_litter_cn, mean_soil_cn, by = c('siteID', 'plotID'))
length_mean_litter_soil_cn  <- aggregate(plotID ~ siteID, length, data = mean_litter_soil_cn)

# Get site means
mean_litter_soil_cn_2 <- mean_litter_soil_cn[-2] %>%
  dplyr::filter(!(siteID=="SJER")) %>% #remove site with only one replicate
  dplyr::filter(!(siteID=="DEJU")) %>%
  dplyr::group_by(siteID) %>%
  dplyr::summarise_all(mean) 
head(mean_litter_soil_cn_2)
plot(soilCNRatio_MHoriz_mean~litterCNRatio_mean,data=mean_litter_soil_cn_2)

#look at  outliers
outlierTest(lm(soilCNRatio_MHoriz_mean~litterCNRatio_mean,data=mean_litter_soil_cn_2))
#no outliers

#summary(lm(soilCNRatio_MHoriz_mean~litterCNRatio_mean,data=mean_litter_soil_cn_2))
#Adjusted R-squared:  0.5386, p-value: 0.001111

#for figure linear fit
litter_soil_cn_lm<-lm(soilCNRatio_MHoriz_mean~litterCNRatio_mean,data=mean_litter_soil_cn_2)


#
#

#now do resorption and soil C:N

mean_resorp_soil_cn <- merge(mean_resorp, mean_soil_cn, by = c('siteID', 'plotID'))
length_mean_mean_resorp_soil_cn  <- aggregate(plotID ~ siteID, length, data = mean_resorp_soil_cn)

# Get site means
mean_resorp_soil_cn_2 <- mean_resorp_soil_cn[-2] %>%
  dplyr::filter(!(siteID=="SJER")) %>% #remove site with only one replicate
  dplyr::filter(!(siteID=="BONA")) %>%
  dplyr::group_by(siteID) %>%
  dplyr::summarise_all(mean) 

plot(soilCNRatio_MHoriz_mean~resorpN,data=mean_resorp_soil_cn_2)

#look at  outliers
outlierTest(lm(soilCNRatio_MHoriz_mean~resorpN,data=mean_resorp_soil_cn_2))
#no outliers

#summary(lm(soilCNRatio_MHoriz_mean~resorpN,data=mean_resorp_soil_cn_2))
#NS

pdf(file='./../output/bivar_litter_resorp_soil_CN.pdf',
    width=8,height=7)

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
plot(soilCNRatio_MHoriz_mean~litterCNRatio_mean,data=mean_litter_soil_cn_2,xlab='',ylab="Total soil C:N")
mtext('Litter C:N',side=1,line=2.25,cex=1.0)
abline(litter_soil_cn_lm, col="red",lwd=2)
text(91, 15, 'R-squared = 0.54',cex=1)
mtext("A", side=side, line=line, cex=cex, adj=adj)

# B: resorp to soil N
plot(soilCNRatio_MHoriz_mean~resorpN,data=mean_resorp_soil_cn_2,xlab='',ylab="")
mtext('N Resporption',side=1,line=2.25,cex=1.0)
mtext("B", side=side, line=line, cex=cex, adj=adj)
#text(35, 0.22, 'N.S',cex=1)

dev.off()


#-------------------------------------------------------------------------------
# mixed effects models for plant effects on soil C:N ------------

head(plot.df)

# get aridity data
vpd <- read.csv('./../data_pre-processed/scaled_vpd.csv')
#head(vpd)

#cleanup
vpd<-vpd[c(2,3)]
colnames(vpd) <- c('siteID','vpd')

# do this without resorption because there was no clear relationship. This
# also adds more sites

mean_soil_cn_lme<-aggregate(soilCNRatio_MHoriz_mean~siteID + plotID
                           + litterCNRatio_mean + Lcclass,mean,data=plot.df)
#head(mean_soil_cn_lme)

#rename to veg type to herb versus woody. 'Croplands' are deemed herbaceous
mean_soil_cn_lme<- rename_lcc(mean_soil_cn_lme,crop = T)
# head(mean_soil_cn_lme)
# unique(mean_soil_cn_lme$Lcclass)

# merge with vpd data frame
mean_soil_cn_lme <- merge(vpd,mean_soil_cn_lme,by=c('siteID'))
#head(mean_soil_cn_lme)

#check sample sizes
length_mean_soil_cn_lme<-aggregate(soilCNRatio_MHoriz_mean~siteID,length,data=mean_soil_cn_lme)

#remove site with only one rep
mean_soil_cn_lme<- mean_soil_cn_lme %>%
  dplyr::filter(!(siteID=="SCBI"))
unique(mean_soil_cn_lme$siteID) # works

# lme functions lets you see P values in summary output
soil_cn_lme.1<-lme(soilCNRatio_MHoriz_mean~ litterCNRatio_mean + vpd + Lcclass, random= ~1|siteID,data=mean_soil_cn_lme)
summary(soil_cn_lme.1)
# r.squaredGLMM(soil_cn_lme.1)
#0.55 - 0.11

#note: nothing significant here in LMEs, most variance attributed to random effects



