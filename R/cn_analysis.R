#C:N analyses

head(plot.df)

# Get sample sizes 
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
#mean_soil_inorganic<-aggregate(inorganicN~siteID + plotID,mean,data=plot.df)

#make a table of sites, mean values, and number of replicates

# #foliar
# mean_site_foliar <-aggregate(foliarNPercent_mean~siteID,mean,data=plot.df)
# mean_site_foliar$Foliar<-round(mean_site_foliar$foliarNPercent_mean,2)
# mean_site_foliar <- mean_site_foliar[c(1,3)]
# mean_site_foliar<-merge(sample_size_foliar,mean_site_foliar ,by=c('siteID'))
# colnames(mean_site_foliar) <-c('siteID','N','%Foliar')
# 
# #root
# mean_site_root <-aggregate(rootNPercent~siteID,mean,data=plot.df)
# mean_site_root$Root<-round(mean_site_root$rootNPercent,2)
# mean_site_root <- mean_site_root[c(1,3)]
# mean_site_root <-merge(sample_size_root,mean_site_root,by=c('siteID'))
# colnames(mean_site_root) <-c('siteID','N','%Root')
# 
# #total soil N
# mean_site_soil <-aggregate(soilNPercent_MHoriz_mean~siteID,mean,data=plot.df)
# mean_site_soil$TotalSoil<-round(mean_site_soil$soilNPercent_MHoriz_mean,2)
# mean_site_soil  <- mean_site_soil[c(1,3)]
# mean_site_soil  <-merge(sample_size_soil,mean_site_soil ,by=c('siteID'))
# colnames(mean_site_soil) <-c('siteID','N','%TotalSoil')

#root and leaf merge
root_leaf_cn <- merge(mean_root_cn,mean_foliar_cn,by=c('siteID','plotID'),na.rm=T)
plot(foliarCNRatio_mean~rootCNratio,data=root_leaf_cn)

# merge foliar and total soil data by plot ID
mean_foliar_soil_cn <- merge(mean_foliar_cn, mean_soil_cn, by = c('siteID', 'plotID'))
#length_mean_foliar_soil <- aggregate(plotID ~ siteID, length, data = mean_foliar_soil)

# Get site-level means for foliar N
mean_foliar_soil_cn_2 <- mean_foliar_soil_cn[-2] %>%
  dplyr::group_by(siteID) %>%
  dplyr::summarise_all(mean) #%>%

plot(foliarCNRatio_mean~soilCNRatio_MHoriz_mean,data=mean_foliar_soil_cn_2)

#look at  outliers
outlierTest(lm(foliarCNRatio_mean~soilCNRatio_MHoriz_mean,data=mean_foliar_soil_cn_2))
#no outliers
summary(lm(foliarCNRatio_mean~soilCNRatio_MHoriz_mean,data=mean_foliar_soil_cn_2))
#Adjusted R-squared:  0.551 p-value: 4.611e-05

#
#

# now do root and soil N
mean_soil_root_cn <- merge(mean_soil_cn, mean_root_cn, by = c('siteID', 'plotID'))
length_mean_soil_root_cn <- aggregate(plotID ~ siteID, length, data = mean_soil_root_cn)

mean_soil_root_cn_2 <- mean_soil_root_cn[-2] %>%
  dplyr::group_by(siteID) %>%
  dplyr::summarise_all(mean) #%>%
#dplyr::filter(soilNPercent_MHoriz_mean < 1) # get rid of anomalously high value

#plot(rootCNratio~soilNPercent_MHoriz_mean,data=mean_soil_root_cn_2)
outlierTest(lm(rootCNratio~soilCNRatio_MHoriz_mean,data=mean_soil_root_cn_2))
#no outliers

plot(rootCNratio~soilCNRatio_MHoriz_mean,data=mean_soil_root_cn_2)

#summary(lm(rootCNratio~soilCNRatio_MHoriz_mean,data=mean_soil_root_cn_2)) 
#Adjusted R-squared:  0.4934 p-value: 0.0002314

pdf(file='./../output/bivar_soil_leaf_root_CN.pdf',
    width=8,height=6)
# mar.default <- c(6,3,5,2) + 0.1
# par(mar = mar.default + c(2, 2, 0, 0),mfrow=c(1,4))

# Set up multi-panel
layout(matrix(1:2, ncol=2))
par(oma=c(6, 5, 6, 5), mar=c(0, 4, 0, 0),pty='s')
#?par
# Panel label setup
line = 0.75 
cex = 1.25
side = 3
adj= - 0.15

# A: soil to root N
plot(rootCNratio~soilCNRatio_MHoriz_mean,xlab='',ylab="",data=mean_soil_root_cn_2)
mtext('% Root C:N',side=2,line=2.25,cex=1.0)
mtext("A", side=side, line=line, cex=cex, adj=adj)

# B: soil leaf N
plot(foliarCNRatio_mean~soilCNRatio_MHoriz_mean,xlab='',ylab="",data=mean_foliar_soil_cn_2)
mtext("B", side=side, line=line, cex=cex, adj=adj)
mtext('% Leaf C:N',side=2,line=2.25,cex=1.0)
mtext('% Soil C:N (M Horizon)',side=1,line=-1,cex=1.25,outer=T)

dev.off()

#now do this with inroganic N


# mixed effect models -----------------

head(plot.df)
library(lme4)

mean_foliar_cn_lme<-aggregate(foliarCNRatio_mean~siteID + plotID
                           + soilCNRatio_MHoriz_mean + Lcclass,mean,data=plot.df)
#head(mean_foliar_lme)

#rename to veg type to herb versus woody. 'Croplands' are deemed herbaceous
mean_foliar_cn_lme <- rename_lcc(mean_foliar_cn_lme,crop = T)

# get aridity data
vpd <- read.csv('./../data_pre-processed/scaled_vpd.csv')
head(vpd)

#cleanup
vpd<-vpd[c(2,3)]
colnames(vpd) <- c('siteID','vpd')

# merge with vpd data frame
mean_foliar_cn_lme <- merge(vpd,mean_foliar_cn_lme,by=c('siteID'))
#head(mean_foliar_lme)

#check sample sizes
length_foliar_root_cn_lme<-aggregate(foliarCNRatio_mean~siteID,length,data=mean_foliar_cn_lme)

#remove site with only one rep
mean_foliar_cn_lme <- mean_foliar_cn_lme %>%
  dplyr::filter(!(siteID=="WREF"))
unique(mean_foliar_cn_lme$siteID) # works

# lme functions lets you see P values in summary output
leaf_cn_lme.1<-lme(foliarCNRatio_mean~ soilCNRatio_MHoriz_mean + vpd + Lcclass, random= ~1|siteID,data=mean_foliar_cn_lme)
summary(leaf_cn_lme.1) #only significant factor is inroganic N
# r.squaredGLMM(leaf_cn_lme.1)
leaf_cn_lme.2<-lmer(foliarCNRatio_mean~soilCNRatio_MHoriz_mean  + vpd  + Lcclass + (1|siteID),data=mean_foliar_cn_lme)
#summary(leaf_cn_lme.2)
#r.squaredGLMM(leaf_cn_lme.2)

#only soil C:N signficiant, more var in conditional 

#stopped here




