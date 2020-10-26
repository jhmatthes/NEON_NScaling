# initial upload adriennes scrript

### NEON N scaling project
# author: Adrienne Keller/Andrew Felton
# last updated: 2020-xx-xx
# notes: EDA, statistical analyses
###

#clear workspace
rm(list=ls())

# load packages
library(dplyr)
library(ggplot2)
library(nlme)
library(car)
library(lattice)
library(grid)
library(rstudioapi)

# #set working directory - folder where the data is stored
# setwd('/Users/andrewfelton/Desktop')

current_path <- getActiveDocumentContext()$path
setwd(dirname(current_path))

# load in plot-level df !?! - climclass has some duplicate site rows due to multiple climate classes...

#load cleaned nitrogen pool data

#take a look
plot.df <- read.csv('./CN_plotID.csv', header = T, stringsAsFactors = F)

summary(plot.df)

#load climate data
climate.df <- read.csv('./MAT_MAP_Allsites.csv', header = T, stringsAsFactors = F)
climate.df$MAP<-climate.df$MAP*10 # change from cm to mm
head(climate.df)

#merge the plot and climate data by site name
plot.df <- left_join(plot.df, climate.df, by ="siteID")
head(plot.df)

#select key columns to simplify (this can change)
myvars <- c('domainID', "siteID", "plotID","rootNPercent",'rootCPercent','rootCNratio',
            'rootSample_n','plotType','litterNPercent','foliarNPercent','foliarCPercent',
            'foliarCNRatio','soilNPercent_MHoriz','soilCNRatio_MHoriz','nlcdClass',
            'x','y', 'MAT', 'MAP')

plot.df <- plot.df[myvars]
head(plot.df)

#remove duplicates resulting from the merge (linked to slight differences in coordinates?)
nrow(plot.df) #check row # before removing duplicates #11256 rows
plot.df <- plot.df[!duplicated(plot.df), ]
nrow(plot.df) #check that this worked #1123 rows

#look at the data
unique(plot.df$nlcdClass) #cover class
unique(plot.df$siteID) #sites

### univariate distributions of %N pools (this is pre outlier removal)
hist(plot.df$foliarNPercent)
hist(plot.df$litterNPercent)
hist(plot.df$rootNPercent)
hist(log(plot.df$soilNPercent_MHoriz)) #log transformed for normality?

#filter out sight (GUAN) with anamolously high soil nitrogen...
plot.df <- plot.df %>%
  dplyr::group_by(siteID) %>%
  dplyr::filter(!siteID=='GUAN')

unique(plot.df$siteID)

#figure 1: distributions of % N pools across all sites #########

# *only with GUAN removed...this could be modified... #

pdf(file='/Users/andrewfelton/Desktop/Figures/Figure_1/histograms.pdf',
    width=6,height=4)
mar.default <- c(6,3,5,2) + 0.1
par(mar = mar.default + c(2, 2, 0, 0),mfrow=c(1,3))
par(mar.default,mfrow=c(1,3))
hist(plot.df$soilNPercent_MHoriz,main='',xlab='% Soil nitrogen',cex.lab=1.75)
hist(plot.df$foliarNPercent,main='',xlab='% Foliar nitrogen',ylab='',cex.lab=1.75)
hist(plot.df$litterNPercent,main='',xlab='% Litter nitrogen',ylab='',cex.lab=1.75)
dev.off()
#########

#Spatial relationships (averages) of N pools across neon sites

# first check that sample sizes will be > five for each site

sample_size_foliar<-aggregate(foliarNPercent~siteID,length,data=plot.df)
sample_size_litter<-aggregate(litterNPercent~siteID,length,data=plot.df)
sample_size_soil<-aggregate(soilNPercent_MHoriz~siteID,length,data=plot.df)

# site HEAL has sample size of 1 for soil N, gets removed from analysis
# anyways because there are no data from any of the other three pools

#get mean plot value for each site
mean_foliar<-aggregate(foliarNPercent~siteID + plotID,mean,data=plot.df)
mean_litter<-aggregate(litterNPercent~siteID + plotID,mean,data=plot.df)
mean_soil<-aggregate(soilNPercent_MHoriz~siteID + plotID,mean,data=plot.df)

### 'global' relationships (cross-site)

# foliar - litter ########

#merge foliar and litter data by plot ID
mean_foliar_litter<-merge(mean_foliar,mean_litter,by=c('siteID','plotID'))
length_mean_foliar_litter<-aggregate(plotID~siteID,length,data=mean_foliar_litter)
#not too many replicates per site... (co-located foliar and litter data...)

#take a peek
plot(litterNPercent~foliarNPercent,data=mean_foliar_litter)
summary(mean_foliar_litter)

#look at spatial relationship
foliar_to_litter_lm<-lm(litterNPercent ~ foliarNPercent,data=mean_foliar_litter) 
outlierTest(foliar_to_litter_lm) #no clear outliers, so proceed
summary(foliar_to_litter_lm)
#R2 = 0.31, P < 0.001, slope = 0.29

########

# plot it: relationships between %litter and %foliar N #######

pdf(file='/Users/andrewfelton/Desktop/Figures/Figure_2/foliar_to_litter.pdf',
    width=6,height=5)
ggplot(mean_foliar_litter, aes(foliarNPercent,litterNPercent),na.rm=TRUE) + 
  scale_y_continuous(limits=c(0.25,1.6)) +
  geom_point(size=10,pch=21,fill='white') +
  geom_smooth(method = "lm",size=1,color='black',se=F) +
  xlab('% Foliar nitrogen') +
  ylab('% Litter nitrogen') +
  theme(
    axis.text.x = element_text(color='black',size=20), 
    axis.text.y = element_text(color='black',size=18),
    axis.title = element_text(color='black',size=30),
    axis.ticks = element_line(color='black'),
    legend.key = element_blank(),
    legend.title = element_text(size=10),
    legend.text = element_text(size=9),
    legend.position = c(0.5,0.6),
    #legend.position = 'top'5
    strip.background =element_rect(fill="white"),
    strip.text = element_text(size=15),
    panel.background = element_rect(fill=NA),
    panel.border = element_blank(), #make the borders clear in prep for just have two axes
    axis.line.x = element_line(colour = "black"),
    axis.line.y = element_line(colour = "black"))

dev.off()

########

# litter to soil spatial relationship ########

# merge the two aggregated soil and litter datasets by plotID #
mean_litter_soil<-merge(mean_soil,mean_litter,by=c('siteID','plotID'))
length_mean_litter_soil<-aggregate(plotID~siteID,length,data=mean_litter_soil)
#again ow sample size per co-located litter and soil plots
summary(mean_litter_soil)

#take a peek
#plot(soilNPercent_MHoriz~litterNPercent,data=mean_litter_soil) 
#there is clearly an outlier here...

mean_litter_soil_lm<-lm(soilNPercent_MHoriz~litterNPercent,data=mean_litter_soil)
outlierTest(mean_litter_soil_lm) # remove observation 28 (or whichever row is identified)

#repeat with outlier remove
mean_litter_soil_lm_2<-lm(soilNPercent_MHoriz~litterNPercent,data=mean_litter_soil[-c(28),])
plot(soilNPercent_MHoriz~litterNPercent,data=mean_litter_soil[-c(28),])
outlierTest(mean_litter_soil_lm_2) #still another clear outlier here (#1)...try again...

# repeat again... #
mean_litter_soil_lm_3<-lm(soilNPercent_MHoriz~litterNPercent,data=mean_litter_soil[-c(1,28),])
plot(soilNPercent_MHoriz~litterNPercent,data=mean_litter_soil[-c(1,28),])
outlierTest(mean_litter_soil_lm_3) #looking better, still an outlier (#39)

# and again... #
mean_litter_soil_lm_4<-lm(soilNPercent_MHoriz~litterNPercent,data=mean_litter_soil[-c(1,28,39),])
plot(soilNPercent_MHoriz~litterNPercent,data=mean_litter_soil[-c(1,28,39),])
outlierTest(mean_litter_soil_lm_4)
summary(mean_litter_soil_lm_4)
#R2 = 0.068, P = 0.035, slope = 0.08

########

# plot it: relationships between %litter and % soil N ########
pdf(file='/Users/andrewfelton/Desktop/Figures/Figure_2/litter_to_soil.pdf',
    width=6,height=5)
ggplot(mean_litter_soil[-c(1,28,39),], aes(litterNPercent,soilNPercent_MHoriz),na.rm=TRUE) + 
  scale_x_continuous(limits=c(0.25,1.6)) +
  geom_point(size=10,pch=21,fill='white') +
  geom_smooth(method = "lm",size=1,color='black',se=F) +
  ylab('% Soil nitrogen') +
  xlab('% Litter nitrogen') +
  theme(
    axis.text.x = element_text(color='black',size=20), 
    axis.text.y = element_text(color='black',size=18),
    axis.title = element_text(color='black',size=30),
    axis.ticks = element_line(color='black'),
    legend.key = element_blank(),
    legend.title = element_text(size=10),
    legend.text = element_text(size=9),
    legend.position = c(0.5,0.6),
    #legend.position = 'top'5
    strip.background =element_rect(fill="white"),
    strip.text = element_text(size=15),
    panel.background = element_rect(fill=NA),
    panel.border = element_blank(), #make the borders clear in prep for just have two axes
    axis.line.x = element_line(colour = "black"),
    axis.line.y = element_line(colour = "black"))

dev.off()

##########

# Soil to leaves spatial relationships #######

#merge the two aggregated datasets
mean_soil_leaves<-merge(mean_soil,mean_foliar,by=c('siteID','plotID'))
length_mean_soil_leaves<-aggregate(plotID~siteID,length,data=mean_soil_leaves)
#higher plot sample size in this one...
summary(mean_soil_leaves)
#plot(foliarNPercent~soilNPercent_MHoriz,data=mean_soil_leaves) #take a peek

#first filter out the very high value...
mean_soil_leaves<-filter(mean_soil_leaves, soilNPercent_MHoriz < 0.6) #remove very high values
summary(mean_soil_leaves)

#do the first spatial model and check for outliers
mean_soil_leaves_lm<-lm(foliarNPercent~soilNPercent_MHoriz,data=mean_soil_leaves) 
outlierTest(mean_soil_leaves_lm) #couple outliers shown, remove and repeat (140,133)

#try again with outliers removed
mean_soil_leaves_lm_2<-lm(foliarNPercent~soilNPercent_MHoriz,data=mean_soil_leaves[-c(140,133),]) 
outlierTest(mean_soil_leaves_lm_2) #looks good, proceed

#take a peek
plot(foliarNPercent~soilNPercent_MHoriz,data=mean_soil_leaves[-c(140,133),])
summary(mean_soil_leaves_lm_2)
#R2 = 0.04, P < 0.01 slope: 1.39 

#plot of the residuals
pdf(file='/Users/andrewfelton/Desktop/Figures/Figure_4/soil_to_leaves.residuals.pdf',
    width=6,height=5)
plot(resid(mean_soil_leaves_lm_2),cex=2,xlab = 'Observation',ylab = 'Unexplained variation (residuals)',
     cex.lab=1.2)
abline(h = 0.0, col="red", lwd=5, lty=2)
dev.off()

#plot it: relaionship between %soil and %foliar N ########

#originally this plotting is set up to have an inset. this can change.

main<-ggplot(mean_soil_leaves[-c(140,133),], aes(soilNPercent_MHoriz,foliarNPercent),na.rm=TRUE) + 
  #scale_x_continuous(limits=c(0.25,1.6)) +
  geom_point(size=10,pch=21,fill='white') +
  geom_smooth(method = "lm",size=1,color='black',se=F) +
  xlab('% Soil nitrogen') +
  ylab('% Foliar nitrogen') +
  theme(
    axis.text.x = element_text(color='black',size=20), 
    axis.text.y = element_text(color='black',size=18),
    axis.title = element_text(color='black',size=30),
    axis.ticks = element_line(color='black'),
    legend.key = element_blank(),
    legend.title = element_text(size=10),
    legend.text = element_text(size=9),
    legend.position = c(0.5,0.6),
    #legend.position = 'top'5
    strip.background =element_rect(fill="white"),
    strip.text = element_text(size=15),
    panel.background = element_rect(fill=NA),
    panel.border = element_blank(), #make the borders clear in prep for just have two axes
    axis.line.x = element_line(colour = "black"),
    axis.line.y = element_line(colour = "black"))

# inset<-ggplot(slopes,aes(id,slope)) +
#   stat_summary(geom='bar',fun.y='mean',color='grey70') +
#   #geom_line(color='black',size=0.5) +
#   scale_y_continuous(expand = c(0,0),limit=c(0,1.4)) +
#   scale_x_discrete(labels=c("1" = "Soil to leaf", "2" = "Leaf to litter",
#                             "3" = 'Litter to soil')) +
#   ylab('Sensitivity (Change in %N)') +
#   xlab('') +
#   ggtitle('') +
#   theme(
#     axis.text.x = element_text(color='black',size=15, angle=25,hjust=1),
#     axis.text.y = element_text(color='black',size=15),
#     axis.title = element_text(color='black',size=16),
#     axis.ticks = element_line(color='black'),
#     legend.key = element_blank(),
#     strip.background =element_rect(fill="white"),
#     strip.text = element_text(size=15),
#     legend.position = c('top'),
#     panel.background = element_rect(fill=NA),
#     panel.border = element_blank(), #make the borders clear in prep for just have two axes
#     axis.line.x = element_line(colour = "black"),
#     axis.line.y = element_line(colour = "black"))

# vp.spatial <- viewport(width = 0.4, height = 0.35, x = 0.77,y=0.345)
# 
# #executing the inset, you create a function the utlizes all the previous code
# full.spatial <- function() {
#   print(main)
#   print(inset, vp = vp.spatial)
# }

#save main plot
pdf(file='/Users/andrewfelton/Desktop/Figures/Figure_2/soil_to_leaves.pdf',
    width=6,height=5)

# full.spatial()
main
dev.off()

# #save slopes plot
# pdf(file='/Users/andrewfelton/Desktop/Figures/Figure_3/slopes.pdf',
#     width=6,height=4)
# inset
# dev.off()

# within-site spatial correlations ########

#just make the removal of outliers its own dataset
mean_soil_leaves_2<-mean_soil_leaves[-c(140,133),]

#get within-site sample sizes
mean_soil_leaves_sample_sizes<-aggregate(plotID~siteID,length,data=mean_soil_leaves_2)

#merge sample size dataset
mean_soil_leaves_2<-merge(mean_soil_leaves_2,mean_soil_leaves_sample_sizes,by=c('siteID'))

#if a site has than 10 replicates, I remove it.
mean_soil_leaves_2<-filter(mean_soil_leaves_2,plotID.y > 9)

#get within-site correlations for soil to foliar N 
soil_leaves_within_site_cor<-mean_soil_leaves_2 %>%
  dplyr::group_by(siteID) %>%
  #dplyr::do(model = lm(foliarNPercent~soilNPercent_MHoriz, data = .)) %>%
  #dplyr::mutate(coef=coef(model)[2])
  dplyr::mutate(pval=coef(model)[2])
  dplyr::mutate(pval=summary(lm(foliarNPercent~soilNPercent_MHoriz, data = .))$coefficients[,4])
  dplyr::summarise(cor=cor(soilNPercent_MHoriz,foliarNPercent))
soil_leaves_within_site_cor<-data.frame(soil_leaves_within_site_cor)

test<-summary(mean_soil_leaves_lm_2)$coefficients[,4]
test[2]
mean_soil_leaves_lm_2

#######

#stopped here 10/16/2020

et<-read.csv('/Users/andrewfelton/Desktop/et.csv')
et$siteID<-et$SiteID
merge_cor_et<-merge(soil_leaves_within_site_cor,et,by=c('siteID'))
plot(cor~mean,data=merge_cor_et)

et_vpd<-merge(et,vpd,by=c('SiteID','siteID'))
plot(mean.x~mean.y,data=et_vpd)
et_vpd_map_mat<-merge(et_vpd,climate.df,by=c('siteID'))
plot(MAP~mean.x,data=et_vpd_map_mat)