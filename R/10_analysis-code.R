# last updated: 2020-10-26
# author: Adrienne Keller
# project: NEON N scaling
# notes:

# Map of sites? -----
library(rgdal)
library(raster)


# future map ------
### Figure 1: Map of sites (perhaps overlay on a climate and/or veg type layer)
# map paired with table of sample size per site for each 'pool' (foliar, litter, 
# soil, root) % N

neon_domains<-readOGR(dsn="./../data_pre-processed/NEONDomains_0",layer="NEON_Domains")
neon_domains<- sp::spTransform(neon_domains, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
plot(neon_domains)

neone_bounadaries<-readOGR(dsn="./../data_pre-processed/Field_Sampling_Boundaries",layer="terrestrialSamplingBoundaries")
plot(neone_bounadaries)

# need to look into this omre...

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

# Bivariate relationships between N pools - cross-site with each point = site mean (Figure 3) ----

# first do this for total soil N (organic + inorganic)

# Get sample sizes 
sample_size_foliar<-aggregate(foliarNPercent_mean~siteID,length,data=plot.df)
sample_size_litter<-aggregate(litterNPercent_mean~siteID,length,data=plot.df) # KONZ only 4
sample_size_soil<-aggregate(soilNPercent_MHoriz_mean~siteID,length,data=plot.df) 
sample_size_root <- aggregate(rootNPercent ~ siteID, length, data = plot.df)
sample_size_soil_inorganic <- aggregate(inorganicN ~ siteID, length, data = plot.df)

# HEAL has sample size of 1 for total soil N, this gets removed from analysis
# anyways because there are no data from any of the other three pools

# Get mean values for each plot
mean_foliar<-aggregate(foliarNPercent_mean~siteID + plotID,mean,data=plot.df)
mean_root <- aggregate(rootNPercent ~ siteID + plotID, mean, data = plot.df)
mean_soil<-aggregate(soilNPercent_MHoriz_mean~siteID + plotID,mean,data=plot.df)
mean_soil_inorganic<-aggregate(inorganicN~siteID + plotID,mean,data=plot.df)

#make a table of sites, mean values, and number of replicates

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


# merge datasets to do bivariate spatial relationships of site means

# merge foliar and total soil data by plot ID
mean_foliar_soil <- merge(mean_foliar, mean_soil, by = c('siteID', 'plotID'))
length_mean_foliar_soil <- aggregate(plotID ~ siteID, length, data = mean_foliar_soil)

# Get site-level means for foliar N
mean_foliar_soil_2 <- mean_foliar_soil[-2] %>%
  dplyr::group_by(siteID) %>%
  dplyr::summarise_all(mean) %>%
  #dplyr::filter(soilNPercent_MHoriz_mean < 1) # get rid of anomalously high value

#look at  outliers
outlierTest(lm(foliarNPercent_mean ~ soilNPercent_MHoriz_mean,data=mean_foliar_soil_2))
#no outliers

#plot(foliarNPercent_mean ~ soilNPercent_MHoriz_mean,data=mean_foliar_soil_2)
#summary(lm(foliarNPercent_mean ~ soilNPercent_MHoriz_mean,data=mean_foliar_soil_2)) 
#not significant with or without high soil N values

#
#

# merge root and soil data by plot ID
mean_soil_root <- merge(mean_soil, mean_root, by = c('siteID', 'plotID'))
length_mean_soil_root <- aggregate(plotID ~ siteID, length, data = mean_soil_root)

mean_soil_root_2 <- mean_soil_root[-2] %>%
  dplyr::group_by(siteID) %>%
  dplyr::summarise_all(mean) #%>%
  #dplyr::filter(soilNPercent_MHoriz_mean < 1) # get rid of anomalously high value

#plot(rootNPercent~soilNPercent_MHoriz_mean,data=mean_soil_root_2)
outlierTest(lm(rootNPercent~soilNPercent_MHoriz_mean,data=mean_soil_root_2))
#no outliers

#summary(lm(rootNPercent~soilNPercent_MHoriz_mean,data=mean_soil_root_2)) 
#not significant with or without high values
#Adjusted R-squared:  0.077
#p-value: 0.07

#plot this out with high soil N values

pdf(file='./../output/bivar_soil_leaf_root.pdf',
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
plot(rootNPercent~soilNPercent_MHoriz_mean,xlab='',ylab="",data=mean_soil_root_2)
mtext('% Root N',side=2,line=2.25,cex=1.0)
mtext("A", side=side, line=line, cex=cex, adj=adj)
text(1, 0.75, 'N.S',cex=1)

# B: soil leaf N
plot(foliarNPercent_mean ~ soilNPercent_MHoriz_mean,xlab='',ylab="",data=mean_foliar_soil_2)
mtext("B", side=side, line=line, cex=cex, adj=adj)
mtext('% Leaf N',side=2,line=2.25,cex=1.0)
mtext('% Soil N (M Horizon)',side=1,line=-1,cex=1.25,outer=T)
text(1, 1, 'N.S',cex=1)

dev.off()


#now do this with total inorganic N

#soil-leaf relationships

# merge foliar and soil data by plot ID
mean_foliar_soil_inorganic <- merge(mean_foliar, mean_soil_inorganic, by = c('siteID', 'plotID'))
length_mean_foliar_soil_inorganic <- aggregate(plotID ~ siteID, length, data = mean_foliar_soil_inorganic)

# Get site-level means for foliar N
mean_foliar_soil_inorganic_2 <- mean_foliar_soil_inorganic[-2] %>%
  dplyr::group_by(siteID) %>%
  dplyr::summarise_all(mean) #%>%
  #dplyr::filter(inorganicN < 1) 
  
  #only under lower levels is there a clear linear relationship. It is more saturating in form.

# plot(foliarNPercent_mean~inorganicN,data=mean_foliar_soil_inorganic_2)
#outlierTest(lm(foliarNPercent_mean~inorganicN,data=mean_foliar_soil_inorganic_2))
#no outliers
# summary(lm(foliarNPercent_mean~inorganicN,data=mean_foliar_soil_inorganic_2))

#Adjusted R-squared:  0.07
#p-value: 0.09

# soil-root relationships  

  # merge root and soil data by plot ID
mean_soil_root_inorganic <- merge(mean_soil_inorganic, mean_root, by = c('siteID', 'plotID'))
length_mean_soil_root_inorganic <- aggregate(plotID ~ siteID, length, data = mean_soil_root_inorganic)

mean_soil_root_inorganic_2 <- mean_soil_root_inorganic[-2] %>%
  dplyr::group_by(siteID) %>%
  dplyr::summarise_all(mean) #%>%
#dplyr::filter(soilNPercent_MHoriz_mean < 1) # get rid of anomalously high value

#plot(rootNPercent~inorganicN,data=mean_soil_root_inorganic_2)
outlierTest(lm(rootNPercent~inorganicN,data=mean_soil_root_inorganic_2))
# no outliers
summary(lm(rootNPercent~inorganicN,data=mean_soil_root_inorganic_2))
# significant
# Adjusted R-squared:  0.25

#give ID for plotting
sensitivity.sol.root.lm<-lm(rootNPercent~inorganicN,data=mean_soil_root_inorganic_2)


# plot comparison of total versus inorganic N relationships

pdf(file='./../output/bivar_soil_leaf_root_four_panel.pdf',
    width=8,height=8)
# mar.default <- c(6,3,5,2) + 0.1
# par(mar = mar.default + c(2, 2, 0, 0),mfrow=c(1,4))


layout(matrix(1:4, ncol=2))
par(oma=c(6, 5, 6, 5), mar=c(4, 0, 0, 0),pty='s')
#?par
# Panel label setup
line = 0.75 
cex = 1.0
side = 3
adj= - 0.1

# A: total soil to root N
plot(rootNPercent~soilNPercent_MHoriz_mean,xlab='',ylab="",data=mean_soil_root_2,cex=1.25)
mtext('% Root N',side=2,line=2.25,cex=1.0)
mtext("A", side=side, line=line, cex=cex, adj=adj)
#mtext('% Total soil N (M Horizon)',side=1,line=2.25,cex=1.0)
#text(1, 0.75, 'N.S',cex=1)

# C: total soil leaf N
plot(foliarNPercent_mean ~ soilNPercent_MHoriz_mean,xlab='',ylab="",data=mean_foliar_soil_2,cex=1.25)
mtext("B", side=side, line=line, cex=cex, adj=adj)
mtext('% Leaf N',side=2,line=2.25,cex=1.0)
mtext('% Total soil N (M Horizon)',side=1,line=2.25,cex=1.0)
#text(1, 1, 'N.S',cex=1)

# B: inorganic soil to root N
plot(rootNPercent~inorganicN,xlab='',ylab="",data=mean_soil_root_inorganic_2,cex=1.25)
#mtext('% Root N',side=2,line=2.25,cex=1.0)
abline(sensitivity.sol.root.lm, col="red",lwd=2)
mtext("C", side=side, line=line, cex=cex, adj=adj)
text(1, 0.62, 'R-squared = 0.25 ',cex=1)

# D: total soil leaf N
plot(foliarNPercent_mean ~ inorganicN,xlab='',ylab="",data=mean_foliar_soil_inorganic_2,cex=1.25)
mtext("D", side=side, line=line, cex=cex, adj=adj)
mtext('% Inorganic soil N (M Horizon)',side=1,line=2.25,cex=1.0)
#text(1, 1, 'N.S',cex=1)


dev.off()



#stopped here 2/8/2021

# Root to foliar N relationship --------------------------------------------------


# merge foliar and root data by plot ID
mean_foliar_root <- merge(mean_foliar, mean_root, by = c('siteID', 'plotID'))
length_mean_foliar_root <- aggregate(plotID ~ siteID, length, data = mean_foliar_root)

mean_foliar_root_2 <- mean_foliar_root[-2] %>%
  dplyr::group_by(siteID) %>%
  dplyr::summarise_all(mean)

summary(mean_foliar_root_2)

plot(rootNPercent~foliarNPercent_mean,data=mean_foliar_root_2)
#plot(foliarNPercent_mean~rootNPercent,data=mean_foliar_root_2)

#see if there are outliers
outlierTest(lm(foliarNPercent_mean~rootNPercent,data=mean_foliar_root_2))
#nope

# Get a sense if a linear or nonlinear fit is better
root_leaf_linear<-lm(foliarNPercent_mean~rootNPercent,data=mean_foliar_root_2)
#summary(soil_leaf_linear) #significant R-square = 0.15
root_leaf_nonlinear<- lm(foliarNPercent_mean ~ poly(rootNPercent, 2, raw = TRUE), data = mean_foliar_root_2)
#summary(soil_leaf_nonlinear)
#AIC(root_leaf_linear,root_leaf_nonlinear)
#nonlinear fits better

# set up nonlinear line fit
X <- data.frame(rootNPercent =seq(0.6, 2.4, by=0.001))
X$leaf<-predict(root_leaf_nonlinear,X)

pdf(file='./../output/bivar_root_leaf.pdf',
    width=6,height=6)

layout(matrix(1:1, ncol=1))
par(oma=c(6, 5, 6, 5), mar=c(0, 0, 0, 0),pty='s')

# merge root and foliar N
#mean_foliar_root <- merge(mean_soil_root_2, mean_foliar_soil_2, by = c('siteID'))
plot(foliarNPercent_mean~rootNPercent,xlab='',ylab='', data=mean_foliar_root_2,cex=1.25)
mtext('% Leaf N',side=2,line=3,cex=1.5)
mtext('% Root N',side=1,line=3,cex=1.5,outer=T)
#abline(soil_leaf_nonlinear, col="red",lwd=2)
points(leaf~rootNPercent, col = "red",data=X,pch=19,cex=1)
legend("bottom",paste("R-squared =",round(summary(root_leaf_nonlinear)$r.squared,2)),bty="n",cex = 1.00)

dev.off()

# now look at how predictive leaf N is of root N

#quick outlier test
outlierTest(lm(rootNPercent~foliarNPercent_mean,data=mean_foliar_root_2))

leaf_root_linear<-lm(rootNPercent~foliarNPercent_mean,data=mean_foliar_root_2[-24,])
#summary(leaf_root_linear)
leaf_root_nonlinear<- lm(rootNPercent ~ poly(foliarNPercent_mean, 2, raw = TRUE), data = mean_foliar_root_2)
#summary(leaf_root_nonlinear)

AIC(leaf_root_linear,leaf_root_nonlinear)
#More of a linear relationship

pdf(file='./../output/bivar_leaf_root.pdf',
    width=6,height=6)

layout(matrix(1:1, ncol=1))
par(oma=c(6, 5, 6, 5), mar=c(0, 0, 0, 0),pty='s')

# merge root and foliar N
#mean_foliar_root <- merge(mean_soil_root_2, mean_foliar_soil_2, by = c('siteID'))
plot(rootNPercent~foliarNPercent_mean,xlab='',ylab='', data=mean_foliar_root_2[-24,],cex=1.25)
mtext('% Root N',side=2,line=3,cex=1.5)
mtext('% Leaf N',side=1,line=3,cex=1.5,outer=T)
abline(leaf_root_linear, col="red",lwd=3)
legend(0.5,1.4,paste("R-squared =",round(summary(leaf_root_linear)$r.squared,2)),bty="n",cex = 1.00)

dev.off()



# Mixed effects models ---------------------------------------------------------

# since we have stablished inorganic N is 'better', we will use this in the % N models models

head(plot.df)

# plot(foliarNPercent_mean~inorganicN,data=mean_foliar_lme)
# plot(rootNPercent ~inorganicN,data=plot.df)

# Do mixed effects analysis for leaf N
library(lme4)

mean_foliar_lme<-aggregate(foliarNPercent_mean~siteID + plotID
                            + inorganicN + Lcclass,mean,data=plot.df)
#head(mean_foliar_lme)

#rename to veg type to herb versus woody. 'Croplands' are deemed herbaceous
mean_foliar_lme <- rename_lcc(mean_foliar_lme,crop = T)
# head(mean_foliar_lme)
# unique(mean_foliar_lme$Lcclass)

# get aridity data
vpd <- read.csv('./../data_pre-processed/scaled_vpd.csv')
head(vpd)

#cleanup
vpd<-vpd[c(2,3)]
colnames(vpd) <- c('siteID','vpd')

# merge with vpd data frame
mean_foliar_lme <- merge(vpd,mean_foliar_lme,by=c('siteID'))
#head(mean_foliar_lme)

#check sample sizes
length_foliar_root_lme<-aggregate(foliarNPercent_mean~siteID,length,data=mean_foliar_lme)

#remove site with only one rep
mean_foliar_lme <- mean_foliar_lme %>%
  dplyr::filter(!(siteID=="WREF"))
unique(mean_foliar_lme$siteID) # works

# lme functions lets you see P values in summary output
leaf_lme.1<-lme(foliarNPercent_mean~ inorganicN + vpd + Lcclass, random= ~1|siteID,data=mean_foliar_lme)
summary(leaf_lme.1) #only significant factor is inroganic N
# r.squaredGLMM(leaf_lme.1)
leaf_lme.2<-lmer(foliarNPercent_mean~ inorganicN + vpd  + Lcclass + (1|siteID),data=mean_foliar_lme)
#summary(leaf_lme.2)
#r.squaredGLMM(leaf_lme.2)
#conditional higher than marginal.

# see if random site effects 'improves' the model, compare to a multiple regression
leaf_lm.1<-lm(foliarNPercent_mean~ inorganicN + vpd + Lcclass,data=mean_foliar_lme)
AIC(leaf_lm.1,leaf_lme.1)
# adding site random effects lowers the AIC for predicting leaf N

#interesting that leaf-soil N is significant when we don't average the sites, the relationship
# ooks somewhat saturating 
plot(foliarNPercent_mean~ inorganicN,data=mean_foliar_lme)

#make a data frame of this
source<-c('Marginal','Conditional')
value<-c(0.41,0.69)
cond.marg.lead<-data.frame(source,value)
cond.marg.lead$pool<-'Leaf'

#
#

# Do mixed effects analysis for root N, same work flow

mean_root_lme <- aggregate(rootNPercent ~ siteID + plotID + inorganicN +
                            Lcclass, mean, data = plot.df)
#head(mean_root_lme)

# Again, remove pctSand for now, as it made the data frame too small for an LME

#rename veg type
mean_root_lme <- rename_lcc(mean_root_lme,crop=F)
#head(mean_root_lme)

#merge the vpd data frame
mean_root_lme <- merge(vpd,mean_root_lme,by=c('siteID'))
#head(mean_root_lme)

#check sample sizes
length_mean_root_lme<-aggregate(rootNPercent~siteID,length,data=mean_root_lme)
# lower than foliar, but looks ok

#now do lmes
root_lme.1<-lme(rootNPercent~ inorganicN + vpd + Lcclass , random= ~1|siteID,data=mean_root_lme)
summary(root_lme.1) # iorganic N barely significant in this case
r.squaredGLMM(root_lme.1)

root_lme.2<-lmer(rootNPercent ~ inorganicN + vpd  + Lcclass + (1|siteID),data=mean_root_lme)
summary(root_lme.2)
r.squaredGLMM(root_lme.2) 

#see if adding random effects 'improves' the model
root_lm.1<-lm(rootNPercent ~ inorganicN + vpd  + Lcclass,data=mean_root_lme)
summary(root_lm.1)

AIC(root_lm.1,root_lme.1)
# multiple regression has lower AaIC

length(length_mean_root_lme$siteID) # 15 sites
dim(mean_root_lme)
length(length_foliar_root_lme$siteID) # 15 sites
dim(mean_foliar_lme)

#more data in the foliar dataset than root dataset

#make a data frame of this
source<-c('Marginal','Conditional')
value<-c(0.11,0.46) # will vary depndening on fixed effects
cond.marg.root<-data.frame(source,value)
cond.marg.root$pool<-'Root'

cond.marg.leaf.root<-rbind(cond.marg.lead,cond.marg.root)

pdf(file='./../output/cond.mar.leaf.root.pdf',
    width=8,height=8)

# stacked bar plot
ggplot(cond.marg.leaf.root,aes(x=pool,y=value,fill=source)) +
  scale_y_continuous(expand = c(0,0)) + #for bar plot
  scale_fill_manual(values=c('Marginal'='grey','Conditional'='blue'),
                    labels=c('Marginal'='Marginal','Conditional'='Conditional')) +
  stat_summary(geom='bar',position="stack",fun='mean',color='black',size=0.25) +
  scale_x_discrete(breaks=c("Leaf","Root"),
                   labels=c('Leaf', "Root")) +
  ylab('R-squared') +
  xlab('') +
  #labs(subtitle = "A") +
  theme(
    axis.text.x = element_text(color='black',size=15),#, angle=25,hjust=1),
    axis.text.y = element_text(color='black',size=15),
    axis.title = element_text(color='black',size=25),
    axis.ticks = element_line(color='black'),
    legend.key = element_blank(),
    legend.title = element_blank(),
    legend.text = element_text(size=15),
    legend.position = "top",
    strip.background =element_rect(fill="white"),
    strip.text = element_text(size=7),
    panel.background = element_rect(fill=NA),
    panel.border = element_blank(), #make the borders clear in prep for just have two axes
    axis.line.x = element_line(colour = "black"),
    axis.line.y = element_line(colour = "black"))
 
dev.off()

# stopped here 2/12/2021

# plant feedbacks to soil N ----------------------------------------------------

head(plot.df)

#plot and site means
mean_litter<-aggregate(litterNPercent_mean~siteID + plotID,mean,data=plot.df)
mean_resorp<-aggregate(resorpN~siteID + plotID,mean,data=plot.df)

# Litter and total soil N
mean_litter_total_soil <- merge(mean_litter, mean_soil, by = c('siteID', 'plotID'))
length_mean_litter_total_soil <- aggregate(plotID ~ siteID, length, data = mean_litter_total_soil)
plot(soilNPercent_MHoriz_mean~litterNPercent_mean,data=mean_litter_total_soil)
summary(lm(soilNPercent_MHoriz_mean~litterNPercent_mean,data=mean_litter_total_soil))
outlierTest(lm(soilNPercent_MHoriz_mean~litterNPercent_mean,data=mean_litter_total_soil))

#without outlier
plot(soilNPercent_MHoriz_mean~litterNPercent_mean,data=mean_litter_total_soil[-c(22,48),])
summary(lm(soilNPercent_MHoriz_mean~litterNPercent_mean,data=mean_litter_total_soil[-c(48,22),]))
# Adjusted R-squared: 0.1194 , p-value: 0.0015

# Get site means
mean_litter_total_soil_2 <- mean_litter_total_soil[-2] %>%
  dplyr::group_by(siteID) %>%
  dplyr::summarise_all(mean) #%>%
  #dplyr::filter(soilNPercent_MHoriz_mean < 1)

plot(soilNPercent_MHoriz_mean~litterNPercent_mean,data=mean_litter_total_soil_2)
outlierTest(lm(soilNPercent_MHoriz_mean~litterNPercent_mean,data=mean_litter_total_soil_2)) # 12
litter_soil_lm<-lm(soilNPercent_MHoriz_mean~litterNPercent_mean,data=mean_litter_total_soil_2[-12,])
summary(litter_soil_lm)
# Adjusted R-squared:  0.2235, p-value: 0.02361
plot(soilNPercent_MHoriz_mean~litterNPercent_mean,data=mean_litter_total_soil_2[-c(12),])

#what if we remove that one point that seems to be driving the relationship?
plot(soilNPercent_MHoriz_mean~litterNPercent_mean,data=mean_litter_total_soil_2[-c(6,12),])
summary(lm(soilNPercent_MHoriz_mean~litterNPercent_mean,data=mean_litter_total_soil_2[-c(6,12),]))
#significance was driven by that single point with high soil and high litter N (GUAN)


# Resorption and soil N
mean_resorp_total_soil <- merge(mean_resorp, mean_soil, by = c('siteID', 'plotID'))
length_mean_resorp_total_soil <- aggregate(plotID ~ siteID, length, data = mean_resorp_total_soil)

# Get site means
mean_resorp_total_soil_2 <- mean_resorp_total_soil[-2] %>%
  dplyr::group_by(siteID) %>%
  dplyr::summarise_all(mean) #%>%
  #dplyr::filter(soilNPercent_MHoriz_mean < 1)

plot(soilNPercent_MHoriz_mean~resorpN,data=mean_resorp_total_soil_2) # no relationship
outlierTest(lm(soilNPercent_MHoriz_mean~resorpN,data=mean_resorp_total_soil_2))
#try without loutlier
resorp_soil_lm<-lm(soilNPercent_MHoriz_mean~resorpN,data=mean_resorp_total_soil_2[-10,])
summary(resorp_soil_lm)
plot(soilNPercent_MHoriz_mean~resorpN,data=mean_resorp_total_soil_2[-10,])


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
plot(soilNPercent_MHoriz_mean~litterNPercent_mean,xlab='',ylab="% Soil N",data=mean_litter_total_soil_2[-12,])
mtext('% Litter N',side=1,line=2.25,cex=1.0)
mtext("A", side=side, line=line, cex=cex, adj=adj)
#text(0.6, 0.75, 'N.S',cex=1)
legend("top",paste("R-squared =",round(summary(litter_soil_lm)$r.squared,2)),bty="n",cex = 1.00)

# high value appears to be driving this significant relationship

# B: resorp to soil N
plot(soilNPercent_MHoriz_mean~resorpN,xlab='',ylab="",data=mean_resorp_soil_2[-14,])
mtext('N Resporption',side=1,line=2.25,cex=1.0)
mtext("B", side=side, line=line, cex=cex, adj=adj)
text(35, 0.22, 'N.S',cex=1)

dev.off()

#now this with inorganic N

#plot and site means
mean_litter<-aggregate(litterNPercent_mean~siteID + plotID,mean,data=plot.df)
mean_resorp<-aggregate(resorpN~siteID + plotID,mean,data=plot.df)

# Litter and total soil N
mean_litter_inorganic_soil <- merge(mean_litter, mean_soil_inorganic, by = c('siteID', 'plotID'))
length_mean_litter_inorganic_soil <- aggregate(plotID ~ siteID, length, data = mean_litter_inorganic_soil)
plot(inorganicN~litterNPercent_mean,data=mean_litter_inorganic_soil)

# Get site means
mean_litter_total_soil_2 <- mean_litter_inorganic_soil[-2] %>%
  dplyr::group_by(siteID) %>%
  dplyr::summarise_all(mean) #%>%
#dplyr::filter(soilNPercent_MHoriz_mean < 1)

plot(inorganicN~litterNPercent_mean,data=mean_litter_total_soil_2)
summary(lm(inorganicN~litterNPercent_mean,data=mean_litter_total_soil_2))

#see if this changes if we remove large value
plot(inorganicN~litterNPercent_mean,data=mean_litter_total_soil_2[-5,])
summary(lm(inorganicN~litterNPercent_mean,data=mean_litter_total_soil_2[-5,]))

#again mostly driven by high value

# stopped here.
