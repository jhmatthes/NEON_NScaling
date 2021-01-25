# last updated: 2020-10-26
# author: Adrienne Keller
# project: NEON N scaling
# notes:

################################################################################

### Figure 1: Map of sites (perhaps overlay on a climate and/or veg type layer)
# map paired with table of sample size per site for each 'pool' (foliar, litter, 
# soil, root) % N

# Distributions of N pools (Figure 2)-------------------------------------------
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
mtext('% Soil N (mineral horizon)',side=1,line=2.2,cex=0.75)
mtext("A", side=side, line=line, cex=cex, adj=adj)

# C
hist(plot.df$litterNPercent_mean,main='',xlab='',ylab='',cex.lab=1.75)
abline(v = mean(plot.df$litterNPercent, na.rm = T), col = "red", lwd = 3)
mtext('% Litter N',side=1,line=2.2,cex=0.75)
mtext("C", side=side, line=line, cex=cex, adj=adj)

# B
hist(plot.df$foliarNPercent_mean,main='',xlab='',ylab='',cex.lab=1.75)
abline(v = mean(plot.df$foliarNPercent_mean, na.rm = T), col = "red", lwd = 3)
mtext('% Foliar N',side=1,line=2.2,cex=0.75)
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
hist(log(plot.df$soilNPercent_MHoriz_mean))
plot.df %>% filter(siteID != "GUAN") %>% 
  ggplot() +
  geom_histogram(aes(log(soilNPercent_MHoriz_mean), fill = siteID))

# Bivariate relationships between N pools - cross-site with each point = site mean (Figure 3) ----

# first check that sample sizes (will be > five for each site?)
sample_size_foliar<-aggregate(foliarNPercent_mean~siteID,length,data=plot.df)
sample_size_litter<-aggregate(litterNPercent_mean~siteID,length,data=plot.df) # KONZ only 4
sample_size_soil<-aggregate(soilNPercent_MHoriz_mean~siteID,length,data=plot.df) 
sample_size_root <- aggregate(rootNPercent ~ siteID, length, data = plot.df)

# HEAL has sample size of 1 for soil N, this gets removed from analysis
# anyways because there are no data from any of the other three pools

# Now look at what sites have co-located foliar, root, litter and soil data
# get mean plot value for each plot

mean_foliar<-aggregate(foliarNPercent_mean~siteID + plotID,mean,data=plot.df)
mean_root <- aggregate(rootNPercent ~ siteID + plotID, mean, data = plot.df)
mean_soil<-aggregate(soilNPercent_MHoriz_mean~siteID + plotID,mean,data=plot.df)

# # merge foliar and litter data by plot ID
# mean_foliar_litter<-merge(mean_foliar,mean_litter,by=c('siteID','plotID'))
# length_mean_foliar_litter<-aggregate(plotID~siteID,length,data=mean_foliar_litter)
# 
# mean_foliar_litter_2 <- mean_foliar_litter[-2] %>%
#   dplyr::group_by(siteID) %>%
#   dplyr::summarise_all(mean) 
#   
# plot(litterNPercent_mean~foliarNPercent_mean,data=mean_foliar_litter_2)


# merge foliar and soil data by plot ID
mean_foliar_soil <- merge(mean_foliar, mean_soil, by = c('siteID', 'plotID'))
length_mean_foliar_soil <- aggregate(plotID ~ siteID, length, data = mean_foliar_soil)

# Get site means for foliar N
mean_foliar_soil_2 <- mean_foliar_soil[-2] %>%
  dplyr::group_by(siteID) %>%
  dplyr::summarise_all(mean) #%>%
  #dplyr::filter(soilNPercent_MHoriz_mean < 1) # get rid of anomalously high value

#plot(foliarNPercent_mean ~ soilNPercent_MHoriz_mean,data=mean_foliar_soil_2)
#summary(lm(foliarNPercent_mean ~ soilNPercent_MHoriz_mean,data=mean_foliar_soil_2)) 
#not significant with or without high soil N values

# merge root and soil data by plot ID
mean_soil_root <- merge(mean_soil, mean_root, by = c('siteID', 'plotID'))
length_mean_soil_root <- aggregate(plotID ~ siteID, length, data = mean_soil_root)

mean_soil_root_2 <- mean_soil_root[-2] %>%
  dplyr::group_by(siteID) %>%
  dplyr::summarise_all(mean) #%>%
  #dplyr::filter(soilNPercent_MHoriz_mean < 1) # get rid of anomalously high value

#plot(rootNPercent~soilNPercent_MHoriz_mean,data=mean_soil_root_2)
#summary(lm(rootNPercent~soilNPercent_MHoriz_mean,data=mean_soil_root_2)) #not significant


#plot this out

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
leaf_root_linear<-lm(rootNPercent~foliarNPercent_mean,data=mean_foliar_root_2)
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
plot(rootNPercent~foliarNPercent_mean,xlab='',ylab='', data=mean_foliar_root_2,cex=1.25)
mtext('% Root N',side=2,line=3,cex=1.5)
mtext('% Leaf N',side=1,line=3,cex=1.5,outer=T)
abline(leaf_root_linear, col="red",lwd=3)
legend(0.5,1.4,paste("R-squared =",round(summary(leaf_root_linear)$r.squared,2)),bty="n",cex = 1.00)

dev.off()



# Mixed effects models ---------------------------------------------------------


head(plot.df)

# head(plot.df)

# Do mixed effects analysis for leaf N
library(lme4)

mean_foliar_lme<-aggregate(foliarNPercent_mean~siteID + plotID
                           + pctSand + Lcclass,mean,data=plot.df)

head(mean_foliar_lme)

#rename veg type
mean_foliar_lme <- rename_lcc(mean_foliar_lme)
head(mean_foliar_lme)
unique(mean_foliar_lme$Lcclass)

# get aridity data
vpd <- read.csv('./../data_pre-processed/scaled_vpd.csv')
head(vpd)

#cleanup
vpd<-vpd[c(2,3)]
colnames(vpd) <- c('siteID','vpd')

#merge with vpd data frame
mean_foliar_lme <- merge(vpd,mean_foliar_lme,by=c('siteID'))
head(mean_foliar_lme)

# I would like to include vegetation in this, but we I think we to
# simplify it so we have fewer veg levels with more data in them: woody versus herbaceous? forest versus grassland? 
library(lme4)
leaf_lme<-lmer(foliarNPercent_mean~ vpd + pctSand + Lcclass + (1|siteID),data=mean_foliar_lme)
summary(leaf_lme)
r.squaredGLMM(leaf_lme) 
#conditional way higher than marginal 

# Do mixed effects analysis for root N

mean_root_lme <- aggregate(rootNPercent ~ siteID + plotID
                           + pctSand + pctSand + Lcclass, mean, data = plot.df)

#rename veg type
mean_root_lme <- rename_lcc(mean_root_lme)
head(mean_root_lme)

#merge the vpd data frame
mean_root_lme <- merge(vpd,mean_root_lme,by=c('siteID'))
head(mean_root_lme)

root_lme<-lmer(rootNPercent ~ vpd + pctSand + Lcclass + (1|siteID),data=mean_root_lme)
summary(root_lme)
r.squaredGLMM(root_lme) 
#conditional still higher than marginal

# stopped here 1/25/2021

# plant feedbacks to soil N ----------------------------------------------------

mean_litter<-aggregate(litterNPercent_mean~siteID + plotID,mean,data=plot.df)
mean_resorp<-aggregate(resorpN~siteID + plotID,mean,data=plot.df)

# Litter and soil N
mean_litter_soil <- merge(mean_litter, mean_soil, by = c('siteID', 'plotID'))
length_mean_litter_soil <- aggregate(plotID ~ siteID, length, data = mean_litter_soil)

# Get site means
mean_litter_soil_2 <- mean_litter_soil[-2] %>%
  dplyr::group_by(siteID) %>%
  dplyr::summarise_all(mean) %>%
  dplyr::filter(soilNPercent_MHoriz_mean < 1)


# Resorption and soil N
mean_resorp_soil <- merge(mean_resorp, mean_soil, by = c('siteID', 'plotID'))
length_mean_resorp_soil <- aggregate(plotID ~ siteID, length, data = mean_resorp_soil)

# Get site means
mean_resorp_soil_2 <- mean_resorp_soil[-2] %>%
  dplyr::group_by(siteID) %>%
  dplyr::summarise_all(mean) %>%
  dplyr::filter(soilNPercent_MHoriz_mean < 1)


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
plot(soilNPercent_MHoriz_mean~litterNPercent_mean,xlab='',ylab="",data=mean_litter_soil_2)
mtext('% Litter N',side=1,line=2.25,cex=1.0)
mtext("A", side=side, line=line, cex=cex, adj=adj)
text(0.6, 0.75, 'N.S',cex=1)

# B: resorp to soil N
plot(soilNPercent_MHoriz_mean~resorpN,xlab='',ylab="",data=mean_resorp_soil_2)
mtext('N Resporption',side=1,line=2.25,cex=1.0)
mtext("B", side=side, line=line, cex=cex, adj=adj)
#text(0.6, 0.75, 'N.S',cex=1)

# no clear relationships

dev.off()

# stopped here AJF 1/13/2021
