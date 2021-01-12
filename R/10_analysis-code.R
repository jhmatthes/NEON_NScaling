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
line =0.75 
cex = 1.0
side = 3
adj=-0.5

# A
hist(plot.df$soilNPercent_MHoriz_mean,main='',xlab='',ylab='',cex.lab=1.75)
abline(v = mean(plot.df$soilNPercent_MHoriz_mean, na.rm = T), col = "red", lwd = 2)
mtext('% Soil N (mineral horizon)',side=1,line=2.2,cex=1)
mtext("A", side=side, line=line, cex=cex, adj=adj)

# C
hist(plot.df$litterNPercent_mean,main='',xlab='',ylab='',cex.lab=1.75)
abline(v = mean(plot.df$litterNPercent, na.rm = T), col = "red", lwd = 2)
mtext('% Litter N',side=1,line=2.2,cex=1)
mtext("C", side=side, line=line, cex=cex, adj=adj)

# B
hist(plot.df$foliarNPercent_mean,main='',xlab='',ylab='',cex.lab=1.75)
abline(v = mean(plot.df$foliarNPercent_mean, na.rm = T), col = "red", lwd = 2)
mtext('% Foliar N',side=1,line=2.2,cex=1)
mtext("B", side=side, line=line, cex=cex, adj=adj)


# D
hist(plot.df$rootNPercent, main = '', xlab = "", cex.lab = 1.75)
abline(v = mean(plot.df$rootNPercent, na.rm = T), col = "red", lwd = 2)
mtext('% Root N',side=1,line=2.2,cex=1)
mtext("D", side=side, line=line, cex=cex, adj=adj)

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

# HEAL has sample size of 1 for soil N, gets removed from analysis
# anyways because there are no data from any of the other three pools

# Now look at what sites have co-located foliar, root, litter and soil data
# get mean plot value for each plot

mean_foliar<-aggregate(foliarNPercent_mean~siteID + plotID,mean,data=plot.df)
mean_root <- aggregate(rootNPercent ~ siteID + plotID, mean, data = plot.df)
mean_litter<-aggregate(litterNPercent_mean~siteID + plotID,mean,data=plot.df)
mean_soil<-aggregate(soilNPercent_MHoriz_mean~siteID + plotID,mean,data=plot.df)

# # merge foliar and root data by plot ID
# mean_foliar_root <- merge(mean_foliar, mean_root, by = c('siteID', 'plotID'))
# length_mean_foliar_root <- aggregate(plotID ~ siteID, length, data = mean_foliar_root)
# 
# mean_foliar_root_2 <- mean_foliar_root[-2] %>%
#   dplyr::group_by(siteID) %>%
#   dplyr::summarise_all(mean) 
# 
# plot(foliarNPercent_mean~rootNPercent,data=mean_foliar_root_2)

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

# Get site means
mean_foliar_soil_2 <- mean_foliar_soil[-2] %>%
  dplyr::group_by(siteID) %>%
  dplyr::summarise_all(mean) %>%
  dplyr::filter(soilNPercent_MHoriz_mean < 1) # get rid of anomalously high value

plot(foliarNPercent_mean ~ soilNPercent_MHoriz_mean,data=mean_foliar_soil_2)

# merge root and soil data by plot ID
mean_soil_root <- merge(mean_soil, mean_root, by = c('siteID', 'plotID'))
length_mean_soil_root <- aggregate(plotID ~ siteID, length, data = mean_foliar_root)

mean_soil_root_2 <- mean_soil_root[-2] %>%
  dplyr::group_by(siteID) %>%
  dplyr::summarise_all(mean) %>%
  dplyr::filter(soilNPercent_MHoriz_mean < 1) # get rid of anomalously high value

plot(rootNPercent~soilNPercent_MHoriz_mean,data=mean_soil_root_2)

#merge root and foliar N
mean_foliar_root <- merge(mean_soil_root_2, mean_foliar_soil_2 , by = c('siteID'))
plot(foliarNPercent_mean~rootNPercent,data=mean_foliar_root)
summary(lm(foliarNPercent_mean~rootNPercent,data=mean_foliar_root))

#stopped here


