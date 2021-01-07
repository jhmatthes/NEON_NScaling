# last updated: 2020-10-26
# author: Adrienne Keller
# project: NEON N scaling
# notes:

################################################################################

### Figure 1: Map of sites (perhaps overlay on a climate and/or veg type layer)
# map paired with table of sample size per site for each 'pool' (foliar, litter, 
# soil, root) % N

### univariate distributions of N pools (Figure 2)----
pdf(file='output/univar-hist.pdf',
    width=6,height=4)
mar.default <- c(6,3,5,2) + 0.1
par(mar = mar.default + c(2, 2, 0, 0),mfrow=c(1,4))
hist(plot.df$soilNPercent_MHoriz_mean,main='',xlab='% Soil N (min hor)',cex.lab=1.75)
abline(v = mean(plot.df$soilNPercent_MHoriz_mean, na.rm = T), col = "red", lwd = 2)
hist(plot.df$foliarNPercent_mean,main='',xlab='% Foliar N',ylab='',cex.lab=1.75)
abline(v = mean(plot.df$foliarNPercent_mean, na.rm = T), col = "red", lwd = 2)
hist(plot.df$litterNPercent_mean,main='',xlab='% Litter N',ylab='',cex.lab=1.75)
abline(v = mean(plot.df$litterNPercent, na.rm = T), col = "red", lwd = 2)
hist(plot.df$rootNPercent, main = '', xlab = "% Root N", cex.lab = 1.75)
abline(v = mean(plot.df$rootNPercent, na.rm = T), col = "red", lwd = 2)
dev.off()
# note: soil N mineral horizon is heavily right-skewed (even when excluding GUAN);
# log-transform
hist(log(plot.df$soilNPercent_MHoriz_mean))
plot.df %>% filter(siteID != "GUAN") %>% 
  ggplot() +
  geom_histogram(aes(log(soilNPercent_MHoriz_mean), fill = siteID))

### bivariate relationships between N pools - cross-site with each point = site mean (Figure 3) ----
# first check that sample sizes will be > five for each site
sample_size_foliar<-aggregate(foliarNPercent_mean~siteID,length,data=plot.df)
sample_size_litter<-aggregate(litterNPercent_mean~siteID,length,data=plot.df) # KONZ only 4
sample_size_soil<-aggregate(soilNPercent_MHoriz_mean~siteID,length,data=plot.df) 
# HEAL has sample size of 1 for soil N, gets removed from analysis
#  anyways because there are no data from any of the other three pools

# now look at what sites have co-located foliar, root, litter and soil data
# get mean plot value for each plot
mean_foliar<-aggregate(foliarNPercent_mean~siteID + plotID,mean,data=plot.df)
mean_root <- aggregate(rootNPercent ~ siteID + plotID, mean, data = plot.df)
mean_litter<-aggregate(litterNPercent_mean~siteID + plotID,mean,data=plot.df)
mean_soil<-aggregate(soilNPercent_MHoriz_mean~siteID + plotID,mean,data=plot.df)
# merge foliar and root data by plot ID
mean_foliar_root <- merge(mean_foliar, mean_root, by = c('siteID', 'plotID'))
length_mean_foliar_root <- aggregate(plotID ~ siteID, length, data = mean_foliar_root)
# merge foliar and litter data by plot ID
mean_foliar_litter<-merge(mean_foliar,mean_litter,by=c('siteID','plotID'))
length_mean_foliar_litter<-aggregate(plotID~siteID,length,data=mean_foliar_litter)
# merge foliar and soil data by plot ID
mean_foliar_soil <- merge(mean_foliar, mean_soil, by = c('siteID', 'plotID'))
length_mean_foliar_soil <- aggregate(plotID ~ siteID, length, data = mean_foliar_soil)

# bivariate regressions of plot means
# foliar to root
foliar_to_root_lm <- lm(rootNPercent ~ foliarNPercent_mean, data = plot.df)
summary(foliar_to_root_lm)
ggplot(plot.df, aes(x = foliarNPercent_mean, y = rootNPercent)) +
  geom_point(aes(colour = siteID, size = 4))
# foliar to litter
foliar_to_litter_lm <- lm(litterNPercent_mean ~ foliarNPercent_mean, data = plot.df)
summary(foliar_to_litter_lm)
ggplot(plot.df, aes(x = foliarNPercent_mean, y = litterNPercent_mean)) +
  geom_point(aes(colour = siteID, size = 4))
# foliar to soil
foliar_to_soil_lm <- lm(soilNPercent_MHoriz_mean ~ foliarNPercent_mean, data = plot.df)
summary(foliar_to_soil_lm)
ggplot(plot.df, aes(x = foliarNPercent_mean, y = soilNPercent_MHoriz_mean)) +
  geom_point(aes(colour = siteID, size = 4))
# litter to soil
litter_to_soil_lm <- lm(litterNPercent_mean ~ soilNPercent_MHoriz_mean, data = plot.df)
summary(litter_to_soil_lm)
ggplot(plot.df, aes(x = litterNPercent_mean, y = soilNPercent_MHoriz_mean)) +
  geom_point(aes(colour = siteID, size = 4))

### Bivariate regressions with C:N (rather than N concentrations)
# foliar to root CN
foliar_to_root_CN_lm <- lm(rootCNratio ~ foliarCNRatio_mean, data = plot.df)
summary(foliar_to_root_CN_lm)
ggplot(plot.df, aes(x = foliarCNRatio_mean, y = rootCNratio)) + 
  geom_point(aes(colour = siteID, size = 3)) + 
  geom_smooth(method = "lm") +
  theme(legend.position = 'none') 
# foliar to litter CN
foliar_to_litter_CN_lm <- lm(litterCNRatio_mean ~ foliarCNRatio_mean, data = plot.df)
summary(foliar_to_litter_CN_lm)
ggplot(plot.df, aes(x = foliarCNRatio_mean, y = litterCNRatio_mean)) + 
  geom_point(aes(colour = siteID, size = 3)) + 
  geom_smooth(method = "lm") +
  theme(legend.position = 'none') 
# foliar to soil CN
foliar_to_soil_CN_lm <- lm(soilCNRatio_MHoriz_mean ~ foliarCNRatio_mean, data = plot.df)
summary(foliar_to_soil_CN_lm)
ggplot(plot.df, aes(x = foliarCNRatio_mean, y = soilCNRatio_MHoriz_mean)) + 
  geom_point(aes(colour = siteID, size = 3)) + 
  geom_smooth(method = "lm") +
  theme_basic + 
  theme(legend.position = 'none') 
# litter to soil CN
litter_to_soil_CN_lm <- lm(soilCNRatio_MHoriz_mean ~ litterCNRatio_mean, data = plot.df)
summary(litter_to_soil_CN_lm)
ggplot(plot.df, aes(x = litterCNRatio_mean, y = soilCNRatio_MHoriz_mean)) + 
  geom_point(aes(colour = siteID, size = 3)) + 
  geom_smooth(method = "lm") +
  theme_basic + 
  theme(legend.position = 'none') 

### Mixed models to consider effect of site on bivariate relationships 
# without and with MAP, veg type, and MAP + veg type as covariates
# foliar to root
foliar_to_root_lmer <- lme(rootNPercent ~ foliarNPercent_mean, ~1|siteID, data = plot.df,
                           na.action = na.omit, method = 'ML')
foliar_to_root_lmer_MAP <- lme(rootNPercent ~ foliarNPercent_mean + MAP, ~1|siteID, data = plot.df,
                           na.action = na.omit, method = 'ML')
anova(foliar_to_root_lmer, foliar_to_root_lmer_MAP)
summary(foliar_to_root_lmer)
r.squaredGLMM(foliar_to_root_lmer)
# foliar to litter
foliar_to_litter_lmer <- lme(litterNPercent_mean ~ foliarNPercent_mean, ~1|siteID, 
                           data = plot.df, na.action = na.omit, method = 'ML')
foliar_to_litter_lmer_MAP <- lme(litterNPercent_mean ~ foliarNPercent_mean + MAP, ~1|siteID, 
                             data = plot.df, na.action = na.omit, method = 'ML')
anova(foliar_to_litter_lmer, foliar_to_litter_lmer_MAP)
summary(foliar_to_litter_lmer)
r.squaredGLMM(foliar_to_litter_lmer)
# foliar to soil
foliar_to_soil_lmer <- lme(soilNPercent_MHoriz_mean ~ foliarNPercent_mean, ~1|siteID, 
                           data = plot.df, na.action = na.omit, method = "ML")
foliar_to_soil_lmer_MAP <- lme(soilNPercent_MHoriz_mean ~ foliarNPercent_mean + MAP, ~1|siteID, 
                           data = plot.df, na.action = na.omit, method = "ML")
anova(foliar_to_soil_lmer, foliar_to_soil_lmer_MAP)
summary(foliar_to_soil_lmer)
r.squaredGLMM(foliar_to_soil_lmer)
# litter to soil
litter_to_soil_lmer <- lme(litterNPercent_mean ~ soilNPercent_MHoriz_mean, ~1|siteID, 
                           data = plot.df, na.action = na.omit, method = "ML")
litter_to_soil_lmer_MAP <- lme(litterNPercent_mean ~ soilNPercent_MHoriz_mean + MAP, ~1|siteID, 
                           data = plot.df, na.action = na.omit, method = "ML")
anova(litter_to_soil_lmer, litter_to_soil_lmer_MAP)
summary(litter_to_soil_lmer)
r.squaredGLMM(litter_to_soil_lmer)


