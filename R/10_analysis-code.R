# last updated: 2020-10-26
# author: Adrienne Keller
# project: NEON N scaling
# notes:

################################################################################

### load functions

### load packages
pkgs <- c("dplyr", "ggplot2", "nlme", "car")
lapply(pkgs, require, character.only = T)

### load compiled data
source(file = "R/03_compile-data.R")

### univariate distributions of N pools
pdf(file='output/univar-hist.pdf',
    width=6,height=4)
mar.default <- c(6,3,5,2) + 0.1
par(mar = mar.default + c(2, 2, 0, 0),mfrow=c(1,4))
hist(plot.df$soilNPercent_MHoriz,main='',xlab='% Soil N (min hor)',cex.lab=1.75)
abline(v = mean(plot.df$soilNPercent_MHoriz, na.rm = T), col = "red", lwd = 2)
hist(plot.df$foliarNPercent,main='',xlab='% Foliar N',ylab='',cex.lab=1.75)
abline(v = mean(plot.df$foliarNPercent, na.rm = T), col = "red", lwd = 2)
hist(plot.df$litterNPercent,main='',xlab='% Litter N',ylab='',cex.lab=1.75)
abline(v = mean(plot.df$litterNPercent, na.rm = T), col = "red", lwd = 2)
hist(plot.df$rootNPercent, main = '', xlab = "% Root N", cex.lab = 1.75)
abline(v = mean(plot.df$rootNPercent, na.rm = T), col = "red", lwd = 2)
dev.off()

### bivariate relationships between N pools - cross-site
# first check that sample sizes will be > five for each site
sample_size_foliar<-aggregate(foliarNPercent~siteID,length,data=plot.df)
sample_size_litter<-aggregate(litterNPercent~siteID,length,data=plot.df) # KONZ only 4
sample_size_soil<-aggregate(soilNPercent_MHoriz~siteID,length,data=plot.df)
# site HEAL has sample size of 1 for soil N, gets removed from analysis
#  anyways because there are no data from any of the other three pools

# Litter N ~ Foliar N
plot(litterNPercent~foliarNPercent,data=plot.df)
foliar_to_litter_lm<-lm(litterNPercent ~ foliarNPercent,data=plot.df) 
outlierTest(foliar_to_litter_lm) #no clear outliers, so proceed
summary(foliar_to_litter_lm)
#R2 = 0.31, P < 0.001, slope = 0.29


plot.df.orig <- plot.df
plot.df <- plot.df %>% filter(siteID != "GUAN")
