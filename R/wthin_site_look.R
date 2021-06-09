
#Within-site look between foliar and soil N

#foliar N and total soil N
# the cross-site relationship was not significant 

# re-run this bit of code if didn't run analysis code:

#start
# merge_foliar_soil_means <-filter_reps(mean_foliar, mean_soil)
# 
# length(merge_foliar_soil_means$siteID) 
# #23 with the limit as 4
# 
# #round to 2 decimal points
# merge_foliar_soil_means$foliarNPercent_mean<-round(merge_foliar_soil_means$foliarNPercent_mean,2)
# merge_foliar_soil_means$soilNPercent_MHoriz_mean<-round(merge_foliar_soil_means$soilNPercent_MHoriz_mean,2)
# 
# #add veg type
# merge_foliar_soil_means <- merge(merge_foliar_soil_means,vegtype.df,by='siteID')
#end

#minimum of 10 within-site reps
within_site_foliar_total <- merge_foliar_soil_means %>%
  dplyr::filter(reps > 9)

within_site_foliar_total <- merge(plot.df.2,within_site_foliar_total[c(1,2)],by='siteID')

#visualize

# # put lines only for significant slopes
# p.vals = sapply(unique(within_site_foliar_total$siteID), function(i) {
#   coef(summary(lm(foliarNPercent_mean ~ soilNPercent_MHoriz_mean, 
#                   data=within_site_foliar_total[siteID==i, ])))[2,4]
# })

ggplot(within_site_foliar_total,aes(soilNPercent_MHoriz_mean,foliarNPercent_mean,fill=Lcclass)) +
  facet_wrap(~siteID,scales = 'free') +
  geom_point(color='black',pch=21,alpha=0.5) +
  scale_fill_manual(values=c('herb'='blue','woody'='red'),
                    labels=c('herb'='Herbaceous','woody'='Woody')) +
  xlab('% Soil N') +
  ylab('% Foliar N') +
  theme(
    axis.text.x = element_text(color='black',size=11), #angle=25,hjust=1),
    axis.text.y = element_text(color='black',size=11),
    axis.title.x = element_text(color='black',size=15),
    axis.title.y = element_text(color='black',size=15),
    axis.ticks = element_line(color='black'),
    legend.key = element_blank(),
    legend.title = element_blank(),
    legend.text = element_text(size=12),
    #legend.position = c(0.75,.2),
    legend.margin =margin(r=5,l=5,t=5,b=5),
    legend.position = 'top',
    strip.background =element_rect(fill="white"),
    strip.text = element_text(size=10),
    panel.background = element_rect(fill=NA),
    panel.border = element_blank(), #make the borders clear in prep for just have two axes
    axis.line.x = element_line(colour = "black"),
    axis.line.y = element_line(colour = "black"))

sites <- unique(within_site_foliar_total$siteID)
sites_pval <- list()

for(i in sites[1:13]){
  
  site_name <- subset(within_site_foliar_total,siteID==i)
  pval<-summary(lm(foliarNPercent_mean ~ soilNPercent_MHoriz_mean,data=site_name))$coefficients[,4]
  sites_pval[[i]] <- pval

  }

# STEI and UKFS are significant: 2/13 sites

#
#

# foliar and soil inorganic N

#run if didn't run analysis script:

# #start
# merge_foliar_soil_inorganic <- filter_reps(mean_foliar, mean_soil_inorganic)
# 
# length(merge_foliar_soil_inorganic$siteID) 
# # 21 sites with the 4 rep thresholds
# 
# #round to two decimal points
# merge_foliar_soil_inorganic$foliarNPercent_mean <- round(merge_foliar_soil_inorganic$foliarNPercent_mean,2)
# merge_foliar_soil_inorganic$inorganicN<- round(merge_foliar_soil_inorganic$inorganicN,2)
# 
# #add veg type
# merge_foliar_soil_inorganic <- merge(merge_foliar_soil_inorganic,vegtype.df,by='siteID')
# #end

#set 10 within-site rep limit
within_site_foliar_inorganic <- merge_foliar_soil_inorganic %>%
  dplyr::filter(reps > 9)

within_site_foliar_inorganic <- merge(plot.df.2,within_site_foliar_inorganic[c(1,2)],by='siteID')

ggplot(within_site_foliar_inorganic,aes(inorganicN,foliarNPercent_mean,fill=Lcclass)) +
  facet_wrap(~siteID,scales = 'free') +
  geom_point(color='black',pch=21,alpha=0.5) +
  scale_fill_manual(values=c('herb'='blue','woody'='red'),
                    labels=c('herb'='Herbaceous','woody'='Woody')) +
  xlab('Inorganic soil N') +
  ylab('% Foliar N') +
  theme(
    axis.text.x = element_text(color='black',size=11), #angle=25,hjust=1),
    axis.text.y = element_text(color='black',size=11),
    axis.title.x = element_text(color='black',size=15),
    axis.title.y = element_text(color='black',size=15),
    axis.ticks = element_line(color='black'),
    legend.key = element_blank(),
    legend.title = element_blank(),
    legend.text = element_text(size=12),
    #legend.position = c(0.75,.2),
    legend.margin =margin(r=5,l=5,t=5,b=5),
    legend.position = 'top',
    strip.background =element_rect(fill="white"),
    strip.text = element_text(size=10),
    panel.background = element_rect(fill=NA),
    panel.border = element_blank(), #make the borders clear in prep for just have two axes
    axis.line.x = element_line(colour = "black"),
    axis.line.y = element_line(colour = "black"))

sites <- unique(within_site_foliar_inorganic$siteID)
sites_pval <- list()

for(i in sites[1:13]){
  
  site_name <- subset(within_site_foliar_inorganic,siteID==i)
  pval<-summary(lm(foliarNPercent_mean ~ inorganicN,data=site_name))$coefficients[,4]
  sites_pval[[i]] <- pval
  
}

#more sites show relationships for inorganic soil N

#KONZ, OAES, STEI moderately significant ~0.06
#JERC, HARV significantly < 0.01

#
#

# foliar and soil C:N

#re-run this code if didn't run analysis script:

#start
# merge_mean_soil_foliar_cn <- filter_reps(mean_soil_cn, mean_foliar_cn)
# length(merge_mean_soil_foliar_cn$siteID) 
# 
# #round to two decimal places
# merge_mean_soil_foliar_cn$foliarCNRatio_mean <- round(merge_mean_soil_foliar_cn$foliarCNRatio_mean,2)
# merge_mean_soil_foliar_cn$soilCNRatio_MHoriz_mean <- round(merge_mean_soil_foliar_cn$soilCNRatio_MHoriz_mean,2)
# 
# #add veg type
# merge_mean_soil_foliar_cn <- merge(merge_mean_soil_foliar_cn,vegtype.df,by='siteID')
#stop

within_site_foliar_soil_cn <- merge_mean_soil_foliar_cn %>%
  dplyr::filter(reps > 9)

within_site_foliar_soil_cn  <- merge(plot.df.2,within_site_foliar_soil_cn[c(1,2)],by='siteID')

#visualize
ggplot(within_site_foliar_soil_cn,aes(soilCNRatio_MHoriz_mean,foliarCNRatio_mean,fill=Lcclass)) +
  facet_wrap(~siteID,scales = 'free') +
  geom_point(color='black',pch=21,alpha=0.5) +
  scale_fill_manual(values=c('herb'='blue','woody'='red'),
                    labels=c('herb'='Herbaceous','woody'='Woody')) +
  xlab('Soil C:N') +
  ylab('Foliar C:N') +
  theme(
    axis.text.x = element_text(color='black',size=11), #angle=25,hjust=1),
    axis.text.y = element_text(color='black',size=11),
    axis.title.x = element_text(color='black',size=15),
    axis.title.y = element_text(color='black',size=15),
    axis.ticks = element_line(color='black'),
    legend.key = element_blank(),
    legend.title = element_blank(),
    legend.text = element_text(size=12),
    #legend.position = c(0.75,.2),
    legend.margin =margin(r=5,l=5,t=5,b=5),
    legend.position = 'top',
    strip.background =element_rect(fill="white"),
    strip.text = element_text(size=10),
    panel.background = element_rect(fill=NA),
    panel.border = element_blank(), #make the borders clear in prep for just have two axes
    axis.line.x = element_line(colour = "black"),
    axis.line.y = element_line(colour = "black"))

sites <- unique(within_site_foliar_soil_cn $siteID)
sites_pval <- list()

for(i in sites[1:10]){
  
  site_name <- subset(within_site_foliar_soil_cn ,siteID==i)
  pval<-summary(lm(foliarCNRatio_mean ~ soilCNRatio_MHoriz_mean,data=site_name))$coefficients[,4]
  sites_pval[[i]] <- pval
  
}

# HARV, JERC, SCBI, UKFs all moderately significant 

# make correlation matrix -----


#pearson correlation coefficients
library(Hmisc)

#head(plot.df.2)

myvars.cor <- c('soilNPercent_MHoriz_mean','inorganicN',"rootNPercent",
                'foliarNPercent_mean','litterNPercent_mean','soilCNRatio_MHoriz_mean',
                'rootCNratio','foliarCNRatio_mean','litterCNRatio_mean',
                'MAT', 'MAP','vpd')

plot.df.cor <- plot.df.2[myvars.cor]

#exp.pue.2 <- exp.pue[, c(3,4)] 
exp.corr.2<-rcorr(as.matrix(plot.df.cor),type="pearson")
correlations<-exp.corr.2$r 
correlations.P <- exp.corr.2$P

# save to csv
write.csv(correlations,file='./../output/manuscript_figures/correlations.csv')

write.csv(correlations.P,file='./../output/manuscript_figures/correlations.Pvals.csv')



