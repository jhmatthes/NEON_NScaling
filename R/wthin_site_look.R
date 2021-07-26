
#Within-site look between foliar and soil N

#foliar N and total soil N
# the cross-site relationship was not significant 

# re-run this bit of code if didn't run analysis code:


#minimum of 10 within-site reps
within_site_foliar_total <- merge_foliar_soil_means %>%
  dplyr::filter(reps > 9)

within_site_foliar_total <- merge(plot.df,within_site_foliar_total[c(1,2)],by='siteID')

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

for(i in sites[1:15]){
  
  site_name <- subset(within_site_foliar_total,siteID==i)
  pval<-summary(lm(foliarNPercent_mean ~ soilNPercent_MHoriz_mean,data=site_name))$coefficients[,4]
  sites_pval[[i]] <- pval

}

sites_pval_df <- do.call('rbind',sites_pval)

# STEI & UKFS significant: 2/15 sites

#
#

# foliar and soil inorganic N


#set 10 within-site rep limit
within_site_foliar_inorganic <- merge_foliar_soil_inorganic %>%
  dplyr::filter(reps > 9)

within_site_foliar_inorganic <- merge(plot.df,within_site_foliar_inorganic[c(1,2)],by='siteID')

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

for(i in sites[1:16]){
  
  site_name <- subset(within_site_foliar_inorganic,siteID==i)
  pval<-summary(lm(foliarNPercent_mean ~ inorganicN,data=site_name))$coefficients[,4]
  sites_pval[[i]] <- pval
  
}

sites_pval_df <- do.call("rbind",sites_pval)
#2/16 sites significant (HARV, JERC)

#more sites show relationships for inorganic soil N

#KONZ, OAES, STEI moderately significant ~0.06
#JERC, HARV significantly < 0.01

#
#

# foliar and soil C:N------
source('06_Plant_Soil_CN_Analyses.R')

within_site_foliar_soil_cn <- merge_mean_soil_foliar_cn %>%
  dplyr::filter(reps > 9)

within_site_foliar_soil_cn  <- merge(plot.df,within_site_foliar_soil_cn[c(1,2)],by='siteID')

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

sites <- unique(within_site_foliar_soil_cn$siteID)
sites_pval <- list()

for(i in sites[1:11]){
  
  site_name <- subset(within_site_foliar_soil_cn ,siteID==i)
  pval<-summary(lm(foliarCNRatio_mean ~ soilCNRatio_MHoriz_mean,data=site_name))$coefficients[,4]
  sites_pval[[i]] <- pval
  
}


sites_pval_df <- do.call("rbind",sites_pval)
# UKFS significant linked. 1/11. Few others moderately significant. 


