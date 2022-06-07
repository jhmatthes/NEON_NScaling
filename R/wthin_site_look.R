
#Within-site look between foliar and soil N
#-------------------------------------------------------------------------------
#foliar N and total soil N----

#If not run already, run:
#source('plant_soil_N_Analyses.R')

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

#loop through and store p-values for each within-site relationship
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
#-------------------------------------------------------------------------------
# foliar and soil inorganic N------


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

#KONZ, OAES, STEI moderately significant ~0.06
#JERC, HARV significantly < 0.01

#
#

#-------------------------------------------------------------------------------
# foliar and soil C:N------

#If not run already, run:
#source('06_Plant_Soil_CN_Analyses.R')

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



#-------------------------------------------------------------------------------
# compare range and variability within  versus across sites------

#range in N pools -----

#within site ranges
range_foliar <- get_data_range_within(val='foliarNPercent_mean')
range_root <- get_data_range_within(val='rootNPercent')
range_soil <- get_data_range_within(val='soilNPercent_MHoriz_mean')
range_inorganic <- get_data_range_within(val='soilInorganicNugPerGram_mean')
range_litter <- get_data_range_within(val='litterNPercent_mean')

#across sites
range_foliar_cross <- get_data_range_across_site(val='foliarNPercent_mean')
range_root_cross <- get_data_range_across_site(val='rootNPercent')
range_soil_cross <- get_data_range_across_site(val='soilNPercent_MHoriz_mean')
range_inorganic_cross <- get_data_range_across_site(val='soilInorganicNugPerGram_mean')
range_litter_across <- get_data_range_across_site(val='litterNPercent_mean')

#foliar range
mean(range_foliar$range)
range_foliar_cross$range

#root range
mean(range_root$range)
range_root_cross$range

#soil range
mean(range_soil$range)
range_soil_cross$range

#inorganic soil range
range_inorganic_soil_2 <- rbind(range_inorganic,range_inorganic_cross)

#litter
range_litter_2 <- rbind(range_litter,range_litter_across)

#

#range in C:N pools

#

#within site ranges
range_foliar_cn <- get_data_range_within(val='foliar_cn_self_calc')
range_root_cn <- get_data_range_within(val='root_cn_self_calc')
range_soil_cn <- get_data_range_within(val='soil_cn_self_calc')
range_litter_cn <- get_data_range_within(val='litter_cn_self_calc')

#across sites
range_foliar_cn_cross <- get_data_range_across_site(val='foliar_cn_self_calc')
range_root_cn_cross <- get_data_range_across_site(val='root_cn_self_calc')
range_soil_cn_cross <- get_data_range_across_site(val='soil_cn_self_calc')
range_litter_cn_cross <- get_data_range_across_site(val='litter_cn_self_calc')

#foliar range
mean(range_foliar_cn$range)
range_foliar_cn_cross$range

#root range
mean(range_root_cn$range)
range_root_cn_cross$range

#soil range
mean(range_soil_cn$range)
range_soil_cn_cross$range

#litter
mean(range_litter_cn$range)
range_litter_cn_cross$range


#within site spatial variability of N pools ------
sd.foliar <- get_data_sd(val='foliarNPercent_mean')
sd.root <- get_data_sd(val='rootNPercent')
sd.soil <- get_data_sd(val='soilNPercent_MHoriz_mean')
sd.inorganic <- get_data_sd(val='soilInorganicNugPerGram_mean')
sd.litter <- get_data_sd(val='litterNPercent_mean')

#Compare average sd across all sites to across sites SD

#foliar N
sd.foliar.cross <- get_data_sd_cross_site(val='foliarNPercent_mean')
#0.50
mean(sd.foliar$sd_val)
#0.44

(0.50 - 0.44)/0.44
#.14

#root N
sd.root.cross <- get_data_sd_cross_site(val='rootNPercent')
#0.24
mean(sd.root$sd_val)
#0.19
(0.26-0.19)/0.19
#0.39

#litter
sd.litter.cross <- get_data_sd_cross_site(val='litterNPercent_mean')
#0.26
mean(sd.litter$sd_val)
#0.18

(0.26-0.18)/0.18
#0.44

#soil N
sd.soil.cross <- get_data_sd_cross_site(val='soilNPercent_MHoriz_mean')
#0.24
mean(sd.soil$sd_val)
#0.12

(0.24-0.12)/0.12
#1

#soil inorganic N
sd.inorganic.cross <- get_data_sd_cross_site(val='soilInorganicNugPerGram_mean')
#5.3
mean(sd.inorganic$sd_val)
#2.4

(5.3-2.4)/2.4
#1.2

#spatial variability in C:N pools

#within site SD
#head(cn_data,1)
sd_foliar_cn <- get_data_sd(val='foliar_cn_self_calc')
sd_root_cn <- get_data_sd(val='root_cn_self_calc')
sd_soil_cn <- get_data_sd(val='soil_cn_self_calc')
sd_litter_cn <- get_data_sd(val='litter_cn_self_calc')

#foliar C:N
sd_foliar_cn_cross <- get_data_sd_cross_site(val='foliar_cn_self_calc')
#10.6
mean(sd_foliar_cn$sd_val)
#9.4

(10.6-9.4)/9.4
#0.13

#root C:N
sd_root_cn_cross <- get_data_sd_cross_site(val='root_cn_self_calc')
#14.6
mean(sd_root_cn$sd_val)
#10.7
(14.6-10.7)/10.7
#0.36

#soil C:N
sd_soil_cn_cross <- get_data_sd_cross_site(val='soil_cn_self_calc')
#6.7
mean(sd_soil_cn$sd_val)
#3.5
(6.7-3.5)/3.5
#0.91

#litter C:N
sd_litter_cn_cross <- get_data_sd_cross_site(val='litter_cn_self_calc')
#29.3
mean(sd_litter_cn$sd_val)
#16.4
(29.3-16.4)/16.4
#0.79
