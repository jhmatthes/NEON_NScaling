# manuscript figures

# set up ----


# get aridity data
vpd <- read.csv('./../data_pre-processed/scaled_vpd.csv')
head(vpd)

#cleanup
vpd<-vpd[c(2,3)]
colnames(vpd) <- c('siteID','vpd')

# merge with vpd data frame
plot.df <- merge(vpd,plot.df,by=c('siteID'))

#unique(plot.df$Lcclass)

#filter to just woody and herb (not NAs)
plot.df.2 <- plot.df %>%
  dplyr::filter(!Lcclass=='NA')

# Table 1 -----

head(plot.df.2)

# Get sample sizes:

sample_size_foliar<-aggregate(foliarNPercent_mean~siteID,length,data=plot.df.2)
sample_size_litter<-aggregate(litterNPercent_mean~siteID,length,data=plot.df.2) # KONZ only 4
sample_size_soil<-aggregate(soilNPercent_MHoriz_mean~siteID,length,data=plot.df.2) # HEAL only 1
sample_size_root <- aggregate(rootNPercent ~ siteID, length, data = plot.df.2)
sample_size_soil_inorganic <- aggregate(inorganicN ~ siteID, length, data = plot.df.2)
sample_size_soil_mineralization<- aggregate(netNminugPerGramPerDay ~ siteID, length, data = plot.df.2)
sample_size_soil_resorpN<- aggregate(resorpN ~ siteID, length, data = plot.df.2)
sample_size_foliar_cn<-aggregate(foliarCNRatio_mean~siteID,length,data=plot.df)
sample_size_soil_cn<-aggregate(soilCNRatio_MHoriz_mean~siteID,length,data=plot.df) 
sample_size_root_cn <- aggregate(rootCNratio ~ siteID, length, data = plot.df)
sample_size_litter_cn <- aggregate(litterNPercent_mean ~ siteID, length, data = plot.df)

# Get means:

#foliar
mean_site_foliar <-aggregate(foliarNPercent_mean~siteID,mean,data=plot.df.2)
mean_site_foliar$Foliar<-round(mean_site_foliar$foliarNPercent_mean,2)
mean_site_foliar <- mean_site_foliar[c(1,3)]
mean_site_foliar<-merge(mean_site_foliar,sample_size_foliar,by=c('siteID'))
colnames(mean_site_foliar) <-c('siteID','%Foliar','N')

#root
mean_site_root <-aggregate(rootNPercent~siteID,mean,data=plot.df.2)
mean_site_root$Root<-round(mean_site_root$rootNPercent,2)
mean_site_root <- mean_site_root[c(1,3)]
mean_site_root <-merge(mean_site_root,sample_size_root,by=c('siteID'))
colnames(mean_site_root) <-c('siteID','%Root','N')

#total soil N
mean_site_soil <-aggregate(soilNPercent_MHoriz_mean~siteID,mean,data=plot.df.2)
mean_site_soil$TotalSoil<-round(mean_site_soil$soilNPercent_MHoriz_mean,2)
mean_site_soil  <- mean_site_soil[c(1,3)]
mean_site_soil  <-merge(mean_site_soil,sample_size_soil, by=c('siteID'))
colnames(mean_site_soil) <-c('siteID','%TotalSoil','N')

#inorganic soil N
mean_site_soil_inorganic <-aggregate(inorganicN~siteID,mean,data=plot.df.2)
mean_site_soil_inorganic$InorganicSoil<-round(mean_site_soil_inorganic$inorganicN,2)
mean_site_soil_inorganic  <- mean_site_soil_inorganic[c(1,3)]
mean_site_soil_inorganic  <-merge(mean_site_soil_inorganic,sample_size_soil_inorganic, by=c('siteID'))
colnames(mean_site_soil_inorganic) <-c('siteID','%InorganicSoil','N')

#mineralization 
mean_site_soil_mineralization <-aggregate(netNminugPerGramPerDay~siteID,mean,data=plot.df.2)
mean_site_soil_mineralization$mineralization<-round(mean_site_soil_mineralization$netNminugPerGramPerDay,2)
mean_site_soil_mineralization  <- mean_site_soil_mineralization[c(1,3)]
mean_site_soil_mineralization  <-merge(mean_site_soil_mineralization,sample_size_soil_mineralization, by=c('siteID'))
colnames(mean_site_soil_mineralization) <-c('siteID','Mineralization','N')

#resorption 
mean_site_soil_resorpN <-aggregate(resorpN~siteID,mean,data=plot.df.2)
mean_site_soil_resorpN$resorption<-round(mean_site_soil_resorpN$resorpN,2)
mean_site_soil_resorpN  <- mean_site_soil_resorpN[c(1,3)]
mean_site_soil_resorpN  <-merge(mean_site_soil_resorpN,sample_size_soil_resorpN,by=c('siteID'))
colnames(mean_site_soil_resorpN) <-c('siteID','Resporption','N')

#litter
mean_site_soil_litter <-aggregate(litterNPercent_mean~siteID,mean,data=plot.df.2)
mean_site_soil_litter$litter<-round(mean_site_soil_litter$litterNPercent_mean,2)
mean_site_soil_litter  <- mean_site_soil_litter[c(1,3)]
mean_site_soil_litter  <-merge(mean_site_soil_litter,sample_size_litter,by=c('siteID'))
colnames(mean_site_soil_litter) <-c('siteID','Litter','N')

#total soil C:N
mean_site_soil_cn <-aggregate(soilCNRatio_MHoriz_mean~siteID,mean,data=plot.df.2)
mean_site_soil_cn$soilCN<-round(mean_site_soil_cn$soilCNRatio_MHoriz_mean,2)
mean_site_soil_cn  <- mean_site_soil_litter[c(1,3)]
mean_site_soil_cn  <-merge(mean_site_soil_cn,sample_size_soil_cn,by=c('siteID'))
colnames(mean_site_soil_cn) <-c('siteID','Soil C:N','N')

# root C:N
mean_site_soil_root_cn <-aggregate(rootCNratio~siteID,mean,data=plot.df.2)
mean_site_soil_root_cn$rootCN<-round(mean_site_soil_root_cn$rootCNratio,2)
mean_site_soil_root_cn  <- mean_site_soil_root_cn[c(1,3)]
mean_site_soil_root_cn  <-merge(mean_site_soil_root_cn,sample_size_root_cn,by=c('siteID'))
colnames(mean_site_soil_root_cn) <-c('siteID','Root C:N','N')

# leaf C:N
mean_site_soil_leaf_cn <-aggregate(foliarCNRatio_mean~siteID,mean,data=plot.df.2)
mean_site_soil_leaf_cn$rootCN<-round(mean_site_soil_leaf_cn$foliarCNRatio_mean,2)
mean_site_soil_leaf_cn  <- mean_site_soil_leaf_cn[c(1,3)]
mean_site_soil_leaf_cn  <-merge(mean_site_soil_leaf_cn,sample_size_foliar_cn,by=c('siteID'))
colnames(mean_site_soil_leaf_cn) <-c('siteID','Leaf C:N','N')

# Litter C:N
mean_site_soil_litter_cn <-aggregate(litterCNRatio_mean~siteID,mean,data=plot.df.2)
mean_site_soil_litter_cn$litterCN<-round(mean_site_soil_litter_cn$litterCNRatio_mean,2)
mean_site_soil_litter_cn  <- mean_site_soil_litter_cn[c(1,3)]
mean_site_soil_litter_cn  <-merge(mean_site_soil_litter_cn,sample_size_litter,by=c('siteID'))
colnames(mean_site_soil_litter_cn) <-c('siteID','Litter C:N','N')

## make tables

# Make table for three initial pools: % total soil N, % root N, % leaf N. All sites.

#root and leaf merge
root_leaf<- left_join(mean_site_root,mean_site_foliar,by=c('siteID'),na.rm=F)
soil_soil<-left_join(mean_site_soil,mean_site_soil_inorganic ,by=c('siteID'),na.rm=F)

# all sites mean and replicate # for three key pools
root_leaf_soil<-left_join(soil_soil,root_leaf,by=c('siteID'),na.rm=F)

#save to file
write.csv(root_leaf_soil,file='./../output/manuscript_figures/main_pools_site_summaries.csv')

# Make table for litter % N, mineralization, and resorption. All sites.

#merge resorption, litter, and mineralization
litter<- left_join(mean_site_soil_litter,mean_site_soil_litter_cn, by=c('siteID'),na.rm=F)
resorp_mineralization <- left_join(mean_site_soil_resorpN,mean_site_soil_mineralization,by=c('siteID'),na.rm=F)
resorp_litter_mineralization<-left_join(litter,resorp_mineralization,by=c('siteID'),na.rm=F)

#save to file
write.csv(resorp_litter_mineralization,file='./../output/manuscript_figures/Plant_feedbacks_site_summaries.csv')

# make table for C:N pools. All sites.
soil_root_cn <- left_join(mean_site_soil_cn,mean_site_soil_root_cn,by=c('siteID'),na.rm=F)
soil_root_leaf_cn <- left_join(soil_root_cn,mean_site_soil_leaf_cn,by=c('siteID'),na.rm=F)

#save to file
write.csv(soil_root_leaf_cn,file='./../output/manuscript_figures/CN_site_summaries.csv')

#make main text table with just means, sd, and N of site per pool


#total soil summary stats
mean_total_soil<-round(mean(mean_site_soil$`%TotalSoil`),2)
sd_total_soil<-round(sd(mean_site_soil$`%TotalSoil`),2)
length_total_soil <-round(mean(mean_site_soil$N),0)
length_total_soil_all <-length(mean_site_soil$N)

Mean<-mean_total_soil
total_soil_df <- data.frame(Mean)
total_soil_df$'Standard Deviation' <- sd_total_soil
total_soil_df$'Average Sample Size' <- length_total_soil
total_soil_df$'No. of Sites' <- length_total_soil_all 
total_soil_df$'Pool or Flux' <- '% Total Soil N'

#inorganic soil summary stats
mean_soil_inorganic<-round(mean(mean_site_soil_inorganic$`%InorganicSoil`),2)
sd__soil_inogranic<-round(sd(mean_site_soil_inorganic$`%InorganicSoil`),2)
length_soil_inorganic <-round(mean(mean_site_soil_inorganic$N),0)
length_soil_inorganic_all <-length(mean_site_soil_inorganic$N)

Mean<-mean_soil_inorganic
soil_inorganic_df <- data.frame(Mean)
soil_inorganic_df$'Standard Deviation' <- sd__soil_inogranic
soil_inorganic_df$'Average Sample Size' <- length_soil_inorganic
soil_inorganic_df$'No. of Sites' <- length_soil_inorganic_all
soil_inorganic_df$'Pool or Flux' <- '% Inorganic Soil N'

# total soil C:N summary stats
mean_soil_cn<-round(mean(mean_site_soil_cn$`Soil C:N`),2)
sd_soil_cn<-round(sd(mean_site_soil_cn$`Soil C:N`),2)
length_soil_cn <-round(mean(mean_site_soil_cn$N),0)
length_soil_cn_all <-length(mean_site_soil_cn$N)

Mean<-mean_soil_cn
soil_cn_df <- data.frame(Mean)
soil_cn_df$'Standard Deviation' <- sd_soil_cn
soil_cn_df$'Average Sample Size' <- length_soil_cn
soil_cn_df$'No. of Sites' <- length_soil_cn_all
soil_cn_df$'Pool or Flux' <- 'Soil C:N'

# root % N summary stats
mean_root<-round(mean(mean_site_root$`%Root`),2)
sd_root<-round(sd(mean_site_root$`%Root`),2)
length_root <-round(mean(mean_site_root$N),0)
length_root_all <-length(mean_site_root$N)

Mean<-mean_root
root_df <- data.frame(Mean)
root_df$'Standard Deviation' <- sd_root
root_df$'Average Sample Size' <- length_root 
root_df$'No. of Sites' <- length_root_all
root_df$'Pool or Flux' <- '% Root N'

# root C:N summary stats
mean_root_cn<-round(mean(mean_site_soil_root_cn$`Root C:N`),2)
sd_root_cn<-round(sd(mean_site_soil_root_cn$`Root C:N`),2)
length_root_cn <-round(mean(mean_site_soil_root_cn$N),0)
length_root_cn_all <-length(mean_site_soil_root_cn$N)

Mean<-mean_root_cn
root_cn_df <- data.frame(Mean)
root_cn_df$'Standard Deviation' <- sd_root_cn
root_cn_df$'Average Sample Size' <- length_root_cn
root_cn_df$'No. of Sites' <- length_root_cn_all
root_cn_df$'Pool or Flux' <- 'Root C:N'

# leaf N summary stats
mean_leaf<-round(mean(mean_site_foliar$`%Foliar`),2)
sd_leaf<-round(sd(mean_site_foliar$`%Foliar`),2)
length_leaf <-round(mean(mean_site_foliar$N),0)
length_leaf_all <-length(mean_site_foliar$N)

Mean<-mean_leaf
leaf_df <- data.frame(Mean)
leaf_df$'Standard Deviation' <- sd_leaf
leaf_df$'Average Sample Size' <- length_leaf
leaf_df$'No. of Sites' <- length_leaf_all
leaf_df$'Pool or Flux' <- '% Leaf N'

# leaf C:N summary stats
mean_leaf_cn<-round(mean(mean_site_soil_leaf_cn$`Leaf C:N`),2)
sd_leaf_cn<-round(sd(mean_site_soil_leaf_cn$`Leaf C:N`),2)
length_leaf_cn <-round(mean(mean_site_soil_leaf_cn$N),0)
length_leaf_cn_all <-length(mean_site_soil_leaf_cn$N)

Mean<-mean_leaf_cn
leaf_cn_df <- data.frame(Mean)
leaf_cn_df$'Standard Deviation' <- sd_leaf_cn
leaf_cn_df$'Average Sample Size' <- length_leaf_cn
leaf_cn_df$'No. of Sites' <- length_leaf_cn_all
leaf_cn_df$'Pool or Flux' <- 'Leaf C:N'

# resorption summary stats
mean_resorp<-round(mean(mean_site_soil_resorpN$Resporption),2)
sd_resorp<-round(sd(mean_site_soil_resorpN$Resporption),2)
length_resorp <-round(mean(mean_site_soil_resorpN$N),0)
length_resorp_all <-length(mean_site_soil_resorpN$N)

Mean<-mean_resorp
resorp_df <- data.frame(Mean)
resorp_df$'Standard Deviation' <- sd_resorp
resorp_df$'Average Sample Size' <- length_resorp 
resorp_df$'No. of Sites' <- length_resorp_all
resorp_df$'Pool or Flux' <- 'N Resorption'

# Litter % N summary stats
mean_litter<-round(mean(mean_site_soil_litter$Litter),2)
sd_litter<-round(sd(mean_site_soil_litter$Litter),2)
length_litter <-round(mean(mean_site_soil_litter$N),0)
length_litter_all <-length(mean_site_soil_litter$N)

Mean<-mean_litter
litter_df <- data.frame(Mean)
litter_df$'Standard Deviation' <- sd_litter
litter_df$'Average Sample Size' <- length_litter
litter_df$'No. of Sites' <- length_litter_all
litter_df$'Pool or Flux' <- '% Litter N'

# Litter C:N summary stats
mean_litter_cn<-round(mean(mean_site_soil_litter_cn$`Litter C:N`),2)
sd_litter_cn<-round(sd(mean_site_soil_litter_cn$`Litter C:N`),2)
length_litter_cn <-round(mean(mean_site_soil_litter_cn$N),0)
length_litter_cn_all <-length(mean_site_soil_litter_cn$N)

Mean<-mean_litter_cn
litter_cn_df <- data.frame(Mean)
litter_cn_df$'Standard Deviation' <- sd_litter_cn
litter_cn_df$'Average Sample Size' <- length_litter_cn 
litter_cn_df$'No. of Sites' <- length_litter_cn_all
litter_cn_df$'Pool or Flux' <- 'Litter C:N'

# mineralization summary stats
mean_mineral<-round(mean(mean_site_soil_mineralization$Mineralization),2)
sd_mineral<-round(sd(mean_site_soil_mineralization$Mineralization),2)
length_mineral<-round(mean(mean_site_soil_mineralization$N),0)
length_mineral_all <-length(mean_site_soil_mineralization$N)

Mean<-mean_mineral
mineral_df <- data.frame(Mean)
mineral_df$'Standard Deviation' <- sd_mineral
mineral_df$'Average Sample Size' <- length_mineral 
mineral_df$'No. of Sites' <- length_mineral_all 
mineral_df$'Pool or Flux' <- 'N Mineralization'

summary_stats <- rbind(total_soil_df,soil_inorganic_df,soil_cn_df,root_df,
                       root_cn_df,leaf_df,leaf_cn_df,resorp_df,litter_df,litter_cn_df,
                       mineral_df)

summary_stats <- summary_stats[c(5,1,2,3,4)]
write.csv(summary_stats,file='./../output/manuscript_figures/Pool_means.csv')








# Figure 1 = conceptual-----

#Figure 2 - Map of sites -----

# Figure 3: histograms of key N pools and bivariates with climate ----



#total soil N
total_soil_hist <-
  ggplot(plot.df.2,aes(soilNPercent_MHoriz_mean,fill=Lcclass)) +
  scale_y_continuous(expand = c(0,0)) +
  geom_histogram(color='black',bins=50) +
scale_fill_manual(values=c('herb'='red','woody'='blue'),
                  labels=c('herb'='Herbaceous','woody'='Woody')) +
  xlab('Total Soil N') +
  ylab('') +
  theme(
    axis.text.x = element_text(color='black',size=10), #angle=25,hjust=1),
    axis.text.y = element_text(color='black',size=10),
    axis.title.x = element_text(color='black',size=13),
    axis.title.y = element_text(color='black',size=13),
    axis.ticks = element_line(color='black'),
    legend.key = element_blank(),
    legend.title = element_blank(),
    legend.text = element_text(size=8.25),
    legend.position = c(0.48,0.8),
    legend.margin =margin(r=5,l=5,t=5,b=5),
    #legend.position = 'none',
    strip.background =element_rect(fill="white"),
    strip.text = element_text(size=10),
    panel.background = element_rect(fill=NA),
    panel.border = element_blank(), #make the borders clear in prep for just have two axes
    axis.line.x = element_line(colour = "black"),
    axis.line.y = element_line(colour = "black"))

#Root N 
root_hist <-
  ggplot(plot.df.2,aes(rootNPercent,fill=Lcclass)) +
  scale_y_continuous(expand = c(0,0)) +
  geom_histogram(color='black',bins=50) +
  scale_fill_manual(values=c('herb'='red','woody'='blue'),
                    labels=c('herb'='Herbaceous','woody'='Woody')) +
  xlab('% Root N') +
  ylab('Count') +
  theme(
    axis.text.x = element_text(color='black',size=10), #angle=25,hjust=1),
    axis.text.y = element_text(color='black',size=10),
    axis.title.x = element_text(color='black',size=13),
    axis.title.y = element_text(color='black',size=13),
    axis.ticks = element_line(color='black'),
    # legend.key = element_blank(),
    # legend.title = element_blank(),
    # legend.text = element_text(size=8.25),
    # legend.position = c(0.48,0.8),
    # legend.margin =margin(r=5,l=5,t=5,b=5),
    legend.position = 'none',
    strip.background =element_rect(fill="white"),
    strip.text = element_text(size=10),
    panel.background = element_rect(fill=NA),
    panel.border = element_blank(), #make the borders clear in prep for just have two axes
    axis.line.x = element_line(colour = "black"),
    axis.line.y = element_line(colour = "black"))

#Leaf N 
leaf_hist <-
  ggplot(plot.df.2,aes(foliarNPercent_mean,fill=Lcclass)) +
  scale_y_continuous(expand = c(0,0)) +
  geom_histogram(color='black',bins=50) +
  scale_fill_manual(values=c('herb'='red','woody'='blue'),
                    labels=c('herb'='Herbaceous','woody'='Woody')) +
  xlab('% Foliar N') +
  ylab('') +
  theme(
    axis.text.x = element_text(color='black',size=10), #angle=25,hjust=1),
    axis.text.y = element_text(color='black',size=10),
    axis.title.x = element_text(color='black',size=13),
    axis.title.y = element_text(color='black',size=13),
    axis.ticks = element_line(color='black'),
    # legend.key = element_blank(),
    # legend.title = element_blank(),
    # legend.text = element_text(size=8.25),
    # legend.position = c(0.48,0.8),
    # legend.margin =margin(r=5,l=5,t=5,b=5),
    legend.position = 'none',
    strip.background =element_rect(fill="white"),
    strip.text = element_text(size=10),
    panel.background = element_rect(fill=NA),
    panel.border = element_blank(), #make the borders clear in prep for just have two axes
    axis.line.x = element_line(colour = "black"),
    axis.line.y = element_line(colour = "black"))

# now make the bivariates with climate variable
summary(lm(soilNPercent_MHoriz_mean~vpd,data=plot.df.2))
# weakly significant, negative effect of vpd on soil N

#total soil N
total_soil_climate <-
  ggplot(plot.df.2,aes(vpd,soilNPercent_MHoriz_mean,fill=Lcclass)) +
  #scale_y_continuous(expand = c(0,0)) +
  geom_point(color='black',pch=21,alpha=0.5) +
  #stat_smooth(method='lm',se=FALSE,size=0.5,color='black') +
  scale_fill_manual(values=c('herb'='blue','woody'='red'),
                    labels=c('herb'='Herbaceous','woody'='Woody')) +
  xlab('') +
  ylab('% Soil N') +
  theme(
    axis.text.x = element_text(color='black',size=10), #angle=25,hjust=1),
    axis.text.y = element_text(color='black',size=10),
    axis.title.x = element_text(color='black',size=13),
    axis.title.y = element_text(color='black',size=13),
    axis.ticks = element_line(color='black'),
    # legend.key = element_blank(),
    # legend.title = element_blank(),
    # legend.text = element_text(size=8.25),
    # legend.position = c(0.48,0.8),
    # legend.margin =margin(r=5,l=5,t=5,b=5),
    legend.position = 'none',
    strip.background =element_rect(fill="white"),
    strip.text = element_text(size=10),
    panel.background = element_rect(fill=NA),
    panel.border = element_blank(), #make the borders clear in prep for just have two axes
    axis.line.x = element_line(colour = "black"),
    axis.line.y = element_line(colour = "black"))

#root N climate
summary(lm(rootNPercent~vpd,data=plot.df.2))
# weakly positive relationship
root_climate <-
  ggplot(plot.df.2,aes(vpd,rootNPercent,fill=Lcclass)) +
  #scale_y_continuous(expand = c(0,0)) +
  geom_point(color='black',pch=21,alpha=0.5) +
  #stat_smooth(method='lm',color='black') +
  scale_fill_manual(values=c('herb'='blue','woody'='red'),
                    labels=c('herb'='Herbaceous','woody'='Woody')) +
  xlab('') +
  ylab('% Root N') +
  theme(
    axis.text.x = element_text(color='black',size=10), #angle=25,hjust=1),
    axis.text.y = element_text(color='black',size=10),
    axis.title.x = element_text(color='black',size=13),
    axis.title.y = element_text(color='black',size=13),
    axis.ticks = element_line(color='black'),
    # legend.key = element_blank(),
    # legend.title = element_blank(),
    # legend.text = element_text(size=8.25),
    # legend.position = c(0.48,0.8),
    # legend.margin =margin(r=5,l=5,t=5,b=5),
    legend.position = 'none',
    strip.background =element_rect(fill="white"),
    strip.text = element_text(size=10),
    panel.background = element_rect(fill=NA),
    panel.border = element_blank(), #make the borders clear in prep for just have two axes
    axis.line.x = element_line(colour = "black"),
    axis.line.y = element_line(colour = "black"))

#leaf N and climate
summary(lm(foliarNPercent_mean~vpd,data=plot.df.2))
#weak negative relationship
leaf_climate <-
  ggplot(plot.df.2,aes(vpd,foliarNPercent_mean,fill=Lcclass)) +
  #scale_y_continuous(expand = c(0,0)) +
  geom_point(color='black',pch=21,alpha=0.5) +
  #stat_smooth(method='lm') +
  scale_fill_manual(values=c('herb'='blue','woody'='red'),
                    labels=c('herb'='Herbaceous','woody'='Woody')) +
  xlab('Long-term vapor pressure deficit') +
  ylab('% Foliar N') +
  theme(
    axis.text.x = element_text(color='black',size=10), #angle=25,hjust=1),
    axis.text.y = element_text(color='black',size=10),
    axis.title.x = element_text(color='black',size=13),
    axis.title.y = element_text(color='black',size=13),
    axis.ticks = element_line(color='black'),
    # legend.key = element_blank(),
    # legend.title = element_blank(),
    # legend.text = element_text(size=8.25),
    # legend.position = c(0.48,0.8),
    # legend.margin =margin(r=5,l=5,t=5,b=5),
    legend.position = 'none',
    strip.background =element_rect(fill="white"),
    strip.text = element_text(size=10),
    panel.background = element_rect(fill=NA),
    panel.border = element_blank(), #make the borders clear in prep for just have two axes
    axis.line.x = element_line(colour = "black"),
    axis.line.y = element_line(colour = "black"))

#

library(cowplot)

pdf(file='./../output/manuscript_figures/historgram_climate.pdf',
    width=8,height=6)

plot_grid(total_soil_hist, total_soil_climate, root_hist,root_climate,leaf_hist,
          leaf_climate, labels = c('A', 'B','C','D','E','F'),ncol = 2, nrow=3,
          rel_widths = c(1.5, 1.5,1.5), rel_heights = c(0.5, 0.5,0.5),label_size = 15)
dev.off()




# Figure 4: bivariate with C:N pools ------

# run this first
#source('cn_analysis.R')
sample_size_foliar_cn<-aggregate(foliarCNRatio_mean~siteID,length,data=plot.df)
sample_size_soil_cn<-aggregate(soilCNRatio_MHoriz_mean~siteID,length,data=plot.df) 
sample_size_root_cn <- aggregate(rootCNratio ~ siteID, length, data = plot.df)

# Get mean values for each plot
mean_foliar_cn<-aggregate(foliarCNRatio_mean~siteID + plotID,mean,data=plot.df)
mean_root_cn <- aggregate(rootCNratio ~ siteID + plotID, mean, data = plot.df)
mean_soil_cn<-aggregate(soilCNRatio_MHoriz_mean~siteID + plotID,mean,data=plot.df)

#final DF
merge_mean_soil_root_cn<-filter_reps(mean_soil_cn, mean_root_cn)
merge_mean_soil_root_cn <- merge(merge_mean_soil_root_cn,vegtype.df,by='siteID')

# length(merge_mean_soil_root_cn$siteID)
# head(merge_mean_soil_root_cn)

root_cn_fig <-
  ggplot(merge_mean_soil_root_cn,aes(soilCNRatio_MHoriz_mean,rootCNratio,color=Lcclass,label=siteID)) +
  scale_x_continuous(expand = c(0,0),limits = c(10,34)) +
  geom_point(size=0.75,alpha=0.75,color='black') +
  geom_text(aes(label=siteID),hjust=0, vjust=0) +
  stat_smooth(method='lm',color='black',se=F,size=0.5) +
  scale_colour_manual(values=c('herb'='blue','woody'='red'),
                    labels=c('herb'='Herbaceous','woody'='Woody')) +
  xlab('Soil C:N') +
  ylab('Root C:N') +
  theme(
    axis.text.x = element_text(color='black',size=11), #angle=25,hjust=1),
    axis.text.y = element_text(color='black',size=11),
    axis.title.x = element_text(color='black',size=15),
    axis.title.y = element_text(color='black',size=15),
    axis.ticks = element_line(color='black'),
    legend.key = element_blank(),
    legend.title = element_blank(),
    legend.text = element_text(size=12),
    legend.position = c(0.75,.2),
    legend.margin =margin(r=5,l=5,t=5,b=5),
    #legend.position = 'none',
    strip.background =element_rect(fill="white"),
    strip.text = element_text(size=10),
    panel.background = element_rect(fill=NA),
    panel.border = element_blank(), #make the borders clear in prep for just have two axes
    axis.line.x = element_line(colour = "black"),
    axis.line.y = element_line(colour = "black"))

# now do foliar
merge_mean_soil_foliar_cn <- filter_reps(mean_soil_cn, mean_foliar_cn)
merge_mean_soil_foliar_cn_2 <- merge(merge_mean_soil_foliar_cn[-1,],vegtype.df,by='siteID')

foliar_cn_fig <-
  ggplot(merge_mean_soil_foliar_cn_2,aes(soilCNRatio_MHoriz_mean,foliarCNRatio_mean,color=Lcclass,label=siteID)) +
  scale_x_continuous(expand = c(0,0),limits = c(10,30.5)) +
  geom_point(size=0.75,alpha=0.75,color='black') +
  stat_smooth(method='lm',color='black',se=F,size=0.5) +
  geom_text(aes(label=siteID),hjust=0, vjust=0) +
  scale_colour_manual(values=c('herb'='blue','woody'='red'),
                      labels=c('herb'='Herbaceous','woody'='Woody')) +
  xlab('Soil C:N') +
  ylab('Foliar C:N') +
  theme(
    axis.text.x = element_text(color='black',size=11), #angle=25,hjust=1),
    axis.text.y = element_text(color='black',size=11),
    axis.title.x = element_text(color='black',size=15),
    axis.title.y = element_text(color='black',size=15),
    axis.ticks = element_line(color='black'),
    # legend.key = element_blank(),
    # legend.title = element_blank(),
    # legend.text = element_text(size=8.25),
    # legend.position = c(0.5,.2),
    # legend.margin =margin(r=5,l=5,t=5,b=5),
    legend.position = 'none',
    strip.background =element_rect(fill="white"),
    strip.text = element_text(size=10),
    panel.background = element_rect(fill=NA),
    panel.border = element_blank(), #make the borders clear in prep for just have two axes
    axis.line.x = element_line(colour = "black"),
    axis.line.y = element_line(colour = "black"))

#now do root and leaf C:N
merge_mean_root_foliar_cn <- filter_reps(mean_root_cn, mean_foliar_cn)
merge_mean_root_foliar_cn  <- merge(merge_mean_root_foliar_cn,vegtype.df,by='siteID')

root_foliar_cn_fig <-
  ggplot(merge_mean_root_foliar_cn,aes(rootCNratio,foliarCNRatio_mean,color=Lcclass,label=siteID)) +
  scale_x_continuous(expand = c(0,0),limits = c(37,90.5)) +
  geom_point(size=0.75,alpha=0.75,color='black') +
  #stat_smooth(method='lm',color='black',se=F,size=0.5) + N.S
  geom_text(aes(label=siteID),hjust=0, vjust=0) +
  scale_colour_manual(values=c('herb'='blue','woody'='red'),
                      labels=c('herb'='Herbaceous','woody'='Woody')) +
  xlab('Root C:N') +
  ylab('Foliar C:N') +
  theme(
    axis.text.x = element_text(color='black',size=11), #angle=25,hjust=1),
    axis.text.y = element_text(color='black',size=11),
    axis.title.x = element_text(color='black',size=15),
    axis.title.y = element_text(color='black',size=15),
    axis.ticks = element_line(color='black'),
    # legend.key = element_blank(),
    # legend.title = element_blank(),
    # legend.text = element_text(size=8.25),
    # legend.position = c(0.5,.2),
    # legend.margin =margin(r=5,l=5,t=5,b=5),
    legend.position = 'none',
    strip.background =element_rect(fill="white"),
    strip.text = element_text(size=10),
    panel.background = element_rect(fill=NA),
    panel.border = element_blank(), #make the borders clear in prep for just have two axes
    axis.line.x = element_line(colour = "black"),
    axis.line.y = element_line(colour = "black"))



pdf(file='./../output/manuscript_figures/CN_foliar_root_bivariate.pdf',
    width=13,height=4)
?plot_grid
plot_grid(root_cn_fig, foliar_cn_fig,root_foliar_cn_fig, labels = c('A', 'B','C'),ncol = 3, nrow=1,
          rel_widths = c(1.5, 1.5,1.5), rel_heights = c(0.5, 0.5,0.5),label_size = 15)
# x.grob <- textGrob("Soil C:N", 
#                    gp=gpar(fontface="bold", col="black", fontsize=15))

dev.off()

# Figure 5: plant feedbacks C:N figure ------

#litter feedbacks to soil CN
mean_litter_cn<-aggregate(litterCNRatio_mean~siteID + plotID,mean,data=plot.df.2)
mean_soil_cn<-aggregate(soilCNRatio_MHoriz_mean~siteID + plotID,mean,data=plot.df.2)
mean_resorp<-aggregate(resorpN~siteID + plotID,mean,data=plot.df.2)
mean_litter_soil_cn_2 <- filter_reps(mean_litter_cn, mean_soil_cn)
mean_litter_soil_cn_2 <- merge(mean_litter_soil_cn_2,vegtype.df,by='siteID')

litter_soil_fig <-
  ggplot(mean_litter_soil_cn_2,aes(litterCNRatio_mean,soilCNRatio_MHoriz_mean,label=siteID)) +
  scale_x_continuous(expand = c(0,0),limits = c(39,125)) +
  geom_point(size=0.75,alpha=0.75,color='black') +
  geom_text(aes(label=siteID),hjust=.0, vjust=0.0,alpha=1,color='red') +
  stat_smooth(method='lm',color='black',se=F,size=0.5) +
  scale_colour_manual(values=c('herb'='blue','woody'='red'),
                      labels=c('herb'='Herbaceous','woody'='Woody')) +
  xlab('Litter C:N') +
  ylab('Soil C:N') +
  theme(
    axis.text.x = element_text(color='black',size=11), #angle=25,hjust=1),
    axis.text.y = element_text(color='black',size=11),
    axis.title.x = element_text(color='black',size=15),
    axis.title.y = element_text(color='black',size=15),
    axis.ticks = element_line(color='black'),
    legend.key = element_blank(),
    legend.title = element_blank(),
    legend.text = element_text(size=12),
    legend.position = c(0.75,.2),
    legend.margin =margin(r=5,l=5,t=5,b=5),
    #legend.position = 'none',
    strip.background =element_rect(fill="white"),
    strip.text = element_text(size=10),
    panel.background = element_rect(fill=NA),
    panel.border = element_blank(), #make the borders clear in prep for just have two axes
    axis.line.x = element_line(colour = "black"),
    axis.line.y = element_line(colour = "black"))

# now do resportion
mean_resorp_soil_cn_2 <- filter_reps(mean_resorp, mean_soil_cn)
mean_resorp_soil_cn_2 <- merge(mean_resorp_soil_cn_2,vegtype.df,by='siteID')

resorp_soil_fig <-
  ggplot(mean_resorp_soil_cn_2,aes(resorpN,soilCNRatio_MHoriz_mean,label=siteID)) +
  #scale_y_continuous(expand = c(0,0)) +
  scale_x_continuous(expand = c(0,0),limits = c(25,69)) +
  geom_point(size=0.75,alpha=0.75,color='black') +
  geom_text(aes(label=siteID),hjust=0, vjust=0,color='red') +
  #stat_smooth(method='lm',color='black',se=F,size=0.5) +
  scale_colour_manual(values=c('herb'='blue','woody'='red'),
                      labels=c('herb'='Herbaceous','woody'='Woody')) +
  xlab('N Resportion') +
  ylab('') +
  theme(
    axis.text.x = element_text(color='black',size=11), #angle=25,hjust=1),
    axis.text.y = element_text(color='black',size=11),
    axis.title.x = element_text(color='black',size=15),
    axis.title.y = element_text(color='black',size=15),
    axis.ticks = element_line(color='black'),
    # legend.key = element_blank(),
    # legend.title = element_blank(),
    # legend.text = element_text(size=12),
    # legend.position = c(0.25,.2),
    # legend.margin =margin(r=5,l=5,t=5,b=5),
    legend.position = 'none',
    strip.background =element_rect(fill="white"),
    strip.text = element_text(size=10),
    panel.background = element_rect(fill=NA),
    panel.border = element_blank(), #make the borders clear in prep for just have two axes
    axis.line.x = element_line(colour = "black"),
    axis.line.y = element_line(colour = "black"))

pdf(file='./../output/manuscript_figures/CN_litter_resportion_bivariate.pdf',
    width=9,height=4)

plot_grid(litter_soil_fig, resorp_soil_fig, labels = c('A', 'B'),ncol = 2, nrow=1,
          rel_widths = c(1.5, 1.5,1.5), rel_heights = c(0.5, 0.5,0.5),label_size = 15)

dev.off()

