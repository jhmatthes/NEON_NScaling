# manuscript figures
library(scico)
source('Models.R')

# Tables summarizing  means and sample sizes of different pools  -----

head(cn_data)

# Get sample sizes:

sample_size_foliar<-aggregate(foliarNPercent_mean~siteID,length,data=cn_data)
sample_size_litter<-aggregate(litterNPercent_mean~siteID,length,data=cn_data) # KONZ only 4
sample_size_soil<-aggregate(soilNPercent_MHoriz_mean~siteID,length,data=cn_data) # HEAL only 1
sample_size_root <- aggregate(rootNPercent ~ siteID, length, data = cn_data)
sample_size_soil_inorganic <- aggregate(soilInorganicNugPerGram_mean ~ siteID, length, data = cn_data)
sample_size_foliar_cn<-aggregate(foliar_cn_self_calc~siteID,length,data=cn_data)
sample_size_soil_cn<-aggregate(soil_cn_self_calc~siteID,length,data=cn_data) 
sample_size_root_cn <- aggregate(root_cn_self_calc~siteID, length, data = cn_data)
sample_size_litter_cn <- aggregate(litter_cn_self_calc ~ siteID, length, data = cn_data)

# Get means:

#foliar
mean_site_foliar <- aggregate(foliarNPercent_mean~siteID,mean,data=cn_data)
mean_site_foliar$Foliar <- round(mean_site_foliar$foliarNPercent_mean,2)
mean_site_foliar <- mean_site_foliar[c(1,3)]
mean_site_foliar <- merge(mean_site_foliar,sample_size_foliar,by=c('siteID'))
colnames(mean_site_foliar) <-c('siteID','Foliar (%N)','replicates')

#root
mean_site_root <- aggregate(rootNPercent~siteID,mean,data=cn_data)
mean_site_root$Root <- round(mean_site_root$rootNPercent,2)
mean_site_root <- mean_site_root[c(1,3)]
mean_site_root <- merge(mean_site_root,sample_size_root,by=c('siteID'))
colnames(mean_site_root) <-c('siteID','Root (%N)','replicates')

#total soil N
mean_site_soil <- aggregate(soilNPercent_MHoriz_mean~siteID,mean,data=cn_data)
mean_site_soil$TotalSoil <- round(mean_site_soil$soilNPercent_MHoriz_mean,2)
mean_site_soil  <- mean_site_soil[c(1,3)]
mean_site_soil  <- merge(mean_site_soil,sample_size_soil, by=c('siteID'))
colnames(mean_site_soil) <- c('siteID','Total Soil (%N)','replicates')

#inorganic soil N
mean_site_soil_inorganic <- aggregate(soilInorganicNugPerGram_mean~siteID,mean,data=cn_data)
mean_site_soil_inorganic$InorganicSoil <- round(mean_site_soil_inorganic$soilInorganicNugPerGram_mean,2)
mean_site_soil_inorganic <- mean_site_soil_inorganic[c(1,3)]
mean_site_soil_inorganic <- merge(mean_site_soil_inorganic,sample_size_soil_inorganic, by=c('siteID'))
colnames(mean_site_soil_inorganic) <- c('siteID','Inorganic Soil N','replicates')

#litter
mean_site_soil_litter <- aggregate(litterNPercent_mean~siteID,mean,data=cn_data)
mean_site_soil_litter$litter <- round(mean_site_soil_litter$litterNPercent_mean,2)
mean_site_soil_litter <- mean_site_soil_litter[c(1,3)]
mean_site_soil_litter <- merge(mean_site_soil_litter,sample_size_litter,by=c('siteID'))
colnames(mean_site_soil_litter) <- c('siteID','Litter (%N)','replicates')

#total soil C:N
mean_site_soil_cn <-aggregate(soilCNRatio_MHoriz_mean~siteID,mean,data=cn_data)
mean_site_soil_cn$soilCN<-round(mean_site_soil_cn$soilCNRatio_MHoriz_mean,2)
mean_site_soil_cn  <- mean_site_soil_litter[c(1,3)]
mean_site_soil_cn  <-merge(mean_site_soil_cn,sample_size_soil_cn,by=c('siteID'))
colnames(mean_site_soil_cn) <-c('siteID','Soil C:N','replicates')

# root C:N
mean_site_soil_root_cn <-aggregate(root_cn_self_calc~siteID,mean,data=cn_data)
mean_site_soil_root_cn$rootCN<-round(mean_site_soil_root_cn$root_cn_self_calc,2)
mean_site_soil_root_cn  <- mean_site_soil_root_cn[c(1,3)]
mean_site_soil_root_cn  <-merge(mean_site_soil_root_cn,sample_size_root_cn,by=c('siteID'))
colnames(mean_site_soil_root_cn) <-c('siteID','Root C:N','replicates')

# leaf C:N
mean_site_soil_leaf_cn <-aggregate(foliar_cn_self_calc~siteID,mean,data=cn_data)
mean_site_soil_leaf_cn$rootCN<-round(mean_site_soil_leaf_cn$foliar_cn_self_calc,2)
mean_site_soil_leaf_cn  <- mean_site_soil_leaf_cn[c(1,3)]
mean_site_soil_leaf_cn  <-merge(mean_site_soil_leaf_cn,sample_size_foliar_cn,by=c('siteID'))
colnames(mean_site_soil_leaf_cn) <-c('siteID','Foliar C:N','replicates')

# Litter C:N
mean_site_soil_litter_cn <-aggregate(litter_cn_self_calc~siteID,mean,data=cn_data)
mean_site_soil_litter_cn$litterCN<-round(mean_site_soil_litter_cn$litter_cn_self_calc,2)
mean_site_soil_litter_cn  <- mean_site_soil_litter_cn[c(1,3)]
mean_site_soil_litter_cn  <-merge(mean_site_soil_litter_cn,sample_size_litter,by=c('siteID'))
colnames(mean_site_soil_litter_cn) <-c('siteID','Litter C:N','replicates')

## make tables

# Make table for three initial pools: % total soil N, % root N, % leaf N. All sites.

#root and leaf merge
root_leaf <- left_join(mean_site_root,mean_site_foliar,by=c('siteID'),na.rm=F)
soil_soil <-left_join(mean_site_soil,mean_site_soil_inorganic ,by=c('siteID'),na.rm=F)

# all sites mean and replicate # for three pools
root_leaf_soil <- left_join(soil_soil,root_leaf,by=c('siteID'),na.rm=F)

#merge with litter
root_leaf_soil_litter <- left_join(root_leaf_soil,mean_site_soil_litter,by=c('siteID'),na.rm=F)

#save to file
write.csv(root_leaf_soil_litter,file='figures/main_pools_site_summaries.csv')

# make table for C:N pools. All sites.
soil_root_cn <- left_join(mean_site_soil_cn,mean_site_soil_root_cn,by=c('siteID'),na.rm=F)
soil_root_leaf_cn <- left_join(soil_root_cn,mean_site_soil_leaf_cn,by=c('siteID'),na.rm=F)
soil_root_leaf_litter_cn <- left_join(soil_root_leaf_cn,mean_site_soil_litter_cn,by=c('siteID'),na.rm=F)

#save to file
write.csv(soil_root_leaf_litter_cn,file='figures/CN_site_summaries.csv')


#make main text table with just means, sd, and N of site per pool

#total soil summary stats
mean_total_soil <- round(mean(mean_site_soil$`Total Soil (%N)`),2)
sd_total_soil <- round(sd(mean_site_soil$`Total Soil (%N)`),2)
length_total_soil <- round(mean(mean_site_soil$replicates),0)
length_total_soil_all <- length(mean_site_soil$replicates)

Mean <- mean_total_soil
total_soil_df <- data.frame(Mean)
total_soil_df$'Standard Deviation' <- sd_total_soil
total_soil_df$'Average Sample Size' <- length_total_soil
total_soil_df$'No. of Sites' <- length_total_soil_all 
total_soil_df$'Pool' <- 'Total Soil N (%)'

#inorganic soil summary stats
mean_soil_inorganic <- round(mean(mean_site_soil_inorganic$`Inorganic Soil N`),2)
sd__soil_inogranic <- round(sd(mean_site_soil_inorganic$`Inorganic Soil N`),2)
length_soil_inorganic <- round(mean(mean_site_soil_inorganic$replicates),0)
length_soil_inorganic_all <- length(mean_site_soil_inorganic$replicates)

Mean <- mean_soil_inorganic
soil_inorganic_df <- data.frame(Mean)
soil_inorganic_df$'Standard Deviation' <- sd__soil_inogranic
soil_inorganic_df$'Average Sample Size' <- length_soil_inorganic
soil_inorganic_df$'No. of Sites' <- length_soil_inorganic_all
soil_inorganic_df$'Pool' <- 'Inorganic Soil N'

# total soil C:N summary stats
mean_soil_cn<-round(mean(mean_site_soil_cn$`Soil C:N`),2)
sd_soil_cn<-round(sd(mean_site_soil_cn$`Soil C:N`),2)
length_soil_cn <-round(mean(mean_site_soil_cn$replicates),0)
length_soil_cn_all <-length(mean_site_soil_cn$replicates)

Mean <- mean_soil_cn
soil_cn_df <- data.frame(Mean)
soil_cn_df$'Standard Deviation' <- sd_soil_cn
soil_cn_df$'Average Sample Size' <- length_soil_cn
soil_cn_df$'No. of Sites' <- length_soil_cn_all
soil_cn_df$'Pool' <- 'Soil C:N'

# root % N summary stats
mean_root <- round(mean(mean_site_root$`Root (%N)`),2)
sd_root <- round(sd(mean_site_root$`Root (%N)`),2)
length_root <- round(mean(mean_site_root$replicates),0)
length_root_all <- length(mean_site_root$replicates)

Mean <- mean_root
root_df <- data.frame(Mean)
root_df$'Standard Deviation' <- sd_root
root_df$'Average Sample Size' <- length_root 
root_df$'No. of Sites' <- length_root_all
root_df$'Pool' <- 'Root N (%)'

# root C:N summary stats
mean_root_cn<-round(mean(mean_site_soil_root_cn$`Root C:N`),2)
sd_root_cn<-round(sd(mean_site_soil_root_cn$`Root C:N`),2)
length_root_cn <-round(mean(mean_site_soil_root_cn$replicates),0)
length_root_cn_all <-length(mean_site_soil_root_cn$replicates)

Mean <- mean_root_cn
root_cn_df <- data.frame(Mean)
root_cn_df$'Standard Deviation' <- sd_root_cn
root_cn_df$'Average Sample Size' <- length_root_cn
root_cn_df$'No. of Sites' <- length_root_cn_all
root_cn_df$'Pool' <- 'Root C:N'

# leaf N summary stats
mean_leaf <- round(mean(mean_site_foliar$`Foliar (%N)`),2)
sd_leaf <- round(sd(mean_site_foliar$`Foliar (%N)`),2)
length_leaf <- round(mean(mean_site_foliar$replicates,0))
length_leaf_all <- length(mean_site_foliar$replicates)

Mean <- mean_leaf
leaf_df <- data.frame(Mean)
leaf_df$'Standard Deviation' <- sd_leaf
leaf_df$'Average Sample Size' <- length_leaf
leaf_df$'No. of Sites' <- length_leaf_all
leaf_df$'Pool' <- 'Leaf N (%)'

# leaf C:N summary stats
mean_leaf_cn <- round(mean(mean_site_soil_leaf_cn$`Foliar C:N`),2)
sd_leaf_cn <- round(sd(mean_site_soil_leaf_cn$`Foliar C:N`),2)
length_leaf_cn <- round(mean(mean_site_soil_leaf_cn$replicates),0)
length_leaf_cn_all <- length(mean_site_soil_leaf_cn$replicates)

Mean <- mean_leaf_cn
leaf_cn_df <- data.frame(Mean)
leaf_cn_df$'Standard Deviation' <- sd_leaf_cn
leaf_cn_df$'Average Sample Size' <- length_leaf_cn
leaf_cn_df$'No. of Sites' <- length_leaf_cn_all
leaf_cn_df$'Pool' <- 'Foliar C:N'

# Litter % N summary stats
mean_litter <- round(mean(mean_site_soil_litter$`Litter (%N)`),2)
sd_litter <- round(sd(mean_site_soil_litter$`Litter (%N)`),2)
length_litter <- round(mean(mean_site_soil_litter$replicates),0)
length_litter_all <- length(mean_site_soil_litter$replicates)

Mean <- mean_litter
litter_df <- data.frame(Mean)
litter_df$'Standard Deviation' <- sd_litter
litter_df$'Average Sample Size' <- length_litter
litter_df$'No. of Sites' <- length_litter_all
litter_df$'Pool' <- 'Litter N (%)'

# Litter C:N summary stats
mean_litter_cn <- round(mean(mean_site_soil_litter_cn$`Litter C:N`),2)
sd_litter_cn <- round(sd(mean_site_soil_litter_cn$`Litter C:N`),2)
length_litter_cn <-round(mean(mean_site_soil_litter_cn$replicates),0)
length_litter_cn_all <-length(mean_site_soil_litter_cn$replicates)

Mean <- mean_litter_cn
litter_cn_df <- data.frame(Mean)
litter_cn_df$'Standard Deviation' <- sd_litter_cn
litter_cn_df$'Average Sample Size' <- length_litter_cn 
litter_cn_df$'No. of Sites' <- length_litter_cn_all
litter_cn_df$'Pool' <- 'Litter C:N'

summary_stats <- rbind(total_soil_df,soil_inorganic_df,root_df,
                       leaf_df,litter_df, soil_cn_df, root_cn_df,leaf_cn_df,
                       litter_cn_df)

summary_stats <- summary_stats[c(5,1,2,3,4)]
write.csv(summary_stats,file='figures/Pool_means.csv')








#-------------------------------------------------------------------------------
# Map of sites -----

#-------------------------------------------------------------------------------
# Histograms of key N pools and data ranges ----

#total soil N
total_soil_hist <-
  ggplot(cn_data,aes(soilNPercent_MHoriz_mean,fill=class_2)) +
  scale_y_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  #geom_histogram(color='black',fill='white',bins=50) +
  geom_density(alpha=0.3,size=0.1) +
  scale_fill_manual(values=c('forest'='red','grassland'='blue','shrubland'='gold',
                             'savanna'='black')) +
  xlab('Total soil nitrogen (%)') +
  ylab('Count') +
  theme(
    axis.text.x = element_text(color='black',size=10), #angle=25,hjust=1),
    axis.text.y = element_text(color='black',size=10),
    axis.title.x = element_text(color='black',size=13),
    axis.title.y = element_text(color='black',size=13),
    axis.ticks = element_line(color='black'),
    legend.key = element_blank(),
    legend.title = element_blank(),
    legend.text = element_text(size=8.25),
    legend.position = c(0.48,0.75),
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
  ggplot(cn_data,aes(rootNPercent,fill=class_2)) +
  scale_y_continuous(expand = c(0,0)) +
  #geom_histogram(color='black',fill='white',bins=50) +
  geom_density(alpha=0.3,size=0.1) +
  scale_fill_manual(values=c('forest'='red','grassland'='blue','shrubland'='gold',
                             'savanna'='black')) +
  xlab('Root nitrogen (%)') +
  ylab('Count') +
  theme(
    axis.text.x = element_text(color='black',size=10), #angle=25,hjust=1),
    axis.text.y = element_text(color='black',size=10),
    axis.title.x = element_text(color='black',size=13),
    axis.title.y = element_text(color='black',size=13),
    axis.ticks = element_line(color='black'),
    legend.position = 'none',
    strip.background =element_rect(fill="white"),
    strip.text = element_text(size=10),
    panel.background = element_rect(fill=NA),
    panel.border = element_blank(), #make the borders clear in prep for just have two axes
    axis.line.x = element_line(colour = "black"),
    axis.line.y = element_line(colour = "black"))

#Leaf N 
leaf_hist <-
  ggplot(cn_data,aes(foliarNPercent_mean,fill=class_2)) +
  scale_y_continuous(expand = c(0,0)) +
  #geom_histogram(color='black',fill='white',bins=50) +
  geom_density(alpha=0.3,size=0.1) +
  scale_fill_manual(values=c('forest'='red','grassland'='blue','shrubland'='gold',
                             'savanna'='black')) +
  xlab('Foliar nitrogen (%)') +
  ylab('Count') +
  theme(
    axis.text.x = element_text(color='black',size=10), #angle=25,hjust=1),
    axis.text.y = element_text(color='black',size=10),
    axis.title.x = element_text(color='black',size=13),
    axis.title.y = element_text(color='black',size=13),
    axis.ticks = element_line(color='black'),
    legend.position = 'none',
    strip.background =element_rect(fill="white"),
    strip.text = element_text(size=10),
    panel.background = element_rect(fill=NA),
    panel.border = element_blank(), #make the borders clear in prep for just have two axes
    axis.line.x = element_line(colour = "black"),
    axis.line.y = element_line(colour = "black"))


#Inorganic N 
inorganic_hist <-
  ggplot(cn_data,aes(soilInorganicNugPerGram_mean,fill=class_2)) +
  scale_y_continuous(expand = c(0,0)) +
  geom_density(alpha=0.3,size=0.1) +
  scale_fill_manual(values=c('forest'='red','grassland'='blue','shrubland'='gold',
                             'savanna'='black')) +
  xlab('Inorganic soil nitrogen (ug N/g soil)') +
  ylab('Count') +
  theme(
    axis.text.x = element_text(color='black',size=10), #angle=25,hjust=1),
    axis.text.y = element_text(color='black',size=10),
    axis.title.x = element_text(color='black',size=13),
    axis.title.y = element_text(color='black',size=13),
    axis.ticks = element_line(color='black'),
    legend.position = 'none',
    strip.background =element_rect(fill="white"),
    strip.text = element_text(size=10),
    panel.background = element_rect(fill=NA),
    panel.border = element_blank(), #make the borders clear in prep for just have two axes
    axis.line.x = element_line(colour = "black"),
    axis.line.y = element_line(colour = "black"))

#data ranges
cn_data$soilInorganicNugPerGram_mean

#within sites
range_foliar <- get_data_range_within(val='foliarNPercent_mean')
range_root <- get_data_range_within(val='rootNPercent')
range_soil <- get_data_range_within(val='soilNPercent_MHoriz_mean')
range_inorganic <- get_data_range_within(val='soilInorganicNugPerGram_mean')

#across sites
range_foliar_across <- get_data_range_across_site(val='foliarNPercent_mean')
range_root_across <- get_data_range_across_site(val='rootNPercent')
range_soil_across <- get_data_range_across_site(val='soilNPercent_MHoriz_mean')
range_inorganic_across <- get_data_range_across_site(val='soilInorganicNugPerGram_mean')

#foliar range
range_foliar_2 <- rbind(range_foliar,range_foliar_across)

foliar_range_plot <- ggplot(range_foliar_2,aes(siteID,mean_val)) +
  geom_hline(yintercept = 1.78,color='grey') +
  geom_point(size=2) +
  geom_errorbar(data=range_foliar_2, 
                mapping=aes(x=siteID, ymin=min_val, ymax=max_val), 
                width=0.25, size=0.25, color="black") +
  ylab('Foliar nitrogen (%)') +
  xlab('') +
  theme(
    axis.text.x = element_text(color='black',size=5, angle=50,hjust=1),
    axis.text.y = element_text(color='black',size=8),
    axis.title.x = element_text(color='black',size=10),
    axis.title.y = element_text(color='black',size=10),
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

#root range
range_root_2 <- rbind(range_root,range_root_across)

root_range_plot <- ggplot(range_root_2,aes(siteID,mean_val)) +
  geom_hline(yintercept = 0.93,color='grey') +
  geom_point(size=2) +
  geom_errorbar(data=range_root_2, 
                mapping=aes(x=siteID, ymin=min_val, ymax=max_val), 
                width=0.25, size=0.25, color="black") +
  ylab('Root nitrogen (%)') +
  xlab('') +
  theme(
    axis.text.x = element_text(color='black', size=5, angle=50,hjust=1),
    axis.text.y = element_text(color='black',size=8),
    axis.title.x = element_text(color='black',size=10),
    axis.title.y = element_text(color='black',size=10),
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

#soil range
range_soil_2 <- rbind(range_soil,range_soil_across)

soil_range_plot <- ggplot(range_soil_2,aes(siteID,mean_val)) +
  geom_hline(yintercept = 0.24,color='grey') +
  geom_point(size=2) +
  geom_errorbar(data=range_soil_2, 
                mapping=aes(x=siteID, ymin=min_val, ymax=max_val), 
                width=0.25, size=0.25, color="black") +
  ylab('Total soil nitrogen (%)') +
  xlab('') +
  theme(
    axis.text.x = element_text(color='black',size=5, angle=50,hjust=1),
    axis.text.y = element_text(color='black',size=8),
    axis.title.x = element_text(color='black',size=10),
    axis.title.y = element_text(color='black',size=10),
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

#inorganic soil range
range_inorganic_soil_2 <- rbind(range_inorganic,range_inorganic_across)

soil_inorganic_range_plot <- ggplot(range_inorganic_soil_2,aes(siteID,mean_val)) +
  geom_hline(yintercept = 3.5,color='grey') +
  geom_point(size=2) +
  geom_errorbar(data=range_inorganic_soil_2, 
                mapping=aes(x=siteID, ymin=min_val, ymax=max_val), 
                width=0.25, size=0.25, color="black") +
  ylab(xlab('Inorganic soil N (ug N/g soil)')) +
  xlab('') +
  theme(
    axis.text.x = element_text(color='black',size=5, angle=50,hjust=1),
    axis.text.y = element_text(color='black',size=8),
    axis.title.x = element_text(color='black',size=10),
    axis.title.y = element_text(color='black',size=8),
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

#pdf
pdf(file='figures/histogram_ranges.pdf',
    width=8,height=10)

cowplot::plot_grid(total_soil_hist, soil_range_plot, 
                   inorganic_hist,
                   soil_inorganic_range_plot, 
                   root_hist,root_range_plot,
                   leaf_hist,foliar_range_plot,
           labels = c('A', 'B','C','D','E','F','G','H'),ncol = 2, nrow=4,
          rel_widths = c(1.5, 1.5,1.5), rel_heights = c(0.5, 0.5,0.5),label_size = 15)
dev.off()

# #png
# png(file='figures/histogram_ranges.png',
#     width=4000,height=5000,res=300)
# 
# 
# print(cowplot::plot_grid(total_soil_hist, soil_range_plot, 
#                    inorganic_hist,
#                    soil_inorganic_range_plot, 
#                    root_hist,root_range_plot,
#                    leaf_hist,foliar_range_plot,
#                    labels = c('A', 'B','C','D','E','F','G','H'),ncol = 2, nrow=4,
#                    rel_widths = c(1.5, 1.5,1.5), rel_heights = c(0.5, 0.5,0.5),label_size = 15))
# dev.off()




#-------------------------------------------------------------------------------
# C:N pool correlations plot ------

#soil to root C:N
#summary(root_soil_cn)
root_soil_cn_plot <- ggplot(root_soil_cn, aes(x = soil_cn_self_calc, y = root_cn_self_calc,
                                         color=npp_g_m2)) +
  geom_abline(slope=1,linetype='dashed') +
  scale_color_scico(bquote('NPP'~(g/m^2)),palette = 'batlow',direction=-1,limits = c(158,3277),
                    breaks = c(158,1700, 3277)) +
  geom_point(shape = 19, size=5,alpha=0.75) + #theme(legend.position = "none") + 
  geom_line(data=root_soil_cn,aes(y=predict.fixed),size=1.25,color='black') +
  xlab('Soil C:N') +
  ylab('Root C:N') +
  annotate("text", x=20, y=23, label= "1:1 line") +
  theme(
    axis.text.x = element_text(color='black',size=15), #angle=25,hjust=1),
    axis.text.y = element_text(color='black',size=15),
    axis.title.x = element_text(color='black',size=25),
    axis.title.y = element_text(color='black',size=25),
    axis.ticks = element_line(color='black'),
    legend.key = element_blank(),
    #legend.title = element_blank(),
    legend.text = element_text(size=10),
    #legend.position = c(0.9,0.4),
    #legend.margin =margin(r=5,l=5,t=5,b=5),
    legend.position = 'top',
    strip.background =element_rect(fill="white"),
    strip.text = element_text(size=10),
    panel.background = element_rect(fill=NA),
    panel.border = element_blank(), #make the borders clear in prep for just have two axes
    axis.line.x = element_line(colour = "black"),
    axis.line.y = element_line(colour = "black"))

#soil to foliar C:N
#back transform for the plot
#summary(foliar_soil_cn)
foliar_soil_cn$predict.fixed_2 <- exp(foliar_soil_cn$predict.fixed)
foliar_soil_cn_plot <- ggplot(foliar_soil_cn, aes(x = soil_cn_self_calc, y = foliar_cn_self_calc,
                                       color=MAP)) +
  #scale_color_scico(bquote('NPP'~(g/m^2)),palette = 'batlow',direction=-1) +
  geom_abline(slope=1,linetype='dashed') +
  scale_color_scico('MAP (mm)',palette = 'batlow',direction=-1,limits = c(238, 1512),
                    breaks = c(238,919,1512)) +
  geom_point(shape = 19, size=5,alpha=0.75) + theme(legend.position = "none") + 
  geom_line(data=foliar_soil_cn,aes(y=predict.fixed_2),size=1.25,color='black') +
  xlab('Soil C:N') +
  ylab('Foliar C:N') +
  annotate("text", x=15, y=12, label= "1:1 line") +
  theme(
    axis.text.x = element_text(color='black',size=15), #angle=25,hjust=1),
    axis.text.y = element_text(color='black',size=15),
    axis.title.x = element_text(color='black',size=25),
    axis.title.y = element_text(color='black',size=25),
    axis.ticks = element_line(color='black'),
    legend.key = element_blank(),
    #legend.title = element_blank(),
    legend.text = element_text(size=10),
    #legend.position = c(0.1,0.8),
    legend.margin =margin(r=5,l=5,t=5,b=5),
    legend.position = 'top',
    strip.background =element_rect(fill="white"),
    strip.text = element_text(size=10),
    panel.background = element_rect(fill=NA),
    panel.border = element_blank(), #make the borders clear in prep for just have two axes
    axis.line.x = element_line(colour = "black"),
    axis.line.y = element_line(colour = "black"))


# litter to soil C:N
#summary(litter_soil_cn)
litter_soil_cn_plot <- ggplot(litter_soil_cn, aes(x = litter_cn_self_calc, y = soil_cn_self_calc,
                                             color=pctSand_mean)) +
  scale_color_scico('% Sand',palette = 'batlow',direction=-1,limits = c(4, 81),
                    breaks = c(4,48,81)) +
  geom_line(data=litter_soil_cn,aes(y=predict.fixed),size=1.25,color='black') +
  geom_point(shape = 19, size=5,alpha=0.75) +
  xlab('Litter C:N') +
  ylab('Soil C:N') +
  geom_abline(slope=1,linetype='dashed') +
  annotate("text", x=43, y=33, label= "1:1 line") +
  theme(
    axis.text.x = element_text(color='black',size=15), #angle=25,hjust=1),
    axis.text.y = element_text(color='black',size=15),
    axis.title.x = element_text(color='black',size=25),
    axis.title.y = element_text(color='black',size=25),
    axis.ticks = element_line(color='black'),
    legend.key = element_blank(),
    #legend.title = element_blank(),
    legend.text = element_text(size=10),
    #legend.position = c(0.9,0.2),
    legend.margin = margin(r=5,l=5,t=5,b=5),
    legend.position = 'top',
    strip.background =element_rect(fill="white"),
    strip.text = element_text(size=10),
    panel.background = element_rect(fill=NA),
    panel.border = element_blank(), #make the borders clear in prep for just have two axes
    axis.line.x = element_line(colour = "black"),
    axis.line.y = element_line(colour = "black"))

#save plot
png(height = 2000,width=6000,res=300,
    'figures/c_n_pools_correlations.png')

print(cowplot::plot_grid(root_soil_cn_plot, foliar_soil_cn_plot ,
                litter_soil_cn_plot,
                labels = c('a', 'b','c'),ncol = 3, nrow=1,
                rel_widths = c(5,5,5), 
                rel_heights = c(1,1,1),label_size = 25))

dev.off()

#-------------------------------------------------------------------------------
# N pools correlations figure ------

#soil to root N
#summary(root_soil)
root_soil_plot <- ggplot(root_soil, aes(x = soilNPercent_MHoriz_mean, y = rootNPercent,
                                              color=MAP)) +
  geom_abline(slope=1,linetype='dashed') +
  annotate("text", x=0.62, y=0.55, label= "1:1 line") +
  scale_color_scico('MAP (mm)',palette = 'batlow',direction=-1,limits = c(238, 2008),
                    breaks = c(238,949,2008)) +
  geom_point(shape = 19, size=5,alpha=0.75) + #theme(legend.position = "none") + 
  #geom_line(data=root_soil,aes(y=predict.fixed),size=1.25,color='black',linetype='dashed') +
  xlab('Total soil nitrogen (%)') +
  ylab('Root nitrogen (%)') +
  theme(
    axis.text.x = element_text(color='black',size=15), #angle=25,hjust=1),
    axis.text.y = element_text(color='black',size=15),
    axis.title.x = element_text(color='black',size=25),
    axis.title.y = element_text(color='black',size=25),
    axis.ticks = element_line(color='black'),
    legend.key = element_blank(),
    #legend.title = element_blank(),
    legend.text = element_text(size=8.25),
    #legend.position = c(0.8,0.3),
    legend.margin =margin(r=5,l=5,t=5,b=5),
    legend.position = 'top',
    strip.background =element_rect(fill="white"),
    strip.text = element_text(size=10),
    panel.background = element_rect(fill=NA),
    panel.border = element_blank(), #make the borders clear in prep for just have two axes
    axis.line.x = element_line(colour = "black"),
    axis.line.y = element_line(colour = "black"))


#soil to foliar C:N
#back transform for the plot
foliar_soil$predict.fixed_2 <- exp(foliar_soil$predict.fixed)
#summary(foliar_soil)
foliar_soil_plot <- ggplot(foliar_soil, aes(x = soilNPercent_MHoriz_mean, y = foliarNPercent_mean)) +
  geom_abline(slope=1,linetype='dashed') +
  annotate("text", x=1.5, y=1.3, label= "1:1 line") +
  # scale_color_scico('MAP (mm)',palette = 'batlow',direction=-1,limits = c(238, 1512),
  #                   breaks = c(238,919,1512)) +
  geom_point(shape = 19, size=5,alpha=0.75,color='grey') + #theme(legend.position = "none") + 
  geom_line(data=foliar_soil,aes(y=predict.fixed_2),size=1.25,color='black') +
  xlab('Total soil nitrogen (%)') +
  ylab('Foliar nitrogen (%)') +
  theme(
    axis.text.x = element_text(color='black',size=15), #angle=25,hjust=1),
    axis.text.y = element_text(color='black',size=15),
    axis.title.x = element_text(color='black',size=25),
    axis.title.y = element_text(color='black',size=25),
    axis.ticks = element_line(color='black'),
    legend.key = element_blank(),
    #legend.title = element_blank(),
    legend.text = element_text(size=8.25),
    #legend.position = c(0.8,0.2),
    legend.margin =margin(r=5,l=5,t=5,b=5),
    legend.position = 'top',
    strip.background =element_rect(fill="white"),
    strip.text = element_text(size=10),
    panel.background = element_rect(fill=NA),
    panel.border = element_blank(), #make the borders clear in prep for just have two axes
    axis.line.x = element_line(colour = "black"),
    axis.line.y = element_line(colour = "black"))

# soil to root N
#summary(litter_soil)
litter_soil_plot <- ggplot(litter_soil, aes(x = litterNPercent_mean, y = soilNPercent_MHoriz_mean,
                                            color=MAP)) +
  geom_abline(slope=1,linetype='dashed') +
  annotate("text", x=1.5, y=1.35, label= "1:1 line") +
  scale_color_scico('MAP (mm)',palette = 'batlow',direction=-1,limits = c(327, 2008),
                    breaks = c(327,1185,2008)) +
  geom_point(shape = 19, size=5,alpha=0.75) + #theme(legend.position = "none") + 
  xlab('Litter nitrogen (%)') +
  ylab('Total soil nitrogen (%)') +
  theme(
    axis.text.x = element_text(color='black',size=15), #angle=25,hjust=1),
    axis.text.y = element_text(color='black',size=15),
    axis.title.x = element_text(color='black',size=25),
    axis.title.y = element_text(color='black',size=25),
    axis.ticks = element_line(color='black'),
    legend.key = element_blank(),
    #legend.title = element_blank(),
    legend.text = element_text(size=8.25),
    #legend.position = c(0.2,0.2),
    legend.margin =margin(r=5,l=5,t=5,b=5),
    legend.position = 'top',
    strip.background =element_rect(fill="white"),
    strip.text = element_text(size=10),
    panel.background = element_rect(fill=NA),
    panel.border = element_blank(), #make the borders clear in prep for just have two axes
    axis.line.x = element_line(colour = "black"),
    axis.line.y = element_line(colour = "black"))

#save plot
png(height = 2000,width=6000,res=300,
    'figures/n_pools_correlations.png')

print(cowplot::plot_grid(root_soil_plot, foliar_soil_plot,
                litter_soil_plot,
                labels = c('a', 'b','c'),ncol = 3, nrow=1,
                rel_widths = c(5,5,5), 
                rel_heights = c(1,1,1),label_size = 25))

dev.off()


#-------------------------------------------------------------------------------
# Root and foliar N and C:N correlation figure
summary(foliar_root)
foliar_root_plot <- ggplot(foliar_root, aes(x = rootNPercent, y = foliarNPercent_mean,
                                        color=pctSand_mean)) +
  geom_abline(slope=1,linetype='dashed') +
  annotate("text", x=1.5, y=1.35, label= "1:1 line") +
  scale_color_scico('% Sand',palette = 'batlow',direction=-1,limits = c(4, 96),
                    breaks = c(4,46, 96)) +
  geom_point(shape = 19, size=5,alpha=0.75) + #theme(legend.position = "none") + 
  geom_line(data=foliar_root,aes(y=predict.fixed),size=1.25,color='black') +
  ylab('Foliar nitrogen (%)') +
  xlab('Root nitrogen (%)') +
  theme(
    axis.text.x = element_text(color='black',size=15), #angle=25,hjust=1),
    axis.text.y = element_text(color='black',size=15),
    axis.title.x = element_text(color='black',size=25),
    axis.title.y = element_text(color='black',size=25),
    axis.ticks = element_line(color='black'),
    legend.key = element_blank(),
    #legend.title = element_blank(),
    legend.text = element_text(size=8.25),
    legend.position = c(0.8,0.2),
    legend.margin =margin(r=5,l=5,t=5,b=5),
    #legend.position = 'top',
    strip.background =element_rect(fill="white"),
    strip.text = element_text(size=10),
    panel.background = element_rect(fill=NA),
    panel.border = element_blank(), #make the borders clear in prep for just have two axes
    axis.line.x = element_line(colour = "black"),
    axis.line.y = element_line(colour = "black"))


#root to foliar C:N
foliar_root_cn$predict.fixed_2 <- exp(foliar_root_cn$predict.fixed)
#summary(foliar_root_cn)
foliar_root_cn_plot <- ggplot(foliar_root_cn, aes(x = root_cn_self_calc, y = foliar_cn_self_calc,
                                               color=pctSand_mean)) +
  geom_abline(slope=1,linetype='dashed') +
  annotate("text", x=84, y=90, label= "1:1 line") +
  scale_color_scico('% Sand',palette = 'batlow',direction=-1,limits = c(4, 96),
                    breaks = c(4,46, 96)) +
  geom_point(shape = 19, size=5,alpha=0.75) + #theme(legend.position = "none") + 
  geom_line(data=foliar_root_cn,aes(y=predict.fixed_2),size=1.25,color='black') +
  xlab('Root C:N') +
  ylab('Foliar C:N') +
  theme(
    axis.text.x = element_text(color='black',size=15), #angle=25,hjust=1),
    axis.text.y = element_text(color='black',size=15),
    axis.title.x = element_text(color='black',size=25),
    axis.title.y = element_text(color='black',size=25),
    axis.ticks = element_line(color='black'),
    legend.key = element_blank(),
    #legend.title = element_blank(),
    legend.text = element_text(size=8.25),
    #legend.position = c(0.8,0.2),
    legend.margin =margin(r=5,l=5,t=5,b=5),
    legend.position = 'none',
    strip.background =element_rect(fill="white"),
    strip.text = element_text(size=10),
    panel.background = element_rect(fill=NA),
    panel.border = element_blank(), #make the borders clear in prep for just have two axes
    axis.line.x = element_line(colour = "black"),
    axis.line.y = element_line(colour = "black"))

#save plot
png(height = 2000,width=4500,res=300,
    'figures/foliar_root_correlation.png')

print(cowplot::plot_grid(foliar_root_plot , foliar_root_cn_plot,
                         litter_soil_plot,
                         labels = c('a', 'b'),ncol = 2, nrow=1,
                         rel_widths = c(5,5), 
                         rel_heights = c(1,1),label_size = 25))

dev.off()
