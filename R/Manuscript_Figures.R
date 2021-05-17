# manuscript figures

# Figure 1 = conceptual

#Figure 2 - Map of sites -----

# Figure 3: histograms of key N pools and bivariates with climate ----



#total soil N
total_soil_hist <-
  ggplot(plot.df,aes(soilNPercent_MHoriz_mean,fill=Lcclass)) +
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
  ggplot(plot.df,aes(rootNPercent,fill=Lcclass)) +
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
  ggplot(plot.df,aes(foliarNPercent_mean,fill=Lcclass)) +
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
summary(lm(soilNPercent_MHoriz_mean~vpd,data=plot.df))

#total soil N
total_soil_climate <-
  ggplot(plot.df,aes(vpd,soilNPercent_MHoriz_mean,fill=Lcclass)) +
  #scale_y_continuous(expand = c(0,0)) +
  geom_point(color='black',pch=21,alpha=0.5) +
  #stat_smooth(method='lm') +
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
root_climate <-
  ggplot(plot.df,aes(vpd,rootNPercent,fill=Lcclass)) +
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

leaf_climate <-
  ggplot(plot.df,aes(vpd,foliarNPercent_mean,fill=Lcclass)) +
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
source('cn_analysis.R')

merge_mean_soil_root_cn <- merge(merge_mean_soil_root_cn,vegtype.df,by='siteID')
merge_mean_soil_root_cn <- rename_lcc(merge_mean_soil_root_cn,crop = F)

head(merge_mean_soil_root_cn)

root_cn_fig <-
  ggplot(merge_mean_soil_root_cn,aes(soilCNRatio_MHoriz_mean,rootCNratio,color=Lcclass,label=siteID)) +
  scale_x_continuous(expand = c(0,0),limits = c(10,33)) +
  geom_point(size=0.01,alpha=0.01) +
  geom_text(aes(label=siteID),hjust=0, vjust=0) +
  stat_smooth(method='lm',color='black',se=F,size=0.5) +
  scale_colour_manual(values=c('herb'='blue','woody'='red'),
                    labels=c('herb'='Herbaceous','woody'='Woody')) +
  xlab('') +
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
merge_mean_soil_foliar_cn_2 <- merge(merge_mean_soil_foliar_cn[-1,],vegtype.df,by='siteID')
merge_mean_soil_root_cn_2 <- rename_lcc(merge_mean_soil_foliar_cn_2,crop = F)

foliar_cn_fig <-
  ggplot(merge_mean_soil_root_cn_2,aes(soilCNRatio_MHoriz_mean,foliarCNRatio_mean,color=Lcclass,label=siteID)) +
  scale_x_continuous(expand = c(0,0),limits = c(10,30.5)) +
  geom_point(size=0.01,alpha=0.01) +
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


pdf(file='./../output/manuscript_figures/CN_foliar_root_bivariate.pdf',
    width=5,height=8)

plot_grid(root_cn_fig, foliar_cn_fig, labels = c('A', 'B'),ncol = 1, nrow=2,
          rel_widths = c(1.5, 1.5,1.5), rel_heights = c(0.5, 0.5,0.5),label_size = 15)
# x.grob <- textGrob("Soil C:N", 
#                    gp=gpar(fontface="bold", col="black", fontsize=15))

dev.off()

# plant feedbacks C:N figure ------

#litter feedbacks to soil CN
mean_litter_soil_cn_2 <- merge(mean_litter_soil_cn_2,vegtype.df,by='siteID')
mean_litter_soil_cn_2 <- rename_lcc(mean_litter_soil_cn_2,crop = F)

litter_soil_fig <-
  ggplot(mean_litter_soil_cn_2,aes(litterCNRatio_mean,soilCNRatio_MHoriz_mean,color=Lcclass,label=siteID)) +
  scale_x_continuous(expand = c(0,0),limits = c(39,125)) +
  geom_point(size=0.01,alpha=0.01) +
  geom_text(aes(label=siteID),hjust=0, vjust=0) +
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
mean_resorp_soil_cn_2 <- merge(mean_resorp_soil_cn_2,vegtype.df,by='siteID')
mean_resorp_soil_cn_2 <- rename_lcc(mean_resorp_soil_cn_2,crop = F)

resorp_soil_fig <-
  ggplot(mean_resorp_soil_cn_2,aes(resorpN,soilCNRatio_MHoriz_mean,color=Lcclass,label=siteID)) +
  #scale_y_continuous(expand = c(0,0)) +
  scale_x_continuous(expand = c(0,0),limits = c(25,69)) +
  geom_point(size=0.01,alpha=0.01) +
  geom_text(aes(label=siteID),hjust=0, vjust=0) +
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
# x.grob <- textGrob("Soil C:N", 
#                    gp=gpar(fontface="bold", col="black", fontsize=15))

dev.off()

