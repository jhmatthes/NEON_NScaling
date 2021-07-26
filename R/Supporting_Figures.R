# Supporting Figures:



# Figure 4: bivariate with N pools using % total soil N ------

# run this first if not already run
source('05_Plant_Soil_N_Analyses.R')

# head(merge_mean_soil_root_cn)

# *Plot with the outlier removed

# Root and foliar N with total soil N -------
root_n_fig <-
  ggplot(merge_soil_root[-20,], aes(soilNPercent_MHoriz_mean,rootNPercent,color=Lcclass,label=siteID)) +
  scale_x_continuous(expand = c(0,0),limits = c(0,0.6)) +
  geom_point(size=0.75,alpha=0.75,color='black') +
  geom_text(aes(label=siteID),hjust=0, vjust=0,size=2.5) +
  scale_colour_manual(values=c('herb'='blue','woody'='red'),
                      labels=c('herb'='Herbaceous','woody'='Woody')) +
  xlab('% Total soil N') +
  ylab('% Root N') +
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
    legend.position = 'none',
    strip.background =element_rect(fill="white"),
    strip.text = element_text(size=10),
    panel.background = element_rect(fill=NA),
    panel.border = element_blank(), #make the borders clear in prep for just have two axes
    axis.line.x = element_line(colour = "black"),
    axis.line.y = element_line(colour = "black"))

# now do foliar

foliar_n_fig <-
  ggplot(merge_foliar_soil_means,aes(soilNPercent_MHoriz_mean,foliarNPercent_mean,color=Lcclass,label=siteID)) +
  scale_x_continuous(expand = c(0,0),limits = c(0,1.7)) +
  geom_point(size=0.75,alpha=0.75,color='black') +
  geom_text(aes(label=siteID),hjust=0, vjust=0,size=2.5) +
  scale_colour_manual(values=c('herb'='blue','woody'='red'),
                      labels=c('herb'='Herbaceous','woody'='Woody')) +
  xlab('% Total soil N') +
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

#now do root and leaf C:N

root_foliar_n_fig <-
  ggplot(merge_mean_foliar_root,aes(rootNPercent,foliarNPercent_mean,color=Lcclass,label=siteID)) +
  #scale_x_continuous(expand = c(0,0),limits = c(37,90.5)) +
  geom_point(size=0.75,alpha=0.75,color='black') +
  geom_text(aes(label=siteID),hjust=0, vjust=0,size=2.5) +
  stat_smooth(method='lm',color='black',se=F,size=0.5) +
  scale_colour_manual(values=c('herb'='blue','woody'='red'),
                      labels=c('herb'='Herbaceous','woody'='Woody')) +
  annotate(x=1.2, y=1.2, label="R-squared = 0.28", geom="text", size=4.0) +
  xlab('% Root N') +
  ylab('% Foliar N') +
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



pdf(file='./../output/manuscript_figures/N_foliar_root_bivariate.pdf',
    width=13,height=4)

cowplot::plot_grid(root_n_fig, foliar_n_fig,root_foliar_n_fig, labels = c('A', 'B','C'),ncol = 3, nrow=1,
                   rel_widths = c(1.5, 1.5,1.5), rel_heights = c(0.5, 0.5,0.5),label_size = 15)

dev.off()

#-------------------------------------------------------------------------------
# Root and foliar N with inorganic soil N -------
root_inorganic_n_fig <-
  ggplot(merge_soil_root_inorganic, aes(inorganicN,rootNPercent,color=Lcclass,label=siteID)) +
  scale_x_continuous(expand = c(0,0),limits = c(0,1.65)) +
  geom_point(size=0.75,alpha=0.75,color='black') +
  geom_text(aes(label=siteID),hjust=0, vjust=0,size=2.5) +
  stat_smooth(method='lm',color='black',se=F,size=0.5) +
  scale_colour_manual(values=c('herb'='blue','woody'='red'),
                      labels=c('herb'='Herbaceous','woody'='Woody')) +
  annotate(x=1.2, y=1.5, label="R-squared = 0.16", geom="text", size=4.0) +
  xlab('') +
  ylab('% Root N') +
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
    legend.position = 'none',
    strip.background =element_rect(fill="white"),
    strip.text = element_text(size=10),
    panel.background = element_rect(fill=NA),
    panel.border = element_blank(), #make the borders clear in prep for just have two axes
    axis.line.x = element_line(colour = "black"),
    axis.line.y = element_line(colour = "black"))

# now do foliar

foliar_inorganic_n_fig <-
  ggplot(merge_foliar_soil_inorganic,aes(inorganicN,foliarNPercent_mean,color=Lcclass,label=siteID)) +
  scale_x_continuous(expand = c(0,0),limits = c(0,3)) +
  geom_point(size=0.75,alpha=0.75,color='black') +
  geom_text(aes(label=siteID),hjust=0, vjust=0,size=2.5) +
  scale_colour_manual(values=c('herb'='blue','woody'='red'),
                      labels=c('herb'='Herbaceous','woody'='Woody')) +
  xlab('') +
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
    legend.position = c(0.75,.2),
    legend.margin =margin(r=5,l=5,t=5,b=5),
    #legend.position = 'top',
    strip.background =element_rect(fill="white"),
    strip.text = element_text(size=10),
    panel.background = element_rect(fill=NA),
    panel.border = element_blank(), #make the borders clear in prep for just have two axes
    axis.line.x = element_line(colour = "black"),
    axis.line.y = element_line(colour = "black"))

library(grid)
library(gridExtra)

pdf(file='./../output/manuscript_figures/Inorganic_N_foliar_root_bivariate.pdf',
    width=10,height=5)

plot<-cowplot::plot_grid(root_inorganic_n_fig, foliar_inorganic_n_fig, labels = c('A', 'B'),ncol = 2, nrow=1,
                   rel_widths = c(1.5, 1.5), rel_heights = c(0.5, 0.5),label_size = 15)

x.grob <- textGrob("Inorganic soil N (mg N/mL KCL)", 
                   gp=gpar(col="black", fontsize=15))

grid.arrange(arrangeGrob(plot, bottom = x.grob))

dev.off()


#-------------------------------------------------------------------------------
# make correlation matrix -----


#pearson correlation coefficients
library(Hmisc)

#head(plot.df.2)

myvars.cor <- c('soilNPercent_MHoriz_mean','inorganicN',"rootNPercent",
                'foliarNPercent_mean','litterNPercent_mean','soilCNRatio_MHoriz_mean',
                'rootCNratio','foliarCNRatio_mean','litterCNRatio_mean',
                'MAP')

plot.df.cor <- plot.df[myvars.cor]

colnames(plot.df.cor) <-c('% Total Soil N','Inorganic Soil N','% Root N',
                          '% Foliar N','% Litter N','Soil C:N','Root C:N',
                          'Foliar C:N','Liter C:N','MAP')

head(plot.df.cor)
#exp.pue.2 <- exp.pue[, c(3,4)] 
exp.corr.2<-rcorr(as.matrix(plot.df.cor),type="pearson")
correlations<-exp.corr.2$r 
correlations.P <- exp.corr.2$P

# save to csv
# write.csv(correlations,file='./../output/manuscript_figures/correlations.csv')
# 
# write.csv(correlations.P,file='./../output/manuscript_figures/correlations.Pvals.csv')

#make a correlation matrix plot:

#version 1 with scatterplot
library(PerformanceAnalytics)

pdf(file='./../output/manuscript_figures/N_pool_correlation_matrix_plot_1.pdf',
    width=10,height=10)
chart.Correlation(plot.df.cor, histogram=F, pch=19)
dev.off()

#version 2 with color-coded bubbles
library(corrplot)
dim(plot.df.cor)
res2<-rcorr(as.matrix(plot.df.cor[,1:10]))

#format data
flattenCorrMatrix <- function(cormat, pmat) {
  ut <- upper.tri(cormat)
  data.frame(
    row = rownames(cormat)[row(cormat)[ut]],
    column = rownames(cormat)[col(cormat)[ut]],
    cor  =(cormat)[ut],
    p = pmat[ut]
  )
}

flattenCorrMatrix(res2$r, res2$P)

#plot and save
pdf(file='./../output/manuscript_figures/N_pool_correlation_matrix_plot_2.pdf',
    width=10,height=10)
corrplot(res2$r, type="upper", order="hclust", 
         p.mat = res2$P, sig.level = 0.05, insig = "blank")
dev.off()

?corrplot



