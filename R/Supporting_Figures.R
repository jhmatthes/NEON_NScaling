# Supporting Figures and analyses:

# misc. look at the data ------
head(cn_data,1)

# tower versus distributed plots
aggregate(siteID ~ plotType,length,data=cn_data)
557 + 970

length(unique(cn_data$siteID))


# number of each vegetation type
veg <- cn_data %>%
  dplyr::select(siteID,class_2)

#one last measure, remove any duplicates
veg <- veg[!duplicated(veg),]
aggregate(siteID~class_2,length,data=veg)

#look to see if they are statistically different
head(cn_data,1)

pool_list <- c('soilNPercent_MHoriz_mean',"soil_cn_self_calc",
               "soilInorganicNugPerGram_mean","rootNPercent",
               "root_cn_self_calc","foliarNPercent_mean","foliar_cn_self_calc",
               "litterNPercent_mean","litter_cn_self_calc")
store_pool_pvals <- list()

for(i in pool_list){
  
  pval <- quick_lme_look_veg(i)
  store_pool_pvals[[i]] <- pval
  
}

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
# correlation matrix of N and C:N pools -----

head(cn_data,1)

#pearson correlation coefficients
library(Hmisc)

#head(plot.df.2)

#ID and select columns
myvars.cor <- c('soilNPercent_MHoriz_mean','soilInorganicNugPerGram_mean',"soil_cn_self_calc",
                'rootNPercent','root_cn_self_calc','foliarNPercent_mean','foliar_cn_self_calc',
                'litterNPercent_mean','litter_cn_self_calc')

cn_data_2 <- cn_data[myvars.cor]

#rename columns
cn_data_2 <- cn_data_2 %>%
  rename('Total soil N (%)' = 'soilNPercent_MHoriz_mean',
         'Inorganic Soil N' = 'soilInorganicNugPerGram_mean',
         'Soil C:N' = "soil_cn_self_calc",
         'Root N (%)' = 'rootNPercent',
         'Root C:N' = 'root_cn_self_calc',
         'Foliar N (%)' = 'foliarNPercent_mean',
         'Foliar C:N' = 'foliar_cn_self_calc',
         'Litter N (%)' = 'litterNPercent_mean',
         'Litter C:N' = 'litter_cn_self_calc')

head(cn_data_2)

#version 1 with scatterplot
library(PerformanceAnalytics)

pdf('figures/N_pool_correlation_matrix.pdf',width=10,height=10)

chart.Correlation(cn_data_2, histogram=F, pch=19)

dev.off()


#-------------------------------------------------------------------------------
# fixed versus random effects -----


library(reshape2)

#store in list
model_list_df <- do.call('rbind',model_list)
head(model_list_df)

long_original_model <- melt(model_list_df, id.vars = c("model"), variable.name = "effect")

fixed_random_original_model <- ggplot(long_original_model,aes(effect,value)) +
  geom_boxplot() +
  xlab('') +
  ylab('R-squared') +
  ggtitle('Original model') +
  theme(
    axis.text.x = element_text(color='black',size=25),
    axis.text.y = element_text(color='black',size=20),
    axis.title.x = element_text(color='black',size=25),
    axis.title.y = element_text(color='black',size=25),
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

#store in list
model_list_df_2 <- do.call('rbind',model_list_2)
head(model_list_df)

long_original_model_2 <- melt(model_list_df_2, id.vars = c("model"), variable.name = "effect")

fixed_random_final_model <- ggplot(long_original_model_2 ,aes(effect,value)) +
  #geom_point() +
  geom_boxplot() +
  xlab('') +
  ylab('') +
  ggtitle('AIC-selected model') +
  theme(
    axis.text.x = element_text(color='black',size=25),
    axis.text.y = element_text(color='black',size=20),
    axis.title.x = element_text(color='black',size=25),
    axis.title.y = element_text(color='black',size=25),
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
  

#save plot
png(height = 2000,width=4000,res=300,
    'figures/fixed_random_histograms.png')

print(plot_grid(fixed_random_original_model, fixed_random_final_model,
                labels = c('a', 'b'),ncol = 2, nrow=1,
                rel_widths = c(5,5), 
                rel_heights = c(1,1),label_size = 25))

dev.off()



#-------------------------------------------------------------------------------
# correlation matrix of covariates ------


head(cn_data,1)

#pearson correlation coefficients
library(Hmisc)

#head(plot.df.2)

#ID and select columns
myvars.cor <- c('siteID','MAP','MAT',"npp_g_m2",
                'pctSand_mean')

cn_data_2 <- cn_data[myvars.cor]

#get a single value per site, since they are already averages

cn_data_2 <- cn_data_2 %>%
  group_by(siteID) %>%
  summarise_all(mean)

#rename columns
cn_data_2 <- cn_data_2 %>%
  select(MAP,MAT,npp_g_m2,pctSand_mean) %>%
  rename('MAP' = 'MAP',
         'MAT' = 'MAT',
         'NPP' = "npp_g_m2",
         '% Sand' = 'pctSand_mean')

  head(cn_data_2)

#version 1 with scatterplot
library(PerformanceAnalytics)

pdf('figures/covariate_correlation_matrix.pdf',width=10,height=10)

chart.Correlation(cn_data_2, histogram=F, pch=19)

dev.off()


#-------------------------------------------------------------------------------
# look at the cut down in sample size with plot-level % sand data ------

#averaged by site
nrow(filter_reps_1("rootNPercent","soilNPercent_MHoriz_mean"))
#115

#plot-evel
nrow(filter_reps_2("rootNPercent","soilNPercent_MHoriz_mean"))
#8


#averaged by site
nrow(filter_reps_1("foliarNPercent_mean","soilNPercent_MHoriz_mean"))
#275

#plot-evel
nrow(filter_reps_2("foliarNPercent_mean","soilNPercent_MHoriz_mean"))
#88
#-------------------------------------------------------------------------------
# NPP, MAP and MAT range -----

#NPP
min(cn_data$npp_g_m2)
max(cn_data$npp_g_m2)

#MAP
min(cn_data$MAP)
max(cn_data$MAP)

#MAT
min(cn_data$MAT)
max(cn_data$MAT)

#% sand
min(cn_data$pctSand_mean)
max(cn_data$pctSand_mean)
