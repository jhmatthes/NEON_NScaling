

#look at bloclim data

head(plot.df) #already just sites for continental US since merged with VPD

# full climate dataset
bioclim<-read.csv('./../data_pre-processed/bioclim_data.csv', #file path is relative
                  header = T, stringsAsFactors = F)
head(bioclim)

# we want bio1 (MAT), bio12 (MAP), and bio15 (PPT seasonality)

bioclim_vars <- bioclim[c(1,2,3,4,15,18)]
head(bioclim_vars)

#rename columns
colnames(bioclim_vars) <- c('x','y','siteID','MAT','MAP','PPT_seasonality')
#head(bioclim_vars)
#hist(bioclim_vars$MAP)

plot(MAP~MAT,data=bioclim_vars)
cor.test(bioclim_vars$MAT,bioclim_vars$MAP)

plot.df.worldclim<-merge(plot.df,bioclim_vars,by=c('siteID'))

#plot(vpd~MAP,data=plot.df.worldclim)

#compare which of these is a better predictor of N pools (use newest MAP)
head(plot.df.worldclim)
library(ggplot2)

#simplify to woody versus herb vegetation types
plot.df.worldclim <- rename_lcc(plot.df.worldclim,crop = F)

#see how correlated the variables are with each other-----
library(PerformanceAnalytics)
chart.Correlation(bioclim_vars[c(4,5,6)], histogram=F, pch=19)
#MAT and MAP most strongly correlated
#


# leaf N -------

# MAT
ggplot(plot.df.worldclim,aes(MAT,foliarNPercent_mean,color=Lcclass)) +
  geom_point()

summary(lm(foliarNPercent_mean~MAT,data=plot.df.worldclim))
#N.S

#MAP
ggplot(plot.df.worldclim,aes(MAP,foliarNPercent_mean,color=Lcclass)) +
  geom_point()

summary(lm(foliarNPercent_mean~MAP,data=plot.df.worldclim))
#N.S

#VPD*
ggplot(plot.df.2,aes(vpd,foliarNPercent_mean,color=Lcclass)) +
  geom_point()

summary(lm(foliarNPercent_mean~vpd,data=plot.df.2))
# SIGNIFICANT - negative effect

#PPT seasonality*
ggplot(plot.df.worldclim,aes(PPT_seasonality,foliarNPercent_mean,color=Lcclass)) +
  geom_point()

summary(lm(foliarNPercent_mean~PPT_seasonality,data=plot.df.worldclim))
# SIGNIFICANT - negative effect

# all pretty weak relationships

#root N ------

# MAT
ggplot(plot.df.worldclim,aes(MAT,rootNPercent,color=Lcclass)) +
  geom_point()

#summary(lm(rootNPercent~MAT.2,data=plot.df)) 
#N.S

# MAP*
ggplot(plot.df.worldclim,aes(MAP,rootNPercent,color=Lcclass)) +
  geom_point()

summary(lm(rootNPercent~MAP,data=plot.df.worldclim)) 
# SIGNIFICANT - negative effect

# VPD*
ggplot(plot.df.2,aes(vpd,rootNPercent,color=Lcclass)) +
  geom_point()

summary(lm(rootNPercent~vpd,data=plot.df.2)) 
#SIGNIFICANT-positive effect

#PPT seasonality
ggplot(plot.df.worldclim,aes(PPT_seasonality,rootNPercent,color=Lcclass)) +
  geom_point()

summary(lm(rootNPercent~PPT_seasonality,data=plot.df.worldclim)) 
#moderately significant 

#total soil N-----

# MAT
ggplot(plot.df.worldclim,aes(MAT,soilNPercent_MHoriz_mean,color=Lcclass)) +
  geom_point()

summary(lm(soilNPercent_MHoriz_mean~MAT,data=plot.df.worldclim)) 
#N.S

# MAP*
ggplot(plot.df.worldclim,aes(MAP,soilNPercent_MHoriz_mean,color=Lcclass)) +
  geom_point()

summary(lm(soilNPercent_MHoriz_mean~MAP,data=plot.df.worldclim)) 
#SIGNIFICANT - positive 

# VPD*
ggplot(plot.df.2,aes(vpd,soilNPercent_MHoriz_mean,color=Lcclass)) +
  geom_point()

summary(lm(soilNPercent_MHoriz_mean~vpd,data=plot.df.2)) 
#SIGNIFICANT negative

#PPT seasonality
ggplot(plot.df.worldclim,aes(PPT_seasonality,soilNPercent_MHoriz_mean,color=Lcclass)) +
  geom_point()

summary(lm(soilNPercent_MHoriz_mean~PPT_seasonality,data=plot.df.worldclim)) 
#NS

#all climate variables explain very little <3% variance in N pools


# leaf N
plot(foliarNPercent_mean~MAT.2,data=plot.df)
plot(foliarNPercent_mean~vpd,data=plot.df)
plot(foliarNPercent_mean~MAP.2,data=plot.df)


# figures -------


# now make the bivariates with climate variable
summary(lm(soilNPercent_MHoriz_mean~MAP,data=plot.df))
# weakly significant, negative effect of vpd on soil N

#total soil N
total_soil_climate <-
  ggplot(plot.df,aes(MAP,soilNPercent_MHoriz_mean)) +
  geom_smooth(aes(group=1),color="black",
              method="lm", se=FALSE, linetype="solid",size=0.5, fullrange=TRUE) +
  annotate(x=450, y=2, label="R-squared = 0.028", geom="text", size=2.5) +
  #scale_y_continuous(expand = c(0,0)) +
  geom_point(color='black',pch=21,alpha=0.5) +
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
summary(lm(rootNPercent~MAP,data=plot.df))
# weakly negative significant relationship
root_climate <-
  ggplot(plot.df,aes(MAP,rootNPercent,fill=Lcclass)) +
  geom_smooth(aes(group=1),color="black",
              method="lm", se=FALSE, linetype="solid",size=0.5, fullrange=TRUE) +
  annotate(x=1800, y=2, label="R-squared = 0.064", geom="text", size=2.5) +
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
summary(lm(foliarNPercent_mean~MAP,data=plot.df))
#weak negative non-significant relationship
leaf_climate <-
  ggplot(plot.df,aes(MAP,foliarNPercent_mean,fill=Lcclass)) +
  #scale_y_continuous(expand = c(0,0)) +
  geom_point(color='black',pch=21,alpha=0.5) +
  #stat_smooth(method='lm') +
  scale_fill_manual(values=c('herb'='blue','woody'='red'),
                    labels=c('herb'='Herbaceous','woody'='Woody')) +
  xlab('Mean annual precipitation (mm)') +
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

