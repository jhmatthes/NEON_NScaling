

#look at bloclim data

head(plot.df.2) #already just sites for continental US since merged with VPD

#We have Adrienne's full climate dataset
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

plot.df.worldclim<-merge(plot.df,bioclim_vars,by=c('siteID'))

#plot(vpd~MAP,data=plot.df.worldclim)

#compare which of these is a better predictor of N pools (use newest MAP)
head(plot.df.worldclim)
library(ggplot2)

#simplify to woody versus herb vegetation types
plot.df.worldclim <- rename_lcc(plot.df.worldclim,crop = F)

#see how correlated the variables are


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

# alll pretty weak relationships

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

