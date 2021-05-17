

#look at bloclim data

head(plot.df)

bioclim<-read.csv('./../data_pre-processed/bioclim_data.csv', #file path is relative
                  header = T, stringsAsFactors = F)
head(bioclim)

# we want bio1 (MAT), bio12 (MAP), and bio15 (PPT seasonality)

bioclim_vars <- bioclim[c(1,2,3,4,15,18)]
head(bioclim_vars)

#rename columns
colnames(bioclim_vars) <- c('x','y','siteID','MAT.2','MAP.2','PPT_seasonality')
head(bioclim_vars)

plot.df<-merge(plot.df,bioclim_vars,by=c('siteID'))

plot(vpd~MAP.2,data=plot.df)

# get aridity data
vpd <- read.csv('./../data_pre-processed/scaled_vpd.csv')
head(vpd)

#cleanup
vpd<-vpd[c(2,3)]
colnames(vpd) <- c('siteID','vpd')

plot.df <- merge(vpd, plot.df,by=c('siteID'))
plot(MAP.2~vpd,data=plot.df)

#compare which of these is a better predictor of N pools (use newest MAP)
head(plot.df)
library(ggplot2)

#simplify to woody versus herb vegetation types
plot.df <- rename_lcc(plot.df,crop = F)

#see how correlated the variables are


# leaf N

# MAT
ggplot(plot.df,aes(MAT.2,foliarNPercent_mean,color=Lcclass)) +
  geom_point()

#MAP
ggplot(plot.df,aes(MAP.2,foliarNPercent_mean,color=Lcclass)) +
  geom_point()

#VPD
ggplot(plot.df,aes(vpd,foliarNPercent_mean,color=Lcclass)) +
  geom_point()

#PPT seasonality
ggplot(plot.df,aes(PPT_seasonality,foliarNPercent_mean,color=Lcclass)) +
  geom_point()

#none especially related to foliar N

#root N

# MAT
ggplot(plot.df,aes(MAT.2,rootNPercent,color=Lcclass)) +
  geom_point()
#summary(lm(rootNPercent~MAT.2,data=plot.df)) N.S

# MAP
ggplot(plot.df,aes(MAP.2,rootNPercent,color=Lcclass)) +
  geom_point()

summary(lm(rootNPercent~MAP.2,data=plot.df)) #significant!

# VPD
ggplot(plot.df,aes(vpd,rootNPercent,color=Lcclass)) +
  geom_point()

#PPT seasonality
ggplot(plot.df,aes(PPT_seasonality,rootNPercent,color=Lcclass)) +
  geom_point()

#total soil N

# MAT
ggplot(plot.df,aes(MAT.2,soilNPercent_MHoriz_mean,color=Lcclass)) +
  geom_point()

# MAP
ggplot(plot.df,aes(MAP.2,soilNPercent_MHoriz_mean,color=Lcclass)) +
  geom_point()

# VPD
ggplot(plot.df,aes(vpd,soilNPercent_MHoriz_mean,color=Lcclass)) +
  geom_point()

#PPT seasonality
ggplot(plot.df,aes(PPT_seasonality,soilNPercent_MHoriz_mean,color=Lcclass)) +
  geom_point()


#ambiguous if any of these are really better. 


# leaf N
plot(foliarNPercent_mean~MAT.2,data=plot.df)
plot(foliarNPercent_mean~vpd,data=plot.df)
plot(foliarNPercent_mean~MAP.2,data=plot.df)

