#neon N transformations ------


library(neonUtilities)

soilData <- loadByProduct(site = "all", dpID = "DP1.10086.001", package = "basic", check.size = F)
library(devtools) 
#install_github("NEONScience/NEON-Nitrogen-Transformations/neonNTrans", dependencies=TRUE)  
library(neonNTrans) 
out <- def.calc.ntrans(kclInt = soilData$ntr_internalLab,
                       kclIntBlank = soilData$ntr_internalLabBlanks,
                       kclExt = soilData$ntr_externalLab,
                       soilMoist = soilData$sls_soilMoisture,
                       dropAmmoniumFlags = "blanks exceed sample value",
                       dropNitrateFlags = "blanks exceed sample value" )

# Read in inogranic N data
min.df<-as.data.frame(out[1])
# Read in soil texture data
min.df_plotID <- min.df %>%
  select(all_data.domainID, all_data.siteID, all_data.plotID, all_data.netNminugPerGramPerDay) %>%
  group_by(all_data.domainID, all_data.siteID, all_data.plotID) %>%
  summarize(netNminugPerGramPerDay = mean(all_data.netNminugPerGramPerDay, na.rm=TRUE)) %>%
  filter(!netNminugPerGramPerDay=='NaN')

colnames(min.df_plotID) <-c('domainID','siteID','plotID','netNminugPerGramPerDay')
min.df_plotID$netNminugPerGramPerDay<-round(min.df_plotID$netNminugPerGramPerDay,2)
head(min.df_plotID)
hist(min.df_plotID$netNminugPerGramPerDay)

# checking soil texture data -----

head(plot.df)

#look at correlations with soil information 
plot(pctSand~pctClay,data=foliar_lme_text)
#these are strongly negatively correlated and thus colinear

plot(pctSand~MAP,data=foliar_lme_text)
# these are not correlated and thus not colinear

#first see if there are any relationships
plot(inorganicN ~ pctClay,data=plot.df)

coarse_fine_class <- aggregate(pctSand~siteID,mean,data=plot.df)
coarse_fine_class$pctSand = round(coarse_fine_class$pctSand,2)

coarse_fine_class_coarse<-coarse_fine_class %>%
  dplyr::filter(pctSand < 50)
coarse_fine_class_coarse$texture <- 'coarse'

coarse_fine_class_fine<-coarse_fine_class %>%
  dplyr::filter(pctSand > 50)
coarse_fine_class_fine$texture <- 'fine'

coarse_fine_class_fine_course <- rbind(coarse_fine_class_fine,coarse_fine_class_coarse)

plot.df <- merge(plot.df,coarse_fine_class_coarse[c(1,3)],by=c('siteID'))
summary(lm())

summary(lm(rootNPercent~inorganicN*texture,data=plot.df))

#test
merge_soil_root_inorganic <- filter_reps(mean_soil_inorganic, mean_root)
merge_soil_root_inorganic_text <- merge(merge_soil_root_inorganic,coarse_fine_class_fine_course,by=c('siteID'))

summary(lm(rootNPercent~inorganicN*texture,data=merge_soil_root_inorganic_text))


plot(soilNPercent_MHoriz_mean~pctSand,data=plot.df)
summary(lm(soilNPercent_MHoriz_mean~pctSand,data=plot.df))

#head(foliar_lme)
# select columns
foliar_lme_text <- select(plot.df,c('siteID','MAP','foliarNPercent_mean',
                                    'inorganicN','Lcclass','pctSand','pctSilt',
                                    'pctClay'))

# eliminate NAs
foliar_lme_text <- foliar_lme_text %>%
  dplyr::filter(!foliarNPercent_mean=='NA') %>%
  dplyr::filter(!inorganicN =='NA') %>%
  dplyr::filter(!pctSand =='NA')

#check sample sizes
length_foliar_lme_text<-aggregate(foliarNPercent_mean~siteID,length,data=foliar_lme_text)
colnames(length_foliar_lme_text) <- c('siteID','reps')
#length_inorganic_lme<-aggregate(inorganicN~siteID,length,data=foliar_lme_text)

#merge
foliar_lme_text <- merge(foliar_lme_text,length_foliar_lme_text,by=c('siteID'))

#filter out low rep sites
foliar_lme_text <- foliar_lme_text %>%
  dplyr::filter(reps > 3)

#check herb reps
herb.count_text<-subset(foliar_lme_text,Lcclass=='herb')
length(herb.count_text$siteID) #16 obs
length(unique(herb.count_text$siteID)) #4 herb sites

#check woody reps
woody.count<-subset(foliar_lme,Lcclass=='woody')
length(woody.count$siteID) #173 obs
length(unique(woody.count$siteID)) #20 herb sites

# mixed effects model: 

# Chose lme functions because it lets you see P values in summary output

#lme

leaf_lme.1_text<-lme(foliarNPercent_mean~ inorganicN, 
                     random= ~1|siteID,data=foliar_lme_text)
summary(leaf_lme.1_text)
r.squaredGLMM(leaf_lme.1_text)

leaf_lme.1_text_2<-lme(foliarNPercent_mean~ inorganicN + pctSand + MAP, 
                random= ~1|siteID,data=foliar_lme_text)

plot(foliarNPercent_mean~MAP,data=plot.df)
summary(leaf_lme.1_text_2)
r.squaredGLMM(leaf_lme.1_text_2)

#lm
summary(lm(foliarNPercent_mean~ inorganicN + MAP + Lcclass + pctSand, data=foliar_lme_text))

AIC(leaf_lme.1_text)
AIC(leaf_lme.1)

plot(foliarNPercent_mean~pctSand,data=foliar_lme_text)
summary(leaf_lme.1) # inorganic N shows as significant
r.squaredGLMM(leaf_lme.1_text)
r.squaredGLMM(leaf_lme.1)

#marginal r-squared increases, AIC decreases, total sample size decreases


# now look at root N

root_lme_text <- select(plot.df,c('siteID','MAP','rootNPercent',
                                  'inorganicN','Lcclass','pctSand'))
#head(root_lme_text)

# Remove NAs
root_lme_text <- root_lme_text %>%
  dplyr::filter(!rootNPercent=='NA') %>%
  dplyr::filter(!inorganicN =='NA') %>%
  dplyr::filter(!pctSand =='NA') 

#check sample sizes
length_root_lme_text<-aggregate(rootNPercent~siteID,length,data=root_lme_text)
colnames(length_root_lme_text) <- c('siteID','reps')
#length_inorganic_lme<-aggregate(inorganicN~siteID,length,data=root_lme_text)

plot(rootNPercent~pctSand,data=root_lme_text)
summary(lm(rootNPercent~pctSand,data=root_lme_text))

#merge
root_lme_text <- merge(root_lme_text,length_root_lme_text,by=c('siteID'))

#filter out low rep sites
root_lme_text <- root_lme_text %>%
  dplyr::filter(reps > 3)


#check herb sample size
herb.count<-subset(root_lme_text,Lcclass=='herb')
length(herb.count$siteID) #28 obs
length(unique(herb.count$siteID)) #6 herb sites

#check woody sample size
woody.count<-subset(root_lme_text,Lcclass=='woody')
length(woody.count$siteID)
length(unique(woody.count$siteID))

# Mixed effects model:

root_lme_text.1<-lme(rootNPercent~ inorganicN + MAP + Lcclass , random= ~1|siteID,data=root_lme_text)
summary(root_lme_text.1) 
r.squaredGLMM(root_lme_text.1)

##
#exploratory

head(plot.df)

#split MAP categorically into high and low-----


map <- aggregate(MAP~siteID,mean,data=plot.df)

# root and inorganic N ------

#site with at least 4 reps
merge_soil_root_inorganic <- filter_reps(mean_soil_inorganic, mean_root)


plot.df.root.soil <- select(plot.df,c('siteID','plotID', 'MAP','rootNPercent',
                                    'inorganicN','climate','texture'))

plot.df.root.soil <-plot.df.root.soil %>%
  dplyr::filter(!rootNPercent=='NA') %>%
  dplyr::filter(!inorganicN =='NA')

plot.df.root.soil <- merge(plot.df.root.soil,merge_soil_root_inorganic[c(1,2)],
                           by=c('siteID'))

#basic LME with just soil

soil_root_inorganic_map_lme <-lme(rootNPercent~inorganicN,
                          random= ~1|siteID,data=plot.df.root.soil)

summary(soil_root_inorganic_map_lme)
r.squaredGLMM(soil_root_inorganic_map_lme)

#now do climate interaction

soil_root_inorganic_map_lme_climate <-lme(rootNPercent~inorganicN + 
                                            climate:inorganicN,
                                  random= ~1|siteID,data=plot.df.root.soil)

summary(soil_root_inorganic_map_lme_climate)
anova.lme(soil_root_inorganic_map_lme_climate,type='marginal')
r.squaredGLMM(soil_root_inorganic_map_lme_climate)

# ggplot(plot.df.root.soil,aes(inorganicN,rootNPercent,color=climate)) +
#   stat_smooth(method='lm') +
#   geom_point()

#now do soil interaction

soil_root_inorganic_map_lme_texture <-lme(rootNPercent~inorganicN +
                                            inorganicN:texture,
                                          random= ~1|siteID,data=plot.df.root.soil)

summary(soil_root_inorganic_map_lme_texture)
r.squaredGLMM(soil_root_inorganic_map_lme_texture)

summary(root_foliar_map_lme_texture)
r.squaredGLMM(root_foliar_map_lme_texture)
  

anova(soil_root_inorganic_map_lme_climate,soil_root_inorganic_map_lme)

#now do root and leaf N-----

?lmer
?lme

plot.df.root.foliar <- select(plot.df,c('siteID','plotID', 'MAP',
                                        'foliarNPercent_mean','rootNPercent',
                                        'texture','climate'))

merge_mean_foliar_root <- filter_reps(mean_foliar, mean_root)

plot.df.root.foliar <- plot.df.root.foliar %>%
  dplyr::filter(!rootNPercent=='NA') %>%
  dplyr::filter(!foliarNPercent_mean =='NA') 

plot.df.root.foliar <- merge(plot.df.root.foliar,merge_mean_foliar_root[c(1,2)],
                             by=c('siteID'))


#basic LME with just soil

root.foliar_lme <-lme(foliarNPercent_mean~rootNPercent,
                                  random= ~1|siteID,data=plot.df.root.foliar)

summary(root.foliar_lme)
r.squaredGLMM(root.foliar_lme)

#now do climate interaction

root.foliar_lme_climate <-lme(foliarNPercent_mean~rootNPercent +
                                rootNPercent:climate,
                                          random= ~1|siteID,data=plot.df.root.foliar)

summary(root.foliar_lme_climate)
r.squaredGLMM(root.foliar_lme_climate)

#now do soil interaction

root.foliar_lme_texture <-lme(foliarNPercent_mean~rootNPercent +
                                rootNPercent:texture,
                              random= ~1|siteID,data=plot.df.root.foliar)

summary(root.foliar_lme_texture)
r.squaredGLMM(root.foliar_lme_texture)


# root and soil C:N ------
head(plot.df)

plot.df.soil.root.cn <- select(plot.df,c('siteID','plotID', 'MAP',
                                        'rootCNratio','soilCNRatio_MHoriz_mean',
                                        'pctSand_mean','climate','texture'))

plot.df.soil.root.cn <-plot.df.soil.root.cn %>%
  dplyr::filter(!rootCNratio=='NA') %>%
  dplyr::filter(!soilCNRatio_MHoriz_mean =='NA') 

merge_mean_soil_root_cn<-filter_reps(mean_soil_cn, mean_root_cn)

plot.df.soil.root.cn <- merge(plot.df.soil.root.cn,merge_mean_soil_root_cn[c(1,2)],
                              by=c('siteID'))


#try lme

#just soil C:N

root_foliar_cn_lme <-lme(rootCNratio~ soilCNRatio_MHoriz_mean,
                                  random= ~1|siteID,data=plot.df.soil.root.cn)

summary(root_foliar_cn_lme)
r.squaredGLMM(root_foliar_cn_lme)

#climate interaction

root_foliar_cn_lme_climate <-lme(rootCNratio~ soilCNRatio_MHoriz_mean + 
                                   soilCNRatio_MHoriz_mean:climate ,
                                  random= ~1|siteID,data=plot.df.soil.root.cn)

summary(root_foliar_cn_lme_climate)
r.squaredGLMM(root_foliar_cn_lme_climate)

ggplot(plot.df.soil.root.cn,aes(soilCNRatio_MHoriz_mean,rootCNratio,color=climate)) +
  stat_smooth(method='lm') +
  geom_point()


#add texture interaction
root_foliar_cn_lme_texture <-lme(rootCNratio~ soilCNRatio_MHoriz_mean +
                                   soilCNRatio_MHoriz_mean:texture,
                                 random= ~1|siteID,data=plot.df.soil.root.cn)

summary(root_foliar_cn_lme_texture)
r.squaredGLMM(root_foliar_cn_lme_texture)

# ggplot(plot.df.soil.root.cn,aes(soilCNRatio_MHoriz_mean,rootCNratio,color=texture)) +
#   stat_smooth(method='lm') +
#   geom_point()


#not significant interaction in dry verses wet sites in mixed effects,
# though soil C:N explains similar levels of variation in root C:N across
# dry and wet sites

#add coarse and fine textured soil category



summary(lm(foliarNPercent_mean~ inorganicN*climate,data=plot.df.high.low.map))

plot(rootNPercent~ inorganicN,data=plot.df.high.low.map)
  

# foliar and soil C:N----

head(plot.df)

plot.df.soil.foliar.cn <- select(plot.df,c('siteID','plotID', 'MAP',
                                         'foliarCNRatio_mean','soilCNRatio_MHoriz_mean',
                                         'climate','texture'))

plot.df.soil.foliar.cn <- plot.df.soil.foliar.cn %>%
  dplyr::filter(!foliarCNRatio_mean=='NA') %>%
  dplyr::filter(!soilCNRatio_MHoriz_mean =='NA')


merge_mean_soil_foliar_cn <- filter_reps(mean_soil_cn, mean_foliar_cn)

plot.df.soil.foliar.cn <- merge(plot.df.soil.foliar.cn,merge_mean_soil_foliar_cn[c(1,2)],
                                by=c('siteID'))

#does the relationship between foliar and soil C:N differ in dry versus wet sites?

#Just with soil covariate
foliar_soil_cn_lme<-lme(foliarCNRatio_mean~soilCNRatio_MHoriz_mean,random=~1|siteID,
           data=plot.df.soil.foliar.cn)
summary(foliar_soil_cn_lme)
r.squaredGLMM(foliar_soil_cn_lme)

#climate interaction
foliar_soil_cn_map_lme <-lme(foliarCNRatio_mean~ soilCNRatio_MHoriz_mean +
                               soilCNRatio_MHoriz_mean:climate, 
                          random= ~1|siteID,data=plot.df.soil.foliar.cn)
summary(foliar_soil_cn_map_lme)
r.squaredGLMM(foliar_soil_cn_map_lme)

ggplot(plot.df.soil.foliar.cn,aes(soilCNRatio_MHoriz_mean,foliarCNRatio_mean,color=climate)) +
  stat_smooth(method='lm') +
  geom_point()

#Significant interaction in dry verses wet sites in mixed effects, folair C:N
# is less sensitive to soil C:N across wet sites.


summary(lm(foliarNPercent_mean~ inorganicN*climate,data=plot.df.high.low.map))

plot(foliarNPercent~ inorganicN,data=plot.df.high.low.map)
#
#--------summary -----

# for the relationships that were significant across all sites, those relationships
# also differ in wet versus dry sites.
# for root and inorganic soil N, the slope is weaker in wet sites, but there is
# a tighter relationship (less unexplained variation)
# for root and soil N, there is both a higher slope and tighter relationship
# in wetter ecosystems
# for root and foliar C:N spatial relationships with soil C:N, the slopes
# are greater in dry ecosystems, but there are similar amounts of variation
# explained in wet and dry systems
 



