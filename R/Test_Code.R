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
# select columns
foliar_lme_text <- select(plot.df,c('siteID','MAP','foliarNPercent_mean',
                                    'inorganicN','Lcclass','pctSand'))
#head(foliar_lme)

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
leaf_lme.1_text<-lme(foliarNPercent_mean~ inorganicN + MAP + Lcclass + pctSand  , 
                random= ~1|siteID,data=foliar_lme_text)

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

# root and inorganic N ------

#low
summary(lm(rootNPercent~ inorganicN + MAP,data=plot.df))
summary(plot.df$MAP)
hist(plot.df$MAP)

plot.df.root.soil <- select(plot.df,c('siteID','plotID', 'MAP','rootNPercent',
                                    'inorganicN'))

plot.df.root.soil <-plot.df.root.soil %>%
  dplyr::filter(!rootNPercent=='NA') %>%
  dplyr::filter(!inorganicN =='NA')
  
plot.df.low.map <- plot.df.root.soil %>% 
  dplyr::filter(MAP < 1000)
dim(plot.df.low.map)

#check # of sites
length(unique(plot.df.low.map$siteID))
plot(rootNPercent~ inorganicN,data=plot.df.low.map)
summary(lm(rootNPercent~ inorganicN,data=plot.df.low.map))  
#slope = 0.64

# high
plot.df.high.map <- plot.df.root.soil %>% 
  dplyr::filter(MAP > 1000)
length(unique(plot.df.high.map$plotID))

plot(rootNPercent~ inorganicN,data=plot.df.high.map)
summary(lm(rootNPercent~ inorganicN,data=plot.df.high.map)) 
#slope = 0.35

plot.df.high.map$climate <- 'wet'
plot.df.low.map$climate <- 'dry'

plot.df.high.low.map <- rbind(plot.df.high.map,plot.df.low.map)
aggregate(MAP~climate,length,data=plot.df.high.low.map)

#does the relationship between root and soil N differ in dry versus wet sites?
summary(lm(rootNPercent~ inorganicN*climate,data=plot.df.high.low.map))
#moderately significant interaction

root_soil_map_lme <-lme(rootNPercent~ inorganicN*climate, random= ~1|siteID,data=plot.df.high.low.map)
summary(root_soil_map_lme)
#not significant interaction in dry verses wet sites in mixed effects

summary(lm(foliarNPercent_mean~ inorganicN*climate,data=plot.df.high.low.map))

plot(rootNPercent~ inorganicN,data=plot.df.high.low.map)


#now do root and leaf N-----

plot.df.root.foliar <- select(plot.df,c('siteID','plotID', 'MAP',
                                        'foliarNPercent_mean','rootNPercent'))

plot.df.root.foliar <- plot.df.root.foliar %>%
  dplyr::filter(!rootNPercent=='NA') %>%
  dplyr::filter(!foliarNPercent_mean =='NA')

plot.df.low.map.root.foliar <- plot.df.root.foliar  %>% 
  dplyr::filter(MAP < 1000)
dim(plot.df.low.map.root.foliar)

#check # of sites
length(unique(plot.df.low.map.root.foliar$siteID))
plot(foliarNPercent_mean~rootNPercent,data=plot.df.low.map.root.foliar)
summary(lm(foliarNPercent_mean~rootNPercent,data=plot.df.low.map.root.foliar))  
#slope = 0.36. N.S.Adjusted R-squared:  0.01663 

# high
plot.df.high.map.root.foliar <- plot.df.root.foliar %>% 
  dplyr::filter(MAP > 1000)
length(unique(plot.df.high.map.root.foliar$plotID))

plot(foliarNPercent_mean~rootNPercent,data=plot.df.high.map.root.foliar)
summary(lm(foliarNPercent_mean~rootNPercent,data=plot.df.high.map.root.foliar)) 
#slope = 1.5. Adjusted R-squared:0.3129

plot.df.high.map.root.foliar$climate <- 'wet'
plot.df.low.map.root.foliar$climate <- 'dry'

plot.df.high.low.map.root.foliar <- rbind(plot.df.high.map.root.foliar,
                                          plot.df.low.map.root.foliar)
aggregate(MAP~climate,length,data=plot.df.high.low.map.root.foliar)

#does the relationship between root and soil N differ in dry versus wet sites?
summary(lm(foliarNPercent_mean~ rootNPercent*climate,
           data=plot.df.high.low.map.root.foliar))
#significant interaction: Wet sites have stronger foliar-root N linkages

root_foliar_map_lme <-lme(foliarNPercent_mean~ rootNPercent*climate, 
                          random= ~1|siteID,data=plot.df.high.low.map.root.foliar)
summary(root_foliar_map_lme)
#not significant interaction in dry verses wet sites in mixed effects

summary(lm(foliarNPercent_mean~ inorganicN*climate,data=plot.df.high.low.map))

plot(rootNPercent~ inorganicN,data=plot.df.high.low.map)


# root and soil C:N ------
head(plot.df)

plot.df.soil.root.cn <- select(plot.df,c('siteID','plotID', 'MAP',
                                        'rootCNratio','soilCNRatio_MHoriz_mean'))

plot.df.soil.root.cn <- plot.df.soil.root.cn %>%
  dplyr::filter(!rootCNratio=='NA') %>%
  dplyr::filter(!soilCNRatio_MHoriz_mean =='NA')

#go to low MAP
plot.df.low.map.soil.root.cn <- plot.df.soil.root.cn  %>% 
  dplyr::filter(MAP < 1000)
dim(plot.df.low.map.soil.root.cn)

plot(rootCNratio~soilCNRatio_MHoriz_mean,data=plot.df.low.map.soil.root.cn)
summary(lm(rootCNratio~soilCNRatio_MHoriz_mean,data=plot.df.low.map.soil.root.cn))  
#slope = 2.2. Adjusted R-squared:  0.3612

# high
plot.df.high.map.soil.root.cn <- plot.df.soil.root.cn %>% 
  dplyr::filter(MAP > 1000)
dim(plot.df.high.map.soil.root.cn)

plot(rootCNratio~soilCNRatio_MHoriz_mean,data=plot.df.high.map.soil.root.cn)
summary(lm(rootCNratio~soilCNRatio_MHoriz_mean,data=plot.df.high.map.soil.root.cn)) 
#slope = 1.5. Adjusted R-squared:0.3129

plot.df.high.map.soil.root.cn$climate <- 'wet'
plot.df.low.map.soil.root.cn$climate <- 'dry'

plot.df.high.low.map.soil.root.cn <- rbind(plot.df.high.map.soil.root.cn,
                                          plot.df.low.map.soil.root.cn)
aggregate(MAP~climate,length,data=plot.df.high.low.map.soil.root.cn)

#does the relationship between root and soil C:N differ in dry versus wet sites?
summary(lm(rootCNratio~soilCNRatio_MHoriz_mean*climate,
           data=plot.df.high.low.map.soil.root.cn))
#significant interaction: Wet sites weaker soil-root C:N slopes

root_foliar_map_lme <-lme(rootCNratio~ rootNPercent*climate, 
                          random= ~1|siteID,data=plot.df.high.low.map.soil.root.cn)
summary(root_foliar_map_lme)
#not significant interaction in dry verses wet sites in mixed effects,
# though soil C:N explains similar levels of variation in root C:N across
# dry and wet sites

summary(lm(foliarNPercent_mean~ inorganicN*climate,data=plot.df.high.low.map))

plot(rootNPercent~ inorganicN,data=plot.df.high.low.map)
  

# foliar and soil C:N----

head(plot.df)

plot.df.soil.foliar.cn <- select(plot.df,c('siteID','plotID', 'MAP',
                                         'foliarCNRatio_mean','soilCNRatio_MHoriz_mean'))

plot.df.soil.foliar.cn <- plot.df.soil.foliar.cn %>%
  dplyr::filter(!foliarCNRatio_mean=='NA') %>%
  dplyr::filter(!soilCNRatio_MHoriz_mean =='NA')

#go to low MAP
plot.df.low.map.soil.foliar.cn <- plot.df.soil.foliar.cn  %>% 
  dplyr::filter(MAP < 1000)
dim(plot.df.low.map.soil.foliar.cn)

plot(foliarCNRatio_mean~soilCNRatio_MHoriz_mean,data=plot.df.low.map.soil.foliar.cn)
summary(lm(foliarCNRatio_mean~soilCNRatio_MHoriz_mean,data=plot.df.low.map.soil.foliar.cn))  
#slope = 2.1 Adjusted R-squared:  0.36

# high
plot.df.high.map.soil.foliar.cn <- plot.df.soil.foliar.cn %>% 
  dplyr::filter(MAP > 1000)
dim(plot.df.high.map.soil.foliar.cn)

plot(foliarCNRatio_mean~soilCNRatio_MHoriz_mean,data=plot.df.high.map.soil.foliar.cn)
summary(lm(foliarCNRatio_mean~soilCNRatio_MHoriz_mean,data=plot.df.high.map.soil.foliar.cn)) 
#slope = 1.1. Adjusted R-squared:0.40

plot.df.high.map.soil.foliar.cn$climate <- 'wet'
plot.df.low.map.soil.foliar.cn$climate <- 'dry'

plot.df.high.low.map.soil.foliar.cn <- rbind(plot.df.high.map.soil.foliar.cn,
                                           plot.df.low.map.soil.foliar.cn)
aggregate(MAP~climate,length,data=plot.df.high.low.map.soil.foliar.cn)

#does the relationship between foliar and soil C:N differ in dry versus wet sites?
summary(lm(foliarCNRatio_mean~soilCNRatio_MHoriz_mean*climate,
           data=plot.df.high.low.map.soil.foliar.cn))
#significant interaction: Wet sites weaker soil-foliar C:N slopes

foliar_soil_cn_map_lme <-lme(foliarCNRatio_mean~ soilCNRatio_MHoriz_mean*climate, 
                          random= ~1|siteID,data=plot.df.high.low.map.soil.foliar.cn)
summary(foliar_soil_cn_map_lme)
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
 



