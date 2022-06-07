# last updated: 2022-5-30
# author: Andrew Felton
# project: NEON N scaling
# notes: 

################################################################################

### source NEON data using R scripts - NOTE: skip this step to use date-stamped
#  data for our manuscript
#source(file = "R/download_data.R")
#source(file = "R/prelim_processing.R")

### source date-stamped NEON data for manuscript - NOTE: skip if using newly downloaded
# data from NEON as sourced above
plot_df <- read.csv(file = "data/CN_full_dataset.csv", stringsAsFactors = F)
plot_df <- plot_df %>%
  filter(!plotType=='NA')

head(plot_df)
sand <- plot_df %>%
  dplyr::select(siteID,plotID,pctSand)
sand <- na.omit(sand)
# head(plot_df)
# names(plot_df)
#unique(plot_df$siteID)

# load climate data (focus on mean annual precipitation)
climate_df <- read.csv('data/bioclim_data.csv', #file path is relative
                  header = T, stringsAsFactors = F)
climate_df <- climate_df %>% dplyr::select('x','y','siteID','Bio.1', 'Bio.12') %>%
  rename('MAT' = 'Bio.1',
         'MAP' = 'Bio.12')

# load mean NPP data and merge with climate data
npp_df <- read.csv('data/NPP_data.csv')
npp_df <- npp_df %>%
  dplyr::select(siteID,npp_g_m2)

#merge
npp_climate_df <- merge(climate_df,npp_df,by=c('siteID'))

#merge the plot and climate data by site name

plot_df <- left_join(plot_df, npp_climate_df, by = "siteID")
# head(plot_df)
#length(unique(plot_df$siteID)) 

#load veg type data 
vegtype_df <- read.csv('data/LCclass_polys.csv', #file path is relative
                       header = T, stringsAsFactors = F)
#unique(vegtype_df$class)

#select columns and remove and duplicates
vegtype_df <- vegtype_df %>%
  dplyr::select(siteID,class)
vegtype_df <- vegtype_df[!duplicated(vegtype_df),]

#merge with main dataframe
plot_df <- merge(plot_df,vegtype_df,by = "siteID")
#length(unique(plot_df$siteID))


#remove NEON sites that are croplands
plot_df <- plot_df %>%
  dplyr::filter(!siteID == c('KONA')) %>%
  dplyr::filter(!siteID == c('LAJA'))

#fix classifications for those identified as croplands; they are grasslands
plot_df$class <- gsub("croplands","grasslands",plot_df$class)
plot_df$class <- gsub("water","deciduous broadleaf forest",plot_df$class)

#merge in inorganic soil N data and with merge with main dataset
inorganic_N_per_gram_df <- read.csv('data/inorganic_soil_n_per_gram.csv')
plot_df <- full_join(inorganic_N_per_gram_df,plot_df,by=c('siteID', 'plotID'))

rm(climate_df,inorganic_N_per_gram_df,npp_climate_df,npp_df,vegtype_df)

#unique(plot_df$class)

# First get mean values for each plot-site combination for each pool ------

#get mean site-plot N to prep for self-calculating C:N
mean_foliar <- aggregate(foliarNPercent_mean ~ siteID + plotID,mean,data = plot_df)
mean_root <- aggregate(rootNPercent ~ siteID + plotID, mean, data = plot_df)
mean_soil <- aggregate(soilNPercent_MHoriz_mean ~ siteID + plotID,mean,data = plot_df)
mean_litter <- aggregate(litterNPercent_mean ~ siteID + plotID,mean,data = plot_df)
mean_inorganic_per_g <- aggregate(soilInorganicNugPerGram_mean ~ siteID + plotID,mean,data = plot_df)

#get mean site-plot C:N to prep for comparing to self-calculated C:N
mean_foliar_cn<-aggregate(foliarCNRatio_mean~siteID + plotID,mean,data=plot_df)
mean_root_cn <- aggregate(rootCNratio ~ siteID + plotID, mean, data = plot_df)
mean_soil_cn<-aggregate(soilCNRatio_MHoriz_mean~siteID + plotID,mean,data=plot_df)
mean_litter_cn<-aggregate(litterCNRatio_mean~siteID + plotID,mean,data=plot_df)

#C:N self calculated ------

#calculate values
plot_df$foliar_cn_self_calc <- plot_df$foliarCPercent_mean/plot_df$foliarNPercent_mean
plot_df$root_cn_self_calc <- plot_df$rootCPercent/plot_df$rootNPercent
plot_df$soil_cn_self_calc <- plot_df$soilCPercent_MHoriz_mean/plot_df$soilNPercent_MHoriz_mean
plot_df$litter_cn_self_calc <- plot_df$litterCPercent_mean/plot_df$litterNPercent_mean

#datasets (unclear if used)
mean_foliar_cn_self_calc <- aggregate(foliar_cn_self_calc~siteID + plotID,mean,data=plot_df)
mean_root_cn_self_calc <- aggregate(root_cn_self_calc ~ siteID + plotID, mean, data = plot_df)
mean_soil_cn_self_calc <-aggregate(soil_cn_self_calc~siteID + plotID,mean,data=plot_df)
mean_litter_cn_self_calc <-aggregate(litter_cn_self_calc~siteID + plotID,mean,data=plot_df)

#compare NEON and self-calculated C:N
cor.test(plot_df$foliar_cn_self_calc,plot_df$foliarCNRatio_mean)
#0.98
cor.test(plot_df$root_cn_self_calc,plot_df$rootCNratio)
#0.94
cor.test(plot_df$soil_cn_self_calc,plot_df$soilCNRatio_MHoriz_mean)
#0.99
cor.test(plot_df$litter_cn_self_calc,plot_df$litterCNRatio_mean)
#0.94

#add site-means of soil texture to plot_df ot prep for use as covariate-----

sand_class <- aggregate(pctSand ~ siteID,mean,data=plot_df)
sand_class <- sand_class %>%
  rename('pctSand_mean' = 'pctSand')
sand_class$pctSand_mean = round(sand_class$pctSand_mean,2)
plot_df <- merge(plot_df,sand_class,by=c('siteID'))

unique(plot_df$class)

#simply land cover classes
class <-  c("deciduous broadleaf forest","woody savannas","grasslands","savannas",
            "mixed forest","open shrublands", "evergreen needleleaf forest")
class_2 <- c("forest","savanna","grassland","savanna","forest","shrubland",
             "forest")

class_3 <- data.frame(class,class_2)

plot_df <- merge(plot_df,class_3,by=c('class'))

#trim down columns to those used for analysis in this study

#colnames(plot_df)

myvars <- c("x","y","siteID",'plotType',"plotID",'soilCPercent_MHoriz_mean',
            'soilNPercent_MHoriz_mean',"soil_cn_self_calc","soilInorganicNugPerGram_mean",
            "rootCPercent","rootNPercent",
            "root_cn_self_calc","foliarCPercent_mean","foliarNPercent_mean","foliar_cn_self_calc",
            "litterCPercent_mean","litterNPercent_mean","litter_cn_self_calc",
            "class_2","MAT","MAP","npp_g_m2","pctSand","pctSand_mean")

plot_df <- plot_df[myvars] 

#one last measure, remove any duplicates
plot_df <- plot_df[!duplicated(plot_df),]

#length(unique(plot_df$siteID))

write.csv(plot_df,'data/C_N_data_for_analysis.csv')

#look at data
#aggregate(siteID~plotType,length,data=plot_df)
# plotType siteID
# 1 distributed    439
# 2       tower    727


