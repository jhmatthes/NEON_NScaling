# last updated: 2020-11-30
# author: Adrienne Keller
# project: NEON N scaling
# notes: 

################################################################################

### source NEON data using R scripts - NOTE: skip this step to use date-stamped
#  data for our manuscript
#source(file = "R/download_data.R")
#source(file = "R/prelim_processing.R")

### source date-stamped NEON data for manuscript - NOTE: skip if using newly downloaded
#  data from NEON as sourced above
plot.df <- read.csv(file = "CN_plotID.csv", stringsAsFactors = F)
# head(plot.df)
# names(plot.df)
#unique(plot.df$siteID)

# estimate resorption
plot.df$resorpN <- (plot.df$foliarNPercent_mean - plot.df$litterNPercent_mean)/
  plot.df$foliarNPercent_mean*100

### load climate data (focus on mean annual precipitation)
climate.df<-read.csv('./../data_pre-processed/bioclim_data.csv', #file path is relative
                  header = T, stringsAsFactors = F)
climate.df <- climate.df[c(1,2,3,15)]
colnames(climate.df) <- c('x','y','siteID','MAP')
# head(climate.df)
# summary(climate.df)

#merge the plot and climate data by site name
# nrow(plot.df)
plot.df <- left_join(plot.df, climate.df, by ="siteID")
# head(plot.df)
length(unique(plot.df$siteID)) # 46 sites

#load veg type data 
vegtype.df <- read.csv('./../data_pre-processed/LCclass_polys.csv', #file path is relative
                       header = T, stringsAsFactors = F)
length(unique(vegtype.df$siteID)) # 47 UNIQUE sites

#simplify to woody versus herbaceous
vegtype.df <- vegtype.df[c(2,4)]
colnames(vegtype.df) <- c('siteID','Lcclass')
vegtype.df<-rename_lcc(vegtype.df,crop=F)
vegtype.df<-vegtype.df[!duplicated(vegtype.df),]
#length(unique(vegtype.df$siteID)) #43 unique sites
# WOOD, DCFS, KONA, LAJA are all croplands and they get removed

#merge plot and veg type data by site name
#nrow(plot.df) # 1636
plot.df <- merge(plot.df, vegtype.df, by = "siteID")
length(unique(plot.df$siteID)) # 42 unique sites

#select key columns to simplify (this can change)
names(plot.df)
myvars <- c('domainID', "siteID", "plotID","rootNPercent",'rootCPercent','rootCNratio',
            'plotType','litterNPercent_mean','foliarNPercent_mean','foliarCPercent_mean',
            'foliarCNRatio_mean','soilNPercent_MHoriz_mean','soilCNRatio_MHoriz_mean',
            'pctSand', 'pctSilt', 'pctClay', 'resorpN','inorganicN', 
            'Lcclass','litterCNRatio_mean','netNminugPerGramPerDay','soilCPercent_MHoriz_mean')

#look at the data
unique(plot.df$siteID) #sites


