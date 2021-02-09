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
head(plot.df)
names(plot.df)

# estimate resorption
plot.df$resorpN <- (plot.df$foliarNPercent_mean - plot.df$litterNPercent_mean)/
  plot.df$foliarNPercent_mean*100

### load climate data
climate.df <- read.csv('./../data_pre-processed/MAT_MAP_Allsites.csv', header = T, 
                       stringsAsFactors = F)
climate.df$MAP<-climate.df$MAP*10 # change from cm to mm
climate.df <- climate.df[ , c("siteID", "MAT", "MAP")]
head(climate.df)

### merge the plot and climate data by site name
nrow(plot.df)
plot.df <- left_join(plot.df, climate.df, by ="siteID")
head(plot.df)

### load veg type data 
vegtype.df <- read.csv('./../data_pre-processed/land-cover-class_Martha_2020-12-09.csv', #file path is relative
                       header = T, stringsAsFactors = F)
vegtype.df <- vegtype.df %>% rename(siteID = site)

### merge plot and veg type data by site name
nrow(plot.df) # 1636
plot.df <- left_join(plot.df, vegtype.df, by = "siteID")

# plot.df <- left_join(plot.df, vegtype.slim, by = "siteID")

#select key columns to simplify (this can change)
names(plot.df)
myvars <- c('domainID', "siteID", "plotID","rootNPercent",'rootCPercent','rootCNratio',
            'plotType','litterNPercent_mean','foliarNPercent_mean','foliarCPercent_mean',
            'foliarCNRatio_mean','soilNPercent_MHoriz_mean','soilCNRatio_MHoriz_mean',
            'pctSand', 'pctSilt', 'pctClay', 'resorpN','inorganicN', 'MAT', 'MAP', 'Lcclass')

plot.df <- plot.df[myvars]
head(plot.df)

#look at the data
unique(plot.df$siteID) #sites




