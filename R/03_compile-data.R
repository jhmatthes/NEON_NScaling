# last updated: 2020-10-26
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
plot.df <- read.csv(file = "data_pre-processed/CN_plotID.csv", stringsAsFactors = F)
head(dat)

### load climate data
climate.df <- read.csv('data_pre-processed/MAT_MAP_Allsites.csv', header = T, 
                       stringsAsFactors = F)
climate.df$MAP<-climate.df$MAP*10 # change from cm to mm
head(climate.df)

### merge the plot and climate data by site name
plot.df <- left_join(plot.df, climate.df, by ="siteID")
head(plot.df)

#select key columns to simplify (this can change)
myvars <- c('domainID', "siteID", "plotID","rootNPercent",'rootCPercent','rootCNratio',
            'rootSample_n','plotType','litterNPercent','foliarNPercent','foliarCPercent',
            'foliarCNRatio','soilNPercent_MHoriz','soilCNRatio_MHoriz','nlcdClass',
            'x','y', 'MAT', 'MAP')

plot.df <- plot.df[myvars]
head(plot.df)

#remove duplicates resulting from the merge (linked to slight differences in coordinates?)
nrow(plot.df) #check row # before removing duplicates #1256 rows
plot.df <- plot.df[!duplicated(plot.df), ]
nrow(plot.df) #check that this worked #1123 rows

#look at the data
unique(plot.df$nlcdClass) #cover class
unique(plot.df$siteID) #sites




