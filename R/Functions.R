#functions

#function to rename cover classes to broader groups------
rename_lcc<-function(x,crop=T){
  
  
  if(crop==T){
  
    x$Lcclass<-
    gsub("DECIDUOUS_FOREST", "woody", x$Lcclass)
  
  #mixed forest to just forest
  x$Lcclass<-
    gsub("EVERGREEN_FOREST", "woody", x$Lcclass)
  
  #woody savannas to just savanna
  x$Lcclass<-
    gsub("WOODY_SAVA", "woody", x$Lcclass)
  
  #rename shrubs
  x$Lcclass<-
    gsub("OPEN_SHRUB", "woody", x$Lcclass)
  
  x$Lcclass<-
    gsub("GRASSLANDS", "herb", x$Lcclass)
  
  x$Lcclass<-
    gsub("SAVANNAS", "herb", x$Lcclass)
  
  x$Lcclass<-
    gsub("CROPLANDS", "herb", x$Lcclass)
  
  x$Lcclass<-
    gsub("MIXED_FOREST", "herb", x$Lcclass)}else{
    
    x<- x %>%
      dplyr::filter(!(Lcclass=="CROPLANDS"))
    
    x$Lcclass<-
        gsub("DECIDUOUS_FOREST", "woody", x$Lcclass)
      
      #mixed forest to just forest
      x$Lcclass<-
        gsub("EVERGREEN_FOREST", "woody", x$Lcclass)
      
      #woody savannas to just savanna
      x$Lcclass<-
        gsub("WOODY_SAVA", "woody", x$Lcclass)
      
      #rename shrubs
      x$Lcclass<-
        gsub("OPEN_SHRUB", "woody", x$Lcclass)
      
      x$Lcclass<-
        gsub("GRASSLANDS", "herb", x$Lcclass)
      
      x$Lcclass<-
        gsub("SAVANNAS", "herb", x$Lcclass)
      
      x$Lcclass<-
        gsub("MIXED_FOREST", "herb", x$Lcclass)
    
    
  }
      
      
  
  return(x)
  
}

# function to select plant-soil N site combinations with at least 4 replicates----

# work from this:

mean_soil_foliar_cn <- merge(mean_soil_cn, mean_foliar_cn, by = c('siteID', 'plotID'))

# get site reps
length_mean_soil_foliar_cn <- aggregate(plotID ~ siteID, length, data = mean_soil_foliar_cn)
colnames(length_mean_soil_foliar_cn) <- c('siteID','reps')

#remove sites with less than 4 replicates
length_mean_soil_foliar_cn_reps <- length_mean_soil_foliar_cn %>%
  dplyr::filter(reps > 3)

#merge so sites with < 4 reps are removed
merge_mean_soil_foliar_cn <- merge(length_mean_soil_foliar_cn_reps,mean_soil_foliar_cn,by=c('siteID'))

#get site means
merge_mean_soil_foliar_cn   <- merge_mean_soil_foliar_cn   %>%
  group_by(siteID) %>%
  summarize(soilCNRatio_MHoriz_mean = mean(soilCNRatio_MHoriz_mean),
            foliarCNRatio_mean = mean(foliarCNRatio_mean))
merge_mean_soil_foliar_cn <- data.frame(merge_mean_soil_foliar_cn)
length(merge_mean_soil_foliar_cn$siteID) 
#N = 20 sites

#round to two decimal places
merge_mean_soil_foliar_cn$foliarCNRatio_mean <- round(merge_mean_soil_foliar_cn$foliarCNRatio_mean,2)
merge_mean_soil_foliar_cn$soilCNRatio_MHoriz_mean <- round(merge_mean_soil_foliar_cn$soilCNRatio_MHoriz_mean,2)