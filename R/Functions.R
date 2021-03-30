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

filter_reps <- function(x,y,name1,name2){

mean_pools <- merge(x, y, by = c('siteID', 'plotID'))

# get site reps
length_mean_pools <- aggregate(plotID ~ siteID, length, data = mean_pools)
colnames(length_mean_pools) <- c('siteID','reps')

#remove sites with less than 4 replicates
length_mean_pools_reps <- length_mean_pools %>%
  dplyr::filter(reps > 3)

#merge so sites with < 4 reps are removed
merge_mean_pools <- merge(length_mean_pools_reps,mean_pools,by=c('siteID'))

#get site means
merge_mean_pools   <- merge_mean_pools %>%
  group_by(siteID) %>%
  summarise_all(mean)
merge_mean_pools <- data.frame(merge_mean_pools)
#length(merge_mean_pools$siteID) 
#N = 20 sites

#round to two decimal places
# merge_mean_pools$name1 <- round(merge_mean_pools$name1,2)
# merge_mean_pools$name2 <- round(merge_mean_pools$name2,2)

return(merge_mean_pools)

}


