#functions

#function to rename cover classes to broader groups
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

