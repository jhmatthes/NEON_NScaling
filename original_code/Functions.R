#functions

#function to rename cover classes to broader groups
rename_lcc<-function(x){

x$LCclass<-
  gsub("deciduous broadleaf forest", "forest", x$LCclass)

#mixed forest to just forest
x$LCclass<-
  gsub("mixed forest", "forest", x$LCclass)

#woody savannas to just savanna
x$LCclass<-
  gsub("woody savannas", "savannas", x$LCclass)

#rename shrubs
x$LCclass<-
  gsub("open shrublands", "shrublands", x$LCclass)

return(x)

}