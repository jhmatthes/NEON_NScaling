# preliminary model structure/analysis code for objectives 1 and 2:

#models
#after mean bivariates= relationships are done. 
#I asume just withwith OlS regression?

# X in place for whatever we name the dataframes...
#MAP = mean annual precipitation
#VEG = dominant vegetation type
#TEX = index of soil texture...% sand...% clay, coarse versus fine
#need: a model selection procedure/improtant valu index 

#soil and MAP
soil.to.leaf.map.lme<-lme(foliarNPercent~soilNPercent_MHoriz + MAP,data=X,
                          random=~1|siteID,na.action=na. omit,method="REML")

#soil and veg
soil.to.leaf.veg.lme<-lme(foliarNPercent~soilNPercent_MHoriz + VEG,data=X,
                          random=~1|siteID,na.action=na. omit,method="REML")

#soil and texture
soil.to.leaf.tex.lme<-lme(foliarNPercent~soilNPercent_MHoriz + TEX,data=X,
                          random=~1|siteID,na.action=na. omit,method="REML")

AIC(soil.to.leaf.map.lme, soil.to.leaf.veg.lme, soil.to.leaf.tex.lme)

#full model

soil.to.leaf.tex.lme<-lme(foliarNPercent~soilNPercent_MHoriz + MAP + VEG + TEX,data=X,
                          random=~1|siteID,na.action=na. omit,method="REML")




