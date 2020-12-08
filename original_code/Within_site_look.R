# look at within-site relationship for soil and foliar N
#this assumes you have ran the import_cleaned_N_data.R script. 

#But if not, run this:

source('import_cleaned_N_data.R')

site<-unique(mean_soil_leaves_2$siteID)
pval.list<-list()
r.square.list<-list()
slope.list<-list()

for(isite in 1:length(site)){
  
  site.subset <- site[isite]
  site.df<-subset(mean_soil_leaves_2,siteID==site.subset)
  
  #run model
  model<-lm(foliarNPercent~soilNPercent_MHoriz, data = site.df)
  
  #get pval
  site.pval<-summary(model)$coefficients[,4]
  site.pval<-site.pval[2]
  site.pval<-data.frame(site.pval)
  site.pval$site<-site.subset
  
  #get r square
  site.r.square<-summary(model)$r.squared
  site.r.square<-data.frame(site.r.square)
  site.r.square$site<-site.subset
  
  #get slope
  site.slope<-summary(model)$coefficients[2]
  site.slope<-data.frame(site.slope)
  site.slope$site<-site.subset
  
  pval.list[[isite]] <- site.pval
  r.square.list[[isite]] <- site.r.square
  slope.list[[isite]] <- site.slope
  
  
}

test<-data.frame(slope.list)
get_coefficients(mean_soil_leaves_2)

#notes: doesn't really apear as is most site have a significant 
#within-site 'spatial' relationship.


#STOPPED HERE

# #cross-site correlations
# cor(mean_soil_leaves_2$soilNPercent_MHoriz,mean_soil_leaves_2$foliarNPercent)
# 
# #cross-site slope
# cor(mean_soil_leaves_2$soilNPercent_MHoriz,mean_soil_leaves_2$foliarNPercent)
# 
# #0.29
# mean(soil_leaves_within_site_cor$cor) 
# #-0.3
# 
# pdf(file='/Users/andrewfelton/Desktop/Figures/Figure_3/within_versus_across.pdf',
#     width=6,height=5)
# mar.default <- c(5,4,2,2) + 0.1
# par(mar = mar.default + c(1, 1, 0, 0))
# hist(soil_leaves_within_site_cor$cor, col="lightblue",
#      xlab='Within-site soil and foliar correlation (%N)',main="",breaks=10, cex.lab=1.5)
# abline(v = 0.29, col="red", lwd=5, lty=2)
# text(.58, 3, "Cross-site correlation",
#      cex = 0.9)
# dev.off()