###############################################################################
# last updated: May 30 2022
# author: Andrew Felton
# project: NEON N scaling
# file Description: 
# Creates additional files used for data filtering and for adding climate and soil
# texture grouping columns to the main dataset
# notes: 
###############################################################################
# First get mean values for each plot-site combination for each pool ------

#get mean site-plot N to prep for self-calculating C:N
mean_foliar <- aggregate(foliarNPercent_mean ~ siteID + plotID,mean,data = plot_df)
mean_root <- aggregate(rootNPercent ~ siteID + plotID, mean, data = plot_df)
mean_soil <- aggregate(soilNPercent_MHoriz_mean ~ siteID + plotID,mean,data = plot_df)
mean_soil_inorganic <- aggregate(inorganicN ~ siteID + plotID,mean,data = plot_df)
mean_soil_mineralization <- aggregate(netNminugPerGramPerDay ~ siteID + plotID,mean,data = plot_df)
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
#0.92
cor.test(plot_df$soil_cn_self_calc,plot_df$soilCNRatio_MHoriz_mean)
#0.99
cor.test(plot_df$litter_cn_self_calc,plot_df$litterCNRatio_mean)
#0.93

#add site-means of soil texture to plot_df ot prep for use as covariate-----
str(sand_class)
sand_class <- aggregate(pctSand ~ siteID,mean,data=plot_df)
sand_class <- sand_class %>%
  rename('pctSand_mean' = 'pctSand')
sand_class$pctSand_mean = round(sand_class$pctSand_mean,2)
plot_df <- merge(plot_df,sand_class,by=c('siteID'))

#trim down columns to those used for analysis in this study

#colnames(plot_df)

myvars <- c("x","y","siteID",'plotType',"plotID",'soilCPercent_MHoriz_mean',
            'soilNPercent_MHoriz_mean',"soil_cn_self_calc","inorganicN","rootCPercent","rootNPercent",
            "root_cn_self_calc","foliarCPercent_mean","foliarNPercent_mean","foliar_cn_self_calc",
            "litterCPercent_mean","litterNPercent_mean","litter_cn_self_calc",
            "class","MAP","npp_g_m2","pctSand_mean")
            
plot_df <- plot_df[myvars] 

#one last measure, remove any duplicates
plot_df <- plot_df[!duplicated(plot_df),]
          
#plot(y~x,plot_df)

#write to a csv

