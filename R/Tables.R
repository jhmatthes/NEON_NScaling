

# root N

mean_root_n<-aggregate(rootNPercent~siteID,mean,data=plot.df)
mean_root_n$rootNPercent <- round(mean_root_n$rootNPercent,2)

#get mean
mean_root_n_mean <-mean(mean_root_n$rootNPercent)
mean_root_n_mean <- round(mean_root_n_mean,2)

#get sd
mean_root_n_sd <-sd(mean_root_n$rootNPercent)
mean_root_n_sd <- round(mean_root_n_sd,2)

#get site number
mean_root_n_count <-length(mean_root_n$rootNPercent)
mean_root_n_count <- round(mean_root_n_count,2)

Pool <- c('% Root N')

#Leaf N

mean_leaf_n<-aggregate(foliarNPercent_mean~siteID,mean,data=plot.df)
mean_leaf_n$rootNPercent <- round(mean_leaf_n$foliarNPercent_mean,2)

#get mean
mean_leaf_n_mean <-mean(mean_leaf_n$foliarNPercent_mean)
mean_leaf_n_mean <- round(mean_leaf_n_mean,2)

#get sd
mean_leaf_n_sd <-sd(mean_leaf_n$foliarNPercent_mean)
mean_leaf_n_sd <- round(mean_leaf_n_sd,2)

#get site number
mean_leaf_n_count <-length(mean_leaf_n$foliarNPercent_mean)
mean_leaf_n_count <- round(mean_leaf_n_count,2)

Pool <- c('% Leaf N')

#total soil N

site_summaries_soil <- plot.df %>%
     dplyr::filter(!(siteID=="HEAL")) #remove site with one replicate

mean_soil_n<-aggregate(soilNPercent_MHoriz_mean~siteID,mean,data=site_summaries_soil)
mean_soil_n$rootNPercent <- round(mean_soil_n$soilNPercent_MHoriz_mean,2)

#get mean
mean_soil_n_mean <-mean(mean_soil_n$soilNPercent_MHoriz_mean)
mean_soil_n_mean <- round(mean_soil_n_mean,2)

#get sd
mean_soil_n_sd <-sd(mean_soil_n$soilNPercent_MHoriz_mean)
mean_soil_n_sd <- round(mean_soil_n_sd,2)

#get site number
mean_soil_n_count <-length(mean_soil_n$soilNPercent_MHoriz_mean)
mean_soil_n_count <- round(mean_soil_n_count,2)

Pool <- c('% Total soil N')

#inorganic soil N

mean_inorganic_soil_n<-aggregate(inorganicN~siteID,mean,data=plot.df)
mean_inorganic_soil_n$inorganicN <- round(mean_inorganic_soil_n$inorganicN,2)

#get mean
mean_inorganic_soil_n_mean <-mean(mean_inorganic_soil_n$inorganicN)
mean_inorganic_soil_n_mean <- round(mean_inorganic_soil_n_mean,2)

#get sd
mean_inorganic_soil_n_sd <-sd(mean_inorganic_soil_n$inorganicN)
mean_inorganic_soil_n_sd <- round(mean_inorganic_soil_n_sd,2)

#get site number
mean_inorganic_soil_n_count <-length(mean_inorganic_soil_n$inorganicN)
mean_inorganic_soil_n_count <- round(mean_inorganic_soil_n_count,2)

Pool <- c('% Inorganic soil N')


Pool <-c('% Root N','% Leaf N','% Total soil N','% Inorganic soil N')
Mean <-c(mean_root_n_mean, mean_leaf_n_mean, mean_soil_n_mean,mean_inorganic_soil_n_mean)
SD <-c(mean_root_n_sd,mean_leaf_n_sd,mean_soil_n_sd,mean_inorganic_soil_n_sd)
N <- c(mean_root_n_count,mean_leaf_n_count,mean_soil_n_count,mean_inorganic_soil_n_count)

summary.table<-data.frame(Pool,Mean,SD,N)
write.csv(summary.table,file='./../output/Table_1_N_Pools.csv')

