#functions


#-------------------------------------------------------------------------------
# function to select plant-soil N site combinations with > replicates----


filter_reps_1 <- function(dependent,independent){
  
  #dependent = "rootNPercent"
  #independent = "soilNPercent_MHoriz_mean"
  
  #select the variables
  select_variables <- cn_data %>%
    dplyr::select(siteID,plotID,dependent,independent,
                  class_2,MAT,MAP,npp_g_m2,pctSand_mean)
  
  #remove any row with an NA
  select_variables <- na.omit(select_variables)
  
  #get site reps and filter out low-rep sites
  select_variables_reps <- select_variables %>%
    dplyr::select(siteID,plotID) %>%
    group_by(siteID) %>%
    summarise(length(siteID)) %>%
    rename('sample_size' = "length(siteID)") %>%
    filter(sample_size > 3)
  
  #merge with original df
  select_variables <- merge(select_variables_reps,select_variables,by=c('siteID'))
  
  
  #plot(root_cn_self_calc~soil_cn_self_calc,data=select_variables)
  
  return(select_variables)
  
}
filter_reps_2 <- function(dependent,independent){
  
  #dependent = "rootNPercent"
  #independent = "soilNPercent_MHoriz_mean"
  
  #select the variables
  select_variables <- cn_data %>%
    dplyr::select(siteID,plotID,dependent,independent,
           class_2,MAT,MAP,npp_g_m2,pctSand,pctSand_mean)
  
  #remove any row with an NA
  select_variables <- na.omit(select_variables)
  
  #get site reps and filter out low-rep sites
  select_variables_reps <- select_variables %>%
    dplyr::select(siteID,plotID) %>%
    group_by(siteID) %>%
    summarise(length(siteID)) %>%
    rename('sample_size' = "length(siteID)") %>%
    filter(sample_size > 3)
  
  #merge with original df
  select_variables <- merge(select_variables_reps,select_variables,by=c('siteID'))
  
  
  #plot(root_cn_self_calc~soil_cn_self_calc,data=select_variables)
  
  return(select_variables)
  
}



#-------------------------------------------------------------------------------
# function to get data range and mean of N pools for sites with >3 within-site replicates ------


get_data_range_within <- function(val){
  
  foliar_length <- dplyr::select(cn_data,c('siteID','plotID',val))
  #head(foliar_length)
  
  #fix column name issue
  colnames(foliar_length) <- c('siteID','plotID','pool')
  
  # eliminate NAs
  foliar_length <- foliar_length %>%
    dplyr::filter(!pool=='NA')
  
  #check sample sizes, merge this with larger dataframe
  foliar_length_2 <-aggregate(pool~siteID,length,data=foliar_length)
  colnames(foliar_length_2) <- c('siteID','reps')
  foliar_length <- merge(foliar_length,foliar_length_2,by=c('siteID'))
  
  #remove sites with less than 4 replicates
  foliar_length <- foliar_length %>%
    dplyr::filter(!reps < 4)
  
  foliar_length  <- foliar_length  %>%
    group_by(siteID) %>%
    summarise(min_val = min(pool, na.rm=TRUE),
              max_val = max(pool, na.rm=TRUE),
              mean_val = mean(pool, na.rm=TRUE))
  foliar_length <- data.frame(foliar_length)
  
  foliar_length$range <- foliar_length$max_val - foliar_length$min_val
  
  foliar_length$pool <- val
  
  return(foliar_length)
  
}


#-------------------------------------------------------------------------------
# function to get cross-site data range and mean for site means with sites with >3 replicates ----

get_data_range_across_site <- function(val){
  
  foliar_length <- dplyr::select(cn_data,c('siteID','plotID',val))
  #head(foliar_length)
  
  #fix column name issue
  colnames(foliar_length) <- c('siteID','plotID','pool')
  
  # eliminate NAs
  foliar_length <- foliar_length %>%
    dplyr::filter(!pool=='NA')
  
  #check sample sizes, merge this with larger dataframe
  foliar_length_2 <-aggregate(pool~siteID,length,data=foliar_length)
  colnames(foliar_length_2) <- c('siteID','reps')
  foliar_length <- merge(foliar_length,foliar_length_2,by=c('siteID'))
  
  #remove sites with less than 4 replicates
  foliar_length <- foliar_length %>%
    dplyr::filter(!reps < 4)
  
  foliar_length <- aggregate(pool~siteID,mean,data=foliar_length)
  
  foliar_length  <- foliar_length  %>%
    summarise(min_val = min(pool, na.rm=TRUE),
              max_val = max(pool, na.rm=TRUE),
              mean_val = mean(pool, na.rm=TRUE))
  foliar_length=data.frame(foliar_length)
  
  foliar_length$pool <- val
  foliar_length$siteID <- 'All'
  
  foliar_length$range <- foliar_length$max - foliar_length$min
  
  return(foliar_length)
  
}

#-------------------------------------------------------------------------------
# function to get cross-site standard deviation >3 reps ----

get_data_sd_cross_site <- function(val){
  
  foliar_length <- dplyr::select(cn_data,c('siteID','plotID',val))
  #head(foliar_length)
  
  #fix column name issue
  colnames(foliar_length) <- c('siteID','plotID','pool')
  
  # eliminate NAs
  foliar_length <- foliar_length %>%
    dplyr::filter(!pool=='NA')
  
  #check sample sizes, merge this with larger dataframe
  foliar_length_2 <- aggregate(pool~siteID,length,data=foliar_length)
  colnames(foliar_length_2) <- c('siteID','reps')
  foliar_length <- merge(foliar_length,foliar_length_2,by=c('siteID'))
  
  #remove sites with less than 4 replicates
  foliar_length <- foliar_length %>%
    dplyr::filter(!reps < 4)
  
  foliar_length <- aggregate(pool~siteID,mean,data=foliar_length)
  
  foliar_length  <- foliar_length  %>%
    summarise(sd_val = sd(pool, na.rm=TRUE))
  
  foliar_length <- data.frame(foliar_length)
  
  foliar_length$pool <- val
  foliar_length$siteID <- 'All'
  
  return(foliar_length)
  
}

#-------------------------------------------------------------------------------
# function to get SD N pools for sites with >3 within-site replicates ------


get_data_sd <- function(val){
  
  foliar_length <- dplyr::select(cn_data,c('siteID','plotID',val))
  #head(foliar_length)
  
  #fix column name issue
  colnames(foliar_length) <- c('siteID','plotID','pool')
  
  # eliminate NAs
  foliar_length <- foliar_length %>%
    dplyr::filter(!pool=='NA')
  
  #check sample sizes, merge this with larger dataframe
  foliar_length_2 <-aggregate(pool~siteID,length,data=foliar_length)
  colnames(foliar_length_2) <- c('siteID','reps')
  foliar_length <- merge(foliar_length,foliar_length_2,by=c('siteID'))
  
  #remove sites with less than 4 replicates
  foliar_length <- foliar_length %>%
    dplyr::filter(!reps < 4)
  
  foliar_length  <- foliar_length  %>%
    group_by(siteID) %>%
    summarise(sd_val = sd(pool, na.rm=TRUE))
  
  foliar_length <- data.frame(foliar_length)
  
  foliar_length$pool <- val
  
  return(foliar_length)
  
}


#-------------------------------------------------------------------------------
# function to get random and fixed effects r-squared from LME----


#x = output from r.squaredGLMM
get_fixed_random_r_squared <- function(x,y){

fix_rand <- data.frame(r.squaredGLMM(x))
fix_rand$random <- fix_rand$R2c - fix_rand$R2m

fix_rand <- fix_rand %>%
  select(R2m,random) %>%
  rename('fixed' = 'R2m')

fix_rand$model <- y

return(fix_rand)

}


#-------------------------------------------------------------------------------
# AIC model selection -------
aic_model_select_1 <- function(dep,ind){
  
  filtered_df <- filter_reps(dep,ind)
  
  
  original <- lme(as.formula(paste(dep," ~ ",paste(ind,collapse="+"))),
                  random = ~1|siteID,
                  data=filtered_df,method = 'ML')
  #
  full_model <- lme(as.formula(paste(dep," ~ ",ind," + ","MAP"," + ","class_2"," + ",
                                     "pctSand_mean"," + ","npp_g_m2")),
                    random = ~1|siteID,
                    data=filtered_df,method = 'ML')
  
  map_class_2_sand <- lme(as.formula(paste(dep," ~ ",ind," + ","MAP"," + ","class_2"," + ",
                                         "pctSand_mean")),
                        random = ~1|siteID,
                        data=filtered_df,method = 'ML')
  
  map_class_2 <- lme(as.formula(paste(dep," ~ ",ind," + ","MAP"," + ","class_2")),
                   random = ~1|siteID,
                   data=filtered_df,method = 'ML')
  
  map <- lme(as.formula(paste(dep," ~ ",ind," + ","MAP")),
             random = ~1|siteID,
             data=filtered_df,method = 'ML')
  
  class_2_sand_npp <- lme(as.formula(paste(dep," ~ ",ind," + ","class_2"," + ",
                                         "pctSand_mean"," + ","npp_g_m2")),
                        random = ~1|siteID,
                        data=filtered_df,method = 'ML')
  
  sand_npp <- lme(as.formula(paste(dep," ~ ",ind," + ",
                                   "pctSand_mean"," + ","npp_g_m2")),
                  random = ~1|siteID,
                  data=filtered_df,method = 'ML')
  
  npp <- lme(as.formula(paste(dep," ~ ",ind," + ",
                              "npp_g_m2")),
             random = ~1|siteID,
             data=filtered_df,method = 'ML')
  
  sand <- lme(as.formula(paste(dep," ~ ",ind," + ",
                               "pctSand_mean")),
              random = ~1|siteID,
              data=filtered_df,method = 'ML')
  
  class_2 <- lme(as.formula(paste(dep," ~ ",ind," + ",
                                "class_2")),
               random = ~1|siteID,
               data=filtered_df,method = 'ML')
  
  aic_df <- data.frame(AIC(original,full_model,map_class_2_sand,map_class_2,map,
                           class_2_sand_npp,sand_npp,npp,class_2,sand))
  aic_df <- aic_df %>%
    arrange(AIC)
  
  return(aic_df[1,])
  
}

aic_model_select_2 <- function(dep,ind){
  
  filtered_df <- filter_reps_1(dep,ind)
  
  #full model
  full_model <- lme(as.formula(paste(dep," ~ ",ind," + ","MAT", " + ", "MAP"," + ",
                                     "class_2"," + ",
                                     "pctSand_mean"," + ","npp_g_m2")),
                    random = ~1|siteID,
                    data=filtered_df,method = 'ML')
  
 return(MASS::stepAIC(full_model,direction = 'both'))
  
}





 
#-------------------------------------------------------------------------------
# function to see if pools significantly differ by veg

#look to see if N or C;N pools are statistically different
quick_lme_look_veg <- function(dep){

#filter to just veg and N or C:N pool
data <- cn_data %>%
  dplyr::select(siteID,dep, class_2)
data  <- na.omit(data)

full_model <- lme(as.formula(paste(dep," ~ ",'class_2')),
                  random = ~1|siteID,
                  data=data,method = 'ML')
val <- anova.lme(full_model)
val_return <- val$`p-value`

return(val_return)

}





