
#colnames(cn_data)
#remember that conditional r-squared is both var explained by random and fixed effects

#Next do AIC model selection procedure with the other covariates

model_list <- list()
model_list_2 <- list()

library(car)

#for creating nice model output tables
# library(sjPlot)
# library(sjmisc)
# library(sjlabelled)


# root and soil N (NS as of June 2022) ------

#import
root_soil <- filter_reps_1("rootNPercent","soilNPercent_MHoriz_mean")

#model (log transformed)
root_soil_lme <- lme(log(rootNPercent) ~ soilNPercent_MHoriz_mean,random= ~1|siteID,
                     data=root_soil,method = 'ML')

#check assumptions
# plot(root_soil_lme)
# plot(resid(root_soil_lme), root_soil$soilNPercent_MHoriz_mean)
# qqnorm(root_soil_lme)
# summary(root_soil_lme)
# anova.lme(root_soil_lme)
# tab_model(root_soil_lme)
# r.squaredGLMM(root_soil_lme)

#fixed effect predictions of model
root_soil_lme_predict <- predict(root_soil_lme,root_soil,level=0:1)

#combine
root_soil <- cbind(root_soil,root_soil_lme_predict[2])

#now do full model selection
aic_model_select_2("rootNPercent","soilNPercent_MHoriz_mean")
# + MAT + MAP

root_soil_lme_2 <- lme(log(rootNPercent) ~ soilNPercent_MHoriz_mean + MAP + MAT,
                               random= ~1|siteID,data=root_soil,method='ML')
# plot(root_soil_lme_2)
# summary(root_soil_lme_2)
tab_model(root_soil_lme_2)
r.squaredGLMM(root_soil_lme_2)
AIC(root_soil_lme_2)

#-------------------------------------------------------------------------------

# root and soil inorganic N (NS as of June 2022) ------

#import
root_inorganic_soil <- filter_reps_1("rootNPercent","soilInorganicNugPerGram_mean")

#model (log transformed)
root_inorganic_soil_lme <- lme(log(rootNPercent) ~ soilInorganicNugPerGram_mean,
                               random= ~1|siteID,data=root_inorganic_soil,method='ML')
#check assumptions
# plot(root_inorganic_soil_lme)
# plot(resid(root_inorganic_soil_lme), root_inorganic_soil$soilInorganicNugPerGram_mean)
# qqnorm(root_soil_lme)
# summary(root_inorganic_soil_lme)
# r.squaredGLMM(root_inorganic_soil_lme)
# tab_model(root_inorganic_soil_lme)

#fixed effect predictions of model
root_inorganic_soil_lme_predict <- predict(root_inorganic_soil_lme,root_inorganic_soil,level=0:1)

#combine
root_inorganic_soil <- cbind(root_inorganic_soil,root_inorganic_soil_lme_predict[2])

#now do full model selection
aic_model_select_2("rootNPercent","soilInorganicNugPerGram_mean")
# + mat + map + sand

root_inorganic_soil_lme_2 <- lme(log(rootNPercent) ~ MAT + MAP + pctSand_mean,
                                random= ~1|siteID,data=root_inorganic_soil,method='ML')
# summary(root_inorganic_soil_lme_2)
# plot(root_inorganic_soil_lme_2)
# tab_model(root_inorganic_soil_lme_2)
AIC(root_inorganic_soil_lme_2)

#-------------------------------------------------------------------------------
# foliar and soil inorganic N (P=0.05 but significant after removing high soil N values) ------

#import
foliar_inorganic_soil <- filter_reps_1("foliarNPercent_mean","soilInorganicNugPerGram_mean")

# #filter out high values
# foliar_inorganic_soil <- foliar_inorganic_soil %>%
#   filter(soilInorganicNugPerGram_mean < 25)

plot(foliarNPercent_mean ~ soilInorganicNugPerGram_mean,data=foliar_inorganic_soil)

#model
foliar_inorganic_soil_lme <- lme(foliarNPercent_mean ~ soilInorganicNugPerGram_mean,
                               random= ~1|siteID,data=foliar_inorganic_soil,method='ML')

#check assumptions
# plot(foliar_inorganic_soil_lme)
# plot(resid(foliar_inorganic_soil_lme), foliar_inorganic_soil$soilInorganicNugPerGram_mean)
# qqnorm(foliar_inorganic_soil_lme)
# summary(foliar_inorganic_soil_lme)
# anova.lme(foliar_inorganic_soil_lme)
# tab_model(foliar_inorganic_soil_lme)
# r.squaredGLMM(foliar_inorganic_soil_lme)

#fixed effect predictions of model
foliar_inorganic_soil_lme_predict <- predict(foliar_inorganic_soil_lme,foliar_inorganic_soil,level=0:1)

#combine
foliar_inorganic_soil <- cbind(foliar_inorganic_soil,foliar_inorganic_soil_lme_predict[2])

#relationship remains significant even after removing high soil values

#now do full model selection
aic_model_select_2("foliarNPercent_mean","soilInorganicNugPerGram_mean")
# original model selected
AIC(foliar_inorganic_soil_lme)

#-------------------------------------------------------------------------------
# litter and soil N (NS as of June 2022) -------
#import
litter_soil <- filter_reps_1("litterNPercent_mean","soilNPercent_MHoriz_mean")

#filter out  high soil N
# litter_soil <- litter_soil %>%
#   filter(soilNPercent_MHoriz_mean < 0.5)

#model (log transformed)
litter_soil_lme <- lme(log(soilNPercent_MHoriz_mean) ~ litterNPercent_mean,
                       random= ~1|siteID,data=litter_soil,method='ML')

#check assumptions
# plot(litter_soil_lme)
# plot(resid(litter_soil_lme), litter_soil$litterNPercent_mean)
# qqnorm(litter_soil_lme)
# summary(litter_soil_lme)
# tab_model(litter_soil_lme)
# r.squaredGLMM(litter_soil_lme)

#fixed effect predictions of model
litter_soil_lme_predict <- predict(litter_soil_lme,litter_soil,level=0:1)

#combine
litter_soil <- cbind(litter_soil,litter_soil_lme_predict[2])

#doesn't matter if cut out high values, not a significant relationship

#now do full model selection
aic_model_select_2("soilNPercent_MHoriz_mean","litterNPercent_mean")
# MAT + MAP

litter_soil_lme_2 <- lme(log(soilNPercent_MHoriz_mean) ~ MAT + MAP,
                       random= ~1|siteID,data=litter_soil,method='ML')
# plot(litter_soil_lme_2)
# summary(litter_soil_lme_2)
tab_model(litter_soil_lme_2)
r.squaredGLMM(litter_soil_lme_2)

AIC(litter_soil_lme_2)

#-------------------------------------------------------------------------------
# litter and inorganic soil N (NS as of June 2022) -------
#import
litter_inorganic_soil <- filter_reps_1("litterNPercent_mean","soilInorganicNugPerGram_mean")

#model (log transformed)
litter_inroganic_soil_lme <- lme(log(soilInorganicNugPerGram_mean) ~ litterNPercent_mean,
                       random= ~1|siteID,data = litter_inorganic_soil,method='ML')

#check assumptions
# plot(litter_inroganic_soil_lme)
# plot(resid(litter_inroganic_soil_lme), litter_inorganic_soil$soilInorganicNugPerGram_mean)
# qqnorm(litter_inroganic_soil_lme)
# summary(litter_inroganic_soil_lme)
# tab_model(litter_inroganic_soil_lme)
# r.squaredGLMM(litter_inroganic_soil_lme)

#fixed effect predictions of model
litter_inorganic_soil_lme_predict <- predict(litter_inroganic_soil_lme,litter_inorganic_soil,level=0:1)

#combine
litter_inorganic_soil <- cbind(litter_inorganic_soil,litter_inorganic_soil_lme_predict[2])

#doesn't matter if cut out high values, not a significant relationship, much like for total soil N

#now do full model selection (todo)

#now do full model selection
aic_model_select_2("soilInorganicNugPerGram_mean","litterNPercent_mean")
# MAT + MAP

litter_inroganic_soil_lme_2 <- lme(log(soilInorganicNugPerGram_mean) ~ + MAT + MAP,
                                 random= ~1|siteID,data = litter_inorganic_soil,method='ML')
# plot(litter_inroganic_soil_lme_2)
# summary(litter_inroganic_soil_lme_2)

AIC(litter_inroganic_soil_lme_2)

#-------------------------------------------------------------------------------

# foliar and soil N (significant as of June 2022) ------ 

#import
foliar_soil <- filter_reps_1("foliarNPercent_mean","soilNPercent_MHoriz_mean")

# #remove high estimates
# foliar_soil <- foliar_soil %>%
#   filter(soilNPercent_MHoriz_mean < 1)

#model
foliar_soil_lme <- lme(log(foliarNPercent_mean) ~ soilNPercent_MHoriz_mean,
                       random= ~1|siteID,data=foliar_soil,method='ML')

#check assumptions
# plot(foliar_soil_lme)
# plot(resid(foliar_soil_lme), foliar_soil$soilNPercent_MHoriz_mean)
# qqnorm(foliar_soil_lme)
# summary(foliar_soil_lme)
# anova.lme(foliar_soil_lme)
# tab_model(foliar_soil_lme)
# r.squaredGLMM(foliar_soil_lme)

#fixed effect predictions of model
foliar_soil_lme_predict <- predict(foliar_soil_lme,foliar_soil,level=0:1)

#combine
foliar_soil <- cbind(foliar_soil,foliar_soil_lme_predict[2])

#now do full model selection (todo)

#removing soil N above 1% weakened the relationship

#now do full model selection
aic_model_select_2("foliarNPercent_mean","soilNPercent_MHoriz_mean")
# original model selected
AIC(foliar_soil_lme)

#-------------------------------------------------------------------------------
# foliar and root N (significant as of June 2022) ------

#import
# plot(foliarNPercent_mean ~ rootNPercent,
#      data=foliar_root)
foliar_root <- filter_reps_1("foliarNPercent_mean","rootNPercent")

#model
foliar_root_lme <- lme(foliarNPercent_mean ~ rootNPercent,random= ~1|siteID,
                       data=foliar_root,method='ML')

#check assumptions
# plot(foliar_soil_lme)
# plot(resid(foliar_soil_lme), foliar_soil$soilNPercent_MHoriz_mean)
# qqnorm(foliar_soil_lme)
# summary(foliar_root_lme)
# anova.lme(foliar_root_lme)
# tab_model(foliar_root_lme)

#fixed effect predictions of model
foliar_root_lme_predict <- predict(foliar_root_lme,foliar_root,level=0:1)

#combine
foliar_root <- cbind(foliar_root,foliar_root_lme_predict[2])

#now do full model selection
aic_model_select_2("foliarNPercent_mean","rootNPercent")
# + sand

foliar_root_lme_2 <- lme(log(foliarNPercent_mean) ~ rootNPercent + pctSand_mean,
                           random= ~1|siteID,data=foliar_root,method='ML')
# summary(foliar_root_lme_2)
# plot(foliar_root_lme_2)
tab_model(foliar_root_lme_2)
r.squaredGLMM(foliar_root_lme_2)
AIC(foliar_root_lme_2)

#-------------------------------------------------------------------------------
# root C:N ~ soil C:N (significant as of june 2022) --------

#import
root_soil_cn <- filter_reps_1('root_cn_self_calc','soil_cn_self_calc')

#model
root_soil_cn_lme <- lme(root_cn_self_calc~soil_cn_self_calc,
                        random= ~1|siteID,data=root_soil_cn,method='ML')

#check assumptions
# plot(root_soil_cn_lme)
# plot(resid(root_soil_cn_lme), root_soil_cn$soil_cn_self_calc)
# qqnorm(root_soil_cn_lme)
# summary(root_soil_cn_lme)
# anova.lme(root_soil_cn_lme)
# tab_model(root_soil_cn_lme)
# r.squaredGLMM(root_soil_cn_lme)

#fixed effect predictions of model
root_soil_cn_lme_predict <- predict(root_soil_cn_lme,root_soil_cn,level=0:1)

#combine
root_soil_cn <- cbind(root_soil_cn,root_soil_cn_lme_predict[2])

#now do full model selection
aic_model_select_2('root_cn_self_calc','soil_cn_self_calc')
# + npp

root_soil_cn_lme_2 <- lme(root_cn_self_calc ~ soil_cn_self_calc + npp_g_m2,
                        random= ~1|siteID,data=root_soil_cn,method='ML')
# summary(root_soil_cn_lme_2)
# plot(root_soil_cn_lme_2)
tab_model(root_soil_cn_lme_2)
AIC(root_soil_cn_lme_2)

#-------------------------------------------------------------------------------
# foliar C:N ~ soil C:N (significant as of June 2022) --------

#import
foliar_soil_cn <- filter_reps_1('foliar_cn_self_calc','soil_cn_self_calc')

#model (log transformed)
foliar_soil_cn_lme <- lme(log(foliar_cn_self_calc)~soil_cn_self_calc,
                          random= ~1|siteID,data=foliar_soil_cn,method='ML')

#check assumptions
plot(foliar_soil_cn_lme)
plot(resid(foliar_soil_cn_lme), foliar_soil_cn$soil_cn_self_calc)
qqnorm(foliar_soil_cn_lme)
summary(foliar_soil_cn_lme)
anova.lme(foliar_soil_cn_lme)
tab_model(foliar_soil_cn_lme)
r.squaredGLMM(foliar_soil_cn_lme)

#fixed effect predictions of model
foliar_soil_cn_lme_predict <- predict(foliar_soil_cn_lme,foliar_soil_cn,level=0:1)

#combine
foliar_soil_cn <- cbind(foliar_soil_cn,foliar_soil_cn_lme_predict[2])

#now do full model selection
aic_model_select_2('foliar_cn_self_calc','soil_cn_self_calc')
# + map

foliar_soil_cn_lme_2 <- lme(log(foliar_cn_self_calc) ~ soil_cn_self_calc + MAP,
                            random= ~1|siteID,data=foliar_soil_cn,method='ML')
summary(foliar_soil_cn_lme_2)
plot(foliar_soil_cn_lme_2)
tab_model(foliar_soil_cn_lme_2)
r.squaredGLMM(foliar_soil_cn_lme_2)
AIC(foliar_soil_cn_lme_2)

#-------------------------------------------------------------------------------
# litter to soil C:N (P=0.05) -----

#import
litter_soil_cn <- filter_reps_1('soil_cn_self_calc','litter_cn_self_calc')

#model
litter_soil_cn_lme <- lme(soil_cn_self_calc ~ litter_cn_self_calc,
                          random= ~1|siteID,data=litter_soil_cn,method='ML')

#check assumptions
plot(litter_soil_cn_lme)
plot(resid(litter_soil_cn_lme), litter_soil_cn$litter_cn_self_calc)
qqnorm(litter_soil_cn_lme)
summary(litter_soil_cn_lme)
anova.lme(litter_soil_cn_lme)
tab_model(litter_soil_cn_lme)
r.squaredGLMM(litter_soil_cn_lme)

#fixed effect predictions of model
litter_soil_cn_lme_predict <- predict(litter_soil_cn_lme,litter_soil_cn,level=0:1)

#combine
litter_soil_cn <- cbind(litter_soil_cn,litter_soil_cn_lme_predict[2])

#now do full model selection
aic_model_select_2('soil_cn_self_calc','litter_cn_self_calc')
# + sand + map + sand + npp

litter_soil_cn_lme_2 <- lme(soil_cn_self_calc ~ litter_cn_self_calc + MAP + 
                              pctSand_mean + npp_g_m2, 
                              random= ~1|siteID,data=litter_soil_cn,method='ML')
summary(litter_soil_cn_lme_2)
plot(litter_soil_cn_lme_2)
tab_model(litter_soil_cn_lme_2)
r.squaredGLMM(litter_soil_cn_lme_2)
AIC(litter_soil_cn_lme_2)

#-------------------------------------------------------------------------------
# foliar and root C:N (P=0.04) -----

#import
foliar_root_cn <- filter_reps_1('foliar_cn_self_calc','root_cn_self_calc')

#model
foliar_root_cn_lme <- lme(log(foliar_cn_self_calc) ~ root_cn_self_calc,
                          random= ~1|siteID,data=foliar_root_cn)

#check assumptions
# plot(foliar_root_cn_lme)
# plot(resid(foliar_root_cn_lme), foliar_root_cn$root_cn_self_calc)
# qqnorm(foliar_root_cn_lme)
# summary(foliar_root_cn_lme)
# anova.lme(foliar_root_cn_lme)
# tab_model(foliar_root_cn_lme)
# r.squaredGLMM(foliar_root_cn_lme)

#fixed effect predictions of model
foliar_root_cn_lme_predict <- predict(foliar_root_cn_lme,foliar_root_cn,level=0:1)

#combine
foliar_root_cn <- cbind(foliar_root_cn,foliar_root_cn_lme_predict[2])

#now do full model selection
aic_model_select_2('foliar_cn_self_calc','root_cn_self_calc')
# sand

foliar_root_cn_lme_2 <- lme(log(foliar_cn_self_calc) ~ root_cn_self_calc + 
                                                  pctSand_mean,
                               random= ~1|siteID,data=foliar_root_cn,method='ML')
# summary(foliar_root_cn_lme_2)
# plot(foliar_root_cn_lme_2)
tab_model(foliar_root_cn_lme_2)
r.squaredGLMM(foliar_root_cn_lme_2)

AIC(foliar_root_cn_lme_2)

#-------------------------------------------------------------------------------
# partial correlation follow-up (likely delete) -----

head(cn_data,1)

library(ppcor)

root_soil_pcor <- root_soil %>%
  dplyr::select(rootNPercent,soilNPercent_MHoriz_mean,MAP,MAT)
pcor(root_soil_pcor)

#correlation between root C:N and NPP after accounting for soil C:N
root_soil_cn_pcor <- root_soil_cn %>%
  dplyr::select(root_cn_self_calc,soil_cn_self_calc,npp_g_m2)

pcor(root_soil_cn_pcor)
#partial cor = 0.30, P = 0.001

#correlation between foliar C:N and NPP after accounting for soil C:N
foliar_soil_cn_pcor <- foliar_soil_cn %>%
  dplyr::select(rootNPercent,soil_cn_self_calc,MAP)

pcor(foliar_soil_cn_pcor)
#partial cor = -0.35, P = < 0.001


#-------------------



foliar_soil_map_cn_lme <- lme(log(foliar_cn_self_calc) ~ soil_cn_self_calc + MAP + npp_g_m2 +
                                class_2,
                              random= ~1|siteID,data=foliar_soil_cn,method='ML')

MASS::stepAIC(foliar_soil_map_cn_lme)

cor.test(cn_data$npp_g_m2,cn_data$MAP)


