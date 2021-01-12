# Test code


# foliar to root
foliar_to_root_lm <- lm(rootNPercent ~ foliarNPercent_mean, data = plot.df)
summary(foliar_to_root_lm)
#Adjusted R-squared:  0.1529 
#Slope = 0.19

# foliar to litter
foliar_to_litter_lm <- lm(litterNPercent_mean ~ foliarNPercent_mean, data = plot.df)
summary(foliar_to_litter_lm)


ggplot(plot.df, aes(x = foliarNPercent_mean, y = soilNPercent_MHoriz_mean)) +
  geom_point(aes(colour = siteID, size = 4))
# litter to soil
litter_to_soil_lm <- lm(soilNPercent_MHoriz_mean~litterNPercent_mean ~ , data = plot.df)
summary(litter_to_soil_lm)
ggplot(plot.df, aes(x = litterNPercent_mean, y = soilNPercent_MHoriz_mean)) +
  geom_point(aes(colour = siteID, size = 4))



### Bivariate regressions with C:N (rather than N concentrations)
# foliar to root CN
foliar_to_root_CN_lm <- lm(rootCNratio ~ foliarCNRatio_mean, data = plot.df)
summary(foliar_to_root_CN_lm)
ggplot(plot.df, aes(x = foliarCNRatio_mean, y = rootCNratio)) + 
  geom_point(aes(colour = siteID, size = 3)) + 
  geom_smooth(method = "lm") +
  theme(legend.position = 'none') 
# foliar to litter CN
foliar_to_litter_CN_lm <- lm(litterCNRatio_mean ~ foliarCNRatio_mean, data = plot.df)
summary(foliar_to_litter_CN_lm)
ggplot(plot.df, aes(x = foliarCNRatio_mean, y = litterCNRatio_mean)) + 
  geom_point(aes(colour = siteID, size = 3)) + 
  geom_smooth(method = "lm") +
  theme(legend.position = 'none') 
# foliar to soil CN
foliar_to_soil_CN_lm <- lm(soilCNRatio_MHoriz_mean ~ foliarCNRatio_mean, data = plot.df)
summary(foliar_to_soil_CN_lm)
ggplot(plot.df, aes(x = foliarCNRatio_mean, y = soilCNRatio_MHoriz_mean)) + 
  geom_point(aes(colour = siteID, size = 3)) + 
  geom_smooth(method = "lm") +
  theme_basic + 
  theme(legend.position = 'none') 
# litter to soil CN
litter_to_soil_CN_lm <- lm(soilCNRatio_MHoriz_mean ~ litterCNRatio_mean, data = plot.df)
summary(litter_to_soil_CN_lm)
ggplot(plot.df, aes(x = litterCNRatio_mean, y = soilCNRatio_MHoriz_mean)) + 
  geom_point(aes(colour = siteID, size = 3)) + 
  geom_smooth(method = "lm") +
  theme_basic + 
  theme(legend.position = 'none') 

### Mixed models to consider effect of site on bivariate relationships 
# without and with MAP, veg type, and MAP + veg type as covariates
# foliar to root
foliar_to_root_lmer <- lme(rootNPercent ~ foliarNPercent_mean, ~1|siteID, data = plot.df,
                           na.action = na.omit, method = 'ML')
foliar_to_root_lmer_MAP <- lme(rootNPercent ~ foliarNPercent_mean + MAP, ~1|siteID, data = plot.df,
                               na.action = na.omit, method = 'ML')
anova(foliar_to_root_lmer, foliar_to_root_lmer_MAP)
summary(foliar_to_root_lmer)
r.squaredGLMM(foliar_to_root_lmer)
# foliar to litter
foliar_to_litter_lmer <- lme(litterNPercent_mean ~ foliarNPercent_mean, ~1|siteID, 
                             data = plot.df, na.action = na.omit, method = 'ML')
foliar_to_litter_lmer_MAP <- lme(litterNPercent_mean ~ foliarNPercent_mean + MAP, ~1|siteID, 
                                 data = plot.df, na.action = na.omit, method = 'ML')
anova(foliar_to_litter_lmer, foliar_to_litter_lmer_MAP)
summary(foliar_to_litter_lmer)
r.squaredGLMM(foliar_to_litter_lmer)
# foliar to soil
foliar_to_soil_lmer <- lme(soilNPercent_MHoriz_mean ~ foliarNPercent_mean, ~1|siteID, 
                           data = plot.df, na.action = na.omit, method = "ML")
foliar_to_soil_lmer_MAP <- lme(soilNPercent_MHoriz_mean ~ foliarNPercent_mean + MAP, ~1|siteID, 
                               data = plot.df, na.action = na.omit, method = "ML")
anova(foliar_to_soil_lmer, foliar_to_soil_lmer_MAP)
summary(foliar_to_soil_lmer)
r.squaredGLMM(foliar_to_soil_lmer)
# litter to soil
litter_to_soil_lmer <- lme(litterNPercent_mean ~ soilNPercent_MHoriz_mean, ~1|siteID, 
                           data = plot.df, na.action = na.omit, method = "ML")
litter_to_soil_lmer_MAP <- lme(litterNPercent_mean ~ soilNPercent_MHoriz_mean + MAP, ~1|siteID, 
                               data = plot.df, na.action = na.omit, method = "ML")
anova(litter_to_soil_lmer, litter_to_soil_lmer_MAP)
summary(litter_to_soil_lmer)
r.squaredGLMM(litter_to_soil_lmer)


