# last updated: 2021-04-23
# author: Adrienne Keller
# project: NutNet plant responses to N, P fert

# file: BNPP (log-transformed) linear mixed model selection & graphical displays,
#  following Zuur 2009 suggested steps, see H-notebook page 38

################################################################################
# 1st: we have established from hypotheses and correlation analyses that covariates
#  to consider will include: N_Dep OR precip_dist (these are highly correalted),
#  AI, diversity (not richness due to high correlation with diversity, and diversity 
#  integrates both richness and evenness), Percent Clay and pH

# 2nd: check transformations of each response variable (not shown)
#  response variables = ln_anpp; ln_bnpp; ln_rss; ln_turnover; rmf_bnpp; rmf_rss

#### N_Dep first ---------------------------------------------------------------
### Now proceed to model selection process per Zuur 2009 ----
vf1 <- varIdent(form = ~1 | site)

### Step 1: Start with linear regression model and maximum explanatory variables.
# Check homogeneity assumptions
explvar <- c("N", "P", "N_Dep", "AI", "pH", "PercentClay", "diversity") # explanatory variables to examine
respvar <- "rmf_rss" # change repsonse variable here for each individual model!
dat.mod <- dat %>% dplyr::select(c(site, block, all_of(explvar), all_of(respvar))) %>% na.omit()
respname <- paste(respvar)
respdat <-  dat.mod[, respname]
fix.mod <- formula(respdat ~ N + P + N:P + N_Dep + AI + diversity + pH + PercentClay +
                     N:N_Dep + P:N_Dep +
                     N:AI + P:AI + N:diversity + P:diversity)
mod.lm <- lm(fix.mod, data = dat.mod)
summary(mod.lm)
par(mfrow = c(2,2))
plot(mod.lm) #(see interpretation notes here https://data.library.virginia.edu/diagnostic-plots/)
dev.off()

### Step 2: Fit same regression model with nlme::gls()
mod0 <- nlme::gls(fix.mod, method = "REML", data = dat.mod)
summary(mod0)

### Step 3: Choose best variance structure 
res <- resid(mod0)
fit <- fitted(mod0)
# varIdent()
vf1 <- varIdent(form = ~1 | site) # !?! error when I include block nested in site, does this matter when I fit weights below? (results don't vary much but statistically/programmatically this might be poor practice?) 
mod1 <- gls(fix.mod, weights = vf1, data = dat.mod, method = "REML")
AIC(mod0, mod1)
# visualize variance structure models
par(mfrow = c(2,2))
plot(resid(mod0,type="normalized")~fitted(mod0),main='Unweighted')
plot(resid(mod1,type="normalized")~fitted(mod1),main='VarIdent')
with(qqnorm(resid(mod0, type = "normalized")),qqline(resid(mod0, type = "normalized")))
with(qqnorm(resid(mod1, type = "normalized")),qqline(resid(mod1, type = "normalized")))
dev.off()

### Step 4: Determine best fixed effects structure, method = "ML"  
# selecting fixed effects that p < 0.05 l.ratio test
# first fit global model with random effects
mod.full <- lme(fix.mod,
                random = ~1|site/block, 
                weights = vf1, 
                data = dat.mod, method = "ML")
summary(mod.full)
formula(mod.full)
?update
# next remove any interaction terms that aren't significant and compare updated models using likelihood ratio test
mod2a <- update(mod.full, .~. - P:diversity) 
anova(mod.full, mod2a) # remove P:diversity
mod2b <- update(mod.full, .~. - N:diversity) 
anova(mod.full, mod2b) # remove  N:diversity
mod2c <- update(mod.full, .~. - P:AI)
anova(mod.full, mod2c) #  KEEP P:AI
mod2d <- update(mod.full, .~. - N:AI)
anova(mod.full, mod2d) # remove N:AI
mod2e <- update(mod.full, .~. - P:N_Dep) 
anova(mod.full, mod2e) # remove P:N_Dep
mod2f <- update(mod.full, .~. - N:N_Dep) 
anova(mod.full, mod2f) # remove N:N_Dep
mod2 <- update(mod.full, .~. - P:diversity - N:diversity - N:AI - P:N_Dep - N:N_Dep)
summary(mod2)
formula(mod2)

# next remove any main effects that aren't significant, keep AI given interaction term
mod3a <- update(mod2, .~. - PercentClay) 
anova(mod2, mod3a)# remove PercentClay
mod3b <- update(mod2, .~. - pH)
anova(mod2, mod3b) # remove pH
mod3c <- update(mod2, .~. - diversity)
anova(mod2, mod3c) # remove diversity
mod3e <- update(mod2, .~. - N_Dep)
anova(mod2, mod3e) # KEEP N_Dep

# final model
mod.rmfrss <- update(mod2, .~. - PercentClay - pH - diversity, method = "REML")
summary(mod.rmfrss)
formula(mod.rmfrss)
r.squaredGLMM(mod.rmfrss)
# output table
tab_model(mod.rmfrss, show.r2 = FALSE, show.re.var = FALSE, title = "RMF-RSS",
          dv.labels = "", file = "R_analyses/output/tab-rmfrss_NDep.html")

#### Precip_dist instead of N_Dep ----------------------------------------------
### Step 1: Start with linear regression model and maximum explanatory variables.
# Check homogeneity assumptions
explvar <- c("N", "P", "precip_dist", "AI", "pH", "PercentClay", "diversity") # explanatory variables to examine
respvar <- "rmf_rss" # change repsonse variable here for each individual model!
dat.mod <- dat %>% dplyr::select(c(site, block, all_of(explvar), all_of(respvar))) %>% na.omit()
respname <- paste(respvar)
respdat <-  dat.mod[, respname]
fix.mod <- formula(respdat ~ N + P + N:P + precip_dist + AI + diversity + pH + PercentClay +
                     N:precip_dist + P:precip_dist +
                     N:AI + P:AI + N:diversity + P:diversity)
mod.lm <- lm(fix.mod, data = dat.mod)
summary(mod.lm)
par(mfrow = c(2,2))
plot(mod.lm) #(see interpretation notes here https://data.library.virginia.edu/diagnostic-plots/)
dev.off()

### Step 2: Fit same regression model with nlme::gls()
mod0 <- nlme::gls(fix.mod, method = "REML", data = dat.mod)
summary(mod0)

### Step 3: Choose best variance structure 
res <- resid(mod0)
fit <- fitted(mod0)
# varIdent()
vf1 <- varIdent(form = ~1 | site) # !?! error when I include block nested in site, does this matter when I fit weights below? (results don't vary much but statistically/programmatically this might be poor practice?) 
mod1 <- gls(fix.mod, weights = vf1, data = dat.mod, method = "REML")
AIC(mod0, mod1)
# visualize variance structure models
par(mfrow = c(2,2))
plot(resid(mod0,type="normalized")~fitted(mod0),main='Unweighted')
plot(resid(mod1,type="normalized")~fitted(mod1),main='VarIdent')
with(qqnorm(resid(mod0, type = "normalized")),qqline(resid(mod0, type = "normalized")))
with(qqnorm(resid(mod1, type = "normalized")),qqline(resid(mod1, type = "normalized")))
dev.off()

### Step 4: Determine best fixed effects structure, method = "ML"  
# selecting fixed effects that p < 0.05 l.ratio test
# first fit global model with random effects
mod.full <- lme(fix.mod,
                random = ~1|site/block, 
                weights = vf1, 
                data = dat.mod, method = "ML")
summary(mod.full)
formula(mod.full)
# next remove any interaction terms that aren't significant and compare updated models using likelihood ratio test
mod2a <- update(mod.full, .~. - P:diversity) 
anova(mod.full, mod2a) # remove P:diversity
mod2b <- update(mod.full, .~. - N:diversity) 
anova(mod.full, mod2b) # remove N:diversity
mod2c <- update(mod.full, .~. - P:AI)
anova(mod.full, mod2c) #  KEEP P:AI
mod2d <- update(mod.full, .~. - N:AI)
anova(mod.full, mod2d) #  remove N:AI
mod2e <- update(mod.full, .~. - P:precip_dist) 
anova(mod.full, mod2e) #  remove P:precip_dist
mod2f <- update(mod.full, .~. - N:precip_dist) 
anova(mod.full, mod2f) #  remove N:precip_dist
mod2 <- update(mod.full, .~. - P:diversity - N:diversity - N:AI - P:precip_dist - N:precip_dist)
summary(mod2)
formula(mod2)

# next remove any main effects that aren't significant - keep AI due to interactive terms
mod3a <- update(mod2, .~. - PercentClay) 
anova(mod2, mod3a)# remove PercentClay
mod3b <- update(mod2, .~. - pH)
anova(mod2, mod3b) # remove pH
mod3c <- update(mod2, .~. - diversity)
anova(mod2, mod3c) # remove diversity
mod3e <- update(mod2, .~. - precip_dist)
anova(mod2, mod3e) # remove precip_dist

# final model
mod.rmfrss <- update(mod2, .~. - PercentClay - pH - diversity - precip_dist, method = "REML")
summary(mod.rmfrss)
formula(mod.rmfrss)
r.squaredGLMM(mod.rmfrss)
# output table
tab_model(mod.rmfrss, show.r2 = FALSE, show.re.var = FALSE, title = "RMF-RSS",
          dv.labels = "", file = "R_analyses/output/tab-rmfrss_precipdist.html")

