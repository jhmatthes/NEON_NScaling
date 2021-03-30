
# compare the same sites for all these relationships. Data varies by sites,
# so relationships being compared in original analysis have different sites

# total N pools
merge_soil_root
colnames(merge_soil_root) <- c('siteID', 'reps.root.soilN', 'plotID', 'soilNPercent_MHoriz_mean_root', 'rootNPercent_soil_N')

merge_foliar_soil_means
colnames(merge_foliar_soil_means) <- c('siteID', 'reps.leaf.soilN', 'plotID','foliarNPercent_mean_soil_N', 'soilNPercent_MHoriz_mean_leaf')

join_n_pools <- merge(merge_soil_root,merge_foliar_soil_means,by=c('siteID'))

# inorganic N pools
merge_soil_root_inorganic
colnames(merge_soil_root_inorganic) <- c('siteID', 'reps.root.inroganic_soilN', 'plotID', 'inorganicN_soil_root', 'rootNPercent_soil_inorganic')

merge_foliar_soil_inorganic
colnames(merge_foliar_soil_inorganic) <- c('siteID', 'reps.leaf.inorganic_soilN', 'plotID','foliarNPercent_mean_soil_inroganic',
                                           'inorganicN_soil_leaf')

join_inorganic_pools <- merge(merge_soil_root_inorganic,merge_foliar_soil_inorganic,by=c('siteID'))

# C:N pools
merge_mean_soil_root_cn
colnames(merge_mean_soil_root_cn) <- c('siteID', 'reps.root.soilCN', 'plotID', 'soilCNRatio_MHoriz_mean.rootCN', 'rootCNratio')

merge_mean_soil_foliar_cn
colnames(merge_mean_soil_foliar_cn) <- c('siteID', 'reps.leaf.soilCN', 'plotID', 'soilCNRatio_MHoriz_mean.leafCN', 'foliarCNRatio_mean')

join_cn_pools <- merge(merge_mean_soil_root_cn,merge_mean_soil_foliar_cn,by="siteID")

#join pools
join_n_inorganic_pools <- merge(join_n_pools,join_inorganic_pools,by=c('siteID'))
join_n_inorganic_cn_pools <- merge(join_n_inorganic_pools,join_cn_pools,by=c('siteID'))

#looks like there are 17 sites these all have in common. Lets see if we have the same qualatative results when we let the sites vary

# total soil N

# soil-root
summary(lm(rootNPercent_soil_N~soilNPercent_MHoriz_mean_root,data=join_n_inorganic_cn_pools))
plot(rootNPercent_soil_N~soilNPercent_MHoriz_mean_root,data=join_n_inorganic_cn_pools)
#Adjusted R-squared:  0.2725 

#soil-leaf
summary(lm(foliarNPercent_mean_soil_N~soilNPercent_MHoriz_mean_leaf,data=join_n_inorganic_cn_pools))
plot(foliarNPercent_mean_soil_N~soilNPercent_MHoriz_mean_leaf,data=join_n_inorganic_cn_pools)
#NS

#inorganic soil N

# soil-root
summary(lm(rootNPercent_soil_inorganic~inorganicN_soil_root,data=join_n_inorganic_cn_pools))
plot(rootNPercent_soil_inorganic~inorganicN_soil_root,data=join_n_inorganic_cn_pools)
#NS

# soil-leaf
summary(lm(foliarNPercent_mean_soil_inroganic~inorganicN_soil_leaf,data=join_n_inorganic_cn_pools))
plot(foliarNPercent_mean_soil_inroganic~inorganicN_soil_leaf,data=join_n_inorganic_cn_pools)
#NS

# C:N

# soil-root
summary(lm(rootCNratio~soilCNRatio_MHoriz_mean.rootCN,data=join_n_inorganic_cn_pools))
plot(rootCNratio~soilCNRatio_MHoriz_mean.rootCN,data=join_n_inorganic_cn_pools)
#Adjusted R-squared:  0.3259 

# soil-leaf
summary(lm(foliarCNRatio_mean~soilCNRatio_MHoriz_mean.leafCN,data=join_n_inorganic_cn_pools))
plot(foliarCNRatio_mean~soilCNRatio_MHoriz_mean.leafCN,data=join_n_inorganic_cn_pools)
#Adjusted R-squared:  0.5136 

# note: Kind of ambiguous for other pools that are not C:N. We see total soil N now being a significant
# predictor of root N, but not inorganic soil N (which was significant when we let sites to vary. 
# They visually don't looks that different. Seems a bit spurious.
# nevertheless, the C:N relationships are still strong, though a bit less so than in the original analysis.


