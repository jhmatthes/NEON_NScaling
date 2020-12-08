# getting things set up (assumes you've run import_cleaned_N_data.r) #####

#make histogram showing MAP distribution
unique(mean_soil_leaves$siteID)
map_means<-merge(mean_soil_leaves,climate.df,by=c('siteID'))
map_means<-aggregate(MAP~siteID,mean,data=map_means)
# pdf(file='/Users/andrewfelton/Desktop/Figures/Figure_4/MAP.pdf',
#     width=6,height=5)
# mar.default <- c(5,4,2,2) + 0.1
# par(mar = mar.default + c(1, 1, 0, 0))
# hist(map_means$MAP,main='',xlab='Mean annual rainfall (mm)',cex.lab=1.75)
dev.off()

#statstical models to build off the initial bi-variate spatial analyses

veg.name<-c('siteID','nlcdClass')
cover_class<-data.frame(plot.df[veg.name])
cover_class <- cover_class[!duplicated(cover_class), ]
cover_class<-na.omit(cover_class)

# import the land cover data to use veg as a covariate in the models #
climclass.df <- read.csv(file = "./climate-class_Martha_2020-04-13.csv", stringsAsFactors = F)
dim(climclass.df)

#clean up the data
climclass.df <- climclass.df[!duplicated(climclass.df), ] #remove any duplcates 
climclass.df<-na.omit(climclass.df)
dim(climclass.df)
#unique(climclass.df$ClimateGrp)
#unique(climclass.df$LCclass)

#a key issue with trying to use veg as a covariate is that you can have diverse
#veg types within the same site from grassland to forest...rendering it kind of
#unreliable for gaining menagingful inference from the data we have

myvarsveg<-c('siteID','LCclass') #isolate columns of interest
lc <- climclass.df[myvarsveg]
lac_no_na<-na.omit(lc) #remove NA rows

#plot out the number of cover classes########
#get counts
lac_no_na_count<-lac_no_na %>% count(LCclass)

# pdf(file='/Users/andrewfelton/Desktop/Figures/Figure_4/veg.pdf',
#     width=6,height=5)
# # ggplot(lac_no_na_count,aes(reorder(LCclass,n),n)) +
# #   stat_summary(fun.y='mean',geom='bar',color='black') +
#   xlab('') +
#   ylab('Number of sites') +
#   ggtitle('') +
#   theme(
#     axis.text.x = element_text(color='black',size=10,angle=25,hjust=1),
#     axis.text.y = element_text(color='black',size=10),
#     axis.title = element_text(color='black',size=20),
#     axis.ticks = element_line(color='black'),
#     legend.key = element_blank(),
#     strip.background =element_rect(fill="white"),
#     strip.text = element_text(size=15),
#     legend.position = c('top'),
#     panel.background = element_rect(fill=NA),
#     panel.border = element_blank(), #make the borders clear in prep for just have two axes
#     axis.line.x = element_line(colour = "black"),
#     axis.line.y = element_line(colour = "black"))
# dev.off()

########

#filter out sites with more than one cover class...
source('Functions.R')
lac_no_na<-rename_lcc(lac_no_na)
lac_no_na<- lac_no_na[!duplicated(lac_no_na), ]
lac_no_na <- lac_no_na %>%
  dplyr::filter(!LCclass %in% c("water","croplands","savannas","shrublands"))

#check and remove any sites with two veg types
unique(lac_no_na$LCclass)
lac_no_na_count<-aggregate(LCclass~siteID,length,data=lac_no_na)
lac_no_na<-merge(lac_no_na,lac_no_na_count,by=c('siteID'))
lac_no_na<-filter(lac_no_na,LCclass.y < 2)
lac_no_na<-lac_no_na[-c(3)]

#combine with climate data
lac_no_na_climate<-merge(lac_no_na,climate.df,by=c('siteID'))
########

# how is the soil-leaf relationship mediated by climate and vegeation structure? ######

#merge grassland/forest veg dataset with soil-to-leaf cleaned up dataset
soil_leaves_veg_climate<-merge(lac_no_na_climate,mean_soil_leaves[-c(140,133),],by=c('siteID'))
soil_leaves_veg_climate <- soil_leaves_veg_climate[!duplicated(soil_leaves_veg_climate), ]

#merge MAP dataset with soil-to-leaf cleaned up dataset (more sites...)
mean_soil_leaves_2_climate<-merge(mean_soil_leaves[-c(140,133),],climate.df,by=c('siteID'))

# plot foliar N and MAP all sites########
# pdf(file='/Users/andrewfelton/Desktop/Figures/Figure_4/rainfall_soil_correlation.pdf',
#     width=6,height=5)
# mar.default <- c(5,4,2,2) + 0.1
# par(mar = mar.default + c(1, 1, 0, 0))
# plot(soilNPercent_MHoriz~MAP,data=mean_soil_leaves_2_climate,cex=2,cex.lab=2,
#      xlab='Mean annual rainfall (mm)',ylab='% Soil nitrogen') #not correlated
# dev.off()
########

#make the models to see what predicts foliar N across sites
head(mean_soil_leaves_2_climate)

# MIXED EFFECTS MODEL #

soil.to.leaf.lme<-lme(foliarNPercent~soilNPercent_MHoriz,data=mean_soil_leaves_2_climate,
                                  random=~1|siteID,na.action=na.omit,method="REML")

summary(soil.to.leaf.lme) #significant

#compare to gls
soil.to.leaf.gls<-gls(foliarNPercent~soilNPercent_MHoriz,data=mean_soil_leaves_2_climate
                      ,na.action=na.omit,method="REML")
summary(soil.to.leaf.gls)

anova(soil.to.leaf.gls,soil.to.leaf.lme)

#look at interactions

#basic lm
soil.to.leaf.climate.all.lm<-lm(foliarNPercent~soilNPercent_MHoriz*MAP,data=mean_soil_leaves_2_climate,
                                 na.action=na.omit)
summary(soil.to.leaf.climate.all.lm)

#lme
soil.to.leaf.climate.all.lme<-lme(foliarNPercent~soilNPercent_MHoriz*MAP,data=mean_soil_leaves_2_climate,
                                  random=~1|siteID,na.action=na.omit,method="REML")

summary(soil.to.leaf.climate.all.lme)
anova(soil.to.leaf.climate.all.lme) #no interaction
Anova(soil.to.leaf.climate.all.lme) 
plot(soil.to.leaf.climate.all.lme)
?gls

#remove random site effect
soil.to.leaf.climate.all.lme.fixed <- gls(foliarNPercent~soilNPercent_MHoriz*MAP,
                  data=mean_soil_leaves_2_climate,
                  method="REML")

summary(soil.to.leaf.climate.all.lme.fixed)
#test random effects
anova(soil.to.leaf.climate.all.lme,
      soil.to.leaf.climate.all.lme.fixed)

#improving randome effects 'singificantly improves' model in model with interaction...

#significant positive interaction in basic lm, but not in an LME!

# library(interplot)
# pdf(file='/Users/andrewfelton/Desktop/Figures/Figure_4/rainfall_soil_interplot.pdf',
#     width=6,height=5)
# interplot(m = soil.to.leaf.climate.all.lme, var1 = "soilNPercent_MHoriz", var2 = "MAP",
#           point=T) +
#   xlab('Mean annual rainfall') +
#   geom_hline(yintercept = 0,alpha=0.5) +
#   ylab('Estimated effect of soil N\non leaf N (slope)') +
#   theme(
#     axis.text.x = element_text(color='black',size=10,angle=90,hjust=1),
#     axis.text.y = element_text(color='black',size=10),
#     axis.title = element_text(color='black',size=20),
#     axis.ticks = element_line(color='black'),
#     legend.key = element_blank(),
#     strip.background =element_rect(fill="white"),
#     strip.text = element_text(size=15),
#     legend.position = c('top'),
#     panel.background = element_rect(fill=NA),
#     panel.border = element_blank(), #make the borders clear in prep for just have two axes
#     axis.line.x = element_line(colour = "black"),
#     axis.line.y = element_line(colour = "black"))
#   dev.off()

#now do just grasslands and forests for abiotic model


#look at the full dataset across all veg types
# soil.to.leaf.grasslands.forests.lm<-lm(foliarNPercent~soilNPercent_MHoriz,data=soil_leaves_veg_climate,na.action=na.omit)
# summary(soil.to.leaf.grasslands.forests.lm) #yikes...

#now look at how MAP modifies this for grasslands and forests
# soil.to.leaf.grasslands.forests.map.lm<-lm(foliarNPercent~soilNPercent_MHoriz*MAP,data=soil_leaves_veg_climate,na.action=na.omit)
# summary(soil.to.leaf.grasslands.forests.map.lm)
#significant interative effect of MAP: the sensitivity of leaf N to soil N increases as MAP increases

#biotic model: how is the relationship of soil and leaf N mediated by
#different vegetation types, ad specifically between grasslands (e.g. herb-dominated) and forests (e.g. woody-dominated)?
  
# MIXED EFFECT MODEL #
soil.to.leaf.grasslands.forests.veg.lme<- lme(foliarNPercent~soilNPercent_MHoriz*LCclass.x,random=~1|siteID,data=soil_leaves_veg_climate)
summary(soil.to.leaf.grasslands.forests.veg.lme)
anova(soil.to.leaf.grasslands.forests.veg.lme) #no evidence for an interaction
Anova(soil.to.leaf.grasslands.forests.veg.lme) #not significant with just forest and grasslands...
#not significant

#remove random site effect
soil.to.leaf.grasslands.forests.veg.fixed <- gls(foliarNPercent~soilNPercent_MHoriz*LCclass.x,
                                          data=soil_leaves_veg_climate,
                                          method="REML")

#test random effects
anova(soil.to.leaf.grasslands.forests.veg.lme,
      soil.to.leaf.grasslands.forests.veg.fixed)

# random effects leads to a 'significantly improved' model...

#visualize this: 

# #forests
# forest<-subset(soil_leaves_veg_climate,LCclass.x==c('forest'))
# summary(lm(foliarNPercent~soilNPercent_MHoriz,data=forest))
# pdf(file='/Users/andrewfelton/Desktop/Figures/Figure_4/forests.pdf',
#     width=6,height=5)
# ggplot(aes(x = soilNPercent_MHoriz  , y = foliarNPercent), data = forest) + 
#   # scale_colour_manual(name='',values=c(forest ="blue", grasslands ="red"),
#   #                   labels=c('forest'='Forests','grasslands'= 'Grasslands')) +
#   stat_smooth(method = "lm",size=0.75, se = F, fullrange = T,aes(linetype=LCclass.x),color='black',show.legend = F) +
#   ylab('% Foliar nitrogen') +
#   #geom_text('P=0.032',mapping = (1,2)) +
#   geom_point(size=4,pch=21,alpha=1) +
#   ggtitle('Forests') +
#   xlab('% Soil nitrogen') +
#   #ggtitle('') +
#   theme(
#     axis.text.x = element_text(color='black',size=15),# angle=25,hjust=1),
#     axis.text.y = element_text(color='black',size=15),
#     axis.title = element_text(color='black',size=20),
#     axis.ticks = element_line(color='black'),
#     legend.key = element_blank(),
#     legend.text= element_text(size=15),
#     strip.background =element_rect(fill="white"),
#     strip.text = element_text(size=15),
#     legend.position = c('top'),
#     panel.background = element_rect(fill=NA),
#     panel.border = element_blank(), #make the borders clear in prep for just have two axes
#     axis.line.x = element_line(colour = "black"),
#     axis.line.y = element_line(colour = "black"))
# dev.off()
# 
# #grasslands
# grasslands<-subset(soil_leaves_veg_climate,LCclass.x==c('grasslands'))
# summary(lm(foliarNPercent~soilNPercent_MHoriz,data=grasslands))
# pdf(file='/Users/andrewfelton/Desktop/Figures/Figure_4/grasslands.pdf',
#     width=6,height=5)
# ggplot(aes(x = soilNPercent_MHoriz  , y = foliarNPercent), data = grasslands) + 
#   # scale_colour_manual(name='',values=c(forest ="blue", grasslands ="red"),
#   #                   labels=c('forest'='Forests','grasslands'= 'Grasslands')) +
#   #stat_smooth(method = "lm",size=0.75, se = F, fullrange = T,aes(linetype=LCclass.x),color='black',show.legend = F) +
#   ylab('% Foliar nitrogen') +
#   #geom_text('P=0.032',mapping = (1,2)) +
#   geom_point(size=4,pch=21,alpha=1) +
#   ggtitle('Grasslands') +
#   xlab('% Soil nitrogen') +
#   #ggtitle('') +
#   theme(
#     axis.text.x = element_text(color='black',size=15),# angle=25,hjust=1),
#     axis.text.y = element_text(color='black',size=15),
#     axis.title = element_text(color='black',size=20),
#     axis.ticks = element_line(color='black'),
#     legend.key = element_blank(),
#     legend.text= element_text(size=15),
#     strip.background =element_rect(fill="white"),
#     strip.text = element_text(size=15),
#     legend.position = c('top'),
#     panel.background = element_rect(fill=NA),
#     panel.border = element_blank(), #make the borders clear in prep for just have two axes
#     axis.line.x = element_line(colour = "black"),
#     axis.line.y = element_line(colour = "black"))
# dev.off()
# 
# #does the effect of climate on soil-N coupling diff by veg type?
# 
# sens<- soil_leaves_veg_climate %>% group_by(siteID) %>%
#   dplyr::do(model = lm(foliarNPercent~soilNPercent_MHoriz, data = .)) %>%
#   dplyr::mutate(coef=coef(model)[2])
# sens<-data.frame(sens)
# sens_map<-merge(sens,climate.df,by=c('siteID'))
# plot(coef~MAP,data=sens_map)

#do a three-way interaction...this gets a little too messy

# # MULTIPLE REGRESSION #
# soil.to.leaf.full <- lm(foliarNPercent~soilNPercent_MHoriz*MAP*LCclass.x ,data=soil_leaves_veg_climate)
# summary(soil.to.leaf.full)
# #little evidence the effect of MAP on soil-leaf N coupling different among grasslands and forests
# 
# # MIXED EFFECTS MODEL #
# soil.to.leaf.full.lme <- lme(foliarNPercent~soilNPercent_MHoriz*MAP*LCclass.x,random=~1|siteID,data=soil_leaves_veg_climate)
# summary(soil.to.leaf.full.lme)


########
####### Leaf to Litter models ##########
#head(sensitivity_conus)

head(mean_foliar_litter)
dim(mean_foliar_litter)

# how is the relationship between foliar and litter N mediated by rainfall? #

#merge the data
foliar_litter_clim_veg<-merge(lac_no_na_climate,mean_foliar_litter,by=c('siteID'))
#dim(foliar_litter_clim_veg) #only 39 rows

# abiotic model #

# MIXED EFFECTS #

leaf.to.litter.lme<-lme(litterNPercent~foliarNPercent,data=foliar_litter_clim_veg,
                      random=~1|siteID,na.action=na.omit,method="REML")

summary(leaf.to.litter.lme) #not significant
plot(litterNPercent~foliarNPercent,data=foliar_litter_clim_veg)

#compare to gls
leaf.to.litter.gls<-gls(litterNPercent~foliarNPercent,data=foliar_litter_clim_veg,
                      na.action=na.omit,method="REML")
summary(leaf.to.litter.gls)

anova(leaf.to.litter.gls,leaf.to.litter.lme)

leaf.to.litter.map.lme<-lme(litterNPercent~foliarNPercent*MAP,random=~1|siteID,
                            data=foliar_litter_clim_veg,na.action=na.omit)

summary(leaf.to.litter.map.lme)
anova(leaf.to.litter.map.lme) #no significant interation...
Anova(leaf.to.litter.map.lme)

#remove random site effect
leaf.to.litter.map.fixed <- gls(litterNPercent~foliarNPercent*MAP,
                                                 data=foliar_litter_clim_veg,
                                                 method="REML")

#test random effects
anova(leaf.to.litter.map.lme,
      leaf.to.litter.map.fixed)

#site random effects 'signficantly improved' the model..but now the 'significant' effect of foliar N on litter
# is gone...


########

######litter to soil N#######

#dim(mean_litter_soil[-c(1,28,39),])
litter_soil_clim_veg<-merge(lac_no_na_climate,mean_litter_soil[-c(1,28,39),],by=c('siteID'))

#climate model: all sites
litter.to.soil.map.all_sites<-merge(mean_litter_soil[-c(1,28,39),],climate.df,by=c('siteID'))

# MIXED EFFECTS MODELS #

litter.to.soil.lme<-lme(soilNPercent_MHoriz~litterNPercent,random=~1|siteID,
                            data=litter.to.soil.map.all_sites,na.action=na.omit,method="REML")
summary(litter.to.soil.lme)

#compare to gls
litter.to.soil.gls<-gls(soilNPercent_MHoriz~litterNPercent,
                        data=litter.to.soil.map.all_sites,na.action=na.omit,method="REML")
summary(litter.to.soil.gls)

anova(litter.to.soil.gls,litter.to.soil.lme)

#interactions
litter.to.soil.map.lme<-lme(soilNPercent_MHoriz~litterNPercent*MAP,random=~1|siteID,
                            data=litter.to.soil.map.all_sites,na.action=na.omit,method="REML")
summary(litter.to.soil.map.lme)
Anova(litter.to.soil.map.lme)
anova(litter.to.soil.map.lme)

#no significant modying effect of MAP in any case

#remove random site effect
litter.to.soil.map.fixed <- gls(soilNPercent_MHoriz~litterNPercent*MAP,
                                data=litter.to.soil.map.all_sites,
                                method="REML")

#test random effects
anova(leaf.to.litter.map.lme,
      litter.to.soil.map.fixed)

#site random effects improves model again...
