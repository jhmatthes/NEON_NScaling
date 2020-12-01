### ggplot theme - minimal
theme_basic <- theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(colour = "black")
  )

# Figure 1 - visualize N pool distributions (univariate)
pdf(file='output/fig1_univariatehists.pdf',
    width=6,height=4)
mar.default <- c(6,3,5,2) + 0.1
par(mar = mar.default + c(2, 2, 0, 0),mfrow=c(1,4))
par(mar.default,mfrow=c(1,4))
hist(plot.df$soilNPercent_MHoriz,main='',xlab='% Soil N',cex.lab=1.75)
hist(plot.df$foliarNPercent,main='',xlab='% Foliar N',ylab='',cex.lab=1.75)
hist(plot.df$litterNPercent,main='',xlab='% Litter N',ylab='',cex.lab=1.75)
hist(plot.df$rootCPercent, main ='', xlab = "% Root N", ylab = '', cex.lab = 1.75)
dev.off()

# Figure 2 - bivariate relationships between N pools
pdf(file = 'output/fig2_bivariateregressions.pdf',
    width = 6, height = 4)
par(mfrow = c(1,4))
# foliar to root
ggplot(plot.df, aes(x = foliarNPercent_mean, y = rootNPercent)) +
  geom_point(aes(colour = siteID)) +
  theme_basic +
  theme(legend.position = 'none') 
# foliar to litter
ggplot(plot.df, aes(x = foliarNPercent_mean, y = litterNPercent_mean)) +
  geom_point(aes(colour = siteID)) +
  theme_basic + 
  theme(legend.position = 'none')
# foliar to soil
ggplot(plot.df, aes(x = foliarNPercent_mean, y = soilNPercent_MHoriz_mean)) +
  geom_point(aes(colour = siteID)) +
  theme_basic +
  theme(legend.position = 'none')
# litter to soil
ggplot(plot.df, aes(x = litterNPercent_mean, y = soilNPercent_MHoriz_mean)) +
  geom_point(aes(colour = siteID)) +
  theme_basic +
  theme(legend.position = 'none')
dev.off()
