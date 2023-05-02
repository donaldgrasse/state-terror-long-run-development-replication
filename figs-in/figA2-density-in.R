#### density plot #### 
df <- readRDS("data/main_0310.rds")
theme_set(theme_tufte())


m1 = rddensity::rddensity(X = df$dist_border)

plot_one <- rddensity::rdplotdensity(m1, X = df$dist_border)


density_plot <- plot_one$Estplot + theme_tufte() + 
  geom_vline(xintercept = 0, lty = 2, col = 'red') + 
  theme(legend.position="none") + 
  xlab('Distance to Border (Measured in km.)') + 
  ylab('Density of Villages') + 
  ggtitle('RD Density Test')+
  theme_tufte() + theme(legend.position="none") 

k_density <- ggplot() + geom_density(aes(df$dist_border)) + 
  geom_vline(xintercept = 0, lty = 2, col = 'red') + 
  xlab('Distance to Border (Measured in km.)') + 
  ylab('Density of Villages') + 
  scale_x_continuous(breaks = round(seq(min(df$dist_border), max(df$dist_border), 20))) + 
  ggtitle('Kernel Density of Observations')+
  theme_tufte()


density_plot = cowplot::plot_grid(k_density, density_plot, labels = 'AUTO')
ggsave('figs-out/density_plot.pdf', density_plot, width = 10)
