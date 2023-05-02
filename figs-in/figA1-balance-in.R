grid_df <- readRDS("data/grid1x1_2022.rds")



balance_df <- grid_df[ ,c('temp_mean', 'temp_var', 'prec_mean', 'prec_var', 
                          'class_12', 'class_40',  'fertile_pct',  'rug', 'elev', 'river_d', 
                          'road_d', 'pop_75', 'build_75_sum', 
                          'treat', 'dist.border', 'dist.segment', 
                          'border_right', 'lat', "long")]

balance_df_20 <- subset(balance_df, abs(dist.border)<= 120000)
dummies <- model.matrix( ~ dist.segment - 1, data=balance_df_20)
balance_df_20 <- cbind.data.frame(balance_df_20, dummies)
balance_df_20$X <- balance_df_20$lat
balance_df_20$Y <- balance_df_20$long

coef_list <- as.data.frame(matrix(NA, ncol = 1, nrow = 10))
se_list <- as.data.frame(matrix(NA, ncol = 1, nrow = 10))
tost_list <- as.data.frame(matrix(NA, ncol = 3, nrow = 10))

for(j in 1:13){
  
  balance_df_20[,j] <- (balance_df_20[,j]-mean(balance_df_20[,j]))/sd(balance_df_20[,j])
}


for(j in 1:13){
  
  coef_list[j,] <- rdrobust(balance_df_20[,j], balance_df_20$dist.border)$Estimate[2]
  
  se_list[j,] <- rdrobust(balance_df_20[,j], balance_df_20$dist.border)$se[2]
  
  tost_list[j,] <-  rdd.tost(est = coef_list[j,], se =   se_list[j,], eps = .36, alpha = .05)
  
}
tost_list

balance_plot <- cbind.data.frame(coef_list, se_list, tost_list)
colnames(balance_plot)[1:5] <- c('est', 'se', 'reject', 
                                 'pval', 'interval')
balance_plot$U <- balance_plot$est+balance_plot$interval
balance_plot$L <- balance_plot$est-balance_plot$interval

balance_plot$names <- c('Mean Temp.', 'Var Temp.', 'Mean Prec.', 'Coef. Var. Prec.', 
                        '% Cropland', '% Forest', '% Fertile Soil', 'Ruggedness', 'Elevation', 
                        'River Density', 'Road Density', 'Pop. 1975', 'Built-up Area 1975')

p1 <- ggplot(balance_plot, aes(est, names)) + 
  geom_point() + 
  geom_errorbar(aes(xmax = U, xmin = L), width=.4,
                position=position_dodge(.9)) + 
  geom_vline(xintercept = 0, col = 'red', lty = 2) + 
  xlab("Standardized Estimate") +
  ylab("Outcome") +
  theme(text = element_text(size=25)) +  
  theme_tufte()

ggsave('figs-out/balance_rdrobust_2021.pdf', p1, width = 10)

