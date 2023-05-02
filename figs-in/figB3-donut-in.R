
df <- readRDS("data/main_0310.rds")

drop_seq <- seq(0, 2, .25)

coef_df <- matrix(, nrow = 9, ncol = 3)

for(j in 1:length(drop_seq)){
  
  df2 <- donut_machine(df, 'dist_border', 'pov', drop_seq[j], 'treat')
  coef_df[j, 1] <- rdrobust(df2$out, df2$new_runvar)$coef[3]
  coef_df[j, 2] <- rdrobust(df2$out, df2$new_runvar)$ci[3,1]
  coef_df[j, 3] <- rdrobust(df2$out, df2$new_runvar)$ci[3,2]
  
  
}
coef_df <- as.data.frame(coef_df)
coef_df <- cbind(coef_df, drop_seq)

poverty_donut <- ggplot(coef_df, aes(drop_seq, V1)) + 
  geom_line(aes(drop_seq, V2), lty = 2, col = 'black') + 
  geom_line(aes(drop_seq, V3), lty = 2, col = 'black') + 
  geom_point() + 
  xlab('Donut Size') + 
  ylab('RD Estimate') +
  ggtitle('Poverty') + 
  geom_hline(yintercept = 0, col = 'red', lty = 2) + 
  theme_tufte()

coef_df <- matrix(, nrow = 9, ncol = 3)

for(j in 1:length(drop_seq)){
  
  df2 <- donut_machine(df, 'dist_border', 'ihs_light', drop_seq[j], 'treat')
  coef_df[j, 1] <- rdrobust(df2$out, df2$new_runvar)$coef[3]
  coef_df[j, 2] <- rdrobust(df2$out, df2$new_runvar)$ci[3,1]
  coef_df[j, 3] <- rdrobust(df2$out, df2$new_runvar)$ci[3,2]
  
  
}
coef_df <- as.data.frame(coef_df)
coef_df <- cbind(coef_df, drop_seq)

light_donut <- ggplot(coef_df, aes(drop_seq, V1)) + 
  geom_line(aes(drop_seq, V2), lty = 2, col = 'black') + 
  geom_line(aes(drop_seq, V3), lty = 2, col = 'black') + 
  geom_point() + 
  xlab('Donut Size') + 
  ylab('RD Estimate') +
  ggtitle('IHS(Luminosity)') + 
  geom_hline(yintercept = 0, col = 'red', lty = 2) + 
  theme_tufte()



census_1998 <- readRDS("data/census_1998_0310.rds")
census_1998$dist_border <- census_1998$dist_border/1000
coef_df <- matrix(, nrow = 9, ncol = 3)


for(j in 1:length(drop_seq)){
  
  df2 <- donut_machine(census_1998, 'dist_border', 'lit_rate', drop_seq[j], 'treat')
  coef_df[j, 1] <- rdrobust(df2$out, df2$new_runvar)$coef[3]
  coef_df[j, 2] <- rdrobust(df2$out, df2$new_runvar)$ci[3,1]
  coef_df[j, 3] <- rdrobust(df2$out, df2$new_runvar)$ci[3,2]
  
  
}
coef_df <- as.data.frame(coef_df)
coef_df <- cbind(coef_df, drop_seq)

lit_donut <- ggplot(coef_df, aes(drop_seq, V1)) + 
  geom_line(aes(drop_seq, V2), lty = 2, col = 'black') + 
  geom_line(aes(drop_seq, V3), lty = 2, col = 'black') + 
  geom_point() + 
  xlab('Donut Size') + 
  ylab('RD Estimate') +
  ggtitle('% Literate 1998') + 
  geom_hline(yintercept = 0, col = 'red', lty = 2) + 
  theme_tufte()


coef_df <- matrix(, nrow = 9, ncol = 3)
for(j in 1:length(drop_seq)){
  
  df2 <- donut_machine(census_1998, 'dist_border', 'no_educ', drop_seq[j], 'treat')
  coef_df[j, 1] <- rdrobust(df2$out, df2$new_runvar)$coef[3]
  coef_df[j, 2] <- rdrobust(df2$out, df2$new_runvar)$ci[3,1]
  coef_df[j, 3] <- rdrobust(df2$out, df2$new_runvar)$ci[3,2]
  
  
}
coef_df <- as.data.frame(coef_df)
coef_df <- cbind(coef_df, drop_seq)

educ_donut <- ggplot(coef_df, aes(drop_seq, V1)) + 
  geom_line(aes(drop_seq, V2), lty = 2, col = 'black') + 
  geom_line(aes(drop_seq, V3), lty = 2, col = 'black') + 
  geom_point() + 
  xlab('Donut Size') + 
  ylab('RD Estimate') +
  ggtitle('% No Education 1998') + 
  geom_hline(yintercept = 0, col = 'red', lty = 2) + 
  theme_tufte()

theme_set(theme_tufte())

donut_plot_2022 <- cowplot::plot_grid(poverty_donut, light_donut, educ_donut, lit_donut, labels = 'AUTO')
ggsave('figs-out/donut_plot_2022.pdf', donut_plot_2022, width = 10)
