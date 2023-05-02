lfs_merge2 = readRDS('data/educDD.rds')

theme_set(theme_tufte())

lfs_merge2$decade <- cut(lfs_merge2$age, 5)

m <- felm(school ~ treat:cohort1 + treat:cohort2 + treat:cohort3 + 
            treat:cohort4 + treat:cohort5 + treat:cohort6 + treat:cohort8
          + treat:cohort9 + treat:cohort10 + treat:cohort11 + poly(c04, 2)
          | wave + vill_code + decade:factor(commune) + c05   | 
            0 | vill_code, data  = lfs_merge2, exactDOF = T)
summary(m)


coef.df <- cbind.data.frame('est' =  m$coefficients[3:nrow( m$coefficients)],
                            'L'=  m$coefficients[3:nrow( m$coefficients)]-1.96*m$cse[3:nrow( m$coefficients)], 
                            'U' = m$coefficients[3:nrow( m$coefficients)]+1.96*m$cse[3:nrow( m$coefficients)])
colnames(coef.df)[1:3] <- c('est', "L", 'U')

coef.df$names <- rev(c('40', '35', '30', '26', '17', '12', '8',  '3', '-1', '-5'))

coef.df.full <- as.data.frame(x = c('40', '35', '30', '21', '26', '17', '12', '8',  '3', '-1', '-5'))
colnames(coef.df.full)[1] <- 'names'
coef.df.full <- merge(coef.df.full, coef.df, by = 'names', all.x = T)
coef.df.full[is.na(coef.df.full)] <- 0 
coef.df.full$y2 <- round(coef.df.full$est, 2)
coef.df.full$names <- as.numeric(as.character(coef.df.full$names))
coef.df.full$y2[coef.df.full$y2 == 0] <- 'Reference Cohort'


dd_plot <- ggplot(coef.df.full, aes(x = as.numeric(names))) +
  geom_line(aes(y = est)) + 
  geom_point(aes(y=est)) + 
  geom_ribbon(aes(ymin=L, ymax=U), alpha=0.2, linetype = 'dashed') +
  geom_hline(yintercept = 0, size = .2) + 
  xlab("Age in 1975 (Onset of Khmer Rouge)") +
  ylab("Diff-in-Diff Estimate") +
  geom_vline(xintercept = 21, col = 'red', lty = 2) + 
  geom_label(data = coef.df.full, aes(x = as.numeric(names), y = est, label = y2), family = 'Times') + 
  theme_tufte() + 
  scale_x_continuous(trans = 'reverse') + 
  geom_label(aes(x = 30, y = 1.8, label = "18 or Older \nBefore DK"), family = 'Times', inherit.aes = F) + 
  geom_label(aes(x = 10, y = 1.8, label = "Schooling Age \nDuring and After DK"), family = 'Times', inherit.aes = F) + 
  ggtitle('Dynamic Effect of SW Zone on Education')


schooling_by_age_plot <- 
  lfs_merge2 %>% 
  filter(age <= 40 & age >= -10) %>% 
  mutate(age_cohort = cut(age, length(seq(min(age), max(age), by = 5)))) %>% 
  group_by(treat, age) %>% 
  summarise(school = mean(school, na.rm = T)
  ) %>% 
  ggplot(aes(x = age, y = school, lty = as.factor(treat), color = as.factor(treat))) + 
  geom_ma(n = 4) + 
  geom_smooth(data = subset(lfs_merge2, age <= 40 & age > 18), aes(x = age, y = school), method = 'lm', se = F) + 
  geom_smooth(data = subset(lfs_merge2, age <= 18 & age >= -10), aes(x = age, y = school), method = 'lm', se = F) + 
  geom_vline(xintercept = 19, col = 'red') + 
  theme_tufte() + 
  theme(legend.position = 'bottom') + 
  labs(lty = '1(SW Zone)', color = "1(SW Zone)") + 
  scale_color_manual(values = c('grey', 'black')) + 
  scale_linetype_manual(values = c('dashed', 'solid'))  +
  xlab('Age in 1975') + 
  ylab('Years of Schooling') + 
  geom_label(aes(x = 30, y = 6, label = "18 or Older \nBefore DK"), family = 'Times', inherit.aes = F) + 
  geom_label(aes(x = 10, y = 6, label = "Schooling Age \nDuring and After DK"), family = 'Times', inherit.aes = F) + 
  scale_x_continuous(trans = 'reverse') + 
  ggtitle('Education Trends by Age and Zone')


education_trends <- cowplot::plot_grid(schooling_by_age_plot, dd_plot, labels = 'AUTO')
ggsave('figs-out/education_trends.pdf', width = 12)
