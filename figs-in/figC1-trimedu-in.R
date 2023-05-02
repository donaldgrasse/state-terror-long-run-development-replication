#### Trimming #### 

lapply(c('ggplot2', 'ggthemes', 'lfe', 
         'dplyr', 'tidyverse'), 
       require, 
       character.only = T
       )


lfs_merge2 = readRDS('data/educDD.rds')
lfs_merge2$percent <- percent_rank(lfs_merge2$school)
lfs_merge2$decade <- cut(lfs_merge2$age, 5)


lfs_merge2$West <- as.numeric(lfs_merge2$treat==0, 1,0)
lfs_merge2$trim <- lfs_merge2$West*lfs_merge2$percent

lfs_merge_trim <- subset(lfs_merge2, trim < .90)

m <- felm(school ~ treat:cohort1 + treat:cohort2 + treat:cohort3 + 
            treat:cohort4 + treat:cohort5 + treat:cohort6 + treat:cohort8
          + treat:cohort9 + treat:cohort10 + treat:cohort11 + poly(c04, 2)
          | wave + vill_code + decade:factor(commune) + c05   | 
            0 | vill_code, data  = lfs_merge_trim, exactDOF = T)

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


dd_plot_90 <- ggplot(coef.df.full, aes(x = as.numeric(names))) +
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
  ggtitle('Trimming 90th Percentile')


lfs_merge_trim <- subset(lfs_merge2, trim < .80)

m <- felm(school ~ treat:cohort1 + treat:cohort2 + treat:cohort3 + 
            treat:cohort4 + treat:cohort5 + treat:cohort6 + treat:cohort8
          + treat:cohort9 + treat:cohort10 + treat:cohort11 + poly(c04, 2)
          | wave + vill_code + decade:factor(commune) + c05   | 
            0 | vill_code, data  = lfs_merge_trim, exactDOF = T)
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


dd_plot_80 <- ggplot(coef.df.full, aes(x = as.numeric(names))) +
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
  ggtitle('Trimming 80th Percentile')



lfs_merge_trim <- subset(lfs_merge2, trim < .70)

m <- felm(school ~ treat:cohort1 + treat:cohort2 + treat:cohort3 + 
            treat:cohort4 + treat:cohort5 + treat:cohort6 + treat:cohort8
          + treat:cohort9 + treat:cohort10 + treat:cohort11 + poly(c04, 2)
          | wave + vill_code + decade:factor(commune) + c05   | 
            0 | vill_code, data  = lfs_merge_trim, exactDOF = T)
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


dd_plot_70 <- ggplot(coef.df.full, aes(x = as.numeric(names))) +
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
  ggtitle('Trimming 70th Percentile')




lfs_merge_trim <- subset(lfs_merge2, trim < .50)

m <- felm(school ~ treat:cohort1 + treat:cohort2 + treat:cohort3 + 
            treat:cohort4 + treat:cohort5 + treat:cohort6 + treat:cohort8
          + treat:cohort9 + treat:cohort10 + treat:cohort11 + poly(c04, 2)
          | wave + vill_code + decade:factor(commune) + c05   | 
            0 | vill_code, data  = lfs_merge_trim, exactDOF = T)
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


dd_plot_50 <- ggplot(coef.df.full, aes(x = as.numeric(names))) +
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
  ggtitle('Trimming 50th Percentile')

lfs_merge_trim <- subset(lfs_merge2, trim < .30)

m <- felm(school ~ treat:cohort1 + treat:cohort2 + treat:cohort3 + 
            treat:cohort4 + treat:cohort5 + treat:cohort6 + treat:cohort8
          + treat:cohort9 + treat:cohort10 + treat:cohort11 + poly(c04, 2)
          | wave + vill_code + decade:factor(commune) + c05   | 
            0 | vill_code, data  = lfs_merge_trim, exactDOF = T)
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


dd_plot_30 <- ggplot(coef.df.full, aes(x = as.numeric(names))) +
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
  ggtitle('Trimming 30th Percentile')



trim_educ_plot <- cowplot::plot_grid(dd_plot_90, dd_plot_80,  dd_plot_70, dd_plot_50, dd_plot_30, labels = 'AUTO')
ggsave('figs-out/trim_educ_plot.pdf', width = 12, height = 9)



