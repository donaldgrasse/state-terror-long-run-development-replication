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

m1 <- felm(school ~ treat:cohort1 + treat:cohort2 + treat:cohort3 + 
            treat:cohort4 + treat:cohort5 + treat:cohort6 + treat:cohort8
          + treat:cohort9 + treat:cohort10 + treat:cohort11 + poly(c04, 2)
          + c05 + wave 
          | vill_code + decade:factor(commune)   | 
            0 | vill_code, data  = lfs_merge_trim, exactDOF = T)

lfs_merge_trim <- subset(lfs_merge2, trim < .80)

m2 <- felm(school ~ treat:cohort1 + treat:cohort2 + treat:cohort3 + 
            treat:cohort4 + treat:cohort5 + treat:cohort6 + treat:cohort8
          + treat:cohort9 + treat:cohort10 + treat:cohort11 + poly(c04, 2)
          + c05 + wave 
          | vill_code + decade:factor(commune)    | 
            0 | vill_code, data  = lfs_merge_trim, exactDOF = T)


lfs_merge_trim <- subset(lfs_merge2, trim < .70)

m3 <- felm(school ~ treat:cohort1 + treat:cohort2 + treat:cohort3 + 
            treat:cohort4 + treat:cohort5 + treat:cohort6 + treat:cohort8
          + treat:cohort9 + treat:cohort10 + treat:cohort11 + poly(c04, 2) 
          + c05 + wave 
          | vill_code + decade:factor(commune)   | 
            0 | vill_code, data  = lfs_merge_trim, exactDOF = T)

lfs_merge_trim <- subset(lfs_merge2, trim < .50)

m4 <- felm(school ~ treat:cohort1 + treat:cohort2 + treat:cohort3 + 
            treat:cohort4 + treat:cohort5 + treat:cohort6 + treat:cohort8
          + treat:cohort9 + treat:cohort10 + treat:cohort11 + poly(c04, 2)
          + c05 + wave 
          | vill_code + decade:factor(commune)   | 
            0 | vill_code, data  = lfs_merge_trim, exactDOF = T)

lfs_merge_trim <- subset(lfs_merge2, trim < .30)

m5 <- felm(school ~ treat:cohort1 + treat:cohort2 + treat:cohort3 + 
            treat:cohort4 + treat:cohort5 + treat:cohort6 + treat:cohort8
          + treat:cohort9 + treat:cohort10 + treat:cohort11 + poly(c04, 2)
          + c05 + wave 
          |  vill_code + decade:factor(commune)   | 
            0 | vill_code, data  = lfs_merge_trim, exactDOF = T)

texreg(list(m1, m2, m3, m4, m5), include.ci = F)
