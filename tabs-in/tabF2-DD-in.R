lfs_merge2 = readRDS('data/educDD.rds')

theme_set(theme_tufte())

lfs_merge2$decade <- cut(lfs_merge2$age, 5)

m <- felm(school ~ treat:cohort1 + treat:cohort2 + treat:cohort3 + 
            treat:cohort4 + treat:cohort5 + treat:cohort6 + treat:cohort8
          + treat:cohort9 + treat:cohort10 + treat:cohort11 + poly(c04, 2)
          | wave + vill_code + decade:factor(commune) + c05   | 
            0 | vill_code, data  = lfs_merge2, exactDOF = T)
summary(m)

texreg(list(m))