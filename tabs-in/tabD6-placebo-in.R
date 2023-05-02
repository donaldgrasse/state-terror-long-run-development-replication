lfs_merge2 = readRDS('data/educDD.rds') %>% 
  mutate(decade = cut(age, 5)) %>% 
  filter(age > 18) 
  
m1 <- felm(school ~ treat*I(as.numeric(age <= 35, 1,0)) + poly(c04, 2)
          | wave + vill_code + decade:factor(commune) + c05   | 
            0 | vill_code, data  = lfs_merge2, exactDOF = T)

m2 <- felm(school ~ treat*I(as.numeric(age <= 30, 1,0)) + poly(c04, 2)
           | wave + vill_code + decade:factor(commune) + c05   | 
             0 | vill_code, data  = lfs_merge2, exactDOF = T)

m3 <- felm(school ~ treat*I(as.numeric(age <= 25, 1,0)) + poly(c04, 2)
           | wave + vill_code + decade:factor(commune) + c05   | 
             0 | vill_code, data  = lfs_merge2, exactDOF = T)

m4 <- felm(school ~ treat*I(as.numeric(age <= 20, 1,0)) + poly(c04, 2)
           | wave + vill_code + decade:factor(commune) + c05   | 
             0 | vill_code, data  = lfs_merge2, exactDOF = T)
