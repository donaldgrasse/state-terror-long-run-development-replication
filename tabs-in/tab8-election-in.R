


ele_df = readRDS("data/eledf.rds")

m1 = felm(hh12 ~ treat*dist.border | district | 0 | 0, data = ele_df, 
          weights = 1 - ele_df$dist.border/max(abs(ele_df$dist.border)))
m2 = felm(hh17 ~ treat*dist.border | district | 0 | 0, data = ele_df, 
          weights = 1 - ele_df$dist.border/max(abs(ele_df$dist.border)))
m3 = felm(CPP_12 ~ treat*dist.border | district | 0 | 0, data = ele_df,
          weights = 1 - ele_df$dist.border/max(abs(ele_df$dist.border)))
m4 = felm(I(CPP_17*100) ~ treat*dist.border | district | 0 | 0, data = ele_df, 
          weights = 1 - ele_df$dist.border/max(abs(ele_df$dist.border))
          )
  

texreg(list(m1, m2, m3, m4), include.ci = F) 
