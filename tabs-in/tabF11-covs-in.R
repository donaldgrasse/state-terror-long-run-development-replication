df <- readRDS('data/main_0310.rds')

m1 = felm(t_tersec ~ treat*dist_border + log(dist_cap+1) 
          | dist.segment | 0  | 0, data = subset(df, abs(dist_border) <= 10))

df <- subset(df, t_tersec <= 18.1025)

m2 = felm(t_tersec ~ treat*dist_border + log(dist_cap+1) 
          | dist.segment | 0  | 0, data = subset(df, abs(dist_border) <= 10))


texreg::texreg(list(m1, m2))