df <-readRDS('data/main_0310.rds')


m1 <- felm(pov ~ treat*dist_border | 0 | 0 | vill_code, 
           data = subset(df, abs(dist_border)  <= 6.34)) 

m2 <- felm(pov ~ treat*dist_border + log(dist_cap+1) | dist.segment | 0 | vill_code, 
           data = subset(df, abs(dist_border)  <= 5.98))

m3 <- felm(pov ~ treat*dist_border + treat*I(dist_border^2) | 0 | 0 | vill_code, 
           data = subset(df, abs(dist_border)  <= 10.99)) 

m4 <- felm(pov ~ treat*dist_border + treat*I(dist_border^2) + log(dist_cap+1) | dist.segment | 0 | vill_code, 
           data = subset(df, abs(dist_border)  <= 10.62))

m5 <- felm(ihs_light ~ treat*dist_border | 0 | 0 | vill_code, 
           data = subset(df, abs(dist_border)  <= 8.9)) 

m6 <- felm(ihs_light ~ treat*dist_border + log(dist_cap+1) | dist.segment | 0 | vill_code, 
           data = subset(df, abs(dist_border)  <= 7.95))

m7 <- felm(ihs_light ~ treat*dist_border + treat*I(dist_border^2) | 0 | 0 | vill_code, 
           data = subset(df, abs(dist_border)  <= 9.64)) 

m8 <- felm(ihs_light ~ treat*dist_border + treat*I(dist_border^2) + log(dist_cap+1) | dist.segment | 0 | vill_code, 
           data = subset(df, abs(dist_border)  <= 12.79))

texreg(list(m1, m2, m3, m4,m5, m6, m7,m8))


