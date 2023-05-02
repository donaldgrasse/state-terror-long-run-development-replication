#### Table 3 #### 

cen98 <- readRDS("data/census_1998_0310.rds") %>% 
  mutate(dist_border = dist_border/1000
  )

cen98$dist.segment[cen98$dist.segment=='1'] <- '2'
cen98$dist.segment <- as.character(cen98$dist.segment)
cen98$dist.segment <- as.factor(cen98$dist.segment)
cen98$dist.segment <- relevel(cen98$dist.segment, ref = '5')


m1 <- felm(no_educ ~ treat*dist_border | 0 | 0 | 0, 
           data = subset(cen98, abs(dist_border)  <= 5.69)) 

m2 <- felm(no_educ ~ treat*dist_border + log(dist_cap+1) + dist.segment2 + dist.segment3 + dist.segment4
           | 0 | 0 | 0, 
           data = subset(cen98, abs(dist_border)  <= 5.95))

m3 <- felm(no_educ ~ treat*dist_border + treat*I(dist_border^2) | 0 | 0 | 0, 
           data = subset(cen98, abs(dist_border)  <= 12.89)) 

m4 <- felm(no_educ ~ treat*dist_border + treat*I(dist_border^2) + log(dist_cap+1)
           + dist.segment2 + dist.segment3 + dist.segment4
           | 0 | 0 | 0, 
           data = subset(cen98, abs(dist_border)  <= 9.8))

m5 <- felm(lit_rate ~ treat*dist_border | 0 | 0 | 0, 
           data = subset(cen98, abs(dist_border)  <= 5.62)) 

m6 <- felm(lit_rate ~ treat*dist_border + log(dist_cap+1) 
           + dist.segment2 + dist.segment3 + dist.segment4
           | 0 | 0 | 0, 
           data = subset(cen98, abs(dist_border)  <= 4.99))

m7 <- felm(lit_rate ~ treat*dist_border + treat*I(dist_border^2) | 0 | 0 | 0, 
           data = subset(cen98, abs(dist_border)  <= 9.87)) 

m8 <- felm(lit_rate ~ treat*dist_border + treat*I(dist_border^2) + log(dist_cap+1) 
           + dist.segment2 + dist.segment3 + dist.segment4
           | 0 | 0 | 0, 
           data = subset(cen98, abs(dist_border)  <= 8.95))

texreg(list(m1, m2, m3, m4,m5, m6, m7,m8))

