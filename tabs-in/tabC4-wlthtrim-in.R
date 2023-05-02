
#### 95 % trim ##### 
dhs_df2 = readRDS('data/dhs_df.rds') %>% 
  mutate(wealth_trim = wlthpct*W) %>% 
  filter(wealth_trim < .95)

m1 <- rdrobust(dhs_df2$wlth, dhs_df2$dist.border, 
               covs = cbind(dhs_df2$wave_year05, 
                            dhs_df2$wave_year10, 
                            dhs_df2$wave_year14, 
                            dhs_df2$hv105, 
                            dhs_df2$hv105^2,
                            dhs_df2$female), 
               cluster =dhs_df2$DHSCLUST,
               vce = 'hc3'
) 

#### 90 % trim ##### 
dhs_df2 = readRDS('data/dhs_df.rds') %>% 
  mutate(wealth_trim = wlthpct*W) %>% 
  filter(wealth_trim < .90)

m2 <- rdrobust(dhs_df2$wlth, dhs_df2$dist.border, 
               covs = cbind(dhs_df2$wave_year05, 
                            dhs_df2$wave_year10, 
                            dhs_df2$wave_year14, 
                            dhs_df2$hv105, 
                            dhs_df2$hv105^2,
                            dhs_df2$female), 
               cluster =dhs_df2$DHSCLUST,
               vce = 'hc3'
) 

#### 85 % trim ##### 
dhs_df2 = readRDS('data/dhs_df.rds') %>% 
  mutate(wealth_trim = wlthpct*W) %>% 
  filter(wealth_trim < .85)

m3 <- rdrobust(dhs_df2$wlth, dhs_df2$dist.border, 
               covs = cbind(dhs_df2$wave_year05, 
                            dhs_df2$wave_year10, 
                            dhs_df2$wave_year14, 
                            dhs_df2$hv105, 
                            dhs_df2$hv105^2,
                            dhs_df2$female), 
               cluster =dhs_df2$DHSCLUST,
               vce = 'hc3'
) 

#### 80 % trim ##### 
dhs_df2 = readRDS('data/dhs_df.rds') %>% 
  mutate(wealth_trim = wlthpct*W) %>% 
  filter(wealth_trim < .80)

m4 <- rdrobust(dhs_df2$wlth, dhs_df2$dist.border, 
               covs = cbind(dhs_df2$wave_year05, 
                            dhs_df2$wave_year10, 
                            dhs_df2$wave_year14, 
                            dhs_df2$hv105, 
                            dhs_df2$hv105^2,
                            dhs_df2$female), 
               cluster =dhs_df2$DHSCLUST,
               vce = 'hc3'
) 
#### 75 % trim ##### 
dhs_df2 = readRDS('data/dhs_df.rds') %>% 
  mutate(wealth_trim = wlthpct*W) %>% 
  filter(wealth_trim < .75)

m5 <- rdrobust(dhs_df2$wlth, dhs_df2$dist.border, 
               covs = cbind(dhs_df2$wave_year05, 
                            dhs_df2$wave_year10, 
                            dhs_df2$wave_year14, 
                            dhs_df2$hv105, 
                            dhs_df2$hv105^2,
                            dhs_df2$female), 
               cluster =dhs_df2$DHSCLUST,
               vce = 'hc3'
) 

#### 70 % trim ##### 
dhs_df2 = readRDS('data/dhs_df.rds') %>% 
  mutate(wealth_trim = wlthpct*W) %>% 
  filter(wealth_trim < .70)

m6 <- rdrobust(dhs_df2$wlth, dhs_df2$dist.border, 
               covs = cbind(dhs_df2$wave_year05, 
                            dhs_df2$wave_year10, 
                            dhs_df2$wave_year14, 
                            dhs_df2$hv105, 
                            dhs_df2$hv105^2,
                            dhs_df2$female), 
               cluster =dhs_df2$DHSCLUST,
               vce = 'hc3'
) 


panel_a <- rdtable(list(m1, m2, m3, m4, m5, m6))

xtable(panel_a)

