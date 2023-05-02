

#### Health Outcomes #### 

packs <- c('rdrobust', 'multiwayvcov', 'dplyr', 'lfe', 'sf')
lapply(packs, require, character.only = T)


dhs_kr <- readRDS('data/dhs_kr.rds') %>% 
  filter(urban ==0)

#note: mass point will be detected in the running variable 
#because there are multiple observations 
#that have the same value of the running variable 
#dist_border, since the observations are at the 
#individual level but the assignment variable is
#the location of a village. to account for this, 
#errors are clustered at the village

#### Outcomes #### 

#index
m1 <- rdrobust(dhs_kr$hlthIndex, 
               dhs_kr$dist.border,
               covs = cbind(dhs_kr$wave_year05, 
                            dhs_kr$wave_year10, 
                            dhs_kr$wave_year14, 
                            dhs_kr$v012, 
                            dhs_kr$v012^2 
               ), 
               cluster = dhs_kr$DHSCLUST, 
               vce = 'hc3', 
               p = 1
) 
dhs_kr_sub <- subset(dhs_kr, abs(dist.border) <= m1$bws[1])
dhs_kr_sub$borderrt <- dhs_kr_sub$dist.border*dhs_kr_sub$SW
dhs_kr_sub$v012sq <- dhs_kr_sub$v012^2

m1_felm <- felm(hlthIndex ~ SW + dist.border + borderrt + v012 + v012sq 
                |wave_year | 0 | DHSCLUST,
                weights = 1 - abs(dhs_kr_sub$dist.border)/m1$bws[1],
                data = dhs_kr_sub
)

# hw5
m1 <- rdrobust(dhs_kr$hw5, 
               dhs_kr$dist.border,
               covs = cbind(dhs_kr$wave_year05, 
                            dhs_kr$wave_year10, 
                            dhs_kr$wave_year14, 
                            dhs_kr$v012, 
                            dhs_kr$v012^2 
               ), 
               cluster = dhs_kr$DHSCLUST, 
               vce = 'hc3', 
               p = 1
) 
dhs_kr_sub <- subset(dhs_kr, abs(dist.border) <= m1$bws[1])
dhs_kr_sub$borderrt <- dhs_kr_sub$dist.border*dhs_kr_sub$SW
dhs_kr_sub$v012sq <- dhs_kr_sub$v012^2

m2_felm <- felm(hw5 ~ SW + dist.border + borderrt + v012 + v012sq 
                |wave_year | 0 | DHSCLUST,
                weights = 1 - abs(dhs_kr_sub$dist.border)/m1$bws[1],
                data = dhs_kr_sub
)


# hw8
m1 <- rdrobust(dhs_kr$hw8, 
               dhs_kr$dist.border,
               covs = cbind(dhs_kr$wave_year05, 
                            dhs_kr$wave_year10, 
                            dhs_kr$wave_year14, 
                            dhs_kr$v012, 
                            dhs_kr$v012^2 
               ), 
               cluster = dhs_kr$DHSCLUST, 
               vce = 'hc3', 
               p = 1
) 
dhs_kr_sub <- subset(dhs_kr, abs(dist.border) <= m1$bws[1])
dhs_kr_sub$borderrt <- dhs_kr_sub$dist.border*dhs_kr_sub$SW
dhs_kr_sub$v012sq <- dhs_kr_sub$v012^2

m3_felm <- felm(hw8 ~ SW + dist.border + borderrt + v012 + v012sq 
                |wave_year | 0 | DHSCLUST,
                weights = 1 - abs(dhs_kr_sub$dist.border)/m1$bws[1],
                data = dhs_kr_sub
)



# hw11
m1 <- rdrobust(dhs_kr$hw11, 
               dhs_kr$dist.border,
               covs = cbind(dhs_kr$wave_year05, 
                            dhs_kr$wave_year10, 
                            dhs_kr$wave_year14, 
                            dhs_kr$v012, 
                            dhs_kr$v012^2 
               ), 
               cluster = dhs_kr$DHSCLUST, 
               vce = 'hc3', 
               p = 1
) 
dhs_kr_sub <- subset(dhs_kr, abs(dist.border) <= m1$bws[1])
dhs_kr_sub$borderrt <- dhs_kr_sub$dist.border*dhs_kr_sub$SW
dhs_kr_sub$v012sq <- dhs_kr_sub$v012^2

m4_felm <- felm(hw11 ~ SW + dist.border + borderrt + v012 + v012sq 
                |wave_year | 0 | DHSCLUST,
                weights = 1 - abs(dhs_kr_sub$dist.border)/m1$bws[1],
                data = dhs_kr_sub
)



texreg::texreg(list(m1_felm, m2_felm, m3_felm, m4_felm), include.ci = F)

