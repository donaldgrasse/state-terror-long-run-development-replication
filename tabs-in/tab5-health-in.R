

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

m1 <- lm(hlthIndex ~ SW + dist.border + borderrt + 
           wave_year05 + wave_year10 + wave_year14 + v012 + v012sq, 
         data = dhs_kr_sub,
         weights = 1 - abs(dist.border)/m1$bws[1] 
)
clust1 <- length(unique(dhs_kr_sub$DHSCLUST))


vcov1 <- cluster.boot(m1, cluster = dhs_kr_sub$DHSID, 
                      boot_type = "wild", wild_type = "rademacher", R = 10000)
sqrt(diag(vcov1))
2*(1 - pt(abs(m1$coefficients[2]/sqrt(diag(vcov1))[2]), df = clust1-1))


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

m1 <- lm(hw5 ~ SW + dist.border + borderrt + 
           wave_year05 + wave_year10 + wave_year14 + v012 + v012sq, 
         data = dhs_kr_sub,
         weights = 1 - abs(dist.border)/max(abs(dist.border)))

vcov2 <- cluster.boot(m1, cluster = dhs_kr_sub$DHSID, 
                      boot_type = "wild", wild_type = "rademacher", R = 10000)
sqrt(diag(vcov2))
clust2 <- length(unique(dhs_kr_sub$DHSCLUST))
2*(1 - pt(abs(m1$coefficients[2]/sqrt(diag(vcov2))[2]), df = clust2-1))



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

m1 <- lm(hw8 ~ SW + dist.border + borderrt + 
           wave_year05 + wave_year10 + wave_year14 + v012 + v012sq, 
         data = dhs_kr_sub,
         weights = 1 - abs(dist.border)/max(abs(dist.border)))

vcov3 <- cluster.boot(m1, cluster = dhs_kr_sub$DHSID, 
                      boot_type = "wild", wild_type = "rademacher", R = 10000)
sqrt(diag(vcov3))

clust3 <- length(unique(dhs_kr_sub$DHSCLUST))
2*(1 - pt(abs(m1$coefficients[2]/sqrt(diag(vcov3))[2]), df = clust3-1))



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

m1 <- lm(hw11 ~ SW + dist.border + borderrt + 
           wave_year05 + wave_year10 + wave_year14 + v012 + v012sq, 
         data = dhs_kr_sub,
         weights = 1 - abs(dist.border)/m1$bws[1])

vcov4 <- cluster.boot(m1, cluster = dhs_kr_sub$DHSID, 
                      boot_type = "wild", wild_type = "rademacher", R = 10000)
sqrt(diag(vcov4))
clust4 <- length(unique(dhs_kr_sub$DHSCLUST))
#2*(1 - pt(abs(-1.1742565/0.3698765674), df = clust4-2))
2*(1 - pt(abs(m1$coefficients[2]/sqrt(diag(vcov4))[2]), df = clust4-1))


texreg::texreg(list(m1_felm, m2_felm, m3_felm, m4_felm), include.ci = F)

