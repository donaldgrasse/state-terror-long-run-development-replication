#### No Education #### 

dhs_kr <- readRDS('/Users/donaldgrasse/Dropbox/dev-legacies-dictatorship-apsr-clean/data/dhs_kr.rds') %>% 
  filter(urban ==0) %>% 
  filter(mother_educ == 1)


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
dhs_kr_sub <- subset(dhs_kr, abs(dist.border) < m1$bws[1])
dhs_kr_sub$borderrt <- dhs_kr_sub$dist.border*dhs_kr_sub$SW
dhs_kr_sub$v012sq <- dhs_kr_sub$v012^2

m1_felm <- felm(hlthIndex ~ SW + dist.border + borderrt + v012 + v012sq 
                |wave_year | 0 | DHSCLUST,
                weights = 1 - abs(dhs_kr_sub$dist.border)/max(abs(dhs_kr_sub$dist.border)),
                data = dhs_kr_sub
)

m1 <- lm(hlthIndex ~ SW + dist.border + borderrt + 
           wave_year05 + wave_year10 + wave_year14 + v012 + v012sq, 
         data = dhs_kr_sub,
         weights = 1 - abs(dist.border)/max(abs(dist.border)))
clust1 <- length(unique(dhs_kr_sub$DHSCLUST))


vcov1 <- cluster.boot(m1, cluster = dhs_kr_sub$DHSID, 
                      boot_type = "wild", wild_type = "rademacher", R = 10000)
se1 = sqrt(diag(vcov1))[2]
pv1 =  2*(1 - pt(abs(m1_felm$coefficients[1]/sqrt(diag(vcov1))[2]), df = clust1-2))


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
dhs_kr_sub <- subset(dhs_kr, abs(dist.border) < m1$bws[1])
dhs_kr_sub$borderrt <- dhs_kr_sub$dist.border*dhs_kr_sub$SW
dhs_kr_sub$v012sq <- dhs_kr_sub$v012^2

m2_felm <- felm(hw5 ~ SW + dist.border + borderrt + v012 + v012sq 
                |wave_year | 0 | DHSCLUST,
                weights = 1 - abs(dhs_kr_sub$dist.border)/max(abs(dhs_kr_sub$dist.border)),
                data = dhs_kr_sub
)

m1 <- lm(hw5 ~ SW + dist.border + borderrt + 
           wave_year05 + wave_year10 + wave_year14 + v012 + v012sq, 
         data = dhs_kr_sub,
         weights = 1 - abs(dist.border)/max(abs(dist.border)))

vcov2 <- cluster.boot(m1, cluster = dhs_kr_sub$DHSID, 
                      boot_type = "wild", wild_type = "rademacher", R = 10000)
se2 = sqrt(diag(vcov2))[2]
clust2 <- length(unique(dhs_kr_sub$DHSCLUST))
pv2 = 2*(1 - pt(abs(m2_felm$coefficients[1]/sqrt(diag(vcov2))[2]), df = clust2-2))



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
dhs_kr_sub <- subset(dhs_kr, abs(dist.border) < m1$bws[1])
dhs_kr_sub$borderrt <- dhs_kr_sub$dist.border*dhs_kr_sub$SW
dhs_kr_sub$v012sq <- dhs_kr_sub$v012^2

m3_felm <- felm(hw8 ~ SW + dist.border + borderrt + v012 + v012sq 
                |wave_year | 0 | DHSCLUST,
                weights = 1 - abs(dhs_kr_sub$dist.border)/max(abs(dhs_kr_sub$dist.border)),
                data = dhs_kr_sub
)

m1 <- lm(hw8 ~ SW + dist.border + borderrt + 
           wave_year05 + wave_year10 + wave_year14 + v012 + v012sq, 
         data = dhs_kr_sub,
         weights = 1 - abs(dist.border)/max(abs(dist.border)))

vcov3 <- cluster.boot(m1, cluster = dhs_kr_sub$DHSID, 
                      boot_type = "wild", wild_type = "rademacher", R = 10000)
se3 = sqrt(diag(vcov3))[2]

clust3 <- length(unique(dhs_kr_sub$DHSCLUST))
pv3 = 2*(1 - pt(abs(m3_felm$coefficients[1]/sqrt(diag(vcov3))[2]), df = clust3-2))



# hw8
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
dhs_kr_sub <- subset(dhs_kr, abs(dist.border) < m1$bws[1])
dhs_kr_sub$borderrt <- dhs_kr_sub$dist.border*dhs_kr_sub$SW
dhs_kr_sub$v012sq <- dhs_kr_sub$v012^2

m4_felm <- felm(hw11 ~ SW + dist.border + borderrt + v012 + v012sq 
                |wave_year | 0 | DHSCLUST,
                weights = 1 - abs(dhs_kr_sub$dist.border)/max(abs(dhs_kr_sub$dist.border)),
                data = dhs_kr_sub
)

m1 <- lm(hw11 ~ SW + dist.border + borderrt + 
           wave_year05 + wave_year10 + wave_year14 + v012 + v012sq, 
         data = dhs_kr_sub,
         weights = 1 - abs(dist.border)/max(abs(dist.border)))

vcov <- cluster.boot(m1, cluster = dhs_kr_sub$DHSID, 
                     boot_type = "wild", wild_type = "rademacher", R = 10000)
se4 = sqrt(diag(vcov))[2]
clust4 <- length(unique(dhs_kr_sub$DHSCLUST))
pv4 = 2*(1 - pt(abs(m1$coefficients[2]/sqrt(diag(vcov))[2]), df = clust4-1))

xtable(cbind(se1, se2, se3, se4))
xtable(cbind(clust1, clust2, clust3, clust4))

texreg::texreg(list(m1_felm, m2_felm, m3_felm, m4_felm), include.ci = F)







#### Yes Education #### 

dhs_kr <- readRDS('/Users/donaldgrasse/Dropbox/dev-legacies-dictatorship-apsr-clean/data/dhs_kr.rds') %>% 
  filter(urban ==0) %>% 
  filter(mother_educ > 1)


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
dhs_kr_sub <- subset(dhs_kr, abs(dist.border) < m1$bws[1])
dhs_kr_sub$borderrt <- dhs_kr_sub$dist.border*dhs_kr_sub$SW
dhs_kr_sub$v012sq <- dhs_kr_sub$v012^2

m1_felm <- felm(hlthIndex ~ SW + dist.border + borderrt + v012 + v012sq 
                |wave_year | 0 | DHSCLUST,
                weights = 1 - abs(dhs_kr_sub$dist.border)/max(abs(dhs_kr_sub$dist.border)),
                data = dhs_kr_sub
)

m1 <- lm(hlthIndex ~ SW + dist.border + borderrt + 
           wave_year05 + wave_year10 + wave_year14 + v012 + v012sq, 
         data = dhs_kr_sub,
         weights = 1 - abs(dist.border)/max(abs(dist.border)))
clust1 <- length(unique(dhs_kr_sub$DHSCLUST))


vcov1 <- cluster.boot(m1, cluster = dhs_kr_sub$DHSID, 
                      boot_type = "wild", wild_type = "rademacher", R = 10000)
se1 = sqrt(diag(vcov1))[2]
2*(1 - pt(abs(m1_felm$coefficients[1]/sqrt(diag(vcov1))[2]), df = clust1-2))


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
dhs_kr_sub <- subset(dhs_kr, abs(dist.border) < m1$bws[1])
dhs_kr_sub$borderrt <- dhs_kr_sub$dist.border*dhs_kr_sub$SW
dhs_kr_sub$v012sq <- dhs_kr_sub$v012^2

m2_felm <- felm(hw5 ~ SW + dist.border + borderrt + v012 + v012sq 
                |wave_year | 0 | DHSCLUST,
                weights = 1 - abs(dhs_kr_sub$dist.border)/max(abs(dhs_kr_sub$dist.border)),
                data = dhs_kr_sub
)

m1 <- lm(hw5 ~ SW + dist.border + borderrt + 
           wave_year05 + wave_year10 + wave_year14 + v012 + v012sq, 
         data = dhs_kr_sub,
         weights = 1 - abs(dist.border)/max(abs(dist.border)))

vcov2 <- cluster.boot(m1, cluster = dhs_kr_sub$DHSID, 
                      boot_type = "wild", wild_type = "rademacher", R = 10000)
se2 = sqrt(diag(vcov2))[2]
clust2 <- length(unique(dhs_kr_sub$DHSCLUST))
2*(1 - pt(abs(m2_felm$coefficients[1]/sqrt(diag(vcov2))[2]), df = clust2-2))



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
dhs_kr_sub <- subset(dhs_kr, abs(dist.border) < m1$bws[1])
dhs_kr_sub$borderrt <- dhs_kr_sub$dist.border*dhs_kr_sub$SW
dhs_kr_sub$v012sq <- dhs_kr_sub$v012^2

m3_felm <- felm(hw8 ~ SW + dist.border + borderrt + v012 + v012sq 
                |wave_year | 0 | DHSCLUST,
                weights = 1 - abs(dhs_kr_sub$dist.border)/max(abs(dhs_kr_sub$dist.border)),
                data = dhs_kr_sub
)

m1 <- lm(hw8 ~ SW + dist.border + borderrt + 
           wave_year05 + wave_year10 + wave_year14 + v012 + v012sq, 
         data = dhs_kr_sub,
         weights = 1 - abs(dist.border)/max(abs(dist.border)))

vcov3 <- cluster.boot(m1, cluster = dhs_kr_sub$DHSID, 
                      boot_type = "wild", wild_type = "rademacher", R = 10000)
se3 = sqrt(diag(vcov3))[2]

clust3 <- length(unique(dhs_kr_sub$DHSCLUST))
2*(1 - pt(abs(m3_felm$coefficients[1]/sqrt(diag(vcov3))[2]), df = clust3-2))



# hw8
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
dhs_kr_sub <- subset(dhs_kr, abs(dist.border) < m1$bws[1])
dhs_kr_sub$borderrt <- dhs_kr_sub$dist.border*dhs_kr_sub$SW
dhs_kr_sub$v012sq <- dhs_kr_sub$v012^2

m4_felm <- felm(hw11 ~ SW + dist.border + borderrt + v012 + v012sq 
                |wave_year | 0 | DHSCLUST,
                weights = 1 - abs(dhs_kr_sub$dist.border)/max(abs(dhs_kr_sub$dist.border)),
                data = dhs_kr_sub
)

m1 <- lm(hw11 ~ SW + dist.border + borderrt + 
           wave_year05 + wave_year10 + wave_year14 + v012 + v012sq, 
         data = dhs_kr_sub,
         weights = 1 - abs(dist.border)/max(abs(dist.border)))

vcov <- cluster.boot(m1, cluster = dhs_kr_sub$DHSID, 
                     boot_type = "wild", wild_type = "rademacher", R = 10000)
se4 = sqrt(diag(vcov))[2]
clust4 <- length(unique(dhs_kr_sub$DHSCLUST))
2*(1 - pt(abs(m1$coefficients[2]/sqrt(diag(vcov))[2]), df = clust4-2))


texreg::texreg(list(m1_felm, m2_felm, m3_felm, m4_felm), include.ci = F)

