

#note: mass point will be detected in the running variable 
#because there are multiple observations 
#that have the same value of the running variable 
#dist_border, since the observations are at the 
#individual level but the assignment variable is
#the location of a village. to account for this, 
#errors are clustered at the village

lfs_income = readRDS("data/lfs_income.rds")

m1 <- rdrobust(lfs_income$self[lfs_income$sector == 'Rural'], 
               lfs_income$dist_border[lfs_income$sector == 'Rural'], 
               covs = cbind(lfs_income$c04[lfs_income$sector == 'Rural'], 
                            lfs_income$c04[lfs_income$sector == 'Rural']^2, 
                            lfs_income$wave_2000[lfs_income$sector == 'Rural'], 
                            lfs_income$female[lfs_income$sector == 'Rural']
               ),
               cluster = lfs_income$vill_code[lfs_income$sector == 'Rural'], 
               vce = 'hc3'
) 

m2 <- rdrobust(lfs_income$ihsincome[lfs_income$sector == 'Rural'], 
               lfs_income$dist_border[lfs_income$sector == 'Rural'], 
               covs = cbind(lfs_income$c04[lfs_income$sector == 'Rural'], 
                            lfs_income$c04[lfs_income$sector == 'Rural']^2, 
                            lfs_income$wave_2000[lfs_income$sector == 'Rural'],
                            lfs_income$female[lfs_income$sector == 'Rural']
                            
               ),
               cluster = lfs_income$vill_code[lfs_income$sector == 'Rural'], 
               vce = 'hc3'
) 

m3 <- rdrobust(lfs_income$prod[lfs_income$sector == 'Rural'], 
               lfs_income$dist_border[lfs_income$sector == 'Rural'], 
               covs = cbind(lfs_income$c04[lfs_income$sector == 'Rural'], 
                            lfs_income$c04[lfs_income$sector == 'Rural']^2, 
                            lfs_income$wave_2000[lfs_income$sector == 'Rural'], 
                            lfs_income$female[lfs_income$sector == 'Rural']
                            
               ),
               cluster = lfs_income$vill_code[lfs_income$sector == 'Rural'], 
               vce = 'hc3'
) 


rdtable <- function(rd_model){
  
  
  table_result <- matrix(NA, 5, length(rd_model))
  
  for (j in 1:length(rd_model)) {
    
    table_result[1,j] <- rd_model[[j]]$coef[3]
    table_result[2,j] <- rd_model[[j]]$se[3]
    table_result[3,j] <- rd_model[[j]]$pv[3]
    table_result[4,j] <- rd_model[[j]]$bws[1]
    table_result[5,j] <- rd_model[[j]]$bws[2]
    
    
  }
  
  return(table_result)
  
}

table <- rdtable(rd_model = list(m1, m2, m3))
xtable::xtable(table)


m1_felm <- felm(self ~ treat*dist_border + poly(c04, 2) + female + wave_2000
                |0 | 0 | vill_code,
                weights = 
                  ifelse(abs(lfs_income$dist_border) <= 9.71, 
                         1 - abs(lfs_income$dist_border)/9.71,
                         0),
                data = lfs_income
)
m2_felm <- felm(asinh(income) ~ treat*dist_border + poly(c04, 2) + female + wave_2000
                |0 | 0 | vill_code,
                weights = 
                  ifelse(abs(lfs_income$dist_border) <= 12.76, 
                         1 - abs(lfs_income$dist_border)/12.76,
                         0),
                data = lfs_income
)

m3_felm <- felm(prod ~ treat*dist_border + poly(c04, 2) + female + wave_2000
                |0 | 0 | vill_code,
                weights = 
                  ifelse(abs(lfs_income$dist_border) <= 19.18, 
                         1 - abs(lfs_income$dist_border)/19.18,
                         0),
                data = lfs_income
)

texreg::texreg(list(m1_felm, m2_felm, m3_felm), include.ci = F)



lfs_income_sub <- lfs_income %>% 
  filter(abs(dist_border) <= 19.18) %>% 
  mutate(co04_sq = c04^2)

m1 <- lm(prod ~ treat*dist_border + c04 + co04_sq + female + wave_2000,
         weights = ifelse(abs(lfs_income_sub$dist_border) <= 19.18, 
                          1 - abs(lfs_income_sub$dist_border)/19.18,
                          0),
         data = lfs_income_sub
)


clust1 <- length(unique(lfs_income_sub$vill_code))


vcov1 <- cluster.boot(m1, cluster = lfs_income_sub$vill_code, 
                      boot_type = "wild", wild_type = "rademacher", R = 10000)
sqrt(diag(vcov1))
2*(1 - pt(abs(m1$coefficients[2]/sqrt(diag(vcov1))[2]), df = clust1-1))


#### Income #### 


lfs_income_sub <- lfs_income %>% 
  filter(abs(dist_border) <= 12.76) %>% 
  mutate(co04_sq = c04^2, 
         ihs_income = asinh(income)
  ) 

m1 <- lm(ihs_income ~ treat*dist_border + c04 + co04_sq + female + wave_2000,
         weights = ifelse(abs(lfs_income_sub$dist_border) <= 12.76, 
                          1 - abs(lfs_income_sub$dist_border)/12.76,
                          0),
         data = lfs_income_sub
)


clust1 <- length(unique(lfs_income_sub$vill_code))


vcov1 <- cluster.boot(m1, cluster = lfs_income_sub$vill_code, 
                      boot_type = "wild", wild_type = "rademacher", R = 10000)
sqrt(diag(vcov1))
2*(1 - pt(abs(m1$coefficients[2]/sqrt(diag(vcov1))[2]), df = clust1-1))


#### self #### 


lfs_income_sub <- lfs_income %>% 
  filter(abs(dist_border) <= 9.71) %>% 
  mutate(co04_sq = c04^2
  ) %>% 
  filter(sector == 'Rural')

m1 <- lm(self ~ treat*dist_border + c04 + co04_sq + female + wave_2000,
         weights = ifelse(abs(lfs_income_sub$dist_border) <= 9.71, 
                          1 - abs(lfs_income_sub$dist_border)/9.71,
                          0),
         data = lfs_income_sub
)


clust1 <- length(unique(lfs_income_sub$vill_code))


vcov1 <- cluster.boot(m1, cluster = lfs_income_sub$vill_code, 
                      boot_type = "wild", wild_type = "rademacher", R = 10000)
sqrt(diag(vcov1))
2*(1 - pt(abs(m1$coefficients[2]/sqrt(diag(vcov1))[2]), df = clust1-1))

