df <- readRDS('data/main_0310.rds')

m1 <- rdrobust(df$pov, df$dist_border, kernel  = 'uni', 
               covs = cbind(df$dist.segment1, df$dist.segment2, 
                            df$dist.segment3, df$dist.segment4)
               
)

power_one <- rdpower::rdpower(data = cbind(df$pov, df$dist_border), kernel = 'uni', tau = m1$coef[3], 
                              covs = cbind(df$dist.segment1, df$dist.segment2, 
                                           df$dist.segment3, df$dist.segment4), plot = F
                              )

m2 <- rdrobust(df$ihs_light, df$dist_border, kernel  = 'uni', 
               covs = cbind(df$dist.segment1, df$dist.segment2, 
                            df$dist.segment3, df$dist.segment4)
               
)

power_two <- rdpower::rdpower(data = cbind(df$ihs_light, df$dist_border), kernel = 'uni', tau = m2$coef[3], 
                              covs = cbind(df$dist.segment1, df$dist.segment2, 
                                           df$dist.segment3, df$dist.segment4), plot = TRUE
)



m1 <- rdrobust(df$pov, df$dist_border, kernel  = 'tri', 
               covs = cbind(df$dist.segment1, df$dist.segment2, 
                            df$dist.segment3, df$dist.segment4)
               
)

power_one <- rdpower::rdpower(data = cbind(df$pov, df$dist_border), kernel = 'tri', tau = m1$coef[3], 
                              covs = cbind(df$dist.segment1, df$dist.segment2, 
                                           df$dist.segment3, df$dist.segment4), plot = F
)

m2 <- rdrobust(df$ihs_light, df$dist_border, kernel  = 'tri', 
               covs = cbind(df$dist.segment1, df$dist.segment2, 
                            df$dist.segment3, df$dist.segment4)
               
)

power_two <- rdpower::rdpower(data = cbind(df$ihs_light, df$dist_border), kernel = 'tri', tau = m2$coef[3], 
                              covs = cbind(df$dist.segment1, df$dist.segment2, 
                                           df$dist.segment3, df$dist.segment4), plot = F
)
