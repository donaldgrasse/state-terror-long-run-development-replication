
dhs_df = readRDS('data/dhs_df.rds')

m1 <- rdrobust(dhs_df$wlth, dhs_df$dist.border, 
               covs = cbind(dhs_df$wave_year05, 
                            dhs_df$wave_year10, dhs_df$wave_year14, 
                            dhs_df$hv105, 
                            dhs_df$hv105^2,
                            dhs_df$female), 
               cluster = dhs_df$DHSCLUST,
               vce = 'hc3'
) 

m2 <- rdrobust(dhs_df$lnwlth, dhs_df$dist.border, 
               covs = cbind(dhs_df$wave_year05, 
                            dhs_df$wave_year10, dhs_df$wave_year14, 
                            dhs_df$hv105, 
                            dhs_df$hv105^2,
                            dhs_df$female), 
               cluster = dhs_df$DHSCLUST,
               vce = 'hc3'
) 


m3 <- rdrobust(dhs_df$wlthqunit, dhs_df$dist.border, 
               covs = cbind(dhs_df$wave_year05, 
                            dhs_df$wave_year10, dhs_df$wave_year14, 
                            dhs_df$hv105, 
                            dhs_df$hv105^2,
                            dhs_df$female), 
               cluster = dhs_df$DHSCLUST,
               vce = 'hc3'
) 

panel_a <- rdtable(list(m1, m2, m3))
xtable(panel_a)
