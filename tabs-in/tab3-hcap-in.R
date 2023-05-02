
cen98 <- readRDS("data/census_1998_0310.rds") %>% 
  mutate(dist.border = dist_border/1000
  )

adjust_set <- cbind(cen98$dist.segment2, 
                    cen98$dist.segment3, cen98$dist.segment4, 
                    log(cen98$dist_cap+1))

m1 <- grdrobust(cen98, run_var = cen98$dist.border, out = cen98$no_educ, treat = cen98$treat, 
                  adjust = F, covs = NA, theta_start = 2, crs = '+proj=utm +zone=48 +datum=WGS84 +units=m +no_defs', 
                  p = 1, lat_long = F, user_bw = F, input_bw = NA)
m2 <- grdrobust(cen98, run_var = cen98$dist.border, out = cen98$no_educ, treat = cen98$treat, 
                  adjust = TRUE, covs = adjust_set, theta_start = 4, 
                  crs = '+proj=utm +zone=48 +datum=WGS84 +units=m +no_defs', 
                  p = 1, lat_long = F, user_bw = T, input_bw = 5.862034)
m3 <- grdrobust(cen98, run_var = cen98$dist.border, out = cen98$no_educ, treat = cen98$treat, 
                  adjust = F, covs = NA, theta_start = 2, crs = '+proj=utm +zone=48 +datum=WGS84 +units=m +no_defs', 
                  p = 2, lat_long = F, user_bw = F, input_bw = NA)
m4 <- grdrobust(cen98, run_var = cen98$dist.border, out = cen98$no_educ, treat = cen98$treat, 
                  adjust = T, covs = adjust_set, theta_start = 2, 
                  crs = '+proj=utm +zone=48 +datum=WGS84 +units=m +no_defs', 
                  p = 2, lat_long = F, user_bw = F, input_bw = NA)
m5 <- grdrobust(cen98, run_var = cen98$dist.border, out = cen98$lit_rate, treat = cen98$treat, 
                  adjust = F, covs = NA, theta_start = 2, crs = '+proj=utm +zone=48 +datum=WGS84 +units=m +no_defs', 
                  p = 1, lat_long = F, user_bw = F, input_bw = NA)
m6 <- grdrobust(cen98, run_var = cen98$dist.border, out = cen98$lit_rate, treat = cen98$treat, 
                  adjust = TRUE, covs = adjust_set, theta_start = 2, 
                  crs = '+proj=utm +zone=48 +datum=WGS84 +units=m +no_defs', 
                  p = 1, lat_long = F, user_bw = F, input_bw = NA)
m7 <- grdrobust(cen98, run_var = cen98$dist.border, out = cen98$lit_rate, treat = cen98$treat,  
                  adjust = F, covs = NA, theta_start = 2, crs = '+proj=utm +zone=48 +datum=WGS84 +units=m +no_defs', 
                  p = 2, lat_long = F, user_bw = F, input_bw = NA)
m8 <- grdrobust(cen98, run_var = cen98$dist.border, out = cen98$lit_rate, treat = cen98$treat, 
                  adjust = T, covs = adjust_set, theta_start = 2, 
                  crs = '+proj=utm +zone=48 +datum=WGS84 +units=m +no_defs', 
                  p = 2, lat_long = F, user_bw = F, input_bw = NA)


human_cap_table <- make_GRD_table(list(m1, m2, m3, m4, m5, m6, m7, m8), action_name = 'SW', 
                                  out_names = c('\\% No Educ.', 'Lit. Rate')
                                  
)
human_cap_table
beepr::beep(sound = 1)
