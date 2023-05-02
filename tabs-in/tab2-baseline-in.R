#### Main Results Tables #### 

df <- readRDS("data/main_0310.rds")
df$dist_cap <- df$dist.cap
source("grd-helper-functions.R")

adjust_set <- cbind(df$dist.segment1, df$dist.segment2, 
                    df$dist.segment3, df$dist.segment4, 
                    log(df$dist_cap+1))

m1 <- grdrobust(df, run_var = df$dist_border, out = df$pov, treat = df$treat, 
                  adjust = F, covs = NA, theta_start = 2, 
                  crs = '+proj=utm +zone=48 +datum=WGS84 +units=m +no_defs', 
                  p = 1, lat_long = F, user_bw = F, input_bw = NA)
m2 <- grdrobust(df, run_var = df$dist_border, out = df$pov, treat = df$treat, 
                  adjust = TRUE, covs = adjust_set, theta_start = 2, 
                  crs = '+proj=utm +zone=48 +datum=WGS84 +units=m +no_defs', 
                  p = 1, lat_long = F, user_bw = F, input_bw = NA)
m3 <- grdrobust(df, run_var = df$dist_border, out = df$pov, treat = df$treat, 
                  adjust = F, covs = NA, theta_start = 2, crs = '+proj=utm +zone=48 +datum=WGS84 +units=m +no_defs', 
                  p = 2, lat_long = F, user_bw = F, input_bw = NA)
m4 <- grdrobust(df, run_var = df$dist_border, out = df$pov, treat = df$treat, 
                  adjust = T, covs = adjust_set, theta_start = 2, 
                  crs = '+proj=utm +zone=48 +datum=WGS84 +units=m +no_defs', 
                  p = 2, lat_long = F, user_bw = F, input_bw = NA)
m5 <- grdrobust(df, run_var = df$dist_border, out = df$ihs_light, treat = df$treat, 
                  adjust = F, covs = NA, theta_start = 2, crs = '+proj=utm +zone=48 +datum=WGS84 +units=m +no_defs', 
                  p = 1, lat_long = F, user_bw = F, input_bw = NA)
m6 <- grdrobust(df, run_var = df$dist_border, out = df$ihs_light, treat = df$treat, 
                  adjust = TRUE, covs = adjust_set, theta_start = 2, 
                  crs = '+proj=utm +zone=48 +datum=WGS84 +units=m +no_defs', 
                  p = 1, lat_long = F, user_bw = F, input_bw = NA)
m7 <- grdrobust(df, run_var = df$dist_border, out = df$ihs_light, treat = df$treat, 
                  adjust = F, covs = NA, theta_start = 2, crs = '+proj=utm +zone=48 +datum=WGS84 +units=m +no_defs', 
                  p = 2, lat_long = F, user_bw = F, input_bw = NA)
m8 <- grdrobust(df, run_var = df$dist_border, out = df$ihs_light, treat = df$treat, 
                  adjust = T, covs = adjust_set, theta_start = 2, 
                  crs = '+proj=utm +zone=48 +datum=WGS84 +units=m +no_defs', 
                  p = 2, lat_long = F, user_bw = F, input_bw = NA)


make_GRD_table(list(m1, m2, m3, m4, m5, m6, m7, m8), action_name = 'SW', out_names = c('\\%Poverty', 'IHS Luminosity'))

beepr::beep(sound = 1)

