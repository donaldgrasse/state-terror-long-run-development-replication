



df <- readRDS('data/main_0310.rds')

df <- st_as_sf(df)

adjust_set <- cbind(df$dist.segment1, df$dist.segment2, 
                    df$dist.segment3, df$dist.segment4, 
                    log(df$dist_cap+1), df$road_d, df$build_75_sum)

m1 <- GRD_shac_se(df, run_var = df$dist_border, out = df$pov, treat = df$treat, 
                  adjust = TRUE, covs = adjust_set, theta_start = 2, 
                  crs = '+proj=utm +zone=48 +datum=WGS84 +units=m +no_defs', 
                  p = 1, lat_long = F, user_bw = F, input_bw = NA)
m2 <- GRD_shac_se(df, run_var = df$dist_border, out = df$pov, treat = df$treat, 
                  adjust = TRUE, covs = adjust_set, theta_start = 2, 
                  crs = '+proj=utm +zone=48 +datum=WGS84 +units=m +no_defs', 
                  p = 2, lat_long = F, user_bw = F, input_bw = NA)
m3 <- GRD_shac_se(df, run_var = df$dist_border, out = df$ihs_light, treat = df$treat, 
                  adjust = TRUE, covs = adjust_set, theta_start = 2, 
                  crs = '+proj=utm +zone=48 +datum=WGS84 +units=m +no_defs', 
                  p = 1, lat_long = F, user_bw = F, input_bw = NA)
m4 <- GRD_shac_se(df, run_var = df$dist_border, out = df$ihs_light, treat = df$treat, 
                  adjust = TRUE, covs = adjust_set, theta_start = 2, 
                  crs = '+proj=utm +zone=48 +datum=WGS84 +units=m +no_defs', 
                  p = 2, lat_long = F, user_bw = F, input_bw = NA)



make_GRD_table(list(m1, m2, m3, m4), action_name = 'SW', out_names = c('\\%Poverty', 'IHS Luminosity'))
