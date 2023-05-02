school_df <- read_sf(dsn = 'data/school_of_cambodia.shp') %>% 
  filter(PROVINCE == 'Kampong Speu')
census_1998 <- readRDS('data/census_1998_0310.rds')


census_1998$dist_school <- as.numeric(st_nn(census_1998, school_df, returnDist = TRUE)$dist)

adjust_set <- cbind(census_1998$dist.segment2, 
                    census_1998$dist.segment3, census_1998$dist.segment4, 
                    log(census_1998$dist_cap+1), census_1998$dist_school)

m1 <- grdrobust(census_1998, run_var = census_1998$dist_border, out = census_1998$lit_rate, 
                  treat = census_1998$treat, 
                  adjust = TRUE, covs = adjust_set, theta_start = 2, 
                  crs = '+proj=utm +zone=48 +datum=WGS84 +units=m +no_defs', 
                  p = 1, lat_long = F, user_bw = F, input_bw = NA)
m2 <- GRD_shac_se(census_1998, run_var = census_1998$dist_border, 
                  out = census_1998$lit_rate, treat = census_1998$treat, 
                  adjust = TRUE, covs = adjust_set, theta_start = 2, 
                  crs = '+proj=utm +zone=48 +datum=WGS84 +units=m +no_defs', 
                  p = 2, lat_long = F, user_bw = F, input_bw = NA)
m3 <- GRD_shac_se(census_1998, run_var = census_1998$dist_border, 
                  out = census_1998$no_educ, treat = census_1998$treat, 
                  adjust = TRUE, covs = adjust_set, theta_start = 2, 
                  crs = '+proj=utm +zone=48 +datum=WGS84 +units=m +no_defs', 
                  p = 1, lat_long = F, user_bw = F, input_bw = NA)
m4 <- GRD_shac_se(census_1998, run_var = census_1998$dist_border, 
                  out = census_1998$no_educ, treat = census_1998$treat, 
                  adjust = TRUE, covs = adjust_set, theta_start = 2, 
                  crs = '+proj=utm +zone=48 +datum=WGS84 +units=m +no_defs', 
                  p = 2, lat_long = F, user_bw = F, input_bw = NA)
m5 <- GRD_shac_se(census_1998, run_var = census_1998$dist_border, 
                  out = census_1998$yrseducv, treat = census_1998$treat, 
                  adjust = TRUE, covs = adjust_set, theta_start = 2, 
                  crs = '+proj=utm +zone=48 +datum=WGS84 +units=m +no_defs', 
                  p = 1, lat_long = F, user_bw = F, input_bw = NA)
m6 <- GRD_shac_se(census_1998, run_var = census_1998$dist_border, 
                  out = census_1998$yrseducv, treat = census_1998$treat, 
                  adjust = TRUE, covs = adjust_set, theta_start = 2, 
                  crs = '+proj=utm +zone=48 +datum=WGS84 +units=m +no_defs', 
                  p = 2, lat_long = F, user_bw = F, input_bw = NA)
m7 <- GRD_shac_se(census_1998, run_var = census_1998$dist_border, 
                  out = census_1998$att_rate, treat = census_1998$treat, 
                  adjust = TRUE, covs = adjust_set, theta_start = 2, 
                  crs = '+proj=utm +zone=48 +datum=WGS84 +units=m +no_defs', 
                  p = 1, lat_long = F, user_bw = F, input_bw = NA)
m8 <- GRD_shac_se(census_1998, run_var = census_1998$dist_border, 
                  out = census_1998$att_rate, treat = census_1998$treat, 
                  adjust = TRUE, covs = adjust_set, theta_start = 2, 
                  crs = '+proj=utm +zone=48 +datum=WGS84 +units=m +no_defs', 
                  p = 2, lat_long = F, user_bw = F, input_bw = NA)

make_GRD_table(list(m1, m2, m3, m4, m5, m6, m7, m8), action_name = 'SW', out_names = c('\\%Poverty', 'IHS Luminosity'))

beepr::beep(sound = 2)
