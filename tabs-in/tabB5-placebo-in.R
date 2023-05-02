




road_df2 <- readRDS("data/road_df2.rds")

road_df2$ihs_light = asinh(road_df2$light_2km)

m1 <- grdrobust(road_df2, run_var = road_df2$dist.NR3, out = road_df2$ihs_light, treat = road_df2$south, 
                  adjust = F, covs = NA, theta_start = 2, 
                  crs = '+proj=utm +zone=48 +datum=WGS84 +units=m +no_defs', 
                  p = 1, lat_long = F, user_bw = F, input_bw = NA)
m2 <- grdrobust(road_df2, run_var = road_df2$dist.NR3, out = road_df2$pov, treat = road_df2$south, 
                  adjust = F, covs = NA, theta_start = 2, 
                  crs = '+proj=utm +zone=48 +datum=WGS84 +units=m +no_defs', 
                  p = 1, lat_long = F, user_bw = F, input_bw = NA)
road_df2 <- na.omit(road_df2)
m3 <- grdrobust(road_df2, run_var = road_df2$dist.NR3, out = road_df2$no_educ, treat = road_df2$south, 
                  adjust = F, covs = NA, theta_start = 2, 
                  crs = '+proj=utm +zone=48 +datum=WGS84 +units=m +no_defs', 
                  p = 1, lat_long = F, user_bw = F, input_bw = NA)
m4 <- grdrobust(road_df2, run_var = road_df2$dist.NR3, out = road_df2$lit_rate, treat = road_df2$south, 
                  adjust = F, covs = NA, theta_start = 2, 
                  crs = '+proj=utm +zone=48 +datum=WGS84 +units=m +no_defs', 
                  p = 1, lat_long = F, user_bw = F, input_bw = NA)

make_GRD_table(list(m1, m2, m3, m4), action_name = 'SW', out_names = c('\\%Poverty', 'IHS Luminosity'))

