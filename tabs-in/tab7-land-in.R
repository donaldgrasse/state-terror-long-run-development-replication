
df <- readRDS("data/main_0310.rds") %>% 
  mutate(
        ihs_land = asinh(land_dispute), 
        land_bin = as.numeric(land_dispute>0, 1,0)
         
         ) %>% 
  filter(is.na(land_dispute) == F)

adjust_set <- cbind(df$dist.segment1, df$dist.segment2, 
                    df$dist.segment3, df$dist.segment4, 
                    log(df$dist_cap+1))

m1 <- GRD_shac_se(df, run_var = df$dist_border, out = df$land_dispute, treat = df$treat, 
                  adjust = T, covs = adjust_set, theta_start = 2, 
                  crs = '+proj=utm +zone=48 +datum=WGS84 +units=m +no_defs', 
                  p = 1, lat_long = F, user_bw = F, input_bw = NA)
m2 <- GRD_shac_se(df, run_var = df$dist_border, out = df$ihs_land, treat = df$treat, 
                  adjust = T, covs = adjust_set, theta_start = 2, 
                  crs = '+proj=utm +zone=48 +datum=WGS84 +units=m +no_defs', 
                  p = 1, lat_long = F, user_bw = F, input_bw = NA)
m3 <- GRD_shac_se(df, run_var = df$dist_border, out = df$land_bin, treat = df$treat, 
                  adjust = T, covs = adjust_set, theta_start = 2, 
                  crs = '+proj=utm +zone=48 +datum=WGS84 +units=m +no_defs', 
                  p = 1, lat_long = F, user_bw = F, input_bw = NA)

make_GRD_table(list(m1, m2, m3), action_name = 'SW', out_names = c('\\%Poverty', 'IHS Luminosity'))

