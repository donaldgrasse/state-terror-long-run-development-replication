

df <- readRDS("data/main_0310.rds") %>% 
  mutate( dist_hlth = dist_hlth/1000,
          dist_schl = dist_schl/1000
    
  )

m1 <- grdrobust(df, run_var = df$dist_border, out = df$dist_hlth, treat = df$treat, 
                  adjust = F, covs = NA, theta_start = 2, 
                  crs = '+proj=utm +zone=48 +datum=WGS84 +units=m +no_defs', 
                  p = 1, lat_long = F, user_bw = F, input_bw = NA)
m2 <- grdrobust(df, run_var = df$dist_border, out = df$dist_schl, treat = df$treat, 
                  adjust = F, covs = NA, theta_start = 2, 
                  crs = '+proj=utm +zone=48 +datum=WGS84 +units=m +no_defs', 
                  p = 1, lat_long = F, user_bw = F, input_bw = NA)

df <- readRDS("data/main_0310.rds") %>% 
  filter(is.na(dist_comm_center) == F) 

m3 <- grdrobust(df, run_var = df$dist_border, out = df$dist_comm_center, treat = df$treat, 
                  adjust = F, covs = NA, theta_start = 2, 
                  crs = '+proj=utm +zone=48 +datum=WGS84 +units=m +no_defs', 
                  p = 1, lat_long = F, user_bw = F, input_bw = NA)

make_GRD_table(list(m1, m2, m3), action_name = 'SW', out_names = c('\\%Poverty', 'IHS Luminosity'))
