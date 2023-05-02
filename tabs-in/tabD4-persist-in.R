df <- readRDS('data/main_0310.rds')
df$no_educ <- ((df$f_noeduc+df$m_noeduc)/df$totpop)*100
df <- st_as_sf(df)

adjust_set <- cbind(df$dist.segment1, df$dist.segment2, 
                    df$dist.segment3, df$dist.segment4, 
                    log(df$dist_cap+1))

m1 <- grdrobust(df, run_var = df$dist_border, out = df$t_attend, treat = df$treat, 
                  adjust = TRUE, covs = adjust_set, theta_start = 2, 
                  crs = '+proj=utm +zone=48 +datum=WGS84 +units=m +no_defs', 
                  p = 1, lat_long = F, user_bw = F, input_bw = NA)
m2 <- grdrobust(df, run_var = df$dist_border, out = df$yrseduc, treat = df$treat, 
                  adjust = TRUE, covs = adjust_set, theta_start = 2, 
                  crs = '+proj=utm +zone=48 +datum=WGS84 +units=m +no_defs', 
                  p = 1, lat_long = F, user_bw = F, input_bw = NA)
m3 <- grdrobust(df, run_var = df$dist_border, out = df$t_lit15, treat = df$treat, 
                  adjust = TRUE, covs = adjust_set, theta_start = 2, 
                  crs = '+proj=utm +zone=48 +datum=WGS84 +units=m +no_defs', 
                  p = 1, lat_long = F, user_bw = F, input_bw = NA)
m4 <- grdrobust(df, run_var = df$dist_border, out = df$no_educ, treat = df$treat, 
                  adjust = TRUE, covs = adjust_set, theta_start = 2, 
                  crs = '+proj=utm +zone=48 +datum=WGS84 +units=m +no_defs', 
                  p = 1, lat_long = F, user_bw = F, input_bw = NA)

m5 <- grdrobust(df, run_var = df$dist_border, out = df$t_attend, treat = df$treat, 
                  adjust = TRUE, covs = adjust_set, theta_start = 2, 
                  crs = '+proj=utm +zone=48 +datum=WGS84 +units=m +no_defs', 
                  p = 2, lat_long = F, user_bw = F, input_bw = NA)
m6 <- grdrobust(df, run_var = df$dist_border, out = df$yrseduc, treat = df$treat, 
                  adjust = TRUE, covs = adjust_set, theta_start = 2, 
                  crs = '+proj=utm +zone=48 +datum=WGS84 +units=m +no_defs', 
                  p = 2, lat_long = F, user_bw = F, input_bw = NA)
m7 <- grdrobust(df, run_var = df$dist_border, out = df$t_lit15, treat = df$treat, 
                  adjust = TRUE, covs = adjust_set, theta_start = 2, 
                  crs = '+proj=utm +zone=48 +datum=WGS84 +units=m +no_defs', 
                  p = 2, lat_long = F, user_bw = F, input_bw = NA)
m8 <- grdrobust(df, run_var = df$dist_border, out = df$no_educ, treat = df$treat, 
                  adjust = TRUE, covs = adjust_set, theta_start = 2, 
                  crs = '+proj=utm +zone=48 +datum=WGS84 +units=m +no_defs', 
                  p = 2, lat_long = F, user_bw = F, input_bw = NA)

make_GRD_table(list(m1, m5, m2, m6, m3, m7, m4, m8), action_name = 'SW', out_names = c('\\%Poverty', 'IHS Luminosity'))
