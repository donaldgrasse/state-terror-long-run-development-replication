

#### Gender Gap Table #### 
census_1998 <- readRDS("data/census_1998_0310.rds") %>% 
  mutate(dist_border = dist_border/1000
  )

census_1998$dist.segment[census_1998$dist.segment=='1'] <- '2'
census_1998$dist.segment <- as.character(census_1998$dist.segment)
census_1998$dist.segment <- as.factor(census_1998$dist.segment)
census_1998$dist.segment <- relevel(census_1998$dist.segment, ref = '5')

adjust_set <- cbind(census_1998$dist.segment2, census_1998$dist.segment3, 
                    census_1998$dist.segment4, 
                    log(census_1998$dist_cap+1))

m9 <- grdrobust(census_1998, run_var = census_1998$dist_border, 
                  out = census_1998$gender_gap_lit, treat = census_1998$treat, 
                  adjust = F, covs = NA, theta_start = 2, crs = '+proj=utm +zone=48 +datum=WGS84 +units=m +no_defs', 
                  p = 1, lat_long = F, user_bw = F, input_bw = NA)
m10 <- grdrobust(census_1998, run_var = census_1998$dist_border, 
                   out = census_1998$gender_gap_lit, treat = census_1998$treat, 
                   adjust = TRUE, covs = adjust_set, theta_start = 2, 
                   crs = '+proj=utm +zone=48 +datum=WGS84 +units=m +no_defs', 
                   p = 1, lat_long = F, user_bw = F, input_bw = NA)
m11 <- grdrobust(census_1998, run_var = census_1998$dist_border, 
                   out = census_1998$gender_gap_lit, treat = census_1998$treat, 
                   adjust = F, covs = NA, theta_start = 2, crs = '+proj=utm +zone=48 +datum=WGS84 +units=m +no_defs', 
                   p = 2, lat_long = F, user_bw = F, input_bw = NA)
m12 <- grdrobust(census_1998, run_var = census_1998$dist_border, 
                   out = census_1998$gender_gap_lit, treat = census_1998$treat, 
                   adjust = T, covs = adjust_set, theta_start = 2, 
                   crs = '+proj=utm +zone=48 +datum=WGS84 +units=m +no_defs', 
                   p = 2, lat_long = F, user_bw = F, input_bw = NA)
m13 <- grdrobust(census_1998, run_var = census_1998$dist_border, 
                   out = census_1998$gender_gap_never, treat = census_1998$treat, 
                   adjust = F, covs = NA, theta_start = 2, crs = '+proj=utm +zone=48 +datum=WGS84 +units=m +no_defs', 
                   p = 1, lat_long = F, user_bw = F, input_bw = NA)
m14 <- grdrobust(census_1998, run_var = census_1998$dist_border, 
                   out = census_1998$gender_gap_never, treat = census_1998$treat, 
                   adjust = TRUE, covs = adjust_set, theta_start = 2, 
                   crs = '+proj=utm +zone=48 +datum=WGS84 +units=m +no_defs', 
                   p = 1, lat_long = F, user_bw = F, input_bw = NA)
m15 <- grdrobust(census_1998, run_var = census_1998$dist_border, 
                   out = census_1998$gender_gap_never, treat = census_1998$treat, 
                   adjust = F, covs = NA, theta_start = 2, crs = '+proj=utm +zone=48 +datum=WGS84 +units=m +no_defs', 
                   p = 2, lat_long = F, user_bw = F, input_bw = NA)
m16 <- grdrobust(census_1998, run_var = census_1998$dist_border, 
                   out = census_1998$gender_gap_never, treat = census_1998$treat, 
                   adjust = T, covs = adjust_set, theta_start = 2, 
                   crs = '+proj=utm +zone=48 +datum=WGS84 +units=m +no_defs', 
                   p = 2, lat_long = F, user_bw = F, input_bw = NA)

make_GRD_table(list(m9, m10, m11, m12, m13, m14, m15, m16), action_name = 'SW', out_names = c('\\%No Educ. Gender Gap', 'Lit. Rate Gender Gap'))


m9 <- grdrobust(census_1998, run_var = census_1998$dist_border, 
                  out = census_1998$f_no_educ, treat = census_1998$treat, 
                  adjust = F, covs = NA, theta_start = 2, crs = '+proj=utm +zone=48 +datum=WGS84 +units=m +no_defs', 
                  p = 1, lat_long = F, user_bw = F, input_bw = NA)
m10 <- grdrobust(census_1998, run_var = census_1998$dist_border, 
                   out = census_1998$f_no_educ, treat = census_1998$treat, 
                   adjust = TRUE, covs = adjust_set, theta_start = 2, 
                   crs = '+proj=utm +zone=48 +datum=WGS84 +units=m +no_defs', 
                   p = 1, lat_long = F, user_bw = F, input_bw = NA)
m11 <- grdrobust(census_1998, run_var = census_1998$dist_border, 
                   out = census_1998$f_no_educ, treat = census_1998$treat, 
                   adjust = F, covs = NA, theta_start = 2, crs = '+proj=utm +zone=48 +datum=WGS84 +units=m +no_defs', 
                   p = 2, lat_long = F, user_bw = F, input_bw = NA)
m12 <- grdrobust(census_1998, run_var = census_1998$dist_border, 
                   out = census_1998$f_no_educ, treat = census_1998$treat, 
                   adjust = T, covs = adjust_set, theta_start = 2, 
                   crs = '+proj=utm +zone=48 +datum=WGS84 +units=m +no_defs', 
                   p = 2, lat_long = F, user_bw = F, input_bw = NA)
m13 <- grdrobust(census_1998, run_var = census_1998$dist_border, 
                   out = census_1998$f_lit_rate, treat = census_1998$treat, 
                   adjust = F, covs = NA, theta_start = 2, crs = '+proj=utm +zone=48 +datum=WGS84 +units=m +no_defs', 
                   p = 1, lat_long = F, user_bw = F, input_bw = NA)
m14 <- grdrobust(census_1998, run_var = census_1998$dist_border, 
                   out = census_1998$f_lit_rate, treat = census_1998$treat, 
                   adjust = TRUE, covs = adjust_set, theta_start = 2, 
                   crs = '+proj=utm +zone=48 +datum=WGS84 +units=m +no_defs', 
                   p = 1, lat_long = F, user_bw = F, input_bw = NA)
m15 <- grdrobust(census_1998, run_var = census_1998$dist_border, 
                   out = census_1998$f_lit_rate, treat = census_1998$treat, 
                   adjust = F, covs = NA, theta_start = 2, crs = '+proj=utm +zone=48 +datum=WGS84 +units=m +no_defs', 
                   p = 2, lat_long = F, user_bw = F, input_bw = NA)
m16 <- grdrobust(census_1998, run_var = census_1998$dist_border, 
                   out = census_1998$f_lit_rate, treat = census_1998$treat, 
                   adjust = T, covs = adjust_set, theta_start = 2, 
                   crs = '+proj=utm +zone=48 +datum=WGS84 +units=m +no_defs', 
                   p = 2, lat_long = F, user_bw = F, input_bw = NA)

make_GRD_table(list(m9, m10, m11, m12, m13, m14, m15, m16), action_name = 'SW', out_names = c('\\%No Educ. Females', 'Lit. Rate Females'))


m9 <- grdrobust(census_1998, run_var = census_1998$dist_border, 
                  out = census_1998$m_no_educ, treat = census_1998$treat, 
                  adjust = F, covs = NA, theta_start = 2, crs = '+proj=utm +zone=48 +datum=WGS84 +units=m +no_defs', 
                  p = 1, lat_long = F, user_bw = F, input_bw = NA)
m10 <- grdrobust(census_1998, run_var = census_1998$dist_border, 
                   out = census_1998$m_no_educ, treat = census_1998$treat, 
                   adjust = TRUE, covs = adjust_set, theta_start = 2, 
                   crs = '+proj=utm +zone=48 +datum=WGS84 +units=m +no_defs', 
                   p = 1, lat_long = F, user_bw = F, input_bw = NA)
m11 <- grdrobust(census_1998, run_var = census_1998$dist_border, 
                   out = census_1998$m_no_educ, treat = census_1998$treat, 
                   adjust = F, covs = NA, theta_start = 2, crs = '+proj=utm +zone=48 +datum=WGS84 +units=m +no_defs', 
                   p = 2, lat_long = F, user_bw = F, input_bw = NA)
m12 <- grdrobust(census_1998, run_var = census_1998$dist_border, 
                   out = census_1998$m_no_educ, treat = census_1998$treat, 
                   adjust = T, covs = adjust_set, theta_start = 2, 
                   crs = '+proj=utm +zone=48 +datum=WGS84 +units=m +no_defs', 
                   p = 2, lat_long = F, user_bw = F, input_bw = NA)
m13 <- grdrobust(census_1998, run_var = census_1998$dist_border, 
                   out = census_1998$m_lit_rate, treat = census_1998$treat, 
                   adjust = F, covs = NA, theta_start = 2, crs = '+proj=utm +zone=48 +datum=WGS84 +units=m +no_defs', 
                   p = 1, lat_long = F, user_bw = F, input_bw = NA)
m14 <- grdrobust(census_1998, run_var = census_1998$dist_border, 
                   out = census_1998$m_lit_rate, treat = census_1998$treat, 
                   adjust = TRUE, covs = adjust_set, theta_start = 2, 
                   crs = '+proj=utm +zone=48 +datum=WGS84 +units=m +no_defs', 
                   p = 1, lat_long = F, user_bw = F, input_bw = NA)
m15 <- grdrobust(census_1998, run_var = census_1998$dist_border, 
                   out = census_1998$m_lit_rate, treat = census_1998$treat, 
                   adjust = F, covs = NA, theta_start = 2, crs = '+proj=utm +zone=48 +datum=WGS84 +units=m +no_defs', 
                   p = 2, lat_long = F, user_bw = F, input_bw = NA)
m16 <- grdrobust(census_1998, run_var = census_1998$dist_border, 
                   out = census_1998$m_lit_rate, treat = census_1998$treat, 
                   adjust = T, covs = adjust_set, theta_start = 2, 
                   crs = '+proj=utm +zone=48 +datum=WGS84 +units=m +no_defs', 
                   p = 2, lat_long = F, user_bw = F, input_bw = NA)

make_GRD_table(list(m9, m10, m11, m12, m13, m14, m15, m16), action_name = 'SW', out_names = c('\\%No Educ. Males', 'Lit. Rate Males'))
