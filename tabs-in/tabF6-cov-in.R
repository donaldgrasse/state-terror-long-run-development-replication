census_1998 <- readRDS('data/census_1998_0310.rds')

adjust_set <- cbind(census_1998$dist.segment2, 
                    census_1998$dist.segment3, census_1998$dist.segment4, 
                    log(census_1998$dist_cap+1), census_1998$dist_school)


m1 <- GRD_shac_se(census_1998, run_var = census_1998$dist_border, out = census_1998$yrseducv, 
                  treat = census_1998$treat, 
                  adjust = F, covs = NA, theta_start = 2, 
                  crs = '+proj=utm +zone=48 +datum=WGS84 +units=m +no_defs', 
                  p = 1, lat_long = F, user_bw = F, input_bw = NA)
m2 <- GRD_shac_se(census_1998, run_var = census_1998$dist_border, 
                  out = census_1998$yrseducv, treat = census_1998$treat, 
                  adjust = F, covs = NA, theta_start = 2, 
                  crs = '+proj=utm +zone=48 +datum=WGS84 +units=m +no_defs', 
                  p = 2, lat_long = F, user_bw = F, input_bw = NA)
m3 <- GRD_shac_se(census_1998, run_var = census_1998$dist_border, 
                  out = census_1998$yrseducv, treat = census_1998$treat, 
                  adjust = TRUE, covs = adjust_set, theta_start = 2, 
                  crs = '+proj=utm +zone=48 +datum=WGS84 +units=m +no_defs', 
                  p = 1, lat_long = F, user_bw = F, input_bw = NA)
m4 <- GRD_shac_se(census_1998, run_var = census_1998$dist_border, 
                  out = census_1998$yrseducv, treat = census_1998$treat, 
                  adjust = TRUE, covs = adjust_set, theta_start = 2, 
                  crs = '+proj=utm +zone=48 +datum=WGS84 +units=m +no_defs', 
                  p = 2, lat_long = F, user_bw = F, input_bw = NA)
m5 <- GRD_shac_se(census_1998, run_var = census_1998$dist_border, 
                  out = census_1998$att_rate, treat = census_1998$treat, 
                  adjust = F, covs = NA, theta_start = 2, 
                  crs = '+proj=utm +zone=48 +datum=WGS84 +units=m +no_defs', 
                  p = 1, lat_long = F, user_bw = F, input_bw = NA)
m6 <- GRD_shac_se(census_1998, run_var = census_1998$dist_border, 
                  out = census_1998$att_rate, treat = census_1998$treat, 
                  adjust = F, covs = NA, theta_start = 2, 
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

grd_table = function(model_df){
  model_df = model_df
  row.id2 = row.names(model_df)
  
  seq_end = length(model_df$Estimate)
  seq_end = seq_end*2 
  
  model_df = model_df %>% 
    mutate(name = row.names(.)) %>% 
    select('name', 'Estimate', 'Std.Error') 
  
  model_dfa = model_df %>% 
    select('Estimate') %>% 
    rename(., one = Estimate) %>% 
    mutate(one = round(one,3))
  
  model_dfa = cbind.data.frame(model_dfa, 'row.id' = seq(1, seq_end, 2), 
                              'row.id2' = row.id2)
  
  model_dfb = model_df %>% 
    select('Std.Error') %>% 
    rename(., one = Std.Error) %>% 
    mutate(one = paste0('(', round(one,3), ')'))
  
  end_2 = seq_end/2
  end_2 = end_2+1
  
  model_dfb = cbind.data.frame(model_dfb, 
                               'row.id' = seq(0, seq_end, 2)[2:end_2])
  
  fill_in = as.data.frame(1:seq_end) %>% 
    rename(.,  row.id = `1:seq_end`)
  
  fill_in = merge(fill_in, model_dfa, by = 'row.id', all.x = T)
  fill_in = merge(fill_in, model_dfb, by = 'row.id', all.x = T)
  
  fill_in = fill_in %>% 
    mutate(outcome = ifelse(is.na(one.x)==TRUE, one.y, 
                            one.x)) %>% 
    select('outcome', 'row.id2')
  
  return(fill_in)
  
}

look = grd_table(m1)

m1_test = grd_table(m1) %>% 
  mutate(
    row.id = seq(1,8,1), 
    )

m2_test = grd_table(m2) %>% 
  mutate(
    row.id = seq(1,12,1)
  )


m3_test = grd_table(m3) %>% 
  mutate(
    row.id = seq(1,16,1)
  )

m4_test = grd_table(m4) %>% 
  mutate(
    row.id = seq(1,20,1)
  )

m5_test = grd_table(m5) %>% 
  mutate(
    row.id = seq(1,8,1)
  )

m6_test = grd_table(m6) %>% 
  mutate(
    row.id = seq(1,12,1)
  )

m7_test = grd_table(m7) %>% 
  mutate(
    row.id = seq(1,16,1)
  )

m8_test = grd_table(m8) %>% 
  mutate(
    row.id = seq(1,20,1)
  )

table = full_join(m1_test, m2_test, by = 'row.id') %>% 
  full_join(., m3_test, by = 'row.id') %>% 
  full_join(., m4_test, by = 'row.id') %>% 
  full_join(., m5_test, by = 'row.id') %>% 
  full_join(., m6_test, by = 'row.id') %>% 
  full_join(., m7_test, by = 'row.id') %>% 
  full_join(., m8_test, by = 'row.id') %>% 
  select('outcome.x', 'outcome.y', 'outcome.x.x', 'outcome.y.y', 
         'outcome.x.x.x', 'outcome.y.y.y', 
         'outcome.x.x.x.x', 'outcome.y.y.y.y'
         )

print(xtable(table), include.rownames = F)
