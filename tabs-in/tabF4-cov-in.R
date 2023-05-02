


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

length(m1$Estimate)

grd_table = function(model_df){
  model_df = model_df
  
  seq_end = length(model_df$Estimate)
  seq_end = seq_end*2 
  
  model_df = model_df %>% 
    mutate(name = row.names(.)) %>% 
    select('name', 'Estimate', 'Std.Error') 
  
  model_dfa = model_df %>% 
    select('Estimate') %>% 
    rename(., one = Estimate) %>% 
    mutate(one = round(one,3))
  
  model_dfa = cbind.data.frame(model_dfa, 'row.id' = seq(1, seq_end, 2))
  
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
    select('outcome')
  
  return(fill_in)
  
}

m1_test = grd_table(m1) %>% 
  mutate(
    row.id = seq(1,22,1)
  )

m2_test = grd_table(m2) %>% 
  mutate(
    row.id = seq(1,26,1)
  )


m3_test = grd_table(m3) %>% 
  mutate(
    row.id = seq(1,22,1)
  )

m4_test = grd_table(m4) %>% 
  mutate(
    row.id = seq(1,26,1)
  )

table = full_join(m1_test, m2_test, by = 'row.id') %>% 
  full_join(., m3_test, by = 'row.id') %>% 
  full_join(., m4_test, by = 'row.id') %>% 
  select('outcome.x', 'outcome.y', 'outcome.x.x', 'outcome.y.y')

print(xtable(table), include.rownames = F)

