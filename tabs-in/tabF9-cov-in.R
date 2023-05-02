
df <- readRDS('data/main_0310.rds')

adjust_set <- cbind(df$dist.segment1, df$dist.segment2, 
                    df$dist.segment3, df$dist.segment4, 
                    log(df$dist_cap+1))
df$ihsntl_4km <- asinh(df$ntl_4km)
df$ihsntl_1km <- asinh(df$ntl_1km)

m1 = GRD_shac_se(df, run_var = df$dist_border, out = df$ihsntl_4km, 
            treat = df$treat, 
            adjust = T, covs = adjust_set, theta_start = 2, 
            crs = '+proj=utm +zone=48 +datum=WGS84 +units=m +no_defs', 
            p = 1, lat_long = F, user_bw = T, input_bw = 10)


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


grd_table(m1)

m2 = GRD_shac_se(df, run_var = df$dist_border, out = df$ihsntl_1km, 
                 treat = df$treat, 
                 adjust = T, covs = adjust_set, theta_start = 2, 
                 crs = '+proj=utm +zone=48 +datum=WGS84 +units=m +no_defs', 
                 p = 1, lat_long = F, user_bw = T, input_bw = 10)

grd_table(m2)

table_df = cbind.data.frame(grd_table(m1), grd_table(m2)) 
colnames(table_df)[1:2] <- c('4km', '1km')
print(xtable(table_df), include.rownames = F, booktabs = T)
