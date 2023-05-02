

readRDS('data/grid1x1_2022.rds') %>% 
  as.data.frame() %>% 
  filter(abs(dist.border) <= 20000) %>% 
  select('treat', 'temp_mean', 'temp_var', 'prec_mean', 'prec_var', 
         'class_12', 'class_30', 'fertile_pct', 
         'rug', 'elev',
         'river_d', 'road_d', 'pop_75', 'build_75_sum'
         
         ) %>% 
  sumtable(., group = 'treat')

