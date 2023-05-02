


road_df2 <- readRDS("data/road_df2.rds")

gadm = readRDS('data/gadm36_KHM_1_sf.rds') %>% 
  filter(NAME_1 == 'Kâmpôt')


road = read_sf('data/road-and-railway-network-and-market-density/Road.shp') %>% 
  filter(Name == 'National Road 3')


plac_map = tm_shape(gadm) + tm_polygons() + 
  tm_shape(road) + tm_lines('red') + 
  tm_shape(road_df2) + 
  tm_dots("South", palette = c('red', 'black'), title = "South of Rd", size = .3) 

tmap_save(plac_map, 'figs-out/plac_map.pdf')


