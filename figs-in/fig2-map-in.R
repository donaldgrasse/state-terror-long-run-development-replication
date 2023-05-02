zone <- read_sf(dsn = 'data/Democratic_Kampuchea_Zones (1)/Democratic_Kampuchea_Zones.shp') %>% 
  st_transform(st_crs("+proj=utm +zone=48 +datum=WGS84 +units=m +no_defs"))

sw <- subset(zone, ZONE_NAME=="Southwest")
w <- subset(zone, ZONE_NAME=="West")

x_and_y <- st_intersection(sw$geometry, w$geometry)
x_and_y <- x_and_y %>% 
  st_transform(st_crs("+proj=utm +zone=48 +datum=WGS84 +units=m +no_defs "))


zone_agg <- zone %>% 
  filter(ZONE_NAME=="Southwest" | ZONE_NAME=="West") %>% 
  group_by() %>% 
  summarise()


#zone_agg_grid <- st_make_grid(zone_agg, cellsize = c(2500, 2500)) %>% 
#  st_sf(grid_id = 1:length(.))

#zone_agg_grid <- st_as_sf(zone_agg_grid)
#grid_cell_df <- st_intersection(zone_agg$geometry, zone_agg_grid$.)
#grid_cell_df_sp <- as_Spatial(grid_cell_df)

#alt <- raster::getData("worldclim",var="alt",res=2.5)
#elev <- raster::extract(alt, grid_cell_df_sp, fun = mean, na.rm = T, method = 'bilineal')
#grid_cell_df <- cbind.data.frame(grid_cell_df, elev)
#grid_cell_df <- na.omit(grid_cell_df)

#beepr::beep(sound = 3)

grid_cell_df = readRDS('data/grid_cell_df_map.rds')

prov <- readRDS('data/gadm36_KHM_1_sf.rds')
prov <- prov %>% 
  st_transform(st_crs("+proj=utm +zone=48 +datum=WGS84 +units=m +no_defs "))
ks_prov <- subset(prov, NAME_1=="Kâmpóng Spœ")


line_as_point <- st_cast(x_and_y, "MULTIPOINT")
line_as_line <- st_cast(line_as_point, "MULTILINESTRING")
river <- read_sf(dsn = 'data/Major_Rivers/Major_Rivers.shp') %>% 
  st_transform(st_crs("+proj=utm +zone=48 +datum=WGS84 +units=m +no_defs "))
river_in_zone <- st_intersection(river, zone_agg)

road <- read_sf(dsn = 'data/Major_Roads/Major_Roads.shp') %>% 
  st_transform(st_crs("+proj=utm +zone=48 +datum=WGS84 +units=m +no_defs "))

road_in_zone <- st_intersection(road, zone_agg)


study_map = tm_shape(st_as_sf(grid_cell_df)) + tm_fill('elev', style = "cont", palette = 'seq', title = 'Elevation (meters)') + 
  #tm_layout(aes.palette = list(seq = 'RdYl')) + 
  tm_shape(ks_prov) + tm_borders('red', lty = 2, lwd = 2) + 
  tm_shape(line_as_line) + tm_lines('black', lwd = 2, lty = 1) + 
  tm_shape(river_in_zone) + tm_lines('blue', lwd = .5, lty = 1) + 
  tm_shape(road_in_zone) + tm_lines('brown', lwd = .5, lty = 5) + 
  tm_credits(text = "Southwest Zone (Mok)", bg.color = 'white') + 
  tm_credits(text = "West Zone (Sy)", bg.color = 'white', position = c('left', 'top')) + 
  tm_add_legend("line", lty = 1, lwd = 2, col = 'black', labels = 'Zone Border (1975-1979)') +
  tm_add_legend("line", lty = 6, lwd = 2, col = 'red', labels = 'Kampong Speu') + 
  tm_add_legend("line", lty = 1, lwd = .5, col = 'blue', labels = 'Major Rivers') + 
  tm_add_legend("line", lty = 5, lwd = .5, col = 'brown', labels = 'Major Roads') + 
  tm_layout(legend.outside = TRUE) + 
  tm_layout(frame = FALSE)

tmap_save(study_map, "figs-out/study_map.pdf")
