
projection <- "+proj=utm +zone=48 +datum=WGS84 +units=m +no_defs"

df_check <- readRDS("data/main_0310.rds") %>% 
  filter(abs(dist_border) <= 20) %>% 
  st_transform(st_crs(projection))

prov <- readRDS('data/gadm36_KHM_3_sf.rds') %>% 
  filter(NAME_1 == "Kâmpóng Spœ") %>% 
  st_transform(st_crs(projection))

df_in_prov <- st_join(df_check, prov, st_within)
df_in_prov <- as.data.frame(df_in_prov)[,c('GID_3', 'NAME_3') ]
df_in_prov <- merge(df_in_prov, prov, by = c('GID_3', 'NAME_3'))
prov <- st_as_sf(df_in_prov) %>% 
  summarise(do_Union = T) 

zone <- read_sf(dsn = 'data/Democratic_Kampuchea_Zones (1)/Democratic_Kampuchea_Zones.shp')
zone <- zone %>% 
  st_transform(st_crs(projection))

sw <- subset(zone, ZONE_NAME == "Southwest")
w <- subset(zone, ZONE_NAME == "West")

x_and_y <- st_intersection(sw$geometry, w$geometry)
x_and_y <- x_and_y %>% 
  st_transform(st_crs(projection))

line_as_point <- st_cast(x_and_y, "MULTIPOINT")
line_as_line <- st_cast(line_as_point, "MULTILINESTRING")

line_as_line <- line_as_line %>% 
  st_set_crs(st_crs(projection))
line_as_line <- st_cast(line_as_point, "LINESTRING")

line_as_line = line_as_line %>% 
  st_transform("+proj=longlat +ellps=WGS84 +no_defs")

df_check = df_check %>% 
  st_transform("+proj=longlat +ellps=WGS84 +no_defs")

prov = prov %>% 
  st_transform("+proj=longlat +ellps=WGS84 +no_defs")
sw = sw %>% 
  st_transform("+proj=longlat +ellps=WGS84 +no_defs")

pov_map <- make_diagnostic_map(df_check, prov, line_as_line, 'pov', 
                               treat = 'treat',
                               legend_title = '% Poverty', n_breaks = 7, 
                               plot_labs = c('West', 'Southwest'), lab_position = c(.3, .9, .5, 0), 
                               treat_poly = sw,
                               different_slope = T)

grave <- read_sf(dsn = 'data/prisons_burials_memorials/Burials.shp') %>% 
  filter(ID_PROVINC == 5)

poverty_map <- pov_map + 
  tm_shape(grave) + tm_bubbles('BODIES', shape = 2, col = 'red')

tmap_save(poverty_map, 'figs-out/poverty_map_append.pdf')

