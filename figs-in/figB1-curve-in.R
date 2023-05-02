
df <- readRDS('data/main_0310.rds')
df$border_point <- as.character(df$dist.segment)
df$border_point[df$border_point==1] <- 3
df$border_point[df$border_point==2] <- 3
df$border_point <- as.numeric(df$border_point)-2


projection = "+proj=utm +zone=48 +datum=WGS84 +units=m +no_defs"

set.seed(30307)
resample_list <- list()
resample_list <- map(seq_len(1000), ~ sample_n(df, size = 1359, replace = T))

att_boot_pov <- list()
for (i in 1:length(resample_list)) {
  
  att_boot_pov[i] <- segment_RD(data = resample_list[[i]], 'pov', 'dist_border', 'border_point', 10, att_only = TRUE)
  
}

sd(unlist(att_boot_pov))
att_pov <- segment_RD(data = df, 'pov', 'dist_border', 'border_point', 10, att_only = TRUE)

att_boot_ntl <- list()
for (i in 1:length(resample_list)) {
  
  att_boot_ntl[i] <- segment_RD(data = resample_list[[i]], 'ihs_light', 'dist_border', 'border_point', 10, att_only = TRUE)
  
}

att_ntl <- segment_RD(data = df, 'ihs_light', 'dist_border', 'border_point', 10, att_only = FALSE)

zone <- read_sf(dsn = 'data/Democratic_Kampuchea_Zones (1)/Democratic_Kampuchea_Zones.shp') %>% 
  st_transform(st_crs("+proj=utm +zone=48 +datum=WGS84 +units=m +no_defs"))

sw <- subset(zone, ZONE_NAME=="Southwest")
w <- subset(zone, ZONE_NAME=="West")

x_and_y <- st_intersection(sw$geometry, w$geometry)
x_and_y <- x_and_y %>% 
  st_transform(st_crs("+proj=utm +zone=48 +datum=WGS84 +units=m +no_defs"))
line_as_point <- st_cast(x_and_y, "POINT")
point <- sf:::as_Spatial(line_as_point)
point <- st_as_sf(point)
point <- point %>% 
  st_set_crs(st_crs("+proj=utm +zone=48 +datum=WGS84 +units=m +no_defs"))

prov <- readRDS('data/gadm36_KHM_1_sf.rds')%>% 
  st_transform(st_crs("+proj=utm +zone=48 +datum=WGS84 +units=m +no_defs")) %>% 
  filter(NAME_1 == "Kâmpóng Spœ")

point_in_prov <- st_join(point, prov, join = st_within)
boundary_point <- point_in_prov %>% 
  filter(NAME_1 == "Kâmpóng Spœ") %>% 
  distinct()

boundary_point <- boundary_point[3:5,]

att_ntl <- cbind.data.frame(att_ntl, boundary_point$geometry)
att_ntl <- st_as_sf(att_ntl)
sd(unlist(att_boot_ntl))



att_pov <- segment_RD(data = df, 'pov', 'dist_border', 'border_point', 10, att_only = FALSE)
att_pov <- cbind.data.frame(att_pov, boundary_point$geometry)
att_pov <- st_as_sf(att_pov)

df2 <- df %>% 
  filter(abs(dist_border) <= 10)

#### Line 3### 
zone <- read_sf(dsn = 'data/Democratic_Kampuchea_Zones (1)/Democratic_Kampuchea_Zones.shp')
zone <- zone %>% 
  st_transform(st_crs(projection))

sw <- subset(zone, ZONE_NAME=="Southwest")
w <- subset(zone, ZONE_NAME=="West")

x_and_y <- st_intersection(sw$geometry, w$geometry)
x_and_y <- x_and_y %>% 
  st_transform(st_crs(projection))

line_as_point <- st_cast(x_and_y, "MULTIPOINT")
line_as_line <- st_cast(line_as_point, "MULTILINESTRING")

line_as_line <- line_as_line %>% 
  st_set_crs(st_crs(projection))
line_as_line <- st_cast(line_as_point, "LINESTRING")


att_pov$estrd <- round(att_pov$est, 2)

poverty_treat_map <- tm_shape(df2) + tm_dots('pov', palette = 'BuPu', size = .5, title = 'Poverty') + 
  tm_shape(att_pov) + tm_text('estrd', ymod = -1.2, bg.color = 'white') + 
  tm_shape(line_as_line) + tm_lines('red') + 
  tm_credits("LATE: 5.67*", size = 1.5, position = c(.1, .5), bg.color = 'white') + 
  tm_credits("(1.54)", size = 1.5, position = c(.1, .5), bg.color = 'white') 

att_ntl$estrd <- round(att_ntl$est, 2)

light_treat_map <- tm_shape(df2) + tm_dots('ihs_light', palette = 'BuPu', size = .5, title = 'ihs(Nighttime Lights)') + 
  tm_shape(att_ntl) + tm_text('estrd', ymod = -1.2, bg.color = 'white') + 
  tm_shape(line_as_line) + tm_lines('red') + 
  tm_credits("LATE: -0.62*", size = 1.5, position = c(.1, .5), bg.color = 'white') + 
  tm_credits("(0.18)", size = 1.5, position = c(.1, .5), bg.color = 'white') 

tmap_save(light_treat_map, 'figs-out/light_treat_map.pdf')
tmap_save(poverty_treat_map, 'figs-out/poverty_treat_map.pdf')



