
sw = read_sf('data/Democratic_Kampuchea_Zones (1)/Democratic_Kampuchea_Zones.shp') %>% 
  filter(ZONE_NAME == 'Southwest')

w = read_sf('data/Democratic_Kampuchea_Zones (1)/Democratic_Kampuchea_Zones.shp') %>% 
  filter(ZONE_NAME == 'West')

nr4 = 
  read_sf(dsn = 'data/road-and-railway-network-and-market-density/Road.shp') %>% 
  filter(Name == 'National Road. 4') %>% 
  st_transform(st_crs(sw))

school_of_cambodia <- read_sf(dsn = 'data/school_of_cambodia.shp') %>% 
  filter(PROVINCE == 'Kampong Speu') %>% 
  mutate(
    staff_student = (Teach_staf/Enrol)*43,
    student_class = Enrol/CLASSES
  ) %>% 
  st_transform(st_crs(sw))

school_of_cambodia$treat <- as.numeric(as.numeric(st_distance(school_of_cambodia$geometry, sw$geometry))==0,1,0)
school_of_cambodia$dist_border <- ifelse(school_of_cambodia$treat ==1, st_distance(school_of_cambodia, nr4)*1, st_distance(school_of_cambodia, nr4)*-1)



m1 <- felm(staff_student ~ treat*poly(dist_border, 1) | 0 | 0 | 0, data = school_of_cambodia)
m2 <- felm(staff_student ~ treat*poly(dist_border, 2) | 0 | 0 | 0, data = school_of_cambodia)
m3 <- felm(student_class ~ treat*poly(dist_border, 1) | 0 | 0 | 0, data = school_of_cambodia)
m4 <- felm(student_class ~ treat*poly(dist_border, 2) | 0 | 0 | 0, data = school_of_cambodia)

texreg(list(m1, m2, m3, m4), include.ci = F)

