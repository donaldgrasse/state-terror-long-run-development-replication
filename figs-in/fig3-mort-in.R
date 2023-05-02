
projection <- "+proj=longlat +ellps=WGS84 +no_defs"


#### read zone shapefile, aggregate to one polygon representing sw/w zone
zone <- read_sf(dsn = 'data/Democratic_Kampuchea_Zones (1)/Democratic_Kampuchea_Zones.shp') %>% 
  filter(ZONE_NAME == 'Southwest' | ZONE_NAME == 'West') %>% 
  st_transform(st_crs(projection)) %>% 
  summarise(do_union = T)

#### read dhs shapefile, filtering to survey areas within the zones 
dhs_2000_shape <- read_sf(dsn = 'data/DHS/KH_2000_DHS_11252020_190_155960/KHGE43FL/KHGE43FL.shp') %>% 
  st_transform(st_crs(projection)) %>% 
  mutate(within_sw_w = as.numeric(st_distance(., zone$geometry))) %>% 
  filter(within_sw_w == 0) %>% 
  filter(ADM1NAME == 'kampong speu') %>% 
  filter(ADM1NAME != "phnom penh") 

shape_covars <- read.csv('data/DHS/KH_2000_DHS_11252020_190_155960/KHGC42FL/KHGC42FL.csv', header = T)
dhs_2000_shape <- merge(dhs_2000_shape, shape_covars, by = 'DHSID')

#### compute whether survey areas are in the southwest zone 
zone <- read_sf(dsn = 'data/Democratic_Kampuchea_Zones (1)/Democratic_Kampuchea_Zones.shp') %>% 
  st_transform(st_crs(projection))
sw <- subset(zone, ZONE_NAME=="Southwest")
w <- subset(zone, ZONE_NAME=="West")

x_and_y <- st_intersection(sw$geometry, w$geometry)
x_and_y <- x_and_y %>% 
  st_transform(st_crs(projection))


dhs_2000_shape$SW <- as.numeric(as.numeric(st_distance(dhs_2000_shape$geometry, sw$geometry))==0, 1,0)
dhs_2000_shape$dist.border <- ifelse(dhs_2000_shape$SW==1, 
                                     as.numeric(st_distance(dhs_2000_shape$geometry, x_and_y))*1, 
                                     as.numeric(st_distance(dhs_2000_shape$geometry, x_and_y))*-1)

#### read data with sibling mortality questions 

dhs_2000 <- foreign::read.dta('data/DHS/KH_2000_DHS_11252020_190_155960/KHIR42DT/KHIR42FL.dta') 

#### Cleaning Sibling Data #### 

# Dataframe of dead siblings at the sibling level 
sibling_death_df <- dhs_2000[ ,c('caseid', paste0(rep("mm6_", 11), str_pad(seq(1, 11, 1), 2, pad = '0'))
)]

sibling_death_df <- tidyr::gather(sibling_death_df, "unit", "year_died", -c(caseid))
sibling_death_df$year_died[sibling_death_df$year_died==99.00] <- NA
sibling_death_df$year_died[sibling_death_df$year_died==98.00] <- NA
sibling_death_df$year_died <- 2000-sibling_death_df$year_died 
sibling_death_df$unit <- substring(sibling_death_df$unit, 5,6)

# Dataframe of year siblings died at the sibling level 

sibling_year_df <- dhs_2000[ ,c('caseid', paste0(rep("mm7_", 11), str_pad(seq(1, 11, 1), 2, pad = '0')))]
sibling_year_df <- tidyr::gather(sibling_year_df, "unit", "age_death", -c(caseid))
sibling_year_df$unit <- substring(sibling_year_df$unit, 5,6)

# Dataframe of sex of all siblings at the sibling level 

sibling_sex_df <- dhs_2000[ ,c('caseid', paste0(rep("mm1_", 11), str_pad(seq(1, 11, 1), 2, pad = '0')))]
sibling_sex_df <- tidyr::gather(sibling_sex_df, "unit", "sex_sibling", -c(caseid))
sibling_sex_df$unit <- substring(sibling_sex_df$unit, 5,6)

# Dataframe of year of birth of all siblings at the sibling level 

sibling_cmc_df <- dhs_2000[ ,c('caseid', paste0(rep("mm4_", 11), str_pad(seq(1, 11, 1), 2, pad = '0')))]
sibling_cmc_df <- tidyr::gather(sibling_cmc_df, "unit", "CMC_birth", -c(caseid))
sibling_cmc_df$unit <- substring(sibling_cmc_df$unit, 5,6)
sibling_cmc_df$year_birth <- 1900 + round(sibling_cmc_df$CMC_birth/12)


sibling_death_df <- merge(sibling_death_df, sibling_year_df, by = c('unit', 'caseid'))
sibling_death_df <- merge(sibling_death_df, sibling_sex_df, by = c('unit', 'caseid'))
sibling_death_df <- merge(sibling_death_df, sibling_cmc_df, by = c('unit', 'caseid'))


sibling_death_df$unit <- paste0(sibling_death_df$caseid, sibling_death_df$unit)
sibling_death_df <- sibling_death_df %>% 
  group_by(unit) %>% 
  mutate(sibling_exists = as.numeric(is.na(year_birth) == F, 1,0)) %>% 
  filter(sibling_exists == 1)


# Data frame of total siblings at the respondent level 
sibling_total_df <- dhs_2000[ ,c('caseid', paste0(rep("mmidx_", 12), str_pad(seq(1, 12, 1), 2, pad = '0')))]
dummy_fun <- function(x){
  ifelse(x > 0, 1,0)
}
sibling_total_df[ ,c(2:13)] <- lapply(sibling_total_df[,c(2:13)], dummy_fun)
sibling_total_df[is.na(sibling_total_df)] <- 0 
sibling_total_df$total_sibling <- rowSums(sibling_total_df[,c(paste0(rep("mmidx_", 12), str_pad(seq(1, 12, 1), 2, pad = '0')))], 
                                          na.rm = T)
sibling_total_df <- sibling_total_df[ ,c('caseid', 'total_sibling')]


sibling_death_df <- merge(sibling_death_df, sibling_total_df, by = 'caseid')
sibling_death_df$age_death[sibling_death_df$age_death==98] <- NA 

sibling_death_df$died <- as.numeric(sibling_death_df$age_death >= 0, 1,0)
sibling_death_df$died[is.na(sibling_death_df$died)] <- 0 

#sibling_death_df <- subset(sibling_death_df, is.na(year_died)==T | year_died >= 1955)

sibling_death_df <- sibling_death_df %>% 
  filter(year_birth <= 1960) %>% 
  mutate(male_sibling = as.numeric(sex_sibling == 'male',1,0), 
         female_sibling = as.numeric(sex_sibling == 'female',1,0), 
         died_after_15 = as.numeric(age_death >= 15, 1,0), 
         died_before_40 = as.numeric(age_death <= 40, 1,0), 
         male_sibling_died = male_sibling*died*died_after_15*died_before_40, 
         female_sibling_died = female_sibling*died*died_after_15*died_before_40, 
         died1540 = died*died_before_40*died_after_15, 
         died15 = died*died_after_15, 
         died = died) 


dhs_2000$DHSID <- paste0("KH200000000", str_pad(dhs_2000$v001, 3, pad = "0"))
dhs_2000 <- merge(dhs_2000, dhs_2000_shape, by = 'DHSID')
dhs_2000_mer <- dhs_2000[ ,c('caseid', 'DHSID', 'SW', 'dist.border', 'LATNUM', 'LONGNUM', 'v012', 'v025', 'URBAN_RURA', 
                             'v005', 'ADM1NAME')]

sibling_death_df <- merge(sibling_death_df, dhs_2000_mer, by = 'caseid') 
sibling_death_df$wt <- sibling_death_df$v005/1000000


unit_id <- cbind.data.frame(sibling_death_df$unit, sibling_death_df$SW, sibling_death_df$year_died, 
                            sibling_death_df$female_sibling, sibling_death_df$year_birth, 
                            sibling_death_df$URBAN_RURA, sibling_death_df$LATNUM, sibling_death_df$LONGNUM, 
                            sibling_death_df$DHSID, sibling_death_df$wt)
year <- 1961:2000
panel <- merge(unit_id, year)
colnames(panel)[1:11] <- c('unit', 'SW', 'year_died', 'gender', 'birth_year', 'URBAN_RURA',  'LATNUM', 'LONGNUM',
                           'DHSID', 'wt',  'year')

#event_df <- sibling_death_df[,c('unit', 'year_died', 'died')]
#panel <- merge(panel, event_df, c('unit', 'year_died'), all.x = T)
#panel$died[is.na(panel$died)] <- 0 

panel$died_ever <- as.numeric(is.na(panel$year_died) == F, 1,0)
panel$died <- as.numeric(panel$year_died == panel$year, 1,0)
panel$died[panel$died_ever == 1 & panel$year_died<panel$year] <- 0 
panel$died[panel$died_ever==0] <- 0 
panel$died <- ifelse(panel$died_ever == 1, ifelse(panel$year>panel$year_died, NA, panel$died), panel$died)
panel$age <- panel$year-panel$birth_year

panel_na_omit <- panel[ ,c('year', 'died', 'SW', 'gender', 'age', 'birth_year', 'URBAN_RURA', 'LATNUM', 'LONGNUM', 
                           'DHSID', 'wt' )]
panel_na_omit <- na.omit(panel_na_omit)

panel_na_omit$pred_death <- -6.0826089  + -0.5331214*panel_na_omit$gender + 
  0.1323024*panel_na_omit$age +-0.0024045 *panel_na_omit$age^2 + 0.5769297*as.numeric(panel_na_omit$URBAN_RURA == 'R', 1,0)

panel_na_omit$pred_death <- exp(panel_na_omit$pred_death)/(1 + exp(panel_na_omit$pred_death))

panel_na_omit$gap <- panel_na_omit$died - panel_na_omit$pred_death

panel_na_omit$Zone <- panel_na_omit$SW
panel_na_omit$Zone[panel_na_omit$Zone==1] <- 'Southwest'
panel_na_omit$Zone[panel_na_omit$Zone==0] <- 'West'
panel_na_omit$Zone <- as.factor(panel_na_omit$Zone)


mort_plot_ts <- panel_na_omit %>%
  group_by(Zone, year) %>% 
  summarise(pred_death = sum(pred_death, na.rm = T), 
            died = sum(died)) %>% 
  ggplot(aes(year, died-pred_death, lty = Zone, shape = Zone)) + 
  geom_line() +
  geom_point() + 
  xlab("Year") + 
  ylab("Excess Mortality") + 
  annotate("rect", xmin = 1975, xmax = 1979, ymin = -Inf, ymax = Inf,
           alpha = .2) + 
  annotate("rect", xmin = 1975, xmax = 1979, ymin = - Inf, ymax = Inf,
           alpha = .2) + 
  theme(legend.position = "bottom")

ggsave("figs-out/mort_plot_ts.pdf", width = 10)