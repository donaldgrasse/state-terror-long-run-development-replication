
zone <- read_sf(dsn = 'data/Democratic_Kampuchea_Zones (1)/Democratic_Kampuchea_Zones.shp') %>% 
  st_transform(st_crs("+proj=utm +zone=48 +datum=WGS84 +units=m +no_defs"))

sw <- subset(zone, ZONE_NAME=="Southwest")
w <- subset(zone, ZONE_NAME=="West")

x_and_y <- st_intersection(sw$geometry, w$geometry)
x_and_y <- x_and_y %>% 
  st_transform(st_crs("+proj=utm +zone=48 +datum=WGS84 +units=m +no_defs"))

mine_data = sf::read_sf('data/baselinesurveymineerw/khm_blscontaminationp_gov_cmaa.shp') %>% 
  filter(Province == 'KAMPONG SPEU') %>% 
  st_transform(st_crs("+proj=utm +zone=48 +datum=WGS84 +units=m +no_defs"))


mine_data$treat = as.numeric(as.numeric(st_distance(mine_data, sw))==0, 1,0)
mine_data$dist_border = ifelse(mine_data$treat == 1, 
                               st_distance(mine_data, x_and_y), 
                               st_distance(mine_data, x_and_y)*-1 
)
mine_data$fear_factor = NA 
mine_data$fear_factor[mine_data$Fear_Level == "High"] <- 3
mine_data$fear_factor[mine_data$Fear_Level == "Medium"] <- 2
mine_data$fear_factor[mine_data$Fear_Level == "Low"] <- 1
mine_data$high_fear = as.numeric(mine_data$fear_factor == 3, 1,0)

mine_cas = read_sf(dsn = 'data/mine-and-erw-causality-2005-2013/khm_casualtyIncidentp_gov_cmaa_cmvis_2005_2013.shp') %>% 
  filter(Province == 'Kampong Speu')
mine_cas = as.data.frame(mine_cas) %>% 
  group_by(VilCode) %>% 
  summarise(VICTIM = sum(VICTIM, na.rm = T), 
            VICTIM_bin = as.numeric(n()>0,1,0))
mine_cas$vill_code <- str_pad(mine_cas$VilCode, 8, pad = '0')

df <- readRDS('data/main_0310.rds')



df <- merge(df, mine_cas, by = 'vill_code', all.x = T)
df$VICTIM[is.na(df$VICTIM)] <- 0 
df$VICTIM_bin[is.na(df$VICTIM_bin)] <- 0 


mine_data = merge(mine_data, mine_cas, by = 'VilCode', 
                  all.x = T)
mine_data$VICTIM[is.na(mine_data$VICTIM)] <- 0 
mine_data$VICTIM_bin[is.na(mine_data$VICTIM_bin)] <- 0 
mine_data = mine_data %>% 
  distinct(VilCode, .keep_all = TRUE)

m = lm(fear_factor ~ treat*dist_border, data = mine_data)
summary(m)
m = lm(VICTIM ~ treat*dist_border, data = mine_data)
summary(m)
m = lm(VICTIM_bin ~ treat*dist_border, data = mine_data)
summary(m)


summary(rdrobust(mine_data$high_fear, mine_data$dist_border))
summary(rdrobust(mine_data$VICTIM_bin, mine_data$dist_border))
summary(rdrobust(mine_data$VICTIM, mine_data$dist_border))

mine_data$dist_border <- mine_data$dist_border/1000
h <- c(5:20)
coef_list <- list()
u_list <- list()
l_list <- list()

for(i in 1:length(h)){
  coef_list[i] <- rdrobust(mine_data$high_fear, mine_data$dist_border, h=h[i], kernel = 'uni')$coef[3] 
  u_list[i] <- rdrobust(mine_data$high_fear, mine_data$dist_border, h=h[i], kernel = 'uni')$ci[3,2]
  l_list[i] <- rdrobust(mine_data$high_fear, mine_data$dist_border, h=h[i], kernel = 'uni')$ci[3,1]
  
}
coef.df <- cbind.data.frame(gather(as.data.frame(coef_list)), 
                            gather(as.data.frame(u_list)), 
                            gather(as.data.frame(l_list)))
coef.df$key <- NULL
coef.df$key <- NULL
coef.df$key <- NULL
colnames(coef.df)[1:3] <- c('est', 'U', 'L')
coef.df$Bandwidth <- 5:20

p1 <- ggplot(coef.df, aes(x = Bandwidth)) +
  geom_line(aes(y = est), linetype = "dashed") + 
  geom_point(aes(y=est)) + 
  geom_errorbar(aes(ymax = U, ymin = L), width=.1,
                position=position_dodge(.9)) + 
  geom_hline(yintercept = 0, size = .2) + 
  xlab("Alternate Bandwidths (km)") +
  ylab("RD Estimate") +
  ggtitle("1(High Fear of Explosion)") + 
  theme_tufte() 
p1


for(i in 1:length(h)){
  coef_list[i] <- rdrobust(df$VICTIM, df$dist_border, h=h[i], kernel = 'uni')$coef[3] 
  u_list[i] <- rdrobust(df$VICTIM, df$dist_border, h=h[i], kernel = 'uni')$ci[3,2]
  l_list[i] <- rdrobust(df$VICTIM, df$dist_border, h=h[i], kernel = 'uni')$ci[3,1]
  
}
coef.df <- cbind.data.frame(gather(as.data.frame(coef_list)), 
                            gather(as.data.frame(u_list)), 
                            gather(as.data.frame(l_list)))
coef.df$key <- NULL
coef.df$key <- NULL
coef.df$key <- NULL
colnames(coef.df)[1:3] <- c('est', 'U', 'L')
coef.df$Bandwidth <- 5:20

p2 <- ggplot(coef.df, aes(x = Bandwidth)) +
  geom_line(aes(y = est), linetype = "dashed") + 
  geom_point(aes(y=est)) + 
  geom_errorbar(aes(ymax = U, ymin = L), width=.1,
                position=position_dodge(.9)) + 
  geom_hline(yintercept = 0, size = .2) + 
  xlab("Alternate Bandwidths (km)") +
  ylab("RD Estimate") +
  ggtitle("Victims of ERW/Landmines (Count)") + 
  theme_tufte() 
p2


for(i in 1:length(h)){
  coef_list[i] <- rdrobust(df$VICTIM_bin, df$dist_border, h=h[i], kernel = 'uni')$coef[3] 
  u_list[i] <- rdrobust(df$VICTIM_bin, df$dist_border, h=h[i], kernel = 'uni')$ci[3,2]
  l_list[i] <- rdrobust(df$VICTIM_bin, df$dist_border, h=h[i], kernel = 'uni')$ci[3,1]
  
}
coef.df <- cbind.data.frame(gather(as.data.frame(coef_list)), 
                            gather(as.data.frame(u_list)), 
                            gather(as.data.frame(l_list)))
coef.df$key <- NULL
coef.df$key <- NULL
coef.df$key <- NULL
colnames(coef.df)[1:3] <- c('est', 'U', 'L')
coef.df$Bandwidth <- 5:20

p3 <- ggplot(coef.df, aes(x = Bandwidth)) +
  geom_line(aes(y = est), linetype = "dashed") + 
  geom_point(aes(y=est)) + 
  geom_errorbar(aes(ymax = U, ymin = L), width=.1,
                position=position_dodge(.9)) + 
  geom_hline(yintercept = 0, size = .2) + 
  xlab("Alternate Bandwidths (km)") +
  ylab("RD Estimate") +
  ggtitle("Victims of ERW/Landmines (Binary)") + 
  theme_tufte() 
p3
erw_mine_plot = cowplot::plot_grid(p1, p2, p3, lables = 'AUTO')
ggsave("figs-out/erw_mine_plot.pdf")