


df <- foreign::read.dta('data/VAW Cambodia 2015 original full STATA9.dta')
df$prov <- str_pad(df$prov, 2, pad = "0")
df$dist <- str_pad(df$dist, 2, pad = "0")
df$com <- str_pad(df$com, 2, pad = "0")
df$vill <- str_pad(df$vill, 2, pad = "0")
df$vill_code <- paste0(df$prov, df$dist, df$com, df$vill)
data_modern <- readRDS('data/main_0310.rds')
data_modern = data_modern[ ,c('vill_code', 'treat')]

vaw_df <- merge(df, data_modern, by = 'vill_code', all.x = T)
vaw_df <- st_as_sf(vaw_df)
vaw_df <- vaw_df %>%
  st_transform(st_crs("+proj=utm +zone=48 +datum=WGS84 +units=m +no_defs"))

vaw_df$com1 <- as.numeric(vaw_df$s101==1, 1,0)
vaw_df$com2 <- as.numeric(vaw_df$s102==1, 1,0)
vaw_df$com3 <- as.numeric(vaw_df$s105==1, 1,0)
vaw_df <- subset(vaw_df, prov == '05')


vaw_df <- as.data.frame(vaw_df)[ ,c('treat', 'com1', 'com2', 'com3')]

vaw_df %>% 
  group_by(treat) %>% 
  summarise(com1 = mean(com1, na.rm = T), 
            com2 = mean(com2, na.rm = T), 
            com3 = mean(com3, na.rm = T), 
            n_respond = n()
            
  )

vaw_df
