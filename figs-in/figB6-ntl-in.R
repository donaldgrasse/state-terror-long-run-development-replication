

df <- readRDS('data/main_0310.rds')

h <- c(5:20)
coef_list <- list()

adjust_set <- cbind(df$dist.segment1, df$dist.segment2, 
                    df$dist.segment3, df$dist.segment4, 
                    log(df$dist_cap+1))
df$ihsntl_4km <- asinh(df$ntl_4km)
df$ihsntl_1km <- asinh(df$ntl_1km)

coef_list <- list()

for(i in 1:length(h)){
  
  coef_list[[i]] <- GRD_shac_se(df, run_var = df$dist_border, out = df$ihsntl_4km, 
                                treat = df$treat, 
                                adjust = T, covs = adjust_set, theta_start = 2, 
                                crs = '+proj=utm +zone=48 +datum=WGS84 +units=m +no_defs', 
                                p = 1, lat_long = F, user_bw = T, input_bw = h[i])
  
  
  
}

est_list <- matrix(NA, nrow = length(h), ncol = 1)
se_list <- matrix(NA, nrow = length(h), ncol = 1)

for(j in 1:length(h)){
  
  
  
  est_list[j] <- coef_list[[j]][2,1] 
  
  se_list[j] <- coef_list[[j]][2,2] 
  
  
}

coef_df <- cbind.data.frame(est_list, se_list)
coef_df$upper <- coef_df$est_list+1.96*coef_df$se_list
coef_df$lower <- coef_df$est_list-1.96*coef_df$se_list
coef_df$bw <- 5:20

ntl_4km <- ggplot(coef_df, aes(bw, est_list)) + 
  geom_point() + 
  geom_line(aes(bw, upper)) + 
  geom_line(aes(bw, lower)) + 
  geom_hline(yintercept = 0, col = 'red', lty = 2) + 
  xlab('Bandwidth (km)') + 
  ylab('Luminosity (4km Buffer)') +
  theme_tufte() + 
  theme(text = element_text(size=20)) 


h <- c(5:20)
coef_list <- list()

adjust_set <- cbind(df$dist.segment1, df$dist.segment2, 
                    df$dist.segment3, df$dist.segment4, 
                    log(df$dist_cap+1))

for(i in 1:length(h)){
  
  coef_list[[i]] <- GRD_shac_se(df, run_var = df$dist_border, out = df$ihsntl_1km, treat = df$treat, 
                                adjust = T, covs = adjust_set, theta_start = 2, crs = '+proj=utm +zone=48 +datum=WGS84 +units=m +no_defs', 
                                p = 1, lat_long = F, user_bw = T, input_bw = h[i])
  
  
  
}

est_list <- matrix(NA, nrow = length(h), ncol = 1)
se_list <- matrix(NA, nrow = length(h), ncol = 1)

for(j in 1:length(h)){
  
  
  
  est_list[j] <- coef_list[[j]][2,1] 
  
  se_list[j] <- coef_list[[j]][2,2] 
  
  
}

coef_df <- cbind.data.frame(est_list, se_list)
coef_df$upper <- coef_df$est_list+1.96*coef_df$se_list
coef_df$lower <- coef_df$est_list-1.96*coef_df$se_list
coef_df$bw <- 5:20

ntl_1km <- ggplot(coef_df, aes(bw, est_list)) + 
  geom_point() + 
  geom_line(aes(bw, upper)) + 
  geom_line(aes(bw, lower)) + 
  geom_hline(yintercept = 0, col = 'red', lty = 2) + 
  xlab('Bandwidth (km)') + 
  ylab('Luminosity (1km Buffer)') +
  theme_tufte() + 
  theme(text = element_text(size=20)) 


ggsave(
  'figs-out/ntl_1km.pdf',
  ntl_1km
  
)
ggsave(
  'figs-out/ntl_4km.pdf',
  ntl_4km
  
)

beepr::beep(
            sound = 
              3
            )