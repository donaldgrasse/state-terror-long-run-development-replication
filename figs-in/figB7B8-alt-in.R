


lapply(c('rdrobust', 'ggplot2', 'ggthemes'), require, character.only = T)


df <- readRDS("data/main_0310.rds")
cen98 <- readRDS("data/census_1998_0310.rds") %>% 
  mutate(dist_border = dist_border/1000)


h <- c(5:40)
coef_list <- list()
u_list <- list()
l_list <- list()

for(i in 1:length(h)){
  coef_list[i] <- rdrobust(df$pov, df$dist_border, h=h[i])$coef[3] 
  u_list[i] <- rdrobust(df$pov, df$dist_border, h=h[i])$ci[3,2]
  l_list[i] <- rdrobust(df$pov, df$dist_border, h=h[i])$ci[3,1]
  
}
coef.df <- cbind.data.frame(gather(as.data.frame(coef_list)), 
                            gather(as.data.frame(u_list)), 
                            gather(as.data.frame(l_list)))
coef.df$key <- NULL
coef.df$key <- NULL
coef.df$key <- NULL
colnames(coef.df)[1:3] <- c('est', 'U', 'L')
coef.df$Bandwidth <- 5:40

p1 <- ggplot(coef.df, aes(x = Bandwidth)) +
  geom_line(aes(y = est), linetype = "dashed") + 
  geom_point(aes(y=est)) + 
  geom_errorbar(aes(ymax = U, ymin = L), width=.1,
                position=position_dodge(.9)) + 
  geom_hline(yintercept = 0, size = .2) + 
  xlab("Alternate Bandwidths (km)") +
  ylab("RD Estimate") +
  ggtitle("Poverty Rate: 2011") + 
  theme_tufte() 
p1


h <- c(5:40)
coef_list <- list()
u_list <- list()
l_list <- list()

for(i in 1:length(h)){
  coef_list[i] <- rdrobust(df$ihs_light, df$dist_border, h=h[i])$coef[3] 
  u_list[i] <- rdrobust(df$ihs_light, df$dist_border, h=h[i])$ci[3,2]
  l_list[i] <- rdrobust(df$ihs_light, df$dist_border, h=h[i])$ci[3,1]
  
}
coef.df <- cbind.data.frame(gather(as.data.frame(coef_list)), 
                            gather(as.data.frame(u_list)), 
                            gather(as.data.frame(l_list)))
coef.df$key <- NULL
coef.df$key <- NULL
coef.df$key <- NULL
colnames(coef.df)[1:3] <- c('est', 'U', 'L')
coef.df$Bandwidth <- 5:40

p2 <- ggplot(coef.df, aes(x = Bandwidth)) +
  geom_line(aes(y = est), linetype = "dashed") + 
  geom_point(aes(y=est)) + 
  geom_errorbar(aes(ymax = U, ymin = L), width=.1,
                position=position_dodge(.9)) + 
  geom_hline(yintercept = 0, size = .2) + 
  xlab("Alternate Bandwidths (km)") +
  ylab("RD Estimate") +
  ggtitle("Nighttime Lights") + 
  theme_tufte() 
p2


h <- c(5:40)
coef_list <- list()
u_list <- list()
l_list <- list()

for(i in 1:length(h)){
  coef_list[i] <- rdrobust(cen98$lit_rate, cen98$dist_border, h=h[i])$coef[3] 
  u_list[i] <- rdrobust(cen98$lit_rate, cen98$dist_border, h=h[i])$ci[3,2]
  l_list[i] <- rdrobust(cen98$lit_rate, cen98$dist_border, h=h[i])$ci[3,1]
  
}
coef.df <- cbind.data.frame(gather(as.data.frame(coef_list)), 
                            gather(as.data.frame(u_list)), 
                            gather(as.data.frame(l_list)))
coef.df$key <- NULL
coef.df$key <- NULL
coef.df$key <- NULL
colnames(coef.df)[1:3] <- c('est', 'U', 'L')
coef.df$Bandwidth <- 5:40

p3 <- ggplot(coef.df, aes(x = Bandwidth)) +
  geom_line(aes(y = est), linetype = "dashed") + 
  geom_point(aes(y=est)) + 
  geom_errorbar(aes(ymax = U, ymin = L), width=.1,
                position=position_dodge(.9)) + 
  geom_hline(yintercept = 0, size = .2) + 
  xlab("Alternate Bandwidths (km)") +
  ylab("RD Estimate") +
  ggtitle("Literacy Rate 1998") + 
  theme_tufte() 
p3


h <- c(5:40)
coef_list <- list()
u_list <- list()
l_list <- list()

for(i in 1:length(h)){
  coef_list[i] <- rdrobust(cen98$no_educ, cen98$dist_border, h=h[i])$coef[3] 
  u_list[i] <- rdrobust(cen98$no_educ, cen98$dist_border, h=h[i])$ci[3,2]
  l_list[i] <- rdrobust(cen98$no_educ, cen98$dist_border, h=h[i])$ci[3,1]
  
}
coef.df <- cbind.data.frame(gather(as.data.frame(coef_list)), 
                            gather(as.data.frame(u_list)), 
                            gather(as.data.frame(l_list)))
coef.df$key <- NULL
coef.df$key <- NULL
coef.df$key <- NULL
colnames(coef.df)[1:3] <- c('est', 'U', 'L')
coef.df$Bandwidth <- 5:40

p4 <- ggplot(coef.df, aes(x = Bandwidth)) +
  geom_line(aes(y = est), linetype = "dashed") + 
  geom_point(aes(y=est)) + 
  geom_errorbar(aes(ymax = U, ymin = L), width=.1,
                position=position_dodge(.9)) + 
  geom_hline(yintercept = 0, size = .2) + 
  xlab("Alternate Bandwidths (km)") +
  ylab("RD Estimate") +
  ggtitle("% No Education 1998") + 
  theme_tufte() 
p4


p5 <- cowplot::plot_grid(p1, p2, p4, p3)

ggsave('figs-out/alt_bw_tri.pdf', p5, width = 12, height = 5)



#### Uniform Kernel #### 


h <- c(5:40)
coef_list <- list()
u_list <- list()
l_list <- list()

for(i in 1:length(h)){
  coef_list[i] <- rdrobust(df$pov, df$dist_border, h=h[i], kernel = 'uni')$coef[3] 
  u_list[i] <- rdrobust(df$pov, df$dist_border, h=h[i], kernel = 'uni')$ci[3,2]
  l_list[i] <- rdrobust(df$pov, df$dist_border, h=h[i], kernel = 'uni')$ci[3,1]
  
}
coef.df <- cbind.data.frame(gather(as.data.frame(coef_list)), 
                            gather(as.data.frame(u_list)), 
                            gather(as.data.frame(l_list)))
coef.df$key <- NULL
coef.df$key <- NULL
coef.df$key <- NULL
colnames(coef.df)[1:3] <- c('est', 'U', 'L')
coef.df$Bandwidth <- 5:40

p1 <- ggplot(coef.df, aes(x = Bandwidth)) +
  geom_line(aes(y = est), linetype = "dashed") + 
  geom_point(aes(y=est)) + 
  geom_errorbar(aes(ymax = U, ymin = L), width=.1,
                position=position_dodge(.9)) + 
  geom_hline(yintercept = 0, size = .2) + 
  xlab("Alternate Bandwidths (km)") +
  ylab("RD Estimate") +
  ggtitle("Poverty Rate: 2011") + 
  theme_tufte() 
p1


h <- c(5:40)
coef_list <- list()
u_list <- list()
l_list <- list()

for(i in 1:length(h)){
  coef_list[i] <- rdrobust(df$ihs_light, df$dist_border, h=h[i], kernel = 'uni')$coef[3] 
  u_list[i] <- rdrobust(df$ihs_light, df$dist_border, h=h[i], kernel = 'uni')$ci[3,2]
  l_list[i] <- rdrobust(df$ihs_light, df$dist_border, h=h[i], kernel = 'uni')$ci[3,1]
  
}
coef.df <- cbind.data.frame(gather(as.data.frame(coef_list)), 
                            gather(as.data.frame(u_list)), 
                            gather(as.data.frame(l_list)))
coef.df$key <- NULL
coef.df$key <- NULL
coef.df$key <- NULL
colnames(coef.df)[1:3] <- c('est', 'U', 'L')
coef.df$Bandwidth <- 5:40

p2 <- ggplot(coef.df, aes(x = Bandwidth)) +
  geom_line(aes(y = est), linetype = "dashed") + 
  geom_point(aes(y=est)) + 
  geom_errorbar(aes(ymax = U, ymin = L), width=.1,
                position=position_dodge(.9)) + 
  geom_hline(yintercept = 0, size = .2) + 
  xlab("Alternate Bandwidths (km)") +
  ylab("RD Estimate") +
  ggtitle("Nighttime Lights") + 
  theme_tufte() 
p2


h <- c(5:40)
coef_list <- list()
u_list <- list()
l_list <- list()

for(i in 1:length(h)){
  coef_list[i] <- rdrobust(cen98$lit_rate, cen98$dist_border, h=h[i], kernel = 'uni')$coef[3] 
  u_list[i] <- rdrobust(cen98$lit_rate, cen98$dist_border, h=h[i], kernel = 'uni')$ci[3,2]
  l_list[i] <- rdrobust(cen98$lit_rate, cen98$dist_border, h=h[i], kernel = 'uni')$ci[3,1]
  
}
coef.df <- cbind.data.frame(gather(as.data.frame(coef_list)), 
                            gather(as.data.frame(u_list)), 
                            gather(as.data.frame(l_list)))
coef.df$key <- NULL
coef.df$key <- NULL
coef.df$key <- NULL
colnames(coef.df)[1:3] <- c('est', 'U', 'L')
coef.df$Bandwidth <- 5:40

p3 <- ggplot(coef.df, aes(x = Bandwidth)) +
  geom_line(aes(y = est), linetype = "dashed") + 
  geom_point(aes(y=est)) + 
  geom_errorbar(aes(ymax = U, ymin = L), width=.1,
                position=position_dodge(.9)) + 
  geom_hline(yintercept = 0, size = .2) + 
  xlab("Alternate Bandwidths (km)") +
  ylab("RD Estimate") +
  ggtitle("Literacy Rate 1998") + 
  theme_tufte() 
p3


h <- c(5:40)
coef_list <- list()
u_list <- list()
l_list <- list()

for(i in 1:length(h)){
  coef_list[i] <- rdrobust(cen98$no_educ, cen98$dist_border, h=h[i], kernel = 'uni')$coef[3] 
  u_list[i] <- rdrobust(cen98$no_educ, cen98$dist_border, h=h[i], kernel = 'uni')$ci[3,2]
  l_list[i] <- rdrobust(cen98$no_educ, cen98$dist_border, h=h[i], kernel = 'uni')$ci[3,1]
  
}
coef.df <- cbind.data.frame(gather(as.data.frame(coef_list)), 
                            gather(as.data.frame(u_list)), 
                            gather(as.data.frame(l_list)))
coef.df$key <- NULL
coef.df$key <- NULL
coef.df$key <- NULL
colnames(coef.df)[1:3] <- c('est', 'U', 'L')
coef.df$Bandwidth <- 5:40

p4 <- ggplot(coef.df, aes(x = Bandwidth)) +
  geom_line(aes(y = est), linetype = "dashed") + 
  geom_point(aes(y=est)) + 
  geom_errorbar(aes(ymax = U, ymin = L), width=.1,
                position=position_dodge(.9)) + 
  geom_hline(yintercept = 0, size = .2) + 
  xlab("Alternate Bandwidths (km)") +
  ylab("RD Estimate") +
  ggtitle("% No Education 1998") + 
  theme_tufte() 
p4


p5 <- cowplot::plot_grid(p1, p2, p4, p3)

ggsave('figs-out/alt_bw_uni.pdf', p5, width = 12, height = 5)
