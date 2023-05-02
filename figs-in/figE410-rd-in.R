
packs <- c('ggplot2', 'ggthemes', 'dplyr')
lapply(packs, require, character.only = T)
theme_set(theme_tufte())


df <- readRDS("data/main_0310.rds")


#### Development #### 

pov_rd <- rdplot_custom_theme(df$pov, df$dist_border, h = 5, ylab_rd = 'Poverty')
ntl_rd <- rdplot_custom_theme(df$ihs_light, df$dist_border, h = 5, ylab_rd = 'ihs(Night Lights)')

ggsave('figs-out/pov_rd.pdf', pov_rd)
ggsave('figs-out/ntl_rd.pdf', ntl_rd)

#### Wealth #### 

dhs_df = readRDS('data/dhs_df.rds')

lvl_rd <- rdplot_custom_theme(dhs_df$wlth, dhs_df$dist.border, h = 10000, ylab_rd = 'Wealth (Level)')
lnw_rd <- rdplot_custom_theme(dhs_df$lnwlth, dhs_df$dist.border, h = 10000, ylab_rd = 'Wealth (Log)')
qnt_rd <- rdplot_custom_theme(dhs_df$wlthqunit, dhs_df$dist.border, h = 10000, ylab_rd = 'Wealth (Quintiles)')

ggsave('figs-out/lvl_rd.pdf', lvl_rd)
ggsave('figs-out/lnw_rd.pdf', lnw_rd)
ggsave('figs-out/qnt_rd.pdf', qnt_rd)


#### Human Capital Plots #### 
census_1998 <- readRDS('data/census_1998_0310.rds') %>% 
  mutate(dist_border = dist_border/1000)

edu_rd <- rdplot_custom_theme(census_1998$no_educ, census_1998$dist_border, h = 5, ylab_rd = '% No Schooling')
lit_rd <- rdplot_custom_theme(census_1998$lit_rate, census_1998$dist_border, h = 5, ylab_rd = '% Literate')
yrs_rd <- rdplot_custom_theme(census_1998$yrseducv, census_1998$dist_border, h = 5, ylab_rd = 'Avg. Yrs. School')
att_rd <- rdplot_custom_theme(census_1998$att_rate, census_1998$dist_border, h = 5, ylab_rd = 'Attend Rate')

ggsave('figs-out/edu_rd.pdf', edu_rd)
ggsave('figs-out/lit_rd.pdf', lit_rd)
ggsave('figs-out/yrs_rd.pdf', yrs_rd)
ggsave('figs-out/att_rd.pdf', att_rd)

#### Labor Markets ####
lfs_income = readRDS('data/lfs_income.rds')
lfs_income$ihs_income <- asinh(lfs_income$income)
slf_rd <- rdplot_custom_theme(lfs_income$self, lfs_income$dist_border, h = 15, ylab_rd = 'Pr(Self Employed)')
inc_rd <- rdplot_custom_theme(lfs_income$ihs_income, lfs_income$dist_border, h = 15, ylab_rd = 'ihs(Income)')
prd_rd <- rdplot_custom_theme(lfs_income$prod, lfs_income$dist_border, h = 15, ylab_rd = 'Productivity')

ggsave('figs-out/slf_rd.pdf', slf_rd)
ggsave('figs-out/inc_rd.pdf', inc_rd)
ggsave('figs-out/prd_rd.pdf', prd_rd)



#### Health Plots #### 

health_df <- readRDS('data/dhs_kr.rds') %>% 
  mutate(dist_border = dist.border/1000)
health_index <- rdplot_custom_theme(health_df$hlthIndex, health_df$dist_border, h = 10, ylab_rd = 'Health Index')
health_stunt <- rdplot_custom_theme(health_df$hw5, health_df$dist_border, h = 10, ylab_rd = 'Stunting')
health_waste <- rdplot_custom_theme(health_df$hw8, health_df$dist_border, h = 10, ylab_rd = 'Wasting')
health_under <- rdplot_custom_theme(health_df$hw11, health_df$dist_border, h = 10, ylab_rd = 'Underweight')

ggsave('figs-out/health_stunt.pdf', health_stunt)
ggsave('figs-out/health_waste.pdf', health_waste)
ggsave('figs-out/health_under.pdf', health_under)
ggsave('figs-out/health_index.pdf', health_index)

#### genocide plot #### 

genocide_plot <- rdplot_custom_theme(df$genocide2, df$dist_border, h = 10, ylab_rd = 'Genocide Intensity Index')
ggsave('figs-out/genocide_plot.pdf', genocide_plot)


