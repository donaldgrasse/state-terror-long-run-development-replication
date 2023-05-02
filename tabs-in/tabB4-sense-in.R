df <- readRDS('data/main_0310.rds')

m1 <- lm(pov ~ treat*dist_border + build_75_sum + road_d, data = subset(df, abs(dist_border) <= 6.340))
sense_out_pov <- sensemakr(m1, 'treat', c('build_75_sum', 'road_d'), kd = c(4), bound_label = c('Built Up Area', 'Road Density'))
ovb_minimal_reporting(sense_out_pov)
summary(sense_out_pov)


m2 <- lm(asinh(ntl_2km) ~ treat*dist_border + build_75_sum + road_d, data = subset(df, abs(dist_border) <= 7.950))
sense_out_ntl <- sensemakr(m2, 'treat', c('build_75_sum', 'road_d'), kd = c(4))
plot(sense_out_ntl)
ovb_minimal_reporting(sense_out_ntl)
summary(sense_out_ntl)


census_1998 <- readRDS('data/census_1998_0310.rds')


m3 <- lm(lit_rate ~ treat*dist_border + build_75_sum + road_d, data = subset(census_1998, abs(dist_border) <= 5960))
sense_out_lit <- sensemakr(m3, 'treat', c('build_75_sum', 'road_d'), kd = c(4))
plot(sense_out_lit)
ovb_minimal_reporting(sense_out_lit)
summary(sense_out_lit)


m4 <- lm(no_educ ~ treat*dist_border + build_75_sum + road_d, data = subset(census_1998, abs(dist_border) <= 5620))
sense_out_edu <- sensemakr(m4, 'treat', c('build_75_sum', 'road_d'), kd = c(4))
plot(sense_out_lit)
ovb_minimal_reporting(sense_out_edu)
summary(sense_out_edu)