


im_df = readRDS('data/im_df.rds')

m1 = felm(IM_POP_P ~ treat*dist.border |0 | 0 | 0 , data = im_df)
m2 = felm(IM_POP_P ~ treat*poly(dist.border,2) |0 | 0 | 0 , data = im_df)
m3 = felm(IM_POP_P ~ treat*dist.border |district | 0 | 0 , data = im_df)
m4 = felm(IM_POP_P ~ treat*poly(dist.border,2) |district | 0 | 0 , data = im_df)


texreg::texreg(list(m1, m2, m3, m4), include.ci = F)