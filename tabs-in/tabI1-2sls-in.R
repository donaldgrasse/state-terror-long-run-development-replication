
df <- readRDS("data/main_0310.rds")

m1_fs = felm(genocide2 ~ treat + log(dist_cap+1) + log(dist.phnompenh) | comm | 0 | comm, 
             data = df)
m2_2sls = felm(pov ~ log(dist_cap+1) + log(dist.phnompenh) | comm | (genocide2 ~ treat) | comm, 
             data = df)
m3_2sls = felm(ihs_light ~ log(dist_cap+1) + log(dist.phnompenh) | comm | (genocide2 ~ treat) | comm, 
               data = df)

texreg::texreg(list(m1_fs, m2_2sls, m3_2sls), include.ci = F)
