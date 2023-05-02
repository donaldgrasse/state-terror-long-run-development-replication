


lfs_income = readRDS('data/lfs_income.rds')

m1 = felm(self ~ school + poly(c04, 2) + sector + female |0 | 0, data = lfs_income )
m2 = felm(ihsincome ~ self  + poly(c04, 2) + sector + female |0 | 0, data = lfs_income )
m3 = felm(ihsincome ~   poly(c04, 2) + sector + female | 0 | (self ~ school), data = lfs_income )
texreg(list(m1, m2, m3), include.ci = F)