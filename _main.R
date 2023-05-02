rm(list=ls())
setwd('~/Dropbox/state-terror-long-run-development-replication')

source('packages.R')
source('grd-helper-functions.R')

#### Figures #### 
list.files("figs-in", full.names = TRUE) %>% walk(source)
beepr::beep(sound = 3)

#### Tables #### 
list.files("tabs-in", full.names = TRUE) %>% walk(source)
beepr::beep(sound = 3)

rm(list=ls())
