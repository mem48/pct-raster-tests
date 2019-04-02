# Profile
library(sf)
library(dplyr)
rf = readRDS("securedata/rf_sf.Rds")
rf = rf[1:1000,]
source("R/overline.R")
source("R/overline_lowmemory.R")
rm(overlined3)

# 
# profvis::profvis(overlined2 <- overline2(x = rf, attrib = c("bicycle","govtarget_slc","dutch_slc"), ncores = 4))
profvis::profvis(overlined3 <- overline3(x = rf, attrib = c("bicycle","govtarget_slc","dutch_slc"), ncores = 4))
# 
# 

overlined2 = overline2(x = rf, attrib = c("bicycle","govtarget_slc","dutch_slc"), ncores = 4)
overlined3 = overline3(x = rf, attrib = c("bicycle","govtarget_slc","dutch_slc"), ncores = 4)
identical(overlined2, overlined3)

profile <- bench::mark(check = TRUE,
            overline2 = overline2(x = rf, attrib = c("bicycle","govtarget_slc","dutch_slc"), ncores = 4),
            overline3 = overline3(x = rf, attrib = c("bicycle","govtarget_slc","dutch_slc"), ncores = 4)
)

library("profmem")
options(profmem.threshold = 2000)
profile2 <- profmem(overline3(x = rf, attrib = c("bicycle","govtarget_slc","dutch_slc"), ncores = 4))
profile2
