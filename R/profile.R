# Profile
library(sf)
library(dplyr)
rf = readRDS("securedata/rf_sf.Rds")
rf = rf[1:10000,]
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



profile <- bench::mark(check = FALSE,
            #overline2s = overline2(x = rf, attrib = c("bicycle","govtarget_slc","dutch_slc"), ncores = 1),
            #overline3s = overline3(x = rf, attrib = c("bicycle","govtarget_slc","dutch_slc"), ncores = 1),
            overline2ns = overline2(x = rf, attrib = c("bicycle","govtarget_slc","dutch_slc"), ncores = 1, simplify = F),
            overline3ns = overline3(x = rf, attrib = c("bicycle","govtarget_slc","dutch_slc"), ncores = 1, simplify = F)
)


overline3ns$text <- sf::st_as_text(overline3ns$geometry)
overline2ns$text <- sf::st_as_text(overline2ns$geometry)
overline3ns <- overline3ns[order(overline3ns$text), ]
overline2ns <- overline2ns[order(overline2ns$text), ]
identical(overline2ns, overline3ns)
identical(overline2ns$bicycle, overline3ns$bicycle)
identical(overline2ns$govtarget_slc, overline3ns$govtarget_slc)
identical(overline2ns$dutch_slc, overline3ns$dutch_slc)
identical(overline2ns$text, overline3ns$text)
identical(overline2ns$geometry, overline3ns$geometry)

summary(overline2ns$geometry %in% overline3ns$geometry)

library(tmap)
tmap_mode("view")
qtm(overline2ns, lines.col = "dutch_slc")
qtm(overline3ns, lines.col = "dutch_slc")
