# Profile
library(sf)
library(dplyr)
#rf = readRDS("securedata/rf_sf.Rds")
#rf = rf[1:500,]
rf = pct::get_pct_routes_fast("west-yorkshire", geography = "lsoa")
#rf = rf[1:000,]
source("R/overline.R")
source("R/overline_lowmemory.R")
source("R/overline_lowmemory2.R")
source("R/overline_highspeed.R")
#rm(overlined3)

# 
# profvis::profvis(overlined2 <- overline2(x = rf, attrib = c("bicycle","govtarget_slc","dutch_slc"), ncores = 4))
#profvis::profvis(overlined3 <- overline3(x = rf, attrib = c("bicycle","govtarget_slc","dutch_slc"), ncores = 4))
# 
# 

# overlined2 = overline2(x = rf, attrib = c("bicycle","govtarget_slc","dutch_slc"), ncores = 4)
# overlined3 = overline3(x = rf, attrib = c("bicycle","govtarget_slc","dutch_slc"), ncores = 4)



profile <- bench::mark(check = FALSE,
            #r1 = stplanr::overline(sl = rf, attrib = c("bicycle","govtarget_slc","dutch_slc")),
            r2 = overline2(x = rf, attrib = c("bicycle","govtarget_slc","dutch_slc")),
            #r3 = overline3(x = rf, attrib = c("bicycle","govtarget_slc","dutch_slc")),
            r4 = overline4(x = rf, attrib = c("bicycle","govtarget_slc","dutch_slc"))
            #r5 = overline5(x = rf, attrib = c("bicycle","govtarget_slc","dutch_slc"))
)
profile[,c(1,7,10)]


# Check Identical
r2 = r4
r2$geometry <- sf::st_as_text(r2$geometry)
r3$geometry <- sf::st_as_text(r3$geometry)
r2 <- as.data.frame(r2)
r3 <- as.data.frame(r3)
r2 <- r2[order(r2$geometry), ]
r3 <- r3[order(r3$geometry), ]
rownames(r2) <- 1:nrow(r2)
rownames(r3) <- 1:nrow(r3)
identical(r2, r3)


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
