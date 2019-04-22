library(sf)
library(sp)

rf_shape <- readRDS("../cyipt-securedata/pct-routes-all.Rds")
rf_shape <- rf_shape[,c("pct.census","pct.gov","pct.gen","pct.dutch","pct.ebike")]
st_crs(rf_shape) <- 27700
names(rf_shape) <- c("co_bicycle","co_govtarget", "co_gendereq","co_dutch","co_ebike","geometry")

source("R/overline_lowmemory.R")

rf_shape <- split(rf_shape, (seq(nrow(rf_shape))-1) %/% 200000) 

#rf_shape <- overline3(rf_shape, attrib = c("co_bicycle","co_govtarget", "co_gendereq","co_dutch","co_ebike"), ncores = 1)

for(i in 1:length(rf_shape)){
  print(i)
  rf_shape[[i]] <- overline3(rf_shape[[i]], attrib = c("co_bicycle","co_govtarget", "co_gendereq","co_dutch","co_ebike"), ncores = 4)
}

rf_shape <- dplyr::bind_rows(rf_shape)
rf_shape <- as.data.frame(rf_shape)
rf_shape$geometry <- st_sfc(rf_shape$geometry)
rf_shape <- st_sf(rf_shape)
st_crs(rf_shape) <- 27700

saveRDS(rf_shape, "old_rnet.Rds")

#rf_shape <- overline3(rf_shape, attrib = c("bicycle_16p","govtarget_slc_r","gendereq_slc_r","dutch_slc_r","ebike_slc_r"), ncores = 1)
#names(rf_shape) <- c("co_bicycle","co_govtarget","co_gendereq","co_dutch","co_ebike","geometry")
com_new <- readRDS("D:/Users/earmmor/OneDrive - University of Leeds/Cycling Big Data/comparebuilds/commute_2018.Rds")
sch_new <- readRDS("D:/Users/earmmor/OneDrive - University of Leeds/Cycling Big Data/comparebuilds/schools_2019.Rds")
com_new <- st_as_sf(com_new)
sch_new <- st_as_sf(sch_new)
names(com_new) <- c("local_id","cn_bicycle","cn_govtarget","cn_govnearmkt", "cn_gendereq","cn_dutch","cn_ebike","geometry")
names(sch_new) <- c("local_id","sc_bicycle","sc_govtarget" ,"sc_dutch","geometry")

com_new$local_id <- NULL
sch_new$local_id <- NULL

names_all <- unique(c(names(rf_shape), names(com_new), names(sch_new)))

rf_shape[names_all[!names_all %in% names(rf_shape)]] <- 0
com_new[names_all[!names_all %in% names(com_new)]] <- 0
sch_new[names_all[!names_all %in% names(sch_new)]] <- 0

names_all <- names_all[names_all != "geometry"]
names_all <- c(names_all,"geometry")

rf_shape <- rf_shape[,names_all]
com_new <- com_new[,names_all]
sch_new <- sch_new[,names_all]

rf_shape <- st_transform(rf_shape, 4326)

route_all <- rbind(rf_shape, com_new)
route_all <- rbind(route_all, sch_new)
nrow(route_all) == sum(c(nrow(rf_shape), nrow(com_new), nrow(sch_new)))
rm(rf_shape, com_new, sch_new)

rnet <- overline3(route_all, attrib = names(route_all)[1:14], ncores = 1, simplify = FALSE)
