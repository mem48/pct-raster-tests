# Rasterisation tests
library(sf)
library(sp)
library(readr)
library(dplyr)
library(raster)
library(fasterize)
source("R/overline.R")



if(file.exists("securedata/rf_sf.Rds")){
  rf_shape = readRDS("securedata/rf_sf.Rds")
}else{
  rf_shape = readRDS("securedata/rf_shape.Rds")
  rf_shape = st_as_sf(rf_shape)
  rf_shape = st_transform(rf_shape, 27700)
  od_raster_attr = read_csv("securedata/od_raster_attributes.csv")
  rf_shape = left_join(rf_shape, od_raster_attr, by = "id")
  rm(od_raster_attr)
  head(rf_shape)
  saveRDS(rf_shape,"securedata/rf_sf.Rds")
}

message(paste0(Sys.time()," Overlineing"))
overlined = overline_malcolm2(x = rf_shape,
                               attrib = c("bicycle","govtarget_slc","dutch_slc"), ncores = 6)

saveRDS(overlined,"schools_overlined.Rds")


