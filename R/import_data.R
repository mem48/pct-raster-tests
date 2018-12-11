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

#rf_test = rf_shape[1:100000,]

message(paste0(Sys.time()," Overlineing"))
overlined = overline_malcolm2(x = rf_shape,
                             attrib = c("bicycle","govtarget_slc","dutch_slc"),
                             ncores = 6)

saveRDS(overlined,"schools_overlined.Rds")

message(paste0(Sys.time()," Buffering"))
overlined_buf = st_buffer(overlined, 10)
message(paste0(Sys.time()," Rastering"))
rast = raster::raster(overlined, resolution = 10)
raster::dataType(rast) <- "INT2U"
types = c("bicycle","govtarget_slc","dutch_slc")
for(i in types){
  rast_tmp = fasterize(overlined, raster = rast, field = i)
  raster::writeRaster(rast_tmp,
                      filename = paste0(i,".tif"),
                      format = "GTiff", 
                      overwrite = T)
  rm(rast_tmp)
}
message(paste0(Sys.time()," Done"))



#qtm(overlined, lines.col = "dutch_slc", lines.lwd = 3)
#qtm(rf_test, lines.col = "dutch_slc", lines.lwd = 3)