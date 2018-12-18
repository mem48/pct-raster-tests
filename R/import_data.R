# Rasterisation tests
library(sf)
library(sp)
library(readr)
library(dplyr)
library(raster)
library(fasterize)
source("R/overline.R")
rasterOptions(maxmemory = 1e+11)
rasterOptions(tmpdir = "F:/RasterTmp/")



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

#rf_test = rf_shape[1:10000,]

message(paste0(Sys.time()," Overlineing"))
overlined = overline_malcolm2(x = rf_shape,
                               attrib = c("bicycle","govtarget_slc","dutch_slc"), ncores = 6)

saveRDS(overlined,"schools_overlined.Rds")

message(paste0(Sys.time()," Buffering"))
overlined_buf = st_buffer(overlined, 10)
overlined_buf = st_transform(overlined_buf, 4326)
message(paste0(Sys.time()," Rastering"))
rm(overlined, rf_shape)

types = c("bicycle","govtarget_slc","dutch_slc")
grid = st_make_grid(overlined_buf, n = c(2,2))
coltable = c("#FFFFFF","#9C9C9C","#FEFE73","#AFFE01","#00FEFE","#30B0FE","#2E5FFE","#0101FE","#FE01C5")
#inter = st_intersects(overlined_buf,grid)

for(i in types){
  message(paste0(Sys.time()," Doing ",i))
  overlined_buf_tmp = overlined_buf[overlined_buf[[i]] > 0, ]
  rast = raster::raster(overlined_buf, resolution = 0.0001)
  raster::dataType(rast) <- "INT2U"
  rast = fasterize::fasterize(overlined_buf_tmp, raster = rast, field = i, fun = "sum")
  rast_col = raster::RGB(rast, breaks = c(0,1,10,50,100,250,500,1000,2000), col = coltable, alpha = T, colNA = NA, datatype = "INT1U")
  raster::dataType(rast_col) <- "INT1U"
  # Save Tiles for main raster
  for(j in 1:length(grid)){
    extent = st_bbox(grid[j])
    extent = c(extent[1],extent[3],extent[2],extent[4])
    rast_crop = raster::crop(rast_col,extent)
    message(paste0(Sys.time()," saving Raster ",i,"-10m-",j))
    raster::writeRaster(rast_crop,
                        filename = paste0("rasters/",i,"-10m-",j,".tif"),
                        format = "GTiff",
                        datatype = "INT1U",
                        overwrite = T)
    rm(rast_crop, extent)
  }
  rm(rast_col)
  rast_50 = raster::aggregate(rast, fact=5, fun=max, na.rm=T)
  rast_50 = raster::RGB(rast_50, breaks = c(0,1,10,50,100,250,500,1000,2000), col = coltable, alpha = T, colNA = NA, datatype = "INT1U")
  raster::dataType(rast_50) <- "INT1U"
  message(paste0(Sys.time()," saving Raster ",i,"-50m"))
  raster::writeRaster(rast_50,
                      filename = paste0("rasters/",i,"-50m.tif"),
                      format = "GTiff",
                      datatype = "INT1U",
                      overwrite = T)
  rm(rast_50)
  rast_100 = raster::aggregate(rast, fact=10, fun=max, na.rm=T)
  rast_100 = raster::RGB(rast_100, breaks = c(0,1,10,50,100,250,500,1000,2000), col = coltable, alpha = T, colNA = NA, datatype = "INT1U")
  raster::dataType(rast_100) <- "INT1U"
  message(paste0(Sys.time()," saving Raster ",i,"-100m"))
  raster::writeRaster(rast_100,
                      filename = paste0("rasters/",i,"-100m.tif"),
                      format = "GTiff",
                      datatype = "INT1U",
                      overwrite = T)
  rm(rast_100,rast)
  
}
message(paste0(Sys.time()," Done"))
