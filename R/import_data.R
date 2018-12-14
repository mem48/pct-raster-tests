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

#rf_test = rf_shape[1:10000,]

message(paste0(Sys.time()," Overlineing"))
# overlined1 = overline_malcolm(x = rf_test,
#                              attrib = c("bicycle","govtarget_slc","dutch_slc"), ncores = 1)
overlined = overline_malcolm2(x = rf_test,
                               attrib = c("bicycle","govtarget_slc","dutch_slc"), ncores = 4)

saveRDS(overlined,"schools_overlined.Rds")

message(paste0(Sys.time()," Buffering"))
overlined_buf = st_buffer(overlined, 10)
message(paste0(Sys.time()," Rastering"))
rasterOptions(maxmemory = 1e+11)

types = c("bicycle","govtarget_slc","dutch_slc")
grid = st_make_grid(overlined_buf, n = c(2,2))
coltable = c("#000000","#9C9C9C","#FFFF73","#AFFF00","#00FFFF","#30B0FF","#2E5FFF","#0000FF","#FF00C5")
#inter = st_intersects(overlined_buf,grid)

for(i in types){
  message(paste0(Sys.time()," Doing ",i))
  overlined_buf_tmp = overlined_buf[overlined_buf[[i]] > 0, ]
  rast = raster::raster(overlined_buf, resolution = 10)
  raster::dataType(rast) <- "INT2U"
  rast = fasterize(overlined_buf_tmp, raster = rast, field = i)
  rast = raster::RGB(rast, breaks = c(0,1,10,50,100,250,500,1000,2000), col = coltable, alpha = T)
  # Save Tiles for main raster
  for(j in 1:length(grid)){
    extent = st_bbox(grid[j])
    extent = c(extent[1],extent[3],extent[2],extent[4])
    rast_crop = raster::crop(rast,extent)
    message(paste0(Sys.time()," saving Raster ",i,"-10m-",j))
    raster::writeRaster(rast_crop,
                        filename = paste0("rasters/",i,"-10m-",j,".tif"),
                        format = "GTiff",
                        overwrite = T)
    rm(rast_crop, extent)
  }
  rast_50 = raster::aggregate(rast, fact=5, fun=max, na.rm=T)
  message(paste0(Sys.time()," saving Raster ",i,"-50m"))
  raster::writeRaster(rast_50,
                      filename = paste0("rasters/",i,"-50m.tif"),
                      format = "GTiff",
                      overwrite = T)
  rm(rast_50)
  rast_100 = raster::aggregate(rast, fact=10, fun=max, na.rm=T)
  message(paste0(Sys.time()," saving Raster ",i,"-100m"))
  raster::writeRaster(rast_50,
                      filename = paste0("rasters/",i,"-100m.tif"),
                      format = "GTiff",
                      overwrite = T)
  rm(rast_100)
  
}
message(paste0(Sys.time()," Done"))


# for(i in types){
#   message(paste0(Sys.time()," Doing ",i))
#   
#   for(j in 1:4){
#     message(paste0(Sys.time()," Doing grid ",j))
#     overlined_buf_tmp = overlined_buf[sapply(inter, function(y){j %in% y}), ]
#     overlined_buf_tmp = overlined_buf_tmp[overlined_buf_tmp[[i]] > 0, ]
#     rast = raster::raster(st_sf(data.frame(id = 1, geometry = grid[j])), resolution = 10)
#     raster::dataType(rast) <- "INT2U"
#     rast_tmp = fasterize(overlined_buf_tmp, raster = rast, field = i)
#     message(paste0(Sys.time()," saving Raster ",i,"-",j))
#     raster::writeRaster(rast_tmp,
#                         filename = paste0("rasters/",i,"-",j,"tif"),
#                         format = "ascii", 
#                         datatype = "INT2U",
#                         overwrite = T)
#     rm(rast_tmp)
#   }
#   
#   
#   
# }
# message(paste0(Sys.time()," Done"))

# point = st_centroid(overlined_fin[1,])
# point = st_buffer(point, 100000)
# qtm(overlined_fin[point,], lines.col = "dutch_slc", lines.lwd = 3)
# #qtm(rf_test, lines.col = "dutch_slc", lines.lwd = 3)