library(raster)
library(fasterize)
library(sf)
library(tmap)
tmap_mode("view")
rasterOptions(maxmemory = 1e+11)
rasterOptions(tmpdir = "F:/RasterTmp/")


overlined = readRDS("schools_overlined2.Rds")

message(paste0(Sys.time()," Buffering"))
overlined_buf = st_buffer(overlined, 10, nQuadSegs = 3)
overlined_buf = st_transform(overlined_buf, 3857)
message(paste0(Sys.time()," Rastering"))
rm(overlined)

types = c("bicycle","govtarget_slc","dutch_slc")
grid = st_make_grid(overlined_buf, n = c(2,2))
coltable = c("#9C9C9C","#FEFE73","#AFFE00","#00FEFE","#30B0FE","#2E5FFE","#0000FE","#FE00C5")
breaks = c(1,9,49,99,249,999,1999,999999)
gc()

for(i in types){
  message(paste0(Sys.time()," Doing ",i))
  overlined_buf_tmp = overlined_buf[overlined_buf[[i]] > 0, ]
  overlined_buf_tmp = overlined_buf_tmp[,i]
  rast = fasterize::raster(overlined_buf, resolution = 10)
  raster::dataType(rast) <- "INT2U"
  rast = fasterize::fasterize(overlined_buf_tmp, raster = rast, field = i, fun = "sum")
  rast_col = raster::RGB(rast, breaks = breaks, col = coltable, alpha = F, colNA = NA, datatype = "INT1U")
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
  
  # Save 50m raster
  rast_50 = raster::aggregate(rast, fact=5, fun=max, na.rm=T)
  rast_50 = raster::RGB(rast_50, breaks = breaks, col = coltable, alpha = F, colNA = NA, datatype = "INT1U")
  raster::dataType(rast_50) <- "INT1U"
  message(paste0(Sys.time()," saving Raster ",i,"-50m"))
  raster::writeRaster(rast_50,
                      filename = paste0("rasters/",i,"-50m.tif"),
                      format = "GTiff",
                      datatype = "INT1U",
                      overwrite = T)
  rm(rast_50)
  
  # Save 100m raster
  rast_100 = raster::aggregate(rast, fact=10, fun=max, na.rm=T)
  rast_100 = raster::RGB(rast_100, breaks = breaks, col = coltable, alpha = F, colNA = NA, datatype = "INT1U")
  raster::dataType(rast_100) <- "INT1U"
  message(paste0(Sys.time()," saving Raster ",i,"-100m"))
  raster::writeRaster(rast_100,
                      filename = paste0("rasters/",i,"-100m.tif"),
                      format = "GTiff",
                      datatype = "INT1U",
                      overwrite = T)
  rm(rast_100,rast)
  removeTmpFiles(h = 3)
  gc()
  
}
message(paste0(Sys.time()," Done"))
gc()
