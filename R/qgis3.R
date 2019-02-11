#devtools::install_github("jannes-m/RQGIS3")
library(RQGIS)
files = list.files("rasters", full.names = T)
set_env(dev = FALSE)
open_app()
# coltable = c("#FFFFFF","#9C9C9C","#FEFE73","#AFFE00","#00FEFE","#30B0FE","#2E5FFE","#0000FE","#FE00C5")
# rast_col = raster::RGB(rast, breaks = c(0,1,10,50,100,250,500,1000,2000), col = coltable, alpha = T, colNA = NA)
# #rast_col = raster::projectRaster(rast_col, crs= "+proj=longlat +datum=WGS84 +no_defs")
# #dataType(rast_col) = "INT1U"
# raster::writeRaster(rast_col,
#                     filename = paste0("rasters/test11.tif"),
#                     format = "GTiff",
#                     overwrite = T,
#                     datatype = "INT1U"
#                     #NAflag = 0
#                     )


# merge 10m rasters
scenarios <- c("bicycle","govtarget_slc","dutch_slc")
alg_merge <- find_algorithms("gdal:merge", name_only = TRUE)
params <- get_args_man(alg_merge)
for(i in 1:3){
  files_sub <- files[grep(paste0(scenarios[i],"-10m"),files)]
  if(length(files_sub) != 4){message("Not 4 files");stop()}
  
}


r1 = raster("rasters/bicycle-10m-1.tif")
r2 = raster("rasters/bicycle-10m-2.tif")
r3 = raster("rasters/bicycle-10m-3.tif")
r4 = raster("rasters/bicycle-10m-4.tif")




alg = find_algorithms("gdal2tiles", name_only = TRUE)

set_env(dev = FALSE)
open_app()
alg = find_algorithms("gdal2tiles", name_only = TRUE)
#open_help(alg)
#get_usage(alg)
params = get_args_man(alg)
params$INPUT = paste0(getwd(),"/rasters/bicycle-100m.tif")
params$PROFILE = "0"
params$RESAMPLING = "0"
params$NODATA = "255" # Test transparency
params$S_SRS = "EPSG:3857"
params$ZOOM = "5-10"
params$WEBVIEWER = "leaflet"
params$OUTPUTDIR = paste0(getwd(),"/tiles/test")

foo = run_qgis(alg, 
               params = params,
               load_output = FALSE)
