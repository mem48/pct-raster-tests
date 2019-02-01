#devtools::install_github("jannes-m/RQGIS3")
library(RQGIS)
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

files = list.files("rasters", full.names = T)
set_env(dev = FALSE)
open_app()
alg = find_algorithms("gdal2tiles", name_only = TRUE)

set_env(dev = FALSE)
open_app()
alg = find_algorithms("gdal2tiles", name_only = TRUE)
#open_help(alg)
#get_usage(alg)
params = get_args_man(alg)
params$INPUT = paste0(getwd(),"/rasters/test11.tif")
params$PROFILE = "0"
params$RESAMPLING = "0"
params$S_SRS = "EPSG:4326"
params$ZOOM = "10-15"
params$WEBVIEWER = "leaflet"
params$OUTPUTDIR = paste0(getwd(),"/tiles")

foo = run_qgis(alg, 
               params = params,
               load_output = FALSE)
