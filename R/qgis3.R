library(RQGIS)
files <- list.files("rasters", full.names = T)
files <- files[!grepl("merge",files)]
files <- paste0(getwd(),"/",files)
set_env(dev = FALSE)
open_app()


# merge 10m rasters
scenarios <- c("bicycle","govtarget_slc","dutch_slc")
alg_merge <- find_algorithms("gdalogr:merge", name_only = TRUE)
#open_help(alg_merge)
params_merge <- get_args_man(alg_merge)

for(i in 1:3){
  message(paste0(Sys.time()," merging ",scenarios[i]))
  files_sub <- files[grep(paste0(scenarios[i],"-10m"),files)]
  if(length(files_sub) != 4){message("Not 4 files");stop()}
  params_merge$INPUT <- paste(files_sub, collapse = ";")
  params_merge$OUTPUT <- paste0(getwd(),"/rasters/",scenarios[i],"-10m-merge.tif")
  run_qgis(alg_merge, params = params_merge, load_output = FALSE)
}


# Tile rasters
alg_tiles = find_algorithms("gdal2tiles", name_only = TRUE)
params_tiles = get_args_man(alg_tiles)
#params_tiles$INPUT = paste0(getwd(),"/rasters/bicycle-100m.tif")
params_tiles$PROFILE = "0"
params_tiles$RESAMPLING = "0"
params_tiles$NODATA = "255" # Test transparency
params_tiles$S_SRS = "EPSG:3857"
#params_tiles$ZOOM = "5-10"
params_tiles$WEBVIEWER = "leaflet"
#params_tiles$OUTPUTDIR = paste0(getwd(),"/tiles/test")

dir.create("tiles")
for(i in 1:3){
  dir.create(paste0("tiles/",scenarios[i]))
}

for(i in 1:3){
  message(paste0(Sys.time()," tiling ",scenarios[i]))
  message(paste0(Sys.time()," tiling 100m rater"))
  params_tiles$INPUT = paste0(getwd(),"/rasters/,",scenarios[i],"-100m.tif")
  params_tiles$ZOOM = "1-9"
  params_tiles$OUTPUTDIR = paste0(getwd(),"/tiles/",scenarios[i])
  run_qgis(alg_tiles, params = params_tiles, load_output = FALSE)
  message(paste0(Sys.time()," tiling 50m rater"))
  params_tiles$INPUT = paste0(getwd(),"/rasters/,",scenarios[i],"-50m.tif")
  params_tiles$ZOOM = "10-11"
  run_qgis(alg_tiles, params = params_tiles, load_output = FALSE)
  message(paste0(Sys.time()," tiling 10m rater"))
  params_tiles$INPUT = paste0(getwd(),"/rasters/,",scenarios[i],"-10m-merge.tif")
  params_tiles$ZOOM = "12-18"
  run_qgis(alg_tiles, params = params_tiles, load_output = FALSE)
}


