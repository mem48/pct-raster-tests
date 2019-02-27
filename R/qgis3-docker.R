# From docker image after running: 
# docker run -e PASSWORD=pass -p 8788:8787 -v ${pwd}:/home/rstudio/data  rqgis3

library(RQGIS3)
RQGIS3::find_algorithms("tile")
get_usage(alg = "gdal:gdal2tiles")
params_merge <- get_args_man("gdal:gdal2tiles")

