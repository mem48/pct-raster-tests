# From docker image after running: 
# docker run -e PASSWORD=pass -p 8788:8787 -v ${pwd}:/home/rstudio/data  rqgis3

# with gdal2tiles-leaflet
u = "https://github.com/commenthol/gdal2tiles-leaflet/archive/master.zip"
download.file(u, "master.zip")
unzip("master.zip")


# in docker with gdal2.4:

# docker run -v ${pwd}:/home  --rm -it geographica/gdal2:2.4.0 /bin/bash


#
# in bash:
gdal2tiles-leaflet-master/gdal2tiles-multiprocess.py -l -p raster -z 5-8 bicycle-100m.tif tiles-multi8
file.copy("gdal2tiles-leaflet-master/test/index.html", "test.html")
file.copy("gdal2tiles-leaflet-master/test/index.js", "index.js")
file.edit("test.html")
file.edit("index.js")





# with qgis3:
library(RQGIS3)
RQGIS3::find_algorithms("tile")
get_usage(alg = "gdal:gdal2tiles")
params_merge <- get_args_man("gdal:gdal2tiles")

RQGIS3::run_qgis("gdal:gdal2tiles", params = list())


# with tile.sh
system("./tile.sh bicycle-100m.tif 100")

library(leaflet)
leaflet() %>% 
  addTiles(urlTemplate = "bicycle-100m/{z}/{x}/{y}.png", options = tileOptions(tms = T, minZoom = 5, maxZoom = 9)) %>% 
  addProviderTiles(provider = providers$Esri.WorldGrayCanvas)
  # addTiles(urlTemplate = "bicycle-100m/{z}/{x}/{y}.png", options =     2\. option=list(continuousWorld=TRUE,                   tileSize="256",minZoom="5",maxZoom="9")) 

remotes::install_github("ropensci/tiler")
library(tiler)
tile_viewer("bicycle-100m", "5-9")

tiler::tile(file = "bicycle-100m.tif", tiles = "test", zoom = "5-9")


