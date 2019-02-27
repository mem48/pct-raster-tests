# Aim: demonstrate multicore tiling using recent GDAL version (2.4)


# 1 run docker container in working directory
docker run -v ${pwd}:/home  --rm -it geographica/gdal2:2.4.0 /bin/bash

# navigate to data dir
cd /home

# check you have the right files
ls

# tile the relevant file with n processes
gdal2tiles.py dutch_slc-10m-merge2.tif -z 5-12 output2.4-5-12 --processes 8