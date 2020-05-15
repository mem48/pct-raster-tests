# Aim: demonstrate multicore tiling using recent GDAL version (2.4)


# 1 run docker container in working directory
docker run -v ${pwd}:/home  --rm -it geographica/gdal2:2.4.0 /bin/bash

# navigate to data dir
cd /home

# check you have the right files
ls

# set nodata value
gdalbuildvrt -srcnodata "255 0 0" -vrtnodata "0 0 0" output.vrt dutch_slc-10m-merge2.tif
gdal_translate -ot Byte output.vrt output2.vrt
gdal2tiles.py output.vrt -z 8  --processes 8 output-vrt-8-8
gdal2tiles.py output.vrt -z 5-12 --processes 8 output-vrt-8-8 --resume

gdalbuildvrt -srcnodata "255 0 0" -vrtnodata "0 0 0" output-bicycle-10m-merge2.vrt bicycle-10m-merge2.tif
gdal_translate -ot Byte output-bicycle-10m-merge2.vrt output2-bicycle-10m-merge2.vrt
gdal2tiles.py output2-bicycle-10m-merge2.vrt -z 5-12 --processes 8 bicycle-tiles --resume

gdalbuildvrt -srcnodata "255 0 0" -vrtnodata "0 0 0" output-govtarget_slc-10m-merge2.vrt govtarget_slc-10m-merge2.tif
gdal_translate -ot Byte output-govtarget_slc-10m-merge2.vrt output2-govtarget_slc-10m-merge2.vrt
gdal2tiles.py output2-govtarget_slc-10m-merge2.vrt -z 5-12 --processes 8 govtarget-tiles --resume

# tests ----

# gdal_translate -a_nodata 255 dutch_slc-10m-merge2.tif dutch_slc-10m-merge2-nd.tif
# gdalwarp -srcnodata "255,0,0" -dstnodata "0,0,0" dutch_slc-10m-merge2-test.tif dutch_slc-10m-merge2-test4.tif 
# gdalwarp -srcnodata "255,0,0" -dstnodata "0,0,0" -wo INIT_DEST="255,255,255,0"  dutch_slc-10m-merge2-test.tif dutch_slc-10m-merge2-test4.tif
# gdal_translate -a_nodata "0,0,0" dutch_slc-10m-merge2-test.tif dutch_slc-10m-merge2-test2.tif 
# gdalbuildvrt -srcnodata "255 0 0" outfile.vrt dutch_slc-10m-merge2-test.tif 
# gdal_translate outfile.vrt final.tif

# tile the relevant file with n processes
# gdal2tiles.py dutch_slc-10m-merge2.tif -z 8 -srcnodata="255 0 0" --processes 8 output2.4-6-8

# mask layer if necessary (failed)
# gdal_translate -mask 4 *.tif 
# gdal_translate -a_nodata "255 0 0" dutch_slc-10m-merge2-test.tif dutch_slc-10m-merge2-test2.tif
# gdal_translate -a_nodata 0,255,255 dutch_slc-10m-merge2.tif dutch_slc-10m-merge2-nodata.tif # fail
# gdal2tiles.py dutch_slc-10m-merge2-masked.tif -z 5-6 output2.4-5-6 --processes 8
# gdal2tiles.py dutch_slc-10m-merge2.tif -z 8 -w all -r average -a 255 output2.4-8-1 --processes 8 # red
# gdal2tiles.py dutch_slc-10m-merge2.tif -z 8 -w all -r average -a 255 output2.4-8-2 --processes 8 # red
# gdal2tiles.py dutch_slc-10m-merge2.tif -z 8 -w all -r average -a 255,255,255 output2.4-8-3 --processes 8 # red
# 