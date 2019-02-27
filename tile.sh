#!/bin/sh
set -e

SCRIPT_DIR=$(dirname "$(readlink -f $0)")
export PATH=$PATH:${SCRIPT_DIR}

FILE=$1
RESOLUTION=$2
BARE_NAME=${FILE%.*}

BAD_ARGS="call using \n\n ./tile.sh [FILE.tiff] [n]\n\nWhere:\n[FILE.tiff] is the file\n[n] is the resolution (in m2 per pixel), it can be 10, 50 or 100 only"

if [ -z "${FILE}" ]; then
  echo "Missing file name, ${BAD_ARGS}"
  exit 1
fi

if [ -z "${RESOLUTION}" ]; then
  echo "Missing resolution, ${BAD_ARGS}"
  exit 1
fi

if [ ! -e "$FILE" ]; then
  echo "File '${FILE}' not found"
  exit 1
fi

if [ $RESOLUTION -eq 10 ]; then
  UPPER_ZOOM=15
  LOWER_ZOOM=12
elif [ $RESOLUTION -eq 50 ]; then
  UPPER_ZOOM=11
  LOWER_ZOOM=10
elif [ $RESOLUTION -eq 100 ]; then
  UPPER_ZOOM=9
  LOWER_ZOOM=5
else
  echo "Resolution must be 10, 50 or 100 but was ${RESOLUTION}, ${BAD_ARGS}"
  exit 1
fi

# COLOURED=${BARE_NAME}-coloured.tiff
# 
# if [ ! -e "$COLOURED" ]; then
#   echo "Colouring in the raster"
#   gdaldem color-relief ${FILE} -nearest_color_entry -alpha colour.txt ${COLOURED}
# fi

rm -rf ${BARE_NAME}
gdal2tiles.py ${FILE} -w none -z ${LOWER_ZOOM}-${UPPER_ZOOM} ${BARE_NAME}

upload.sh ${BARE_NAME}