#!/bin/bash

MEDIA_TYPE=`file -b --mime-type "$1"`
EXTENSION=${1##*.}

function office-open-xml {
    echo $1
    TEMP_DIR=`mktemp -d`
    unzip "$1" -d $TEMP_DIR > /dev/null
    saxon -it:main office-open-xml/main.xsl url=${TEMP_DIR}/
    rm -rf $TEMP_DIR
}

if [ ! -e "$1" ] ; then
  echo "File does not exist: $1"
  exit 1
fi

case "$MEDIA_TYPE" in
  "application/vnd.ms-excel")
    office-open-xml "$1"
    ;;
  *)
    case "$EXTENSION" in
      "xlsx")
        office-open-xml "$1"
        ;;
      *)
        echo "Could not determine file type"
        exit 1
        ;;
      esac
    ;;
esac

