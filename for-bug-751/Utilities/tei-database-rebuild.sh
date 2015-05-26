#!/bin/sh
# Rebuild TEI eXist database
# Peter Stadler
# 2014-09-15
# License: GPL

LOCAL_PATH_P5SUBSET=/var/www/vhosts/tei-c.org/projects/tei/web/Vault/P5/current/xml/tei/odd/p5subset.xml
REMOTE_PATH_P5SUBSET=localhost:8080/exist/rest/db/TEI/p5subset.xml
LOCAL_PATH_INDEX_CONFIGURATION=/var/www/vhosts/tei-c.org/projects/tei/web/Vault/P5/current/xml/tei/xquery/index.xconf
REMOTE_PATH_INDEX_CONFIGURATION=localhost:8080/exist/rest/db/system/config/db/TEI/index.xconf
EXIST_USER=
EXIST_USER_PASS=

# All the parameters above can and shall be overwritten by a config file
CONFIG=~/etc/tei-exist.conf

# Checking config with credentials …
if [ -f $CONFIG ] ; then
    source $CONFIG
else
    echo >&2 "Config file with credentials is missing"
    echo >&2 "Moving on with hard coded values …"
fi

# Checking for curl …
if [ ! `command -v curl 2>/dev/null` ] ; then 
    echo >&2 "You do not have curl installed"  
    echo >&2 "Dying …"
    exit 1
fi

# First, upload the index configuration
curl --user $EXIST_USER:$EXIST_USER_PASS -f --upload-file $LOCAL_PATH_INDEX_CONFIGURATION $REMOTE_PATH_INDEX_CONFIGURATION

# Second, upload the new p5subset.xml
curl --user $EXIST_USER:$EXIST_USER_PASS -f --upload-file $LOCAL_PATH_P5SUBSET $REMOTE_PATH_P5SUBSET
