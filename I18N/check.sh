#!/bin/sh
onvdl examples.nvdl $1 \
 | grep -v ': error: unfinished element$' \
 | grep -v ': error: unfinished element .* required to finish the element$'
