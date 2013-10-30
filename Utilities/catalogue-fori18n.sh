#!/bin/bash
#
# Sebastian Rahtz, October 2013
# copyright: TEI Consortium
# license: GPL
#
LANG=fr
while test $# -gt 0; do
  case $1 in
    --lang=*)       LANG=`echo $1 | sed 's/.*=//'`;;
     *) if test "$1" = "${1#--}" ; then 
	   break
	else
	   echo "WARNING: Unrecognized option '$1' ignored"
	   echo "For usage syntax issue $0 --help"
	fi ;;
  esac
  shift
done
echo Writing result file tei$LANG-i18n.html
saxon -o:tei-$LANG-i18n.html p5.xml Utilities/catalogue-fori18n.xsl lang=$LANG