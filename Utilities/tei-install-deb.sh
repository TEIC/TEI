#!/bin/bash
#
# Install TEI debian packages
#
# Sebastian Rahtz, March 2011
# copyright: TEI Consortium
# license: GPL
#
die()
{
    echo; echo
    echo "ERROR: $@."
    D=`date "+%Y-%m-%d %H:%M:%S"`
    echo "$D. That was a fatal error"
    exit 1
}

Jenkins=http://tei.oucs.ox.ac.uk/jenkins/job
version=
package=TEIP5
while test $# -gt 0; do
  case $1 in
    --package=*)   package=`echo $1 | sed 's/.*=//'`;;
    --version=*)   version=`echo $1 | sed 's/.*=//'`;;
   *) if test "$1" = "${1#--}" ; then 
	   break
	else
	   echo "WARNING: Unrecognized option '$1' ignored"
	   echo "For usage syntax issue $0 --help"
	fi ;;
  esac
  shift
done
dir=${Jenkins}/${package}/lastSuccessfulBuild/artifact
echo Try to fetch $version package from $dir
case $package in 
  TEIP5)         names="tei-p5-source tei-p5-schema tei-p5-database tei-p5-doc tei-p5-exemplars tei-p5-test";;
  Stylesheets-xslt1)   names="tei-p5-xsl";;
  Stylesheets)   names="tei-xsl-common tei-p5-xsl2";;
  Roma)          names=tei-roma;;
    *) echo "Error: package $package unsupported"; exit 1;;
esac
for p in ${names}
do
    echo get component $p
    rm -f ${p}-${version}_*
    curl -O -s $dir/${p}_${version}_all.deb
    curl -O -s $dir/${p}_${version}_i386.changes
    curl -O -s $dir/${p}_${version}_i386.build
    sudo dpkg -i  ${p}_${version}_all.deb
done