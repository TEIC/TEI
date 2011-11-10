#!/bin/bash
#
# Install TEI packages on web site
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

Vault=/projects/tei/web/Vault
Jenkins=http://tei.oucs.ox.ac.uk/jenkins/job
ECHO=
SFUSER=rahtz
version=
package=TEIP5
while test $# -gt 0; do
  case $1 in
    --sfuser)      SFUSER=`echo $1 | sed 's/.*=//'`;;
    --dummy)       ECHO=echo;;
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
sfname=$package
case $package in 
  TEIP5)         name=P5;          pname=tei;      SFNAME=TEIP5-All;;
  Stylesheets)   name=Stylesheets; pname=tei-xsl;;
  Stylesheets-xslt1)   name=Stylesheets; pname=tei-xslt1;;
  Roma)          name=Roma;        pname=tei-roma;;
    *) echo "Error: package $package unsupported"; exit 1;;
esac
rm -f ${pname}-${version}.zip
${ECHO} curl -O -s $dir/${pname}-${version}.zip || \
    die "Unable to fetch package $dir/${pname}-${version}.zip"
${ECHO} mkdir -p ${Vault}/${name}/${version}
echo unpack to ${Vault}/${name}/${version}
${ECHO} unzip -q -o ${pname}-${version}.zip -d ${Vault}/${name}/${version}
${ECHO} rm ${Vault}/${name}/current
echo link ${Vault}/${name}/${version} to ${Vault}/${name}/current
${ECHO} ln -s ${Vault}/${name}/${version} ${Vault}/${name}/current
echo upload ${pname}-${version}.zip to Sourceforge ${SFNAME} as user ${SFUSER}
${ECHO} rsync -e ssh ${pname}-${version}.zip ${SFUSER},tei@frs.sourceforge.net:/home/frs/project/t/te/tei/${SFNAME}/${pname}-${version}.zip

case $package in 
  Roma)
	${ECHO} unzip -q -o ${pname}-${version}.zip -d /usr/share;;
  TEIP5)
	rm -f teiwebsiteguidelines.zip;
	echo Get special HTML pages for TEI web site;
	${ECHO} curl -O -s $dir/teiwebsiteguidelines.zip || die "Unable to fetch package $dir/teiwebsiteguidelines.zip";
	echo unpack web guidelines to ${Vault}/${name}/${version};
	${ECHO} unzip -q -o teiwebsiteguidelines -d ${Vault}/${name}/${version}/doc/tei-p5-doc;;
esac
