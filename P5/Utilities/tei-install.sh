#!/bin/bash
#
# Install TEI packages on web site
#
# Sebastian Rahtz, May 2014
# copyright: TEI Consortium
# license: GPL
#

# changed 2014-09-16 by Syd in the middle of trying to release 2.7.0:
# bug fix: changed $dir to $jenkinsdir in the "Get special HTML pages
# for TEI web site" section of install() function.

# Changed 2016-03-30 by MDH to ensure that the oxygen folder with
# updateSite.oxygen in it is copied from the old current release to
# the folder which will become the new one, so that it stays available
# for users, and can also be retrieved for updating by the oxygen-tei
# build script.

install()
{
    echo Install ${name}/${version} in ${Vault} as current
    ${ECHO} mkdir -p ${Vault}/${name}/${version}
    echo unpack to ${Vault}/${name}/${version}
    ${ECHO} unzip -q -o ${pname}-${version}.zip -d ${Vault}/${name}/${version}
    case $package in
	Roma)
	    ${ECHO} unzip -q -o ${pname}-${version}.zip -d /usr/share;;
	TEIP5)
	    ${ECHO} rm -f teiwebsiteguidelines.zip;
	    echo Get special HTML pages for TEI web site;
	    ${ECHO} curl -L -O -s $jenkinsdir/teiwebsiteguidelines.zip || die "Unable to fetch package $jenkinsdir/teiwebsiteguidelines.zip";
	    echo Copy oxygen folder with updateSite.oxygen to ${Vault}/${name}/${version}
	    ${ECHO} cp -r ${Vault}/${name}/current/oxygen ${Vault}/${name}/${version}
	    echo unpack web guidelines to ${Vault}/${name}/${version};
	    ${ECHO} unzip -q -o teiwebsiteguidelines -d ${Vault}/${name}/${version}/doc/tei-p5-doc;
            ${ECHO} rm teiwebsiteguidelines.zip;;
    esac
}

makecurrent()
{
    ${ECHO} rm ${Vault}/${name}/current
    echo link ${Vault}/${name}/${version} to ${Vault}/${name}/current
    ${ECHO} ln -s ${Vault}/${name}/${version} ${Vault}/${name}/current
}

upload()
{
    echo upload ${pname}-${version}.zip to Sourceforge ${SFNAME} as user ${SFUSER}
    ${ECHO} rsync -e ssh ${pname}-${version}.zip ${SFUSER},tei@frs.sourceforge.net:/home/frs/project/t/te/tei/${SFNAME}/${pname}-${version}.zip
}

die()
{
    echo; echo
    echo "ERROR: $@."
    D=`date "+%Y-%m-%d %H:%M:%S"`
    echo "$D. That was a fatal error"
    exit 1
}

Vault=/projects/tei/web/Vault
Jenkins=http://jenkins.tei-c.org/job
ECHO=
SFUSER=rahtz
version=
package=
JOB=all
while test $# -gt 0; do
  case $1 in
      --sfuser=*)   SFUSER=`echo $1 | sed 's/.*=//'`;;
      --dummy)      ECHO=echo;;
      --package=*)  package=`echo $1 | sed 's/.*=//'`;;
      --version=*)  version=`echo $1 | sed 's/.*=//'`;;
      --upload)  JOB=upload;;
      --makecurrent)  JOB=makecurrent;;
      --install)  JOB=install;;
   *) if test "$1" = "${1#--}" ; then
	   break
	else
	   echo "WARNING: Unrecognized option '$1' ignored"
	fi ;;
  esac
  shift
done
if [ -z $version ]
then
 echo You must use the --version option to specify which version of the package you are installing
 exit 1
fi
if [ -z $package ]
then
 echo You must use the --package option to specify which package you are installing
 exit 1
fi
jenkinsdir=${Jenkins}/${package}/lastSuccessfulBuild/artifact
SFNAME=$package
case $package in
  TEIP5)         name=P5;           pname=tei;      SFNAME=TEI-P5-all; jenkinsdir=${jenkinsdir}/P5;;
  Stylesheets1)  name=Stylesheets1; pname=tei-xslt1;;
  Stylesheets)   name=Stylesheets;  pname=tei-xsl;;
  Roma)          name=Roma;         pname=tei-roma;;
    *) echo "Error: package $package unsupported"; exit 1;;
esac
echo Try to fetch version $version of $package from $jenkinsdir
rm -f ${pname}-${version}.zip
${ECHO} curl -L -O -s $jenkinsdir/${pname}-${version}.zip || \
    die "Unable to fetch package $jenkinsdir/${pname}-${version}.zip"

echo Selected task is $JOB
case $JOB in
  all) install; makecurrent; upload;;
  install) install ;;
  makecurrent) makecurrent ;;
  upload) upload;;
esac

${ECHO} rm ${pname}-${version}.zip
