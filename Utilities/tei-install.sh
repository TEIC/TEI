#!/bin/bash
#
# Install TEI packages on web site
#
# Sebastian Rahtz, May 2014 2011
# copyright: TEI Consortium
# license: GPL
#
install()
{
    echo Install ${name}/${version} in ${Vault} as current
    ${ECHO} mkdir -p ${Vault}/${name}/${version}
    echo unpack to ${Vault}/${name}/${version}
    ${ECHO} unzip -q -o ${pname}-${version}.zip -d ${Vault}/${name}/${version}
    ${ECHO} rm ${Vault}/${name}/current
    echo link ${Vault}/${name}/${version} to ${Vault}/${name}/current
    ${ECHO} ln -s ${Vault}/${name}/${version} ${Vault}/${name}/current
    case $package in 
	Roma)
	    ${ECHO} unzip -q -o ${pname}-${version}.zip -d /usr/share;;
	TEIP5)
	    ${ECHO} rm -f teiwebsiteguidelines.zip;
	    echo Get special HTML pages for TEI web site;
	    ${ECHO} curl -O -s $dir/teiwebsiteguidelines.zip || die "Unable to fetch package $dir/teiwebsiteguidelines.zip";
	    echo unpack web guidelines to ${Vault}/${name}/${version};
	    ${ECHO} unzip -q -o teiwebsiteguidelines -d ${Vault}/${name}/${version}/doc/tei-p5-doc;
            ${ECHO} rm teiwebsiteguidelines.zip;;
    esac
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
Jenkins=http://bits.nsms.ox.ac.uk:8080/jenkins/job
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
  TEIP5)         name=P5;           pname=tei;      SFNAME=TEI-P5-all;;
  Stylesheets1)  name=Stylesheets1; pname=tei-xslt1;;
  Stylesheets)   name=Stylesheets;  pname=tei-xsl;;
  Roma)          name=Roma;         pname=tei-roma;;
    *) echo "Error: package $package unsupported"; exit 1;;
esac
echo Try to fetch version $version of $package from $jenkinsdir
rm -f ${pname}-${version}.zip
${ECHO} curl -O -s $jenkinsdir/${pname}-${version}.zip || \
    die "Unable to fetch package $jenkinsdir/${pname}-${version}.zip"

case $JOB in 
  all) install; upload;;
  install) install ;;
  upload) upload;;
esac

${ECHO} rm ${pname}-${version}.zip 
