#!/bin/bash 
#
# Roma: transform TEI ODD files to schema and documentation
#
# Sebastian Rahtz, April 2005
# copyright: TEI Consortium
# license: GPL
# $Id$
#

makeODD() 
{
    echo "1. expand and simplify ODD "
    xsltproc -o $N.compiled.odd \
    --stringparam TEISERVER $TEISERVER  \
    $TEIXSLDIR/odds/odd2odd.xsl $ODD 
}

makeRelax() 
{
    echo "2. make Relax NG from compiled ODD"
    xsltproc  \
             --stringparam RNGDIR $RESULTS       \
             $TEIXSLDIR/odds/odd2relax.xsl $N.compiled.odd
    (cd $RESULTS; \
    echo "3. make Relax NG compact from XML"; \
    trang $N.rng $N.rnc  || die " trang conversion to RNC fails"; \
    xmllint --format $N.rng > $$.xml; mv $$.xml $N.rng )
    $doc && makeDOC
 }

makeXSD()
{
    echo "4. make XSD from Relax NG"
    (cd $RESULTS; \
    trang  -o disable-abstract-elements $N.rng $N.xsd || die " trang fails";\
    test -f xml.xsd && perl -p -i -e 's/<xs:import.*//' xml.xsd)
}

makeDTD()
{
    echo "5. make DTD from compiled ODD"
    xsltproc  \
             --stringparam outputDir $RESULTS       \
             $TEIXSLDIR/odds/odd2dtd.xsl $N.compiled.odd
}

makeDOC() 
{
    echo "6. make expanded documented ODD"
    xsltproc --stringparam TEISERVER $TEISERVER $TEIXSLDIR/odds/subsetGuidelines.xsl $ODD \
    | xmllint --format - > $N.doc.xml 
}

die()
{
    echo; echo
    echo "ERROR: $@."
    D=`date "+%Y-%m-%d %H:%M:%S.%N"`
    echo "This was a fatal error. $D"
    exit 1
}

usageMsg()
{
echo "Roma -- reads in a TEI ODD file that specifies a schema, and tangles it"
echo "into RelaxNG schema, DTD, and W3C XML Schema, and can weave it into an"
echo "expanded, self-documented single ODD file. Note that only the first"
echo "<schemaSpec> encountered in the input ODD file is processed; all others"
echo "are ignored."
echo "  Usage: roma [options] schemaspec [output_directory]"
echo "  options, shown with defaults:"
echo "  --xsl=$TEIXSLDIR"
echo "  --teiserver=$TEISERVER"
echo "  --localsource=$LOCALSOURCE # local copy of P5 sources
echo "  options, binary switches:"
echo "  --doc         # create expanded documented ODD"
echo "  --nodtd       # suppress DTD creation"
echo "  --norelax     # suppress RelaxNG creation"
echo "  --noxsd       # suppress W3C XML Schema creation"
echo "  --debug       # leave temporary files, etc."
exit 1
}

# --------- main routine starts here --------- #
TEISERVER=http://tei.oucs.ox.ac.uk/Query/
TEIXSLDIR=/usr/share/xml/tei/stylesheet
LOCALSOURCE=
debug=false
dtd=true
relax=true
xsd=true
doc=false
while test $# -gt 0; do
  case $1 in
    --xsl=*)    TEIXSLDIR=`echo $1 | sed 's/.*=//'`;;
    --doc)      doc=true;;
    --teiserver=*) TEISERVER=`echo $1 | sed 's/.*=//'`;;
    --localsource=*) LOCALSOURCE=`echo $1 | sed 's/.*=//'`;;
    --nodtd)    dtd=false;;
    --norelax)  relax=false;;
    --noxsd)    xsd=false;;
    --debug)    debug=true;;
    --help)     usageMsg;;
     *) if test "$1" = "${1#--}" ; then 
	   break
	else
	   echo "WARNING: Unrecognized option '$1' ignored"
	   echo "For usage syntax issue $0 --help"
	fi ;;
  esac
  shift
done
ODD=${1:?"no schemaspec (i.e., ODD file) supplied; for usage syntax issue $0 --help"}
RESULTS=${2:-RomaResults}
H=`pwd`
D=`date "+%Y-%m-%d %H:%M:%S"`
echo "========= $D Roma starts, info:"
echo "Test for software: xmllint, xsltproc, trang, and perl"
which xmllint || die "you do not have xmllint"
which xsltproc || die "you do not have xsltproc"
which trang || die "you do not have trang"
which perl || die "you do not have perl"
test -f $ODD || die "file $ODD does not exist"
echo "TEI database server: $TEISERVER"
echo "TEI stylesheet tree: $TEIXSLDIR"
test -d $TEIXSLDIR/odds || \
     GET -e -d $TEIXSLDIR/odds/odd2odd.xsl > /dev/null || \
     die "stylesheet location $TEIXSLDIR is not accessible"
N=$(xsltproc $TEIXSLDIR/odds/extract-schemaSpec-ident.xsl $1 | head -1)
N=${N:?"Unable to ascertain ident= of <schemaSpec>"}
echo "Results to: $RESULTS"
mkdir -p $RESULTS || die "cannot make directory $RESULTS"
D=`date "+%Y-%m-%d %H:%M:%S.%N"`
echo "Process $ODD to create $N{.dtd|.xsd|.doc.xml|.rng|.rnc} in $RESULTS"
echo "========= $D Roma starts, execution:"
makeODD
$relax && makeRelax
$relax && $xsd && makeXSD
$dtd && makeDTD
$debug || rm  $N.compiled.odd
D=`date "+%Y-%m-%d %H:%M:%S.%N"`
echo "========= $D Roma ends"
