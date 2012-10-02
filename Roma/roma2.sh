#!/bin/bash
#
# Roma: transform TEI ODD files to schema and documentation
#
# Sebastian Rahtz, April 2005
# copyright: TEI Consortium
# license: GPL
# $Id: roma.sh 5777 2009-03-15 21:19:34Z rahtz $
#

makeODD() 
{
    echo "1. expand and simplify ODD "
    if test "x$lang" = "x"
    then
	 saxon -xi:on -s:$ODD  -o:$RESULTS/$ODD.compiled  \
	    -xsl:$TEIXSLDIR/odds2/odd2odd.xsl \
	    $SELECTEDSCHEMA  \
	    $LANGUAGE\
	    $DOCLANG \
	    currentDirectory=$H \
	    useVersionFromTEI=$useVersionFromTEI \
	    autoGlobal=$AUTOGLOBAL $SOURCE $DEBUG  
    else
	echo  [names translated to language $lang]
	saxon -xi:on -s:$ODD -xsl:$TEIXSLDIR/odds2/odd2odd.xsl \
	    autoGlobal=$AUTOGLOBAL \
	    currentDirectory=$H \
	    useVersionFromTEI=$useVersionFromTEI \
	    $SOURCE $DEBUG  \
	    | saxon -o:$RESULTS/$ODD.compiled -s:- -xsl:$TEIXSLDIR/odds2/translate-odd.xsl \
	    $DEBUG $LANGUAGE $DOCLANG 
   fi
}

makeRelax() 
{
    echo "2. make RELAX NG from compiled ODD"
    saxon   -s:$RESULTS/$ODD.compiled -xsl:$TEIXSLDIR/odds2/odd2relax.xsl \
    $PATTERN $DEBUG $LANGUAGE $DOCLANG  $SELECTEDSCHEMA \
	     parameterize=$parameterize \
             autoGlobal=$AUTOGLOBAL \
             outputDir=$RESULTS   
    (cd $RESULTS; \
    echo "3. make RELAX NG compact from XML"; \
    trang $schema.rng $schema.rnc  || die " trang conversion to RNC fails"; \
    xmllint --format $schema.rng > $$.xml; mv $$.xml $schema.rng )
 }

makeXSD()
{
    echo "4. make XSD from RELAX NG"
    (cd $RESULTS; \
    trang  -o disable-abstract-elements $schema.rng $schema.xsd || die " trang fails";\
    test -f xml.xsd && perl -p -i -e 's+\"xml.xsd\"+\"http://www.w3.org/2004/10/xml.xsd\"+' $schema.xsd)
}

makeDTD()
{
    echo "5. make DTD from compiled ODD"
    saxon -s:$RESULTS/$ODD.compiled -xsl:$TEIXSLDIR/odds2/odd2dtd.xsl \
	$DEBUG $LANGUAGE $DOCLANG   $SELECTEDSCHEMA \
	    parameterize=$parameterize \
	    autoGlobal=$AUTOGLOBAL \
            outputDir=$RESULTS       
}

makeSCH()
{
    echo "8. extract Schematron from compiled ODD"
    saxon  -s:$RESULTS/$ODD.compiled -xsl:$TEIXSLDIR/odds2/extract-sch.xsl \
    $DEBUG $LANGUAGE $DOCLANG   $SELECTEDSCHEMA \
	> $RESULTS/$schema.sch
}

makeISOSCH()
{
    echo "9. extract Schematron from compiled ODD"
    saxon  -s:$RESULTS/$ODD.compiled -xsl:$TEIXSLDIR/odds2/extract-isosch.xsl \
    $DEBUG $LANGUAGE $DOCLANG   $SELECTEDSCHEMA \
	> $RESULTS/$schema.isosch
}

makeHTMLDOC() 
{
    echo "10. make HTML documentation $schema.doc.html "
    saxon -o:$RESULTS/$schema.doc.html -s:$RESULTS/$ODD.compiled -xsl:$TEIXSLDIR/odds2/odd2html.xsl \
	$DOCFLAGS  \
	$DEBUG  $LANGUAGE $DOCLANG autoGlobal=$AUTOGLOBAL \
	STDOUT=true \
	splitLevel=-1 
}

makePDFDOC() 
{
    echo "7. make PDF documentation $schema.doc.pdf and $schema.doc.tex "
    saxon  -o:$RESULTS/$schema.doc.tex -s:$RESULTS/$schema.doc.xml -xsl:$TEIXSLDIR/latex2/tei.xsl \
	$DEBUG $DOCFLAGS $LANGUAGE $DOCLANG autoGlobal=$AUTOGLOBAL useHeaderFrontMatter=true reencode=false \
	preQuote=“ postQuote=”
    cat > $RESULTS/perl$$.pl<<EOF
#!/usr/bin/perl
while (<>) {
      if ( / \| /) {
	  chop;
	  \$line = \$_;
	  \$N=length(\$line);
	  if (\$N > 60) {
	      \$_=substr(\$line,1,45);
	      my (\$indent,\$first) = /([ ]*)(.*)/;
	      print \$indent, \$first;
	      \$_=substr(\$line,46);
	      my (\$bef,\$after) = /([^\|]*)(\|?.*)/;
	      print "\$bef\n\$indent\$after\n";
	  }
	  else
	  {
	      print "\$_\n";
	  }

      }
else 
     {
	 print;
     }
}

EOF
    perl $RESULTS/perl$$.pl < $RESULTS/$schema.doc.tex > $RESULTS/perl$$.tex
    if $debug
	then
	diff $RESULTS/perl$$.tex $RESULTS/$schema.doc.tex
    fi
    rm $RESULTS/perl$$.pl
    mv $RESULTS/perl$$.tex $RESULTS/$schema.doc.tex
    (cd $RESULTS; xelatex --interaction=batchmode $schema.doc.tex; xelatex --interaction=batchmode $schema.doc.tex)
}

makeXMLDOC() 
{
    echo "6. make expanded documented ODD $schema.doc.xml "
    saxon -o:$RESULTS/$schema.doc.xml  \
    -s:$RESULTS/$ODD.compiled -xsl:$TEIXSLDIR/odds2/odd2lite.xsl \
    $DEBUG $DOCFLAGS  $LANGUAGE $DOCLANG $SOURCE autoGlobal=$AUTOGLOBAL 
}


die()
{
    echo; echo
    echo "ERROR: $@."
    D=`date "+%Y-%m-%d %H:%M:%S"`
    echo "This was a fatal error. $D"
    exit 1
}

usageMsg()
{
echo "Roma -- reads in a TEI ODD file that specifies a schema, and tangles it"
echo "into RelaxNG schema, DTD, and W3C XML Schema, and can weave it into an"
echo "expanded, self-documented single ODD file. "
echo "  Usage: roma [options] schemaspec [output_directory]"
echo "  options, shown with defaults:"

echo "  --xsl=$TEIXSLDIR"
echo "  --localsource=$LOCALSOURCE  # local copy of P5 sources"
echo "  options, binary switches:"
echo "  --compile          # create compiled file odd"
echo "  --debug            # leave temporary files, etc."
echo "  --doc              # create expanded documented ODD (TEI Lite XML)"
echo "  --docflags=STRING  # pass parameters to doc creation transforms"
echo "  --dochtml          # create HTML version of doc"
echo "  --doclang=LANG     # language for documentation"
echo "  --docpdf           # create PDF version of doc"
echo "  --lang=LANG        # language for names of attributes and elements"
echo "  --nodtd            # suppress DTD creation"
echo "  --norelax          # suppress RELAX NG creation"
echo "  --autoglobal       # adds global attribute class to all elements"
echo "  --noxsd            # suppress W3C XML Schema creation"
echo "  --schematron       # extract Schematron rules"
echo "  --isoschematron    # extract ISO Schematron rules"
echo "  --useteiversion    # use version data from TEI P5"
echo "  --parameterize     # create parameterized DTD"
echo "  --patternprefix=STRING # prefix RELAX NG patterns with STRING"
echo "  --schema=NAME      # select named schema spec"
echo "  --version          # which version of Roma is this"
exit 1
}

# --------- main routine starts here --------- #
TEIXSLDIR=http://www.tei-c.org/release/xml/tei/stylesheet
useVersionFromTEI=true
LOCALSOURCE=
AUTOGLOBAL=false
DOCFLAGS=
doclang=
lang=
compile=false
debug=false
dtd=true
schematron=false
isoschematron=false
relax=true
xsd=true
doc=false
docpdf=false
dochtml=false
parameterize=false
SELECTEDSCHEMA=
schema=
PATTERNPREFIX=
while test $# -gt 0; do
  case $1 in
    --compile)     compile=true;dtd=false;xsd=false;doc=false;relax=false;;
    --debug)       debug=true;;
    --doc)         doc=true;;
    --docflags=*)  DOCFLAGS=`echo $1 | sed 's/.*docflags=//'`;;
    --dochtml)     dochtml=true;;
    --doclang=*)   doclang=`echo $1 | sed 's/.*=//'`;;
    --docpdf)      docpdf=true;;
    --lang=*)      lang=`echo $1 | sed 's/.*=//'`;;
    --localsource=*) LOCALSOURCE=`echo $1 | sed 's/.*=//'`;;
    --nodtd)       dtd=false;;
    --norelax)     relax=false;;
    --autoglobal)      AUTOGLOBAL=true;;
    --noxsd)       xsd=false;;
    --schematron)      schematron=true;;
    --isoschematron)      isoschematron=true;;
    --useteiversion=*) useVersionFromTEI=`echo $1 | sed 's/.*=//'`;;
    --version) echo Roma version is VVVERSION;;
    --parameterize)       parameterize=true;;
    --schema=*)    schema=`echo $1 | sed 's/.*=//'`;;
    --patternprefix=*) PATTERNPREFIX=`echo $1 | sed 's/.*=//'`;;
    --xsl=*)       TEIXSLDIR=`echo $1 | sed 's/.*=//'`;;
    --help)        usageMsg;;
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
H=`pwd`/
D=`date "+%Y-%m-%d %H:%M:%S"`
echo "========= Roma starts at $D ==========="
# "Test for software: xmllint, saxon, trang, and perl"
which  xmllint > /dev/null || die "you do not have xmllint"
which  saxon > /dev/null || die "you do not have saxon installed"
which trang > /dev/null || die "you do not have trang installed"
which perl > /dev/null || die "you do not have perl installed"
test -f $ODD || die "file $ODD does not exist"
echo "TEI stylesheet tree: $TEIXSLDIR"
test -d $TEIXSLDIR/odds2 || \
     curl $TEIXSLDIR/odds2/odd2odd.xsl > /dev/null || \
     die "stylesheet location $TEIXSLDIR is not accessible"
if test "x$schema" = "x"
then
 schema=$(saxon -s:$1 -xsl:$TEIXSLDIR/odds2/extract-schemaSpec-ident.xsl | head -1 )
 schema=${schema:?"Unable to ascertain ident= of <schemaSpec>"}
fi
echo "Results to: $RESULTS"
mkdir -p $RESULTS || die "cannot make directory $RESULTS"
D=`date "+%Y-%m-%d %H:%M:%S.%N"`
echo "Process $ODD to create $schema{.dtd|.xsd|.doc.xml|.rng|.rnc} in $RESULTS"
echo "========= Roma version VVVERSION starts execution at $D ==========="
if test "x$PATTERNPREFIX" = "x"
then
   PATTERN=" "
else
   PATTERN=" patternPrefix=$PATTERNPREFIX"
fi

if $debug
then
    DEBUG=" verbose=true"
fi
SELECTEDSCHEMA=" selectedSchema=$schema "

if test "x$doclang" = "x"
then
  DOCLANG=" "
else 
  DOCLANG=" doclang=$doclang documentationLanguage=$doclang"
fi

if test "x$lang" = "x"
then
  LANGUAGE=" "
else 
  LANGUAGE=" lang=$lang "
fi

if test "x$LOCALSOURCE" = "x"
then
  SOURCE=""
else 
  REALD=`dirname "$LOCALSOURCE"`
  SDIR=`(cd $REALD; pwd)`
  REALSOURCE=$SDIR/`basename $LOCALSOURCE`
  echo using $REALSOURCE as default source
  SOURCE="defaultSource=$REALSOURCE"
fi

makeODD || die "odd2odd process failed"
( $relax || $xsd ) && makeRelax
if $xsd 
then
   if ! $relax 
   then
      echo Ignored --norelax, RELAX NG required\
     to generate W3C XML Schema
   fi
   makeXSD
fi
$dtd && makeDTD
$schematron && makeSCH
$isoschematron && makeISOSCH
$dochtml && doc=true
$docpdf && doc=true
if $doc
then 
    makeXMLDOC
    if $docpdf
    then
	makePDFDOC
    fi
fi
$dochtml && makeHTMLDOC
$compile || $debug || rm  $RESULTS/$ODD.compiled
test -f subset.xsl && rm subset.xsl
test -f tei$$.xml && rm tei$$.xml
D=`date "+%Y-%m-%d %H:%M:%S.%N"`
echo "========= $D Roma ends"
