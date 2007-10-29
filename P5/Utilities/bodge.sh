#!/bin/bash
# script to run an XSLT transformation on each XML source file 
# without parsing the whole shooting match


script=/home/lou/TEI/trunk/P5/Utilities/bodge.xsl
fakes=/home/lou/TEI/trunk/P5/Utilities/fake-entities.dtd

if test -z $1
then 
    echo Usage: bodge inputDirectory outputDirectory
    exit
fi

if test -d $1
then 
   echo Input from $1
   inDIR=$1
elif test -f $1
then
    echo Input from $1
    inFILE=$1
else
   echo $1 should be a directory name
   exit
fi

if test -z $2
then
   echo Output to /tmp/Bodged
   mkdir /tmp/Bodged
   outDIR=/tmp/Bodged
else
   outDIR=$2
fi

if test -d $outDIR
then 
   echo Output to $outDIR
fi

if test -z $inDIR
then
    echo $inFILE
  echo "<!DOCTYPE div1 [<!ENTITY % fake-entities SYSTEM '$fakes'>%fake-entities;]>" > /tmp/$inFILE;
  cat $inFILE >> /tmp/$inFILE;
  xsltproc $script /tmp/$inFILE | perl -p -e "s|<!--££([^;]+);-->|&\1;|g" > $outDIR/$inFILE;
else
 cd $inDIR
 for f in *.xml 
 do
  echo  $f
  echo "<!DOCTYPE div1 [<!ENTITY % fake-entities SYSTEM '$fakes'>%fake-entities;]>" > /tmp/$f;
  cat $f >> /tmp/$f;
  xsltproc $script /tmp/$f | perl -p -e "s|<!--££([^;]+);-->|&\1;|g" > $outDIR/$f;
  done
fi
  
