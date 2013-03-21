#! /bin/sh
# 
# Check translation versions
# Sebastian Rahtz 2007/10/19
#
# extract every gloss and desc into a distinct file
# 
rm -rf today
xsltproc --stringparam dir today Utilities/list_gloss_by_date.xsl  \
   Source/Guidelines/en/guidelines-en.xml

# go over all the dates on which sets for translation
# were issued, get the source from that, and do the same extraction
for i in 2006-06-05 2006-10-28 2007-01-21 2007-05-02 2007-05-19 2007-06-12 2007-08-25
do
 mkdir -p $i-Source
 cd $i-Source
 svn export -r "{$i}"  https://sourceforge.net/p/tei/code/HEAD/tree/trunk/P5/Source
 cd ..
 rm -rf $i
 xsltproc --stringparam dir $i Utilities/list_gloss_by_date.xsl \
 $i-Source/Source/Guidelines/en/guidelines-en.xml

# compare with todays results. ignore <gloss> for now.
#

 diff -r -q $i today | grep -v Only > $i.DIFF
done
