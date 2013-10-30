#!/bin/sh
# run over all the *Spec files and add the date of the English version of gloss/desc/remarks
for i in *.xml
do
    echo cp $i safe_$i
    svn log $i | grep "^r[0-9]" | perl -p -e "s@([^\|]+)..[^\|]+..([^ |]+).*@svn up -\1 $i; xmllint --noout $i && saxon -o:new_$i safe_$i ../../Utilities/addDateToGlossDesc.xsl date=\2 file=$i;mv new_$i safe_$i@"
    echo mv safe_$i $i
done

