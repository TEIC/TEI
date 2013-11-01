#!/bin/sh
# run over all the *Spec files and add the date of the English version of gloss/desc/remarks.
# Get the SVN log, and parse it to create a shell script which
# checks out each revision in turn, and compares that to the previous version.
#
# if the checked-out version isnt a well-formed XML file, dont even try the comparison.
cd Source/Specs
for i in $1
do
    echo cp $i safe_$i
    svn log $i | grep "^r[0-9]" | perl -p -e "s@([^\|]+)..[^\|]+..([^ |]+).*@svn cat -\1 $i > old_$i; xmllint --noout old_$i && saxon -o:new_$i safe_$i ../../Utilities/addDateToGlossDesc.xsl date=\2 file=old_$i && mv new_$i safe_$i@"
    echo svn up $i
    echo mv safe_$i $i
    echo rm old_$i
done

