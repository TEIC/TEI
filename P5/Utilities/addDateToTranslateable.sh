#!/bin/sh
# run over *Spec files (listed as parameter to the script)
# and add the date of the English (by default) version of translateable objects
# Get the SVN log, and parse it to create a shell script which
# checks out each revision in turn, and compares that to the previous version.
#
# if the checked-out version isnt a well-formed XML file, dont even try the comparison.
#
L=en
echo pushd Source/Specs
cd Source/Specs
for i in $1
do
    echo svn up $i
    echo cp $i safe_$i
    svn log $i | grep "^r[0-9]" | perl -p -e "s@([^\|]+)..[^\|]+..([^ |]+).*@svn cat -\1 $i > old_$i; xmllint --noout old_$i && saxon safe_$i ../../Utilities/addDateToTranslateable.xsl lang=$L date=\2 file=old_$i | xmllint --format - > new_$i && mv new_$i safe_$i@"
    echo mv safe_$i $i
    echo rm old_$i
done
echo popd

