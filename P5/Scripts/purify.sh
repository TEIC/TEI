# run this script inside P5
# first create pure dataSpecs
#cd Source/Specs
#for f in data.*; do echo $f; saxon -s:$f -xsl:../../Scripts/purifyDataSpecs.xsl; done
#cd ../..
# then do content  models and datatypes, looking at each 
# non dataspec file in turn
#mkdir Source/impureSpecs
#cp Source/Specs/* Source/impureSpecs
#rm Source/impureSpecs/data.* Source/impureSpecs/teidata.*
cd Source/impureSpecs/
for f in *.xml; do echo $f; saxon -o:../Specs/$f -s:$f -xsl:../../Scripts/purify.xsl; done
