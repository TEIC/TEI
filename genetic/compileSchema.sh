#!/bin/sh

#roma --nodtd --noxsd geneticTEI.xml compiledSchema/
/usr/bin/roma2 --dochtml --docflags="summaryDoc=true" \
  --nodtd --noxsd geneticTEI.xml compiledSchema/
rsync compiledSchema/geneticTEI.doc.html linux.ox.ac.uk:public_html/wip/
rnv compiledSchema/geneticTEI.rnc constr_example/transcript_D1.xml
