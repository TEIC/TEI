schema:
	xsltproc -stringparam verbose true http://localhost/TEI/Stylesheets/odd2relax.xsl teilite.xml
	(cd Schema; for i in *rng; do xmllint --format $$i > x; mv x $$i;done)
	(cd Schema; for i in *rng; do trang $$i `basename $$i .rng`.rnc;done)

test:
	jing  Schema/teilite.rng testlite.xml

html:
	xsltproc http://localhost/TEI/Stylesheets/subsetGuidelines.xsl teilite.xml | xsltproc http://localhost/TEI/Stylesheets/odd2html.xsl -

