schema:
	xsltproc -stringparam verbose true /usr/share/xml/tei/stylesheet/base/p5/odds/odd2relax.xsl teilite.xml
	(cd Schema; for i in *rng; do xmllint --format $$i > x; mv x $$i;done)
	(cd Schema; for i in *rng; do trang $$i `basename $$i .rng`.rnc;done)

test:
	jing  Schema/teilite.rng testlite.xml

html:
	xsltproc http://localhost/stylesheets/teic/subsetGuidelines.xsl teilite.xml | xsltproc http://localhost/TEI/Stylesheets/odd2html.xsl -

