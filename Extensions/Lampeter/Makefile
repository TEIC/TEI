schema:
	-rm Schema/*
	xsltproc http://localhost/TEI/Stylesheets/odd2relax.xsl lampext.xml
	(cd Schema; for i in *rng; do xmllint --format $$i > x; mv x $$i;done)
	(cd Schema; for i in *rng; do trang $$i `basename $$i .rng`.rnc;done)

html:
	xsltproc http://localhost/TEI/Stylesheets/subsetGuidelines.xsl lampext.xml | xsltproc http://localhost/TEI/Stylesheets/odd2html.xsl -
