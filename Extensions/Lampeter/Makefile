schema:
	-rm Schema/*
	xsltproc http://www.tei-c.org/Stylesheets/odd2relax.xsl lampext.xml
	(cd Schema; for i in *rng; do xmllint --format $$i > x; mv x $$i;done)
	(cd Schema; for i in *rng; do trang $$i `basename $$i .rng`.rnc;done)

html:
	xsltproc ../subsetGuidelines.xsl lampext.xml | xsltproc http://www.tei-c.org/Stylesheets/odd2html.xsl -
