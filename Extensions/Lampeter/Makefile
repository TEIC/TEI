schema:
	xsltproc http://www.tei-c.org/Stylesheets/odd2relax.xsl lampext.xml
	(cd Schema; for i in *rng; do xmllint --format $$i > x; mv x $$i;done)
