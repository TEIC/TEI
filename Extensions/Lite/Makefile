schema:
	xsltproc -stringparam verbose true http://www.tei-c.org/Stylesheets/odd2relax.xsl teilite.xml
	(cd Schema; for i in *rng; do xmllint --format $$i > x; mv x $$i;done)

test:
	jing  Schema/teilite.rng testlite.xml
