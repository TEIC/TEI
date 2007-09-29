go:
	rm japanese.xml; \
	echo "<TEI xmlns=\"http://www.tei-c.org/ns/1.0\">" > japanese.xml; \
	echo "<text><body>" >> japanese.xml; \
	for i in *.rng; do \
	   X=`basename $i .rng`; \
	   xsltproc -o $$X.xml rngtoodd.xsl $$i; \
	   cat $$X.xml >> japanese.xml; \
	done ; \
	echo "</body></text></TEI>" >> japanese.xml