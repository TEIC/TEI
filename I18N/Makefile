test:
	xsltproc -o teilite-de.xml --stringparam verbose true --stringparam TEINAMES teinames.xml --stringparam lang de translate-odd.xsl ../Extensions/Lite/teilite.xml
	perl -p -i -e 's/schema ident=.teilite./schema ident="teilite-de"/' teilite-de.xml
	xsltproc -stringparam ODDROOT /TEI/P5/ /TEI/web/Stylesheets/odd2relax.xsl teilite-de.xml
	(cd Schema; perl -p -i -e 's+www.tei-c.org/P5+www.tei-c.org.uk/P5+' teilite-de.rng )
	(cd Schema; trang teilite-de.rng teilite-de.rnc)
	xsltproc -o test-de.xml --stringparam verbose true --stringparam TEINAMES teinames.xml --stringparam lang de translate-instance.xsl test.xml
	rnv Schema/teilite-de.rnc test-de.xml
	xsltproc -o test-back.xml --stringparam TEINAMES teinames.xml --stringparam lang de translate-frominstance.xsl test-de.xml
