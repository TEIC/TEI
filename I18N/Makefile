test-spanish:
	xsltproc -o teilite-es.xml --stringparam verbose true --stringparam TEINAMES teinames.xml --stringparam lang es translate-odd.xsl ../Extensions/Lite/teilite.xml
	perl -p -i -e 's/schema ident=.teilite./schema ident="teilite-es"/' teilite-es.xml
	xsltproc http://localhost/TEI/Stylesheets/odd2relax.xsl teilite-es.xml
	(cd Schema; perl -p -i -e 's+www.tei-c.org/P5+www.tei-c.org.uk/P5+' teilite-es.rng )
	(cd Schema; trang teilite-es.rng teilite-es.rnc)
	xsltproc -o test-es.xml --stringparam verbose true --stringparam TEINAMES teinames.xml --stringparam lang es translate-instance.xsl test.xml
	-rnv Schema/teilite-es.rnc test-es.xml
	xsltproc -o test-back.xml --stringparam TEINAMES teinames.xml --stringparam lang es translate-frominstance.xsl test-es.xml
	xsltproc http://localhost/TEI/Stylesheets/subsetGuidelines.xsl teilite-es.xml | xsltproc http://localhost/TEI/Stylesheets/odd2html.xsl -
	(cd Schema ; xsltproc http://localhost/TEI/Stylesheets/P5/odds/expandincludes.xsl teilite-es.rng | xsltproc http://localhost/TEI/Stylesheets/P5/odds/nomorechoice.xsl - > teilite-compiled-es.rng; trang -o disable-abstract-elements teilite-compiled-es.rng teilite-compiled-es.xsd)

test-german:
	xsltproc -o teilite-de.xml --stringparam verbose true --stringparam TEINAMES teinames.xml --stringparam lang de translate-odd.xsl ../Extensions/Lite/teilite.xml
	perl -p -i -e 's/schema ident=.teilite./schema ident="teilite-de"/' teilite-de.xml
	xsltproc http://localhost/TEI/Stylesheets/odd2relax.xsl teilite-de.xml
	(cd Schema; perl -p -i -e 's+www.tei-c.org/P5+www.tei-c.org.uk/P5+' teilite-de.rng )
	(cd Schema; trang teilite-de.rng teilite-de.rnc)
	xsltproc -o test-de.xml --stringparam verbose true --stringparam TEINAMES teinames.xml --stringparam lang de translate-instance.xsl test.xml
	-rnv Schema/teilite-de.rnc test-de.xml
	xsltproc -o test-back.xml --stringparam TEINAMES teinames.xml --stringparam lang de translate-frominstance.xsl test-de.xml
	xsltproc http://localhost/TEI/Stylesheets/subsetGuidelines.xsl teilite-de.xml | xsltproc http://www.tei-c.org/Stylesheets/odd2html.xsl -
