S=/usr/share/xml/tei/stylesheet
lang=es

test:
	xsltproc  -o teilite-$(lang).xml --stringparam lang $(lang) $(S)/odds/translate-odd.xsl ../Extensions/Lite/teilite.xml 
	perl -p -i -e 's/schemaSpec ident=.teilite./schemaSpec ident="teilite-$(lang)"/' teilite-$(lang).xml
	roma --teiserver=http://localhost/Query/ teilite-$(lang).xml Schema
	xsltproc -o test-$(lang).xml --stringparam verbose true --stringparam lang $(lang) translate-instance.xsl test.xml
	-rnv Schema/teilite-$(lang).rnc test-$(lang).xml
	xsltproc -o test-back.xml --stringparam lang $(lang) translate-frominstance.xsl test-$(lang).xml
	xsltproc $S/odds/subsetGuidelines.xsl teilite-$(lang).xml | xsltproc $S/odds/odd2html.xsl -

clean:
	-rm test-back.xml test-??.xml  teilite-??.xml
	-rm `find . -name "*~"`
	-rm -rf Schema Guidelines
