S=/usr/share/xml/tei/stylesheet
lang=es

test:
	xsltproc -o teilite-$(lang).xml --stringparam verbose true --stringparam lang $(lang) translate-odd.xsl ../Extensions/Lite/teilite.xml
	perl -p -i -e 's/schemaSpec ident=.teilite./schemaSpec ident="teilite-$(lang)"/' teilite-$(lang).xml
	xsltproc --stringparam schemaBaseURL /usr/share/xml/tei/schema/relaxng/p5/ $S/base/p5/odds/odd2relax.xsl teilite-$(lang).xml
	(cd Schema; trang teilite-$(lang).rng teilite-$(lang).rnc)
	xsltproc -o test-$(lang).xml --stringparam verbose true --stringparam lang $(lang) translate-instance.xsl test.xml
	-rnv Schema/teilite-$(lang).rnc test-$(lang).xml
	xsltproc -o test-back.xml --stringparam lang $(lang) translate-frominstance.xsl test-$(lang).xml
	xsltproc $S/base/p5/odds/subsetGuidelines.xsl teilite-$(lang).xml | xsltproc $S/base/p5/odds/odd2html.xsl -
	(cd Schema ; xsltproc $S/base/p5/odds/expandincludes.xsl teilite-$(lang).rng | xsltproc $S/base/p5/odds/nomorechoice.xsl - > teilite-compiled-$(lang).rng; trang -o disable-abstract-elements teilite-compiled-$(lang).rng teilite-compiled-$(lang).xsd; trang teilite-compiled-$(lang).rng teilite-compiled-$(lang).dtd; perl -p -i -e 's/ns1://g' teilite-compiled-$(lang).xsd )

clean:
	-rm test-back.xml test-??.xml  teilite-??.xml
	-rm -rf Schema Guidelines
