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

demo:
	echo "<html><body><h1>Sample TEI internationalisation</h1>" > demo.html
	echo "<table rules=\"all\" border=\"1\">" >> demo.html
	for j in `grep "<text xml:lang=" ../Stylesheets/i18n.xml | sed 's/.*text xml:lang=\"\([^\"]*\)\".*/\1/' | sort -u `; \
	do \
	echo "<tr>" >> demo.html; \
	for i in person.odd.*; \
	do \
	 X=`echo $$i | sed 's/person.odd.//'`; \
	 echo do $$X and $$j; \
	 xsltproc --stringparam lang $$j testi18n.xsl $$i > person-$$X-$$j.html; \
	echo "<td>"  >> demo.html; \
	test $$X = $$j && echo "<b>"  >> demo.html; \
	echo "<a href=\"person-$$X-$$j.html\">$$X / $$j </a>" >> demo.html; \
	test $$X = $$j && echo "</b>"  >> demo.html; \
	echo "</td>"  >> demo.html; \
	done; \
	echo "</tr>" >> demo.html; \
	done	
	echo "</table></body><html>" >> demo.html