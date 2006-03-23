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
	cp demohead.xml demo.xml
	echo "<table rend=\"rules\">" >> demo.xml
	for j in `grep "<text xml:lang=" ../Stylesheets/i18n.xml | sed 's/.*text xml:lang=\"\([^\"]*\)\".*/\1/' | sort -u `; \
	do \
	echo "<row>" >> demo.xml; \
	for i in person.odd.*; \
	do \
	 X=`echo $$i | sed 's/person.odd.//'`; \
	 echo do $$X and $$j; \
	 xsltproc --stringparam lang $$j testi18n-to-xml.xsl $$i > person-$$X-$$j.xml; \
	 perl -p -i -e "s/ILANG/$$j/;s/LANG/$$X/" person-$$X-$$j.xml; \
	echo "<cell>"  >> demo.xml; \
	test $$X = $$j && echo "<hi>"  >> demo.xml; \
	echo "<xref url=\"person-$$X-$$j.xml\">$$X / $$j </xref>" >> demo.xml; \
	test $$X = $$j && echo "</hi>"  >> demo.xml; \
	echo "</cell>"  >> demo.xml; \
	done; \
	echo "</row>" >> demo.xml; \
	done	
	echo "</table></body></text></TEI>" >> demo.xml

