S=/usr/share/xml/tei/stylesheet
lang=es

test:
	perl -p -e 's/schemaSpec ident=.test./schemaSpec ident="test-$(lang)"/' \
	testodd.xml > testodd-$(lang).xml
	roma --localsource=../P5/Source/Guidelines/en/driver.xml --translate=es  testodd-$(lang).xml Schema
	xsltproc -o test-$(lang).xml --stringparam verbose true --stringparam lang $(lang) translate-instance.xsl test.xml
	-rnv Schema/test-$(lang).rnc test-$(lang).xml
	xsltproc -o test-back.xml --stringparam lang $(lang) translate-frominstance.xsl test-$(lang).xml
	xsltproc $S/odds/subsetGuidelines.xsl test-$(lang).xml | xsltproc $S/odds/odd2html.xsl -

clean:
	-rm test-back.xml test-??.xml  testodd-??.xml
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

