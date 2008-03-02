package:
	(cd TEIP4; jar cf ../teioop4.jar *)
	(cd TEIP5; jar cf ../teioop5.jar *)
u:
	xsltproc testunicode.xsl ../unicode.xml > chars.xml
	xmllint --format --noent test11.xml | grep -v chars.xml > test12.xml

test-from:
	for i in test-from-oo/*.fodt;	do \
	  xsltproc -o test-from-oo/`basename $$i .fodt`.xml TEIP5/TEI/oo-to-teip5.xsl $$i;\
	xmllint --noout test-from-oo/`basename $$i .fodt`.xml ; \
	done

clean:
	rm *.jar
