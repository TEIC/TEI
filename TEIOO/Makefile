package:
	(cd TEIP4; jar cf ../teioop4.jar *)
	(cd TEIP5; jar cf ../teioop5.jar *)
u:
	xsltproc testunicode.xsl ../unicode.xml > chars.xml
	xmllint --format --noent test11.xml | grep -v chars.xml > test12.xml

clean:
	rm *.jar
