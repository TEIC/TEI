default:	
	echo no default action

u:
	xsltproc testunicode.xsl ../unicode.xml > chars.xml
	xmllint --format --noent test11.xml | grep -v chars.xml > test12.xml

package:
	cp teip4-to-oo.xsl jar/TEIP4/TEI.2/
	cp oo-to-teip4.xsl jar/TEIP4/TEI.2/
	cp teip5-to-oo.xsl jar/TEIP5/TEI
	cp oo-to-teip5.xsl jar/TEIP5/TEI
	(cd jar/TEIP4; jar cf ../../teioop4.jar *)
	(cd jar/TEIP5; jar cf ../../teioop5.jar *)
clean:
	rm *.jar
