default:	
	echo no default action

u:
	xsltproc testunicode.xsl ../unicode.xml > chars.xml
	xmllint --format --noent test11.xml | grep -v chars.xml > test12.xml

package:
	cp tei-to-oo.xsl jar/TEI.2/
	cp oo-to-teioucs.xsl jar/TEI.2/
	cp oo-to-teip5.xsl jarp5/TEIP5/
	cp teip5-to-oo.xsl jarp5/TEIP5/
	(cd jar; jar cf ../teioo.jar *)
	(cd jarp5; jar cf ../teioop5.jar *)
