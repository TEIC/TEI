default:	
	echo no default action

u:
	xsltproc testunicode.xsl ../unicode.xml > chars.xml
	xmllint --format --noent test11.xml | grep -v chars.xml > test12.xml

package:
	zip teioo TEI.stw tei-to-oo.xsl teioucs-to-oo.xsl oo-to-teioucs.xsl tei-oucs.dtd test12.xml test4.xml test3.xml test0.xml

