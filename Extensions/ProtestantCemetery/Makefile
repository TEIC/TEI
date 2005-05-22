PREFIX=/usr
XSL=/usr/share/xml/tei/stylesheet
TEISERVER=http://localhost/Query/

schema:
	roma --xsl=${XSL} --teiserver=${TEISERVER} cem.odd .

test:
	xmllint --noent /TEI/Projects/ProtestantCemetery/cem.xml | xmllint --noout --relaxng teicem.rng - 

test2:
	xmllint --noent /TEI/Projects/ProtestantCemetery/cem.xml > cemall.xml
	jing -c teicem.rnc cemall.xml

test3:
	xmllint --noent /TEI/Projects/ProtestantCemetery/cem.xml > cemall.xml
	jing -c teicem.compiled.rnc cemall.xml

clean:
	-rm *.rng *.rnc *.xsd *.dtd *.doc.xml *~ cemall.xml



