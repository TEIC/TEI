PREFIX=/usr
XSL=/usr/share/xml/tei/stylesheet
TEISERVER=http://localhost/Query/

schema:
	roma --xsl=${XSL} --teiserver=${TEISERVER} --doc teilite.xml .

doc: schema
	xsltproc --stringparam lookupDatabase false --stringparam cssFile /usr/share/xml/tei/stylesheet/teic/teic.css ${XSL}/odds/odd2html.xsl teilite.doc.xml 

install: clean doc
	mkdir -p ${PREFIX}/share/xml/tei/schema/relaxng/p5
	mkdir -p ${PREFIX}/share/xml/tei/schema/dtd/p5
	mkdir -p ${PREFIX}/share/xml/tei/schema/xsd/p5
	mv teilite.rnc teilite.rng ${PREFIX}/share/xml/tei/schema/relaxng/p5
	mv teilite.dtd ${PREFIX}/share/xml/tei/schema/dtd/p5
	mv teilite.xsd xml.xsd ${PREFIX}/share/xml/tei/schema/xsd/p5
	mkdir -p ${PREFIX}/share/doc/tei/Lite
	mv Guidelines teilite.doc.xml ${PREFIX}/share/doc/tei/Lite

clean:
	-rm -rf teilite.rnc teilite.xsd teilite.rng teilite.dtd teilite.doc.xml xml.xsd Guidelines *~


