PREFIX=/usr
TEISERVER=http://localhost/Query/

schema:
	roma --localsource=../../P5/Source-driver.xml --dochtml teilite.xml .

install: schema
	mkdir -p ${PREFIX}/share/xml/tei/schema/relaxng/p5
	mkdir -p ${PREFIX}/share/xml/tei/schema/dtd/p5
	mkdir -p ${PREFIX}/share/xml/tei/schema/xsd/p5
	mv teilite.rnc teilite.rng ${PREFIX}/share/xml/tei/schema/relaxng/p5
	mv teilite.dtd ${PREFIX}/share/xml/tei/schema/dtd/p5
	mv teilite.xsd ${PREFIX}/share/xml/tei/schema/xsd/p5
	mkdir -p ${PREFIX}/share/doc/tei/Lite
	mv Guidelines teilite.doc.xml ${PREFIX}/share/doc/tei/Lite

clean:
	-rm -rf teilite.rnc teilite.xsd teilite.rng teilite.dtd teilite.doc.xml xml.xsd Guidelines *~


