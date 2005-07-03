PREFIX=/usr
ROMAOPTS= --dochtml  --localsource=../Source-driver.xml

schemas:
	for i in *.odd; do roma $(ROMAOPTS) $$i .; done
	-rm xlink.xsd xml.xsd mathml*xsd p5examples.xsd tei_odds.xsd
	-rm p5odds.xsd structure.xsd ns1.xsd teix.xsd
	-rm p5odds.dtd p5examples.dtd



dist: schemas
	rm -rf tei[0-9]*.xml release
	mkdir -p release/share/xml/tei/schema/relaxng/p5
	mkdir -p release/share/xml/tei/schema/dtd/p5
	mkdir -p release/share/xml/tei/schema/xsd/p5
	mkdir -p release/share/xml/tei/exemplars/p5
	mkdir -p release/share/xml/tei/odd/exemplars/p5
	mkdir -p release/share/doc/tei/html/exemplars/p5
	mkdir -p release/share/doc/tei/xml/exemplars/p5
	cp `ls *.xml | grep -v '\.doc\.'` release/share/xml/tei/odd/exemplars/p5
	cp *.xsd release/share/xml/tei/schema/xsd/p5
	cp *.dtd release/share/xml/tei/schema/dtd/p5
	cp *.dtd release/share/xml/tei/schema/dtd/p5
	cp *.odd release/share/xml/tei/odd/exemplars/p5
	cp *.rnc *.rng *.mod release/share/xml/tei/schema/relaxng/p5
	cp *.doc.html release/share/doc/tei/html/exemplars/p5
	cp *.doc.xml  release/share/doc/tei/xml/exemplars/p5

install: dist
	mkdir -p ${PREFIX}/share/xml/tei
	(cd release; tar cf - share) | \
	(cd ${PREFIX}; tar xf -)

clean:
	-rm *.xsd *.dtd *.doc.* *.rnc tei*.rng *.compiled.*
