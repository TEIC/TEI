PREFIX=/usr
ROMAOPTS= --dochtml  --localsource=../Source-driver.xml

schemas:
	for i in *.odd; do roma $(ROMAOPTS) $$i .; done



dist: schemas
	rm -rf tei[0-9]*.xml release
	mkdir -p release/share/xml/tei/schema/relaxng/p5
	mkdir -p release/share/xml/tei/schema/dtd/p5
	mkdir -p release/share/xml/tei/schema/xsd/p5
	mkdir -p release/share/doc/tei/skeletons/p5
	mkdir -p release/share/xml/tei/odd
	cp *.xsd release/share/xml/tei/schema/xsd/p5
	cp *.dtd release/share/xml/tei/schema/dtd/p5
	cp *.dtd release/share/xml/tei/schema/dtd/p5
	cp *.odd release/share/xml/tei/odd
	cp *.rnc *.rng release/share/xml/tei/schema/relaxng/p5
	cp *.xml release/share/doc/tei/skeletons/p5
	cp *.doc.html release/share/doc/tei/skeletons/p5

install: dist
	mkdir -p ${PREFIX}/share/xml/tei
	(cd release; tar cf - share) | \
	(cd ${PREFIX}; tar xf -)

clean:
	-rm *.xsd *.dtd *.doc.* *.rnc tei*.rng *.compiled.*
