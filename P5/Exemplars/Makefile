ROMAOPTS= --dochtml  --localsource=../P5/Source-driver.xml

schemas:
	for i in *.odd; do roma $(ROMAOPTS) $$i .; done



dist: schemas
	mkdir -p release/share/xml/tei/schema/relaxng/p5
	mkdir -p release/share/xml/tei/schema/dtd/p5
	mkdir -p release/share/xml/tei/schema/xsd/p5
	mkdir -p release/share/doc/tei/skeletons/p5
	cp *.xsd release/share/xml/tei/schema/xsd/p5
	cp *.dtd release/share/xml/tei/schema/dtd/p5
	cp *.rnc *.rng release/share/xml/tei/schema/relaxng/p5
	cp *.xml release/share/doc/tei/skeletons/p5
	cp *.doc.html release/share/doc/tei/skeletons/p5

clean:
	-rm *.xsd *.dtd *.doc.* *.rnc tei*.rng
