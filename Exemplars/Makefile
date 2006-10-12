PREFIX=/usr
LANGUAGE=en
DRIVER=guidelines-${LANGUAGE}.xml
XSL=/usr/share/xml/tei/stylesheet/
ROMAOPTS=--dochtml --localsource=../Source/Guidelines/${LANGUAGE}/${DRIVER}

schemas:
	for i in *.odd; do echo Process $$i; $(PREFIX)/bin/roma $(ROMAOPTS) $$i .; done
	-rm xlink.xsd xml.xsd mathml*xsd p5examples.xsd tei_odds.xsd
	-rm p5odds.xsd structure.xsd ns1.xsd teix.xsd
	-rm p5odds.dtd p5examples.dtd


dist: schemas
	rm -rf tei[0-9]*.xml ../release/tei-p5-exemplars
	mkdir -p ../release/tei-p5-exemplars/share/xml/tei/custom/schema/relaxng
	mkdir -p ../release/tei-p5-exemplars/share/xml/tei/custom/schema/dtd
	mkdir -p ../release/tei-p5-exemplars/share/xml/tei/custom/schema/xsd
	mkdir -p ../release/tei-p5-exemplars/share/xml/tei/custom/templates
	mkdir -p ../release/tei-p5-exemplars/share/xml/tei/custom/odd
	mkdir -p ../release/tei-p5-exemplars/share/doc/tei-p5-exemplars/html
	mkdir -p ../release/tei-p5-exemplars/share/doc/tei-p5-exemplars/xml
	cp `ls *.xml | grep -v '\.doc\.'` ../release/tei-p5-exemplars/share/xml/tei/custom/templates
	cp *.xsd ../release/tei-p5-exemplars/share/xml/tei/custom/schema/xsd
	cp *.dtd ../release/tei-p5-exemplars/share/xml/tei/custom/schema/dtd
	cp *.dtd ../release/tei-p5-exemplars/share/xml/tei/custom/schema/dtd
	cp *.odd ../release/tei-p5-exemplars/share/xml/tei/custom/odd
	cp *.rnc *.rng *.mod ../release/tei-p5-exemplars/share/xml/tei/custom/schema/relaxng
	cp *.doc.html ../release/tei-p5-exemplars/share/doc/tei-p5-exemplars/html
	cp *.doc.xml  ../release/tei-p5-exemplars/share/doc/tei-p5-exemplars/xml
	cp ../catalog.p5.custom ../release/tei-p5-exemplars/share/xml/tei/custom/schema/catalog.xml
	(cd ../release; 	\
	ln -s tei-p5-exemplars tei-p5-exemplars-`cat ../VERSION` ; \
	zip -r tei-p5-exemplars-`cat ../VERSION`.zip tei-p5-exemplars-`cat ../VERSION` )


clean:
	-rm *.xsd *.dtd *.doc.* *.rnc tei*.rng *.compiled.* *~
