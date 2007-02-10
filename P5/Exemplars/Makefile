PREFIX=/usr/bin
LANGUAGE=en
DRIVER=guidelines-${LANGUAGE}.xml
XSL=/usr/share/xml/tei/stylesheet/
ROMAOPTS=--dochtml --localsource=../Source/Guidelines/${LANGUAGE}/${DRIVER} --xsl=${XSL}

schemas:
	xsltproc Utilities/makenames.xsl  ../Source/Guidelines/${LANGUAGE}/${DRIVER} > names.xml
	for i in *.odd; do echo Process $$i; ${PREFIX}/roma ${ROMAOPTS} $$i .; done
	-rm tei_odds.dtd tei_odds.xsd
	-rm tei_allPlus.dtd tei_allPlus.xsd
	-rm tei_svg.dtd tei_svg.xsd
	-rm tei_math.dtd tei_math.xsd
	-rm tei_xinclude.dtd tei_xinclude.xsd


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
