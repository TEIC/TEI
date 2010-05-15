PREFIX=/usr
BIN=${PREFIX}/bin
ODDS=tei_all tei_allPlus tei_bare tei_corpus tei_dictionaries	\
	tei_drama tei_its tei_math tei_minimal tei_ms tei_odds	\
	tei_speech tei_svg tei_xinclude teilite tei_tite isofs tei_enrich
DOCODDS=tei_tite teilite
ROMA=roma2
LANGUAGE=en
DRIVER=guidelines-${LANGUAGE}.xml
XSL=/usr/share/xml/tei/stylesheet/
ROMAOPTS= --localsource=../Source/Guidelines/${LANGUAGE}/${DRIVER} --xsl=${XSL}

schemas: names
	xmllint --xinclude tei_odds.odd.pre > tei_odds.odd
	xmllint --xinclude tei_allPlus.odd.pre > tei_allPlus.odd
	for i in $(ODDS); do echo Process $$i; ${ROMA} ${ROMAOPTS} $$i.odd .; done
	for i in $(DOCODDS); do echo Process $$i; ${ROMA} ${ROMAOPTS} --noxsd --nodtd --norelax --docflags="summaryDoc=true" --docpdf --dochtml $$i.odd .; done
	-rm tei_odds.dtd tei_odds.xsd
	-rm tei_allPlus.dtd tei_allPlus.xsd
	-rm tei_svg.dtd tei_svg.xsd
	-rm tei_math.dtd tei_math.xsd
	-rm tei_its.dtd tei_its.xsd
	-rm tei_xinclude.dtd tei_xinclude.xsd

names:
	saxon -o names.xml ../Source/Guidelines/${LANGUAGE}/${DRIVER} ../Utilities/makenames.xsl  
	saxon -o exnames.xml ../Source/Guidelines/${LANGUAGE}/${DRIVER} ../Utilities/makeexnames.xsl  


dist: schemas
	rm -rf tei[0-9]*.xml ../release/tei-p5-exemplars
	mkdir -p ../release/tei-p5-exemplars/share/xml/tei/custom/schema/relaxng
	mkdir -p ../release/tei-p5-exemplars/share/xml/tei/custom/schema/dtd
	mkdir -p ../release/tei-p5-exemplars/share/xml/tei/custom/schema/xsd
	mkdir -p ../release/tei-p5-exemplars/share/xml/tei/custom/templates
	mkdir -p ../release/tei-p5-exemplars/share/xml/tei/custom/odd
	mkdir -p ../release/tei-p5-exemplars/share/doc/tei-p5-exemplars/html
	mkdir -p ../release/tei-p5-exemplars/share/doc/tei-p5-exemplars/xml
	mkdir -p ../release/tei-p5-exemplars/share/doc/tei-p5-exemplars/pdf
	cp `ls *.xml | grep -v '\.doc\.'` ../release/tei-p5-exemplars/share/xml/tei/custom/templates
	cp *.xsd ../release/tei-p5-exemplars/share/xml/tei/custom/schema/xsd
	cp *.dtd ../release/tei-p5-exemplars/share/xml/tei/custom/schema/dtd
	cp *.dtd ../release/tei-p5-exemplars/share/xml/tei/custom/schema/dtd
	cp *.odd *.xsl ../release/tei-p5-exemplars/share/xml/tei/custom/odd
	cp *names.xml ../release/tei-p5-exemplars/share/xml/tei/custom/odd
	cp *.rnc *.rng *.mod ../release/tei-p5-exemplars/share/xml/tei/custom/schema/relaxng
	-for i in ${DOCODDS}; do  \
	cp $$i.doc.html ../release/tei-p5-exemplars/share/doc/tei-p5-exemplars/html ; \
	cp $$i.doc.xml  ../release/tei-p5-exemplars/share/doc/tei-p5-exemplars/xml ; \
	test -f $$i.doc.pdf && cp $$i.doc.pdf  ../release/tei-p5-exemplars/share/doc/tei-p5-exemplars/pdf ; \
	done
	cp ../catalog.p5.custom ../release/tei-p5-exemplars/share/xml/tei/custom/schema/catalog.xml
	(cd ../release; 	\
	ln -s tei-p5-exemplars tei-p5-exemplars-`cat ../VERSION` ; \
	zip -r tei-p5-exemplars-`cat ../VERSION`.zip tei-p5-exemplars-`cat ../VERSION` )


clean:
	-rm *.xsd *.dtd *.doc.* *.rnc tei*.rng *.compiled.* *~
	-rm tei_allPlus.odd
	-rm exnames.xml
	-rm enrich.rng isofs.rng
	-rm tei_odds.odd
	-rm names.xml
