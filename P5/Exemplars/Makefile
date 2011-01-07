PREFIX=/usr
BIN=${PREFIX}/bin
ODDS=tei_allPlus tei_bare tei_corpus tei_dictionaries	\
	tei_drama tei_its tei_math tei_minimal tei_ms tei_odds	\
	tei_speech tei_svg tei_xinclude isofs tei_enrich \
	tei_all 
DOCODDS=tei_tite teilite
ROMA=roma2
LANGUAGE=en
DRIVER=guidelines-${LANGUAGE}.xml
XSL=/usr/share/xml/tei/stylesheet/
ROMAOPTS= --localsource=../p5subset.xml --xsl=${XSL}

schemas: 
	${ROMA} ${ROMAOPTS} teilite.odd .
	for i in $(DOCODDS); do echo Process $$i; ${ROMA} ${ROMAOPTS} --noxsd --nodtd --norelax --docflags="summaryDoc=true" --docpdf --dochtml $$i.odd .; echo TEST schema; jing $$i.rng $$i.xml && echo $$i validates OK;done
	-for i in $(ODDS) ; do echo Process $$i; xmllint --xinclude $$i.odd > $$i.xi;${ROMA} ${ROMAOPTS} $$i.xi .; echo TEST schema; jing $$i.rng $$i.xml && echo $$i validates OK ;done
	-rm tei_odds.dtd tei_odds.xsd
	-rm tei_allPlus.dtd tei_allPlus.xsd
	-rm tei_svg.dtd tei_svg.xsd
	-rm tei_math.dtd tei_math.xsd
	-rm tei_its.dtd tei_its.xsd
	-rm tei_xinclude.dtd tei_xinclude.xsd

names:
	saxon -o:exnames.xml ../Source/Guidelines/${LANGUAGE}/${DRIVER} ../Utilities/makeexnames.xsl  


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
	cp *.odd *.xsl ../release/tei-p5-exemplars/share/xml/tei/custom/odd
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
	-rm *.xsd *.dtd *.doc.* *.rnc tei*.rng *.compiled.* *~ *.xi
	-rm exnames.xml
	-rm enrich.rng isofs.rng
	-rm names.xml
