PREFIX=/usr
BIN=${PREFIX}/bin
ODDS= \
	tei_bare.dtd \
	tei_corpus.dtd \
	tei_dictionaries.dtd	\
	tei_drama.dtd \
	tei_minimal.dtd \
	tei_ms.dtd \
	tei_speech.dtd \
	tei_all.dtd 

XODDS=	isofs.rng \
	tei_allPlus.rng \
	tei_svg.rng \
	tei_enrich.dtd \
	tei_xinclude.rng \
	tei_its.rng \
	tei_math.rng \
	tei_odds.rng	

DOCODDS=tei_tite.doc.pdf tei_lite.doc.pdf
ROMA=roma2
LANGUAGE=en
P5=`(cd ..; pwd)`
DRIVER=guidelines-${LANGUAGE}.xml
XSL=/usr/share/xml/tei/stylesheet/
ROMAOPTS=--xsl=${XSL} --isoschematron
SOURCE=--localsource=${P5}/p5subset.xml 


%.dtd: %.odd
	@echo "BUILD: Test schema from $<"
	xmllint --xinclude $*.odd > $*.xi
	@echo Validate $< itself
	which rnv && rnv ../p5odds.rnc $*.xi
	${ROMA} ${ROMAOPTS} ${SOURCE} $*.odd .
	@echo Test schema built from $<
	jing $*.rng $*.xml && echo $* validates OK
	@echo Validate using xmllint for DTD  
	xmllint --dropdtd $*.xml | xmllint --noout --dtdvalid $*.dtd -
	@echo Validate using Schematron
	saxon $*.isosch ../Utilities/iso_schematron_message_xslt2.xsl  > $*.xsl
	saxon $*.xml $*.xsl


%.rng: %.odd
	@echo "BUILD: Test schema from $<"
	xmllint --xinclude $*.odd > $*.xi
	@echo Validate $< itself
	which rnv && rnv ../p5odds.rnc $*.xi
	${ROMA} ${ROMAOPTS} ${SOURCE} $*.odd .
	@echo Test schema built from $<
	jing $*.rng $*.xml && echo $* validates OK
	@echo Validate using Schematron
	saxon $*.isosch ../Utilities/iso_schematron_message_xslt2.xsl  > $*.xsl
	saxon $*.xml $*.xsl

%.doc.pdf: %.odd
	@echo "BUILD: Test schema from $<"
	@echo Validate $< itself
	which rnv && rnv ../p5odds.rnc $< 
	${ROMA} ${ROMAOPTS} ${SOURCE}--docflags="summaryDoc=true" --docpdf --dochtml $< .
	teitoepub ${SOURCE} --summaryDoc --profile=tei --odd $< $*.doc.epub
	@echo Test schema built from $<
	jing $*.rng $*.xml && echo $* validates OK
	@echo Validate using xmllint for DTD  
	xmllint --dropdtd $*.xml | xmllint --noout --dtdvalid $*.dtd -
	@echo Validate using Schematron
	saxon $*.isosch ../Utilities/iso_schematron_message_xslt2.xsl  > $*.xsl
	saxon $*.xml $*.xsl

schemas: $(ODDS) $(XODDS) $(DOCODDS)
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
	mkdir -p ../release/tei-p5-exemplars/share/doc/tei-p5-exemplars/epub
	cp `ls *.xml | grep -v '\.doc\.'` ../release/tei-p5-exemplars/share/xml/tei/custom/templates
	cp *.xsd ../release/tei-p5-exemplars/share/xml/tei/custom/schema/xsd
	cp *.dtd ../release/tei-p5-exemplars/share/xml/tei/custom/schema/dtd
	cp *.odd *.xsl ../release/tei-p5-exemplars/share/xml/tei/custom/odd
	cp *.rnc *.rng *.mod ../release/tei-p5-exemplars/share/xml/tei/custom/schema/relaxng
	cp *.doc.html ../release/tei-p5-exemplars/share/doc/tei-p5-exemplars/html 
	cp *.doc.pdf ../release/tei-p5-exemplars/share/doc/tei-p5-exemplars/pdf 
	cp *.doc.epub ../release/tei-p5-exemplars/share/doc/tei-p5-exemplars/epub
	cp ../catalog.p5.custom ../release/tei-p5-exemplars/share/xml/tei/custom/schema/catalog.xml

clean:
	rm -f *.xsd *.dtd *.doc.* *.rnc tei*.rng *.compiled.* *~ *.xi
	rm -f exnames.xml
	rm -f enrich.rng isofs.rng
	rm -f names.xml
