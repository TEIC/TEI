PREFIX=/usr
BIN=${PREFIX}/bin
ODDS= \
	tei_bare.dtd \
	tei_corpus.dtd \
	tei_drama.dtd \
	tei_minimal.dtd \
	tei_ms.dtd \
	tei_speech.dtd \
	tei_all.dtd 

XODDS=	isofs.rng \
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
SOURCE=${P5}/p5subset.xml 
ANT=ANT_OPTS="-Xss2m -Xmx752m" ant -lib /usr/share/java/jing.jar:/usr/share/saxon/saxon9he.jar -Dtrang=/usr/share/trang/trang.jar  -Dprofile=tei -DdefaultSource=${SOURCE} -DXSL=${XSL} -Dbasedir=`pwd`


%.rng: %.odd 
	$(ANT) -f ../Test/antruntest.xml -Doutputname=$* -Dtestfile2=$*.template -Dtestfile=$*.tei -DoddFile=$*.odd validateodd compileodd dtd rng validaterng validatesecondrng isoschematron validateschematron 
	trang   -o disable-abstract-elements $*.rng $*.xsd
	@echo Validate XSD using Jing ...
	perl -p -i -e 's+http://www.w3.org/2004/10/xml.xsd+xml.xsd+' $*.xsd
	-test -f teix.xsd && perl -p -i -e 's+<.*\"xml.xsd\".*++' teix.xsd
	-test -f spec.xsd && perl -p -i -e 's+<.*\"xml.xsd\".*++' spec.xsd
	-test -f teix.xsd && mv teix.xsd $*.teix.xsd
	-test -f $*.teix.xsd && perl -p -i -e "s/teix.xsd/$*.teix.xsd/" $*.xsd

%.dtd: %.rng
	$(ANT) -f ../Test/antruntest.xml -Doutputname=$* -Dtestfile=$*.tei -DoddFile=$*.odd  dtd cleanup
	@echo Validate using xmllint for DTD  
	xmllint --noout --dtdvalid $*.dtd $*.tei


%.doc.pdf: %.rng
	$(ANT) -f ../Test/antruntest.xml -Doutputname=$* -Dtestfile=$*.tei -DoddFile=$*.odd dtd docepub docpdf dochtml cleanup
	@echo Validate using xmllint for DTD  
	xmllint --noout --dtdvalid $*.dtd $*.tei
	-rm $*.doc.xml

schemas: $(ODDS) $(XODDS) $(DOCODDS)
	xmllint --xinclude tei_allPlus.odd > tei_allPlus.odd.xinclude
	$(ANT) -f ../Test/antruntest.xml -Doutputname=tei_allPlus -Dtestfile2=tei_allPlus.template -Dtestfile=tei_allPlus.tei -DoddFile=tei_allPlus.odd.xinclude validateodd compileodd rng validaterng validatesecondrng isoschematron validateschematron 
	rm tei_allPlus.odd.xinclude
	trang   -o disable-abstract-elements tei_allPlus.rng tei_allPlus.xsd
	-rm *.odd.compiled
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
	for i in *.template; do saxon -it:main make_template.xsl file=`basename $$i .template`;done
	cp *.tei *.odd *.xsl ../release/tei-p5-exemplars/share/xml/tei/custom/odd
	cp *.xml  ../release/tei-p5-exemplars/share/xml/tei/custom/templates
	-rm ../release/tei-p5-exemplars/share/xml/tei/custom/templates/*.doc.xml
	cp *.properties  ../release/tei-p5-exemplars/share/xml/tei/custom/templates
	cp *.xsd ../release/tei-p5-exemplars/share/xml/tei/custom/schema/xsd
	cp *.dtd ../release/tei-p5-exemplars/share/xml/tei/custom/schema/dtd
	cp *.rnc *.rng *.mod ../release/tei-p5-exemplars/share/xml/tei/custom/schema/relaxng
	cp *.doc.html ../release/tei-p5-exemplars/share/doc/tei-p5-exemplars/html 
	cp *.doc.pdf ../release/tei-p5-exemplars/share/doc/tei-p5-exemplars/pdf 
	cp *.doc.epub ../release/tei-p5-exemplars/share/doc/tei-p5-exemplars/epub
	cp ../catalog.p5.custom ../release/tei-p5-exemplars/share/xml/tei/custom/schema/catalog.xml

clean:
	rm -f *.xsd *.dtd *.doc.* *.rnc tei*.rng *.compiled.* *~ *.xi *.isosch 
	rm -f exnames.xml
	rm -f enrich.rng isofs.rng
	rm -f names.xml
	for i in $(ODDS); do  rm -f  `basename $$i .dtd`.xsl;done
	for i in $(XODDS); do  rm -f  `basename $$i .rng`.xsl;done
	for i in $(DOCODDS); do  rm -f  `basename $$i .doc.pdf`.xsl;done
