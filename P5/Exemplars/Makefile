PREFIX=/usr
SAXON=java -Xmx752m -jar ../Utilities/lib/saxon-9.1.0.8.jar -ext:on
BIN=${PREFIX}/bin
ODDS= \
	tei_bare.dtd \
	tei_corpus.dtd \
	tei_drama.dtd \
	tei_minimal.dtd \
	tei_ms.dtd \
	tei_speech.dtd \
	tei_all.dtd 

MOREODDS=	isofs.rng \
	tei_svg.rng \
	tei_enrich.dtd \
	tei_xinclude.rng \
	tei_its.rng \
	tei_math.rng \
	tei_odds.rng

XODDS = tei_allPlus.special	

DOCODDS=tei_tite.doc.pdf tei_lite.doc.pdf
ROMA=roma2
LANGUAGE=en
P5=`(cd ..; pwd)`
DRIVER=guidelines-${LANGUAGE}.xml
XSL=/usr/share/xml/tei/stylesheet/
ROMAOPTS=--xsl=${XSL} --isoschematron
SOURCE=${P5}/p5subset.xml 
ANT=ANT_OPTS="-Xss2m -Xmx752m" ant -lib /usr/share/java/jing.jar:/usr/share/saxon/saxon9he.jar -Dtrang=/usr/share/trang/trang.jar  -Dprofile=tei -DdefaultSource=${SOURCE} -DXSL=${XSL} -Dbasedir=`pwd`

default: schemas

%.special: %.odd
	xmllint --xinclude $*.odd > $*.odd.xinclude
	$(ANT) -f ../Test/antruntest.xml -Doutputname=$* -Dtestfile2=$*.template -Dtestfile=$*.tei -DoddFile=$*.odd.xinclude validateodd compileodd rng rnc validaterng validatesecondrng isoschematron validateschematron 
	rm $*.odd.xinclude

%.rng: %.odd 
	$(ANT) -f ../Test/antruntest.xml -Doutputname=$* -Dtestfile2=$*.template -Dtestfile=$*.tei -DoddFile=$*.odd validateodd compileodd dtd rng rnc validaterng validatesecondrng isoschematron validateschematron 

%.dtd: %.odd
	$(ANT) -f ../Test/antruntest.xml -Doutputname=$* -Dtestfile=$*.tei -DoddFile=$*.odd validateodd compileodd rng dtd xsd rnc isoschematron validateschematron cleanup
	@echo Validate XSD using Jing ...
	perl -p -i -e 's+http://www.w3.org/2004/10/xml.xsd+xml.xsd+' $*.xsd
	-test -f teix.xsd && perl -p -i -e 's+<.*\"xml.xsd\".*++' teix.xsd
	-test -f spec.xsd && perl -p -i -e 's+<.*\"xml.xsd\".*++' spec.xsd
	-test -f teix.xsd && mv teix.xsd $*.teix.xsd
	-test -f $*.teix.xsd && perl -p -i -e "s/teix.xsd/$*.teix.xsd/" $*.xsd
	jing $*.xsd $*.tei
	@echo Validate using xmllint for DTD  
	xmllint --noout --dtdvalid $*.dtd $*.tei


%.doc.pdf: %.odd
	$(ANT) -f ../Test/antruntest.xml -Doutputname=$* -Dtestfile=$*.tei -DoddFile=$*.odd validateodd compileodd rng dtd docepub docpdf dochtml cleanup
	@echo Validate using xmllint for DTD  
	xmllint --noout --dtdvalid $*.dtd $*.tei
	-rm $*.doc.xml

schemas: $(ODDS) $(MOREODDS) $(DOCODDS) $(XODDS)
	echo Done

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
	rm -f *compiled* *.doc.tex
	for i in *.template; do $(SAXON) -it:main make_template.xsl file=`basename $$i .template`;done
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
	rm -f *.xsd *.dtd *.doc.* *.rnc tei*.xsl tei*.rng *.compiled* *~ *.xi *.isosch *.epub *.pdf *.html 
	rm -f exnames.xml
	rm -f enrich.rng isofs.rng
	rm -f names.xml

