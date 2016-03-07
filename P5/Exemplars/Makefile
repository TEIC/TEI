PREFIX=/usr
SAXON=java -Xmx752m -jar ../Utilities/lib/saxon9he.jar -ext:on
TRANG=java -jar ../Utilities/lib/trang.jar
JING=java -jar ../Utilities/lib/jing.jar
BIN=${PREFIX}/bin
NORMALODDS= \
	tei_bare.dtd \
	tei_corpus.dtd \
	tei_drama.dtd \
	tei_minimal.dtd \
	tei_ms.dtd \
	tei_speech.dtd \
	tei_enrich.dtd \
	tei_all.dtd \
	tei_tite.dtd \
	tei_lite.dtd

ODDSJUSTRELAX=	isofs.rng \
	tei_svg.rng \
	tei_xinclude.rng \
	tei_its.rng \
	tei_math.rng \
	tei_jtei.rng \
	tei_odds.rng

DOCONLYODDS=tei_lite_fr.doc.epub

SPECIALODDS = tei_allPlus.special

DOCODDS=tei_tite.doc.pdf tei_lite.doc.pdf tei_jtei.doc.pdf

ADDITIONALXSDS=tei_jtei.xsd

LANGUAGE=en
P5=`(cd ..; pwd)`
DRIVER=guidelines-${LANGUAGE}.xml
XSL=/usr/share/xml/tei/stylesheet/
SOURCE=${P5}/p5subset.xml
ANT=ANT_OPTS="-Xss2m -Xmx752m -Djava.awt.headless=true" ant -lib ../Utilities/lib/saxon9he.jar:../Utilities/lib/jing.jar -Dtrang=../Utilities/lib/trang.jar  -Dprofile=tei -DdefaultSource=${SOURCE} -DXSL=${XSL} -Dbasedir=`pwd`

default: schemas

schemas: $(NORMALODDS) $(ODDSJUSTRELAX) $(SPECIALODDS) $(ADDITIONALXSDS)
	echo Done

docschemas: $(DOCODDS) $(DOCONLYODDS)
	echo Done schemas which needed doc

%.special: %.odd
	xmllint --xinclude $*.odd > $*.odd.xinclude
	$(ANT) -f ../Test/antruntest.xml -Doutputname=$* -Dtestfile2=$*.template -Dtestfile=$*.tei -DoddFile=$*.odd.xinclude validateodd compileodd rng validaterng validatesecondrng isoschematron validateschematron exampleschema cleanup
	../run-onvdl $*.nvdl $*.odd.xinclude
	rm $*.odd.xinclude
	${TRANG} $*.rng $*.rnc

%.rng: %.odd
	$(ANT) -f ../Test/antruntest.xml -Doutputname=$* -Dtestfile=$*.tei -DoddFile=$*.odd validateodd compileodd dtd rng validaterng  isoschematron validateschematron cleanup
	${TRANG}  $*.rng $*.rnc

%.dtd: %.odd
	$(ANT) -f ../Test/antruntest.xml -Doutputname=$* -Dtestfile=$*.tei -DoddFile=$*.odd validateodd compileodd rng dtd exampleschema isoschematron validateschematron cleanup
	../run-onvdl $*.nvdl $*.odd
	${TRANG}  $*.rng $*.rnc
	${TRANG}   -o disable-abstract-elements $*.rng $*.xsd
	-test -f xml.xsd && mv xml.xsd $*_xml.xsd                && perl -p -i -e "s+teix.xsd+$*_teix.xsd+;s+ns1.xsd+$*_ns1.xsd+" $*_xml.xsd && perl -p -i -e "s+xml.xsd+$*_xml.xsd+" $*.xsd
	-test -f teix.xsd && mv teix.xsd $*_teix.xsd && perl -p -i -e "s+xml.xsd+$*_xml.xsd+;s+ns1.xsd+$*_ns1.xsd+" $*_teix.xsd      && perl -p -i -e "s/teix.xsd/$*_teix.xsd/" $*.xsd
	-test -f dcr.xsd && mv dcr.xsd $*_dcr.xsd                && perl -p -i -e "s+xml.xsd+$*_xml.xsd+;s+ns1.xsd+$*_ns1.xsd+;s+teix.xsd+$*_teix.xsd+" $*_dcr.xsd  && perl -p -i -e "s/dcr.xsd/$*_dcr.xsd/" $*.xsd && perl -p -i -e "s/dcr.xsd/$*_dcr.xsd/" $*_xml.xsd
	-test -f spec.xsd && mv spec.xsd $*_spec.xsd && perl -p -i -e "s+xml.xsd+$*_xml.xsd+;s+ns1.xsd+$*_ns1.xsd+;s+teix.xsd+$*_teix.xsd+" $*_spec.xsd && perl -p -i -e "s/spec.xsd/$*_spec.xsd/" $*.xsd
	-test -f ns1.xsd && mv ns1.xsd $*_ns1.xsd    && perl -p -i -e "s+xml.xsd+$*_xml.xsd+" $*_ns1.xsd  && perl -p -i -e "s/ns1.xsd/$*_ns1.xsd/" $*.xsd
	-test -f tite.xsd && mv tite.xsd $*_tite.xsd && perl -p -i -e "s+xml.xsd+$*_xml.xsd+" $*_tite.xsd && perl -p -i -e "s/tite.xsd/$*_tite.xsd/" $*.xsd
	@echo Validate XSD using Jing ...
	${JING} $*.xsd $*.tei
	@echo Validate using xmllint for DTD
	xmllint --noout --dtdvalid $*.dtd $*.tei

%.doc.pdf: %.odd
	$(ANT) -f ../Test/antruntest.xml -Doutputname=$* -Dtestfile=$*.tei -DoddFile=$*.odd validateodd compileodd docepub docpdf dochtml cleanup

%.doc.epub: %.odd
	$(ANT) -f ../Test/antruntest.xml -Doutputname=$* -Dlang=`echo $* | sed 's/.*_//'` -Dtestfile=$*.tei -DoddFile=$*.odd validateodd compileodd rng docepub docpdf dochtml exampleschema cleanup
	../run-onvdl $*.nvdl $*.odd
	-rm $*.doc.xml
	-rm $*.rng

%.xsd: %.rng
	${TRANG}   -o disable-abstract-elements $*.rng $*.xsd
	-test -f xml.xsd && mv xml.xsd $*_xml.xsd                && perl -p -i -e "s+teix.xsd+$*_teix.xsd+;s+ns1.xsd+$*_ns1.xsd+" $*_xml.xsd && perl -p -i -e "s+xml.xsd+$*_xml.xsd+" $*.xsd
	-test -f teix.xsd && mv teix.xsd $*_teix.xsd && perl -p -i -e "s+xml.xsd+$*_xml.xsd+;s+ns1.xsd+$*_ns1.xsd+" $*_teix.xsd      && perl -p -i -e "s/teix.xsd/$*_teix.xsd/" $*.xsd
	-test -f dcr.xsd && mv dcr.xsd $*_dcr.xsd                && perl -p -i -e "s+xml.xsd+$*_xml.xsd+;s+ns1.xsd+$*_ns1.xsd+;s+teix.xsd+$*_teix.xsd+" $*_dcr.xsd  && perl -p -i -e "s/dcr.xsd/$*_dcr.xsd/" $*.xsd && perl -p -i -e "s/dcr.xsd/$*_dcr.xsd/" $*_xml.xsd
	-test -f spec.xsd && mv spec.xsd $*_spec.xsd && perl -p -i -e "s+xml.xsd+$*_xml.xsd+;s+ns1.xsd+$*_ns1.xsd+;s+teix.xsd+$*_teix.xsd+" $*_spec.xsd && perl -p -i -e "s/spec.xsd/$*_spec.xsd/" $*.xsd
	-test -f ns1.xsd && mv ns1.xsd $*_ns1.xsd    && perl -p -i -e "s+xml.xsd+$*_xml.xsd+" $*_ns1.xsd  && perl -p -i -e "s/ns1.xsd/$*_ns1.xsd/" $*.xsd
	-test -f tite.xsd && mv tite.xsd $*_tite.xsd && perl -p -i -e "s+xml.xsd+$*_xml.xsd+" $*_tite.xsd && perl -p -i -e "s/tite.xsd/$*_tite.xsd/" $*.xsd
	@echo Validate XSD using Jing ...
	${JING} $*.xsd $*.tei

dist: schemas docschemas
	rm -rf tei[0-9]*.xml ../release/tei-p5-exemplars
	rm -f *-teix*
	rm -f *compiled* *.doc.tex
	mkdir -p ../release/tei-p5-exemplars/share/xml/tei/custom/schema/relaxng
	mkdir -p ../release/tei-p5-exemplars/share/xml/tei/custom/schema/dtd
	mkdir -p ../release/tei-p5-exemplars/share/xml/tei/custom/schema/xsd
	mkdir -p ../release/tei-p5-exemplars/share/xml/tei/custom/templates
	mkdir -p ../release/tei-p5-exemplars/share/xml/tei/custom/odd
	mkdir -p ../release/tei-p5-exemplars/share/doc/tei-p5-exemplars/html
	mkdir -p ../release/tei-p5-exemplars/share/doc/tei-p5-exemplars/xml
	mkdir -p ../release/tei-p5-exemplars/share/doc/tei-p5-exemplars/pdf
	mkdir -p ../release/tei-p5-exemplars/share/doc/tei-p5-exemplars/epub
	for i in *.template; do $(SAXON) -it:main make_template.xsl file=`basename $$i .template`;done
	cp *.tei *.odd *.xsl ../release/tei-p5-exemplars/share/xml/tei/custom/odd
	cp *.xml  ../release/tei-p5-exemplars/share/xml/tei/custom/templates
	-rm -f../release/tei-p5-exemplars/share/xml/tei/custom/templates/*.doc.xml
	mv *.properties  ../release/tei-p5-exemplars/share/xml/tei/custom/templates
	mv *.xsd ../release/tei-p5-exemplars/share/xml/tei/custom/schema/xsd
	mv *.dtd ../release/tei-p5-exemplars/share/xml/tei/custom/schema/dtd
	mv isofs.rng tei*.rnc tei*.rng ../release/tei-p5-exemplars/share/xml/tei/custom/schema/relaxng
	cp mathml*rng relaxng.rng its.rng svg*rng *.mod ../release/tei-p5-exemplars/share/xml/tei/custom/schema/relaxng
	cp -r images ../release/tei-p5-exemplars/share/doc/tei-p5-exemplars/html
	mv *.doc.html ../release/tei-p5-exemplars/share/doc/tei-p5-exemplars/html
	mv *.doc.pdf ../release/tei-p5-exemplars/share/doc/tei-p5-exemplars/pdf
	mv *.doc.epub ../release/tei-p5-exemplars/share/doc/tei-p5-exemplars/epub
	cp ../catalog.p5.custom ../release/tei-p5-exemplars/share/xml/tei/custom/schema/catalog.xml

clean:
	mv tei_lite_fr.nvdl tei_lite_fr.nvdl.bak
	rm -f *.processed *.xsd *.dtd *.doc.* *.rnc tei*.xsl tei*.rng *.compiled* *~ *.xi *.isosch *.epub *.pdf *.html *.nvdl *-teix*
	mv tei_lite_fr.nvdl.bak tei_lite_fr.nvdl
	rm -f exnames.xml
	rm -f enrich.rng isofs.rng
	rm -f names.xml *properties
	rm -f [A-Z]*xml
