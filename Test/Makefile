LANGUAGE=en
ROMA=roma2
INPUTLANGUAGE=en
DOCUMENTATIONLANGUAGE=en
DRIVER=guidelines-${INPUTLANGUAGE}.xml
XSL=/usr/share/xml/tei/stylesheet/
ROMAOPTS=--isoschematron --doclang=${DOCUMENTATIONLANGUAGE} --localsource=`pwd`/../p5subset.xml
ANT=ANT_OPTS="-Xss2m -Xmx752m" ant -lib /usr/share/java/jing.jar:/usr/share/saxon/saxon9he.jar -Dtrang=/usr/share/trang/trang.jar  -DdefaultSource=`pwd`/../p5subset.xml -DXSL=${XSL} 

NORMALFILES= \
	testbare.dtd \
	testbasic.xsd \
	testplus.special 

EXTENDEDFILES = \
	testall.examples \
	testfand2.xsd \
	testfand3.xsd \
	testfand5.xsd \
	testconstraint2.xsd \
	testjustfs.xsd \
	testp4compat.xsd \
	testplace-kml.special 
#testi18n.xsd \

default: normal detest special testmeta2010 test-extns test-frag 

normal: $(NORMALFILES)
special: $(EXTENDEDFILES)

testall.examples: testall.xsd
	saxon -o:testall-ex.odd -s:testall.odd -xsl:../Utilities/odd2exodd.xsl 
	$(ANT) -f antruntest.xml -Doutputname=testall-examples -DoddFile=testall-ex.odd compileodd rng cleanup
	@echo check egXML in testall.odd
	perl -p -e "s/testlite-examples/testall-examples/" ex.nvdl > testall.nvdl
	../run-onvdl testall.nvdl testall.odd

%.dtd: %.odd
	$(ANT) -f antruntest.xml -Doutputname=$* -Dtestfile=$*.xml -DoddFile=$*.odd validateodd compileodd dtd rng validaterng cleanup
	@echo Check file using DOCTYPE in instance
	xmllint --noout --valid $*.xml 

%.xsd: %.odd
	$(ANT) -f antruntest.xml -Doutputname=$* -Dtestfile=$*.xml -DoddFile=$*.odd validateodd compileodd rng validaterng  isoschematron validateschematron cleanup
	trang   -o disable-abstract-elements $*.rng $*.xsd
	@echo Validate XSD using Jing ...
	cp xml.xsd.w3c xml.xsd
	perl -p -i -e 's+http://www.w3.org/2004/10/xml.xsd+xml.xsd+' $*.xsd
	-test -f teix.xsd && perl -p -i -e 's+<.*\"xml.xsd\".*++' teix.xsd
	-test -f spec.xsd && perl -p -i -e 's+<.*\"xml.xsd\".*++' spec.xsd
	-test -f teix.xsd && mv teix.xsd $*.teix.xsd
	-test -f $*.teix.xsd && perl -p -i -e "s/teix.xsd/$*.teix.xsd/" $*.xsd
	jing $*.xsd $*.xml

%.special: %.odd
	@echo "BUILD:  Test schema from (special) $<" 
	xmllint --xinclude $< > xi_$<
	$(ANT) -f antruntest.xml -Dtestfile=$*.xml -DoddFile=xi_$*.odd -Doutputname=$* validateodd compileodd rng validaterng cleanup
	trang $*.rng $*.rnc
	xmllint --noent --dropdtd  $*.xml | rnv $*.rnc  
	rm xi_$*.odd

testmeta2010:
	$(ANT) -f antruntest.xml -DoddFile=testmeta2010.odd -Dtestfile=testmeta2010.xml validateodd
	for j in `grep "<schemaSpec" testmeta2010.odd | sed 's/.*ident=.//' | sed 's/\".*//' | grep _old `; do \
		echo found schemaSpec $$j in testmeta2010; \
		$(ANT) -f antruntest.xml -DoddFile=testmeta2010.odd -Dtestfile=testmeta2010.xml -Doutputname=$$j -DautoGlobal=true -Dschema=$$j  compileodd rng validaterng cleanup; \
	done
	for j in `grep "<schemaSpec" testmeta2010.odd | sed 's/.*ident=.//' | sed 's/\".*//' | grep _new `; do \
		echo found schemaSpec $$j in testmeta2010; \
		$(ANT) -f antruntest.xml -DoddFile=testmeta2010.odd -Dtestfile=testmeta2010.xml -Doutputname=$$j -Dschema=$$j  compileodd rng validaterng cleanup; \
	done

test-frag:
	@echo BUILD: Testing files which exercise inclusion of fragments
	jing -t -c frag.rnc testfrag.xml
	rnv frag.rnc testfrag.xml 


test-extns:
	@echo BUILD: Testing files which demonstrate user extensions
	xmllint --noout --valid extns.xml

test-sgml:
	@echo BUILD: Test SGML extension mechanism
	onsgmls -e -g -s  sgmldecl.tei extns.sgm 

detest:
	@echo BUILD: Test file with deliberate mistakes
	$(ANT) -f antruntest.xml -Doutputname=detest -DoddFile=detest.odd compileodd rng isoschematron cleanup
	echo -jing -t detest.rng detest.xml  > detest.log.all
	trang detest.rng detest.rnc
	-rnv detest.rnc detest.xml 2>> detest.log.all
	@echo Validate using Schematron
	saxon detest.isosch ../Utilities/iso_schematron_message_xslt2.xsl generate-paths=true full-path-notation=2 > detest.xsl
	saxon detest.xml detest.xsl 2>> detest.log.all
	sed 's/.*detest.xml:/detest.xml:/' < detest.log.all | sed 's/Elapsed time.*//' >  detest.log
	diff detest.log expected-results/detest.log


clean:
	rm -f test*.doc.xml test*.rnc test*.dtd test*.compiled.* test*.teix.xsd test*.xsd test*.rnc test*.rng test*.xsl test*.isosch
	rm -rf LOG *~ *.xsd Schema RomaResults DTD 
	rm -rf *.doc.*
	rm -f detest.log detest.log.all
	rm -f *-examples.rng *-examples.rnc *test*.nvdl *-ex.odd
	rm -f detest.rnc    detest.rng   detest.dtd   detest.isosch



