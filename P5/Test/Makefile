LANGUAGE=en
INPUTLANGUAGE=en
DOCUMENTATIONLANGUAGE=en
DRIVER=guidelines-${INPUTLANGUAGE}.xml
XSL=/usr/share/xml/tei/stylesheet/
TRANG=java -jar ../Utilities/lib/trang.jar
JING=java -jar ../Utilities/lib/jing.jar
SAXON=java -Xmx752m -jar ../Utilities/lib/Saxon-HE-9.4.0.6.jar -ext:on
ANT=ANT_OPTS="-Xss2m -Xmx752m" ant -lib ../Utilities/lib/Saxon-HE-9.4.0.6.jar:../Utilities/lib/jing.jar -Dtrang=../Utilities/lib/trang.jar  -DdefaultSource=`pwd`/../p5subset.xml -DXSL=${XSL} 

NORMALFILES= \
	testbare.dtd \
	testbasic.xsd \
	testall.xsd \
	testplus.special 

EXTENDEDFILES = \
	testjustfs.xsd \
	testp4compat.xsd \
	testplace-kml.special 
#testi18n.xsd \

default: normal special testmeta2010 test-extns test-frag detest

normal: $(NORMALFILES)
special: $(EXTENDEDFILES)

testall.examples: testall.xsd
	$(SAXON) -o:testall-ex.odd -s:testall.odd -xsl:../Utilities/odd2exodd.xsl 
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
	${TRANG}   -o disable-abstract-elements $*.rng $*.xsd
	@echo Validate XSD using Jing ...
	cp xml.xsd.w3c xml.xsd
	perl -p -i -e 's+http://www.w3.org/2004/10/xml.xsd+xml.xsd+' $*.xsd
	-test -f teix.xsd && perl -p -i -e 's+<.*\"xml.xsd\".*++' teix.xsd
	-test -f spec.xsd && perl -p -i -e 's+<.*\"xml.xsd\".*++' spec.xsd
	-test -f teix.xsd && mv teix.xsd $*.teix.xsd
	-test -f $*.teix.xsd && perl -p -i -e "s/teix.xsd/$*.teix.xsd/" $*.xsd
	${JING} $*.xsd $*.xml

%.special: %.odd
	@echo "BUILD:  Test schema from (special) $<" 
	xmllint --xinclude $< > xi_$<
	$(ANT) -f antruntest.xml -Dtestfile=$*.xml -DoddFile=xi_$*.odd -Doutputname=$* validateodd compileodd rng validaterng cleanup
	${TRANG} $*.rng $*.rnc
	xmllint --noent --dropdtd  $*.xml | rnv $*.rnc  
	rm xi_$*.odd

testmeta2010:
	echo check that TEI web site is up and responding
	curl -s --head http://www.tei-c.org/Vault/P5/1.5.0/xml/tei/odd/p5subset.xml
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
	${JING} -t -c frag.rnc testfrag.xml
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
	${TRANG} detest.rng detest.rnc
	@echo Validate ODD file using Schematron
	$(ANT) -f antruntest.xml -Doutputname=detest -DoddFile=detest.odd validateodd > detest1.log 2>&1
	@echo Validate ODD file using RELAX NG schema
	-rnv detest.rnc detest.xml > detest2.log 2>&1
	@echo Validate XML file using Schematron
	$(SAXON) detest.xml detest.xsl > detest3.log 2>&1
	grep "\[xslt\]" detest1.log | sed 's/Processing.*detest.odd//' | sed 's/Loading.*message.isosch.xsl//' > detest.log
	sed 's/.*detest.xml:/detest.xml:/' detest2.log >> detest.log
	sed 's/.*detest.xml:/detest.xml:/' detest3.log >> detest.log
	diff detest.log expected-results/detest.log
	rm -f detest*log

clean:
	rm -f test*.doc.xml test*.rnc test*.dtd test*.compiled.* test*.teix.xsd test*.xsd test*.rnc test*.rng test*.xsl test*.isosch
	rm -rf LOG *~ *.xsd Schema RomaResults DTD 
	rm -rf *.doc.*
	rm -f detest.log detest.log.all
	rm -f *-examples.rng *-examples.rnc *test*.nvdl *-ex.odd
	rm -f detest.rnc    detest.rng   detest.dtd   detest.isosch



