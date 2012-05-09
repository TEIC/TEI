LANGUAGE=en
ROMA=roma2
INPUTLANGUAGE=en
DOCUMENTATIONLANGUAGE=en
DRIVER=guidelines-${INPUTLANGUAGE}.xml
XSL=/usr/share/xml/tei/stylesheet/
ROMAOPTS=--isoschematron --doclang=${DOCUMENTATIONLANGUAGE} --localsource=`pwd`/../p5subset.xml

TESTFILES= \
	testClasses.dtd \
	testbare.dtd \
	testcardinality.dtd \
	testdictionaries.dtd \
	testecco.dtd \
	testfand.dtd \
	testfand2.dtd \
	testfand3.dtd \
	testfand4.dtd \
	testfand5.dtd \
	testfs.dtd \
	testlite.dtd \
	testglobals.dtd \
	testlangs.dtd 

TESTFILES2 = \
	testall.xsd \
	testconstraint.xsd \
	testconstraint2.xsd \
	testi18n.xsd \
	testjustfs.xsd \
	testmav.xsd \
	testns.xsd \
	testp4compat.xsd \
	testrendition.xsd \
	testtranscr.xsd 

TESTSPECIALFILES= \
	testallplus.special \
	testits.special \
	testmathml.special \
	testodds.special \
	testtagdocs.special \
	testplace-gml.special \
	testplace-kml.special \
	testrend.special \
	testsvg.special  

TESTBADFILES= \
	detest 


default: normal normaldtd special testmeta2010 test-extns test-frag detest

normal: $(TESTFILES2)
normaldtd: $(TESTFILES)
special: $(TESTSPECIALFILES)

%.rng: %.odd
	@echo "BUILD: Test schema from $<"
	@echo Validate $< itself
	rnv ../p5odds.rnc $< 
	saxon -o:$*-ex.odd $< ../Utilities/odd2exodd.xsl 
	$(ROMA) $(ROMAOPTS) --nodtd --noxsd --xsl=$(XSL) $*-ex.odd . 
	@echo check egXML in $<
	perl -p -e "s/testlite-examples/$*-examples/" ex.nvdl > $*.nvdl
	../run-onvdl $*.nvdl $< 
	@echo "Run Roma on $<" 
	$(ROMA) $(ROMAOPTS) --xsl=$(XSL) $< .
	@echo Validate using Jing for RELAX NG... 
	jing -t $*.rng $*.xml
	@echo Validate using RNV.... 
	xmllint --noent --dropdtd  $*.xml | rnv $*.rnc 
	@echo Validate using xmllint for DTD  
	xmllint --dropdtd $*.xml | xmllint --noout --dtdvalid $*.dtd -
	@echo Validate using Schematron
	saxon $*.isosch ../Utilities/iso_schematron_message_xslt2.xsl  > $*.xsl
	saxon $*.xml $*.xsl

%.dtd: %.rng
	@echo Check file using DOCTYPE in instance
	xmllint --noout --valid $*.xml 

%.xsd: %.rng
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
	@echo Try  to validate $<
	xmllint --xinclude $< > xi_$<
	rnv ../p5odds.rnc xi_$*.odd
	$(ROMA) --noxsd --nodtd --patternprefix=tei_ $(ROMAOPTS) --xsl=$(XSL)   xi_$< .
	@echo JING RelaxNG... 
	jing -t $*.rng $*.xml
	xmllint --noent --dropdtd  $*.xml | rnv $*.rnc  
	rm xi_$*.odd

testmeta2010:
	for j in `grep "<schemaSpec" testmeta2010.odd | sed 's/.*ident=.//' | sed 's/\".*//' | grep _old `; do \
		echo found schemaSpec $$j in testmeta2010; \
		$(ROMA) $(ROMAOPTS) --autoglobal --nodtd --noxsd --xsl=$(XSL) --schema=$$j  testmeta2010.odd .; \
		echo Now validating instances documents against $$j schema; \
		echo Validate using Jing for RelaxNG... ; \
		jing -t $$j.rng testmeta2010.xml; \
		echo Validate using RNV.... ; \
		xmllint --noent --dropdtd  testmeta2010.xml | rnv $$j.rnc ;   \
	done
	for j in `grep "<schemaSpec" testmeta2010.odd | sed 's/.*ident=.//' | sed 's/\".*//' | grep _new `; do \
		echo found schemaSpec $$j in testmeta2010; \
		$(ROMA) $(ROMAOPTS) --nodtd --noxsd --xsl=$(XSL) --schema=$$j  testmeta2010.odd .; \
		echo Now validating instances documents against $$j schema; \
		echo Validate using Jing for RelaxNG... ; \
		jing -t $$j.rng testmeta2010.xml; \
		echo Validate using RNV.... ; \
		xmllint --noent --dropdtd  testmeta2010.xml | rnv $$j.rnc ;   \
	done

test-frag:
	@echo BUILD: Testing files which exercise inclusion of fragments
	test -f testlite.dtd &&	jing -t -c frag.rnc testlite.xml
	xmllint --dropdtd testlite.xml | rnv frag.rnc 


test-extns:
	@echo BUILD: Testing files which demonstrate user extensions
	xmllint --noout --valid extns.xml

test-sgml:
	@echo BUILD: Test SGML extension mechanism
	onsgmls -e -g -s  sgmldecl.tei extns.sgm 

detest:
	@echo BUILD: Test file with deliberate mistakes
	$(ROMA) $(ROMAOPTS) --nodtd --noxsd --isoschematron --xsl=$(XSL) detest.odd .
	echo -jing -t detest.rng detest.xml  > detest.log.all
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



