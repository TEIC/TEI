LANGUAGE=en
ROMA=roma2
INPUTLANGUAGE=en
DOCUMENTATIONLANGUAGE=en
DRIVER=guidelines-${INPUTLANGUAGE}.xml
XSL=/usr/share/xml/tei/stylesheet/
ROMAOPTS=--isoschematron --doclang=${DOCUMENTATIONLANGUAGE} --localsource=`pwd`/../p5subset.xml

TESTDTDFILES= \
	testns \
	testtite 

TESTFILES= \
	testClasses \
	testTripReport \
	testbare \
	testchinese \
	testconstraint \
	testconstraint2 \
	testcorpus \
	testdictionaries \
	testdrama \
	testfand \
	testfand2 \
	testfand3 \
	testfand4 \
	testfand5 \
	testfs \
	testglobals \
	testgos \
	testi18n \
	testjustfs \
	testlangs \
	testlite \
	testmav \
	testminimal \
	testms \
	testnames \
	testoucs \
	testp4compat \
	testplace \
	testrendition \
	testspoken \
	testtagdocs \
	testtranscr \
	test_meta2010 \
	testall 

TESTBADFILES= \
	detest \
	testerrmav

TESTSPECIALFILES= \
	testallplus \
	testits  \
	testmathml \
	testodds \
	testplace-gml \
	testplace-kml \
	testrend \
	testsvg  

default: schemas test

test: test-extns test-schematron test-bad test-frag

schemas:
	echo SETUP IS $(ROMAOPTS)
	Modules="tei core textstructure figures header linking" 
	-for i in $(TESTFILES) $(TESTDTDFILES)  $(TESTBADFILES); do \
		echo ; echo PROCESS $$i ; \
		echo Try  to validate $$i.odd; \
		rnv ../p5odds.rnc $$i.odd && echo $$i validates OK stage; \
		saxon -o $$i-ex.odd $$i.odd ../Utilities/odd2exodd.xsl TEIC=true ;\
		$(ROMA) $(ROMAOPTS) --nodtd --noxsd  --xsl=$(XSL)	$$i-ex.odd . ; \
		echo check egXML in $$i.odd; \
		perl -p -e "s/testlite-examples/$$i-examples/" ex.nvdl > $$i.nvdl; \
		onvdl $$i.nvdl $$i.odd | grep -v ": error: unfinished element$$" | grep -v "unknown element .egXML" ; \
		$(ROMA) $(ROMAOPTS) --xsl=$(XSL)   $$i.odd .; \
		cp xml.xsd.w3c xml.xsd ; \
		test -f teix.xsd && perl -p -i -e 's+<.*\"xml.xsd\".*++' teix.xsd; \
		test -f spec.xsd && perl -p -i -e 's+<.*\"xml.xsd\".*++' spec.xsd; \
		test -f teix.xsd && mv teix.xsd $$i.teix.xsd ; \
		test -f $$i.teix.xsd && perl -p -i -e "s/teix.xsd/$$i.teix.xsd/" $$i.xsd; \
		echo Now validating instances documents against $$i schema; \
		echo JING RelaxNG... ; \
		jing -t $$i.rng $$i.xml; \
		echo RNV.... ; \
		xmllint --noent --dropdtd  $$i.xml | rnv $$i.rnc ;   \
		echo XMLLINT .. for RELAXNG ; \
		xmllint --dropdtd --noout --relaxng $$i.rng $$i.xml; \
		echo JING .. for XSD.... ; \
		jing $$i.xsd $$i.xml; \
		echo xmllint DTD  ; \
		xmllint --dropdtd $$i.xml | xmllint --noout --dtdvalid $$i.dtd - ; \
		echo xmllint DTD using DTD fragments.... ; \
		grep -H DOCTYPE $$i.xml && xmllint --noout --valid $$i.xml ; \
		done
	-for i in $(TESTSPECIALFILES) ; do \
		echo ; echo PROCESS $$i ; \
		echo Try  to validate $$i.odd; \
		rnv ../p5odds.rnc $$i.odd && echo $$i validates OK stage; \
		$(ROMA) --patternprefix=tei_ $(ROMAOPTS) --xsl=$(XSL)   $$i.odd .; \
		cp xml.xsd.w3c xml.xsd ; \
		test -f teix.xsd && perl -p -i -e 's+<.*\"xml.xsd\".*++' teix.xsd; \
		test -f teix.xsd && mv teix.xsd $$i.teix.xsd ; \
		test -f $$i.teix.xsd && perl -p -i -e "s/teix.xsd/$$i.teix.xsd/" $$i.xsd; \
		echo JING RelaxNG... ; \
		jing -t $$i.rng $$i.xml; \
		echo RNV.... ; \
		xmllint --noent --dropdtd  $$i.xml | rnv $$i.rnc  ; \
		echo XMLLINT .. for RELAXNG ; \
		xmllint --dropdtd --noout --relaxng $$i.rng $$i.xml; \
		echo JING .. for XSD.... ; \
		jing $$i.xsd $$i.xml;  \
		done



test-frag:
	test -f testlite.dtd &&	jing -t -c testfrag.rnc testlite.xml
	xmllint --dropdtd testlite.xml | 	rnv testfrag.rnc 


test-xsd-valid:
	echo XSDVALID
	echo =======================================
	echo = 
	echo =======================================
	- which xsdvalid && for i in $(TESTFILES) $(TESTDTDFILES);do echo $$i: ; xsdvalid -s $$i.xsd $$i.xml; done

test-xerces:
	echo xerces XSD
	echo =======================================
	echo = Should be no errors from Jing/xerces XSD tests
	echo =======================================
	-for i  in $(TESTFILES) $(TESTDTDFILES) ;do echo $$i: ; java -cp ~/jar/xercesSamples.jar jaxp.SourceValidator -HS -VA -GA -a $$i.xsd -i $$i.xml; done


test-extns:
	echo check whether user extensions work ....
	xmllint --noout --valid testextns.xml

test-sgml:
	echo Check SGML extension mechanism
	onsgmls -e -g -s  sgmldecl.tei testextns.sgm 

test-schematron:
	echo Run Schematron tests - these may produce warnings
	for i in $(TESTFILES) $(TESTDTDFILES) $(TESTSPECIALFILES); do \
	 echo Schematron on $$i: ; \
	 saxon $$i.isosch ../Utilities/iso_schematron_message_xslt2.xsl  > $$i.xsl; \
	 saxon $$i.xml $$i.xsl ; \
	done

test-bad:
	echo Now test file with deliberate errors
	echo =======================================
	echo Many errors will occur here
	echo =======================================
	echo JING RelaxNG...
	-for i in $(TESTBADFILES); do \
	jing -t $$i.rng $$i.xml ; \
	echo RNV.... ; \
	xmllint --noent --dropdtd  $$i.xml | rnv $$i.rnc ; \
	echo JING XSD; \
	jing $$i.xsd $$i.xml; done


clean:
	-for i in $(TESTFILES) $(TESTDTDFILES)  $(TESTSPECIALFILES) $(TESTBADFILES); do rm -f $$i.doc.xml $$i.rnc $$i.dtd $$i.compiled.* $$i.teix.xsd $$i.xsd $$i.rnc $$i.rng $$i.xsl; done
	-rm -rf LOG *~ *.xsd Schema RomaResults DTD 
	-rm -rf *.doc.*
	-rm *-examples.rng *-examples.rnc *test*.nvdl *-ex.odd


