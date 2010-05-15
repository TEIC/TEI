LANGUAGE=en
ROMA=roma2
INPUTLANGUAGE=en
DOCUMENTATIONLANGUAGE=en
DRIVER=guidelines-${INPUTLANGUAGE}.xml
XSL=/usr/share/xml/tei/stylesheet/
ROMAOPTS=--isoschematron --doclang=${DOCUMENTATIONLANGUAGE} --localsource=../Source/Guidelines/${INPUTLANGUAGE}/${DRIVER}

TESTFILESDTD= \
	testns \
	testtite 

TESTFILES= \
	testClasses \
	testTripReport \
	testall \
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
	testtranscr 

TESTBADFILES= \
	detest \
	testerrmav

TESTDOCBOOKFILES= 
# testdocbook1 testdocbook2 testdocbook3 testdocbook4 testdocbook5 

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

test: test-schema test-dtd test-dtd-xmllint test-extns test-xsd test-xsd-valid test-schematron test-bad

schemas:
	echo SETUP IS $(ROMAOPTS)
	saxon -o  names.xml \
	../Source/Guidelines/en/guidelines-en.xml \
	../Utilities/makenames.xsl \
	Modules="tei core textstructure figures header linking" 
	-for i in $(TESTFILES) $(TESTFILESDTD) $(TESTDOCBOOKFILES) $(TESTBADFILES); do \
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
	done
	-for i in $(TESTSPECIALFILES) ; do \
		$(ROMA) --patternprefix=tei_ $(ROMAOPTS) --xsl=$(XSL)   $$i.odd .; \
		cp xml.xsd.w3c xml.xsd ; \
		test -f teix.xsd && perl -p -i -e 's+<.*\"xml.xsd\".*++' teix.xsd; \
		test -f teix.xsd && mv teix.xsd $$i.teix.xsd ; \
		test -f $$i.teix.xsd && perl -p -i -e "s/teix.xsd/$$i.teix.xsd/" $$i.xsd; \
	done

test-schema: test-jing test-rnv 

test-jing:
	echo JING RelaxNG...
	echo =======================================
	echo = Expect to see errors about schemaLocation on testminimal
	echo = 
	echo = testallplus has error about ref and MathML
	echo =======================================
	for i in $(TESTFILES) $(TESTFILESDTD) $(TESTDOCBOOKFILES) $(TESTSPECIALFILES); do echo $$i: ; jing -t $$i.rng $$i.xml; done
	jing -t -c testfrag.rnc testlite.xml

test-rnv:
	echo RNV....
	echo =======================================
	echo = Expect errors about schemaLocation on testminimal
	echo =======================================
	for i in $(TESTFILES) $(TESTDOCBOOKFILES) $(TESTSPECIALFILES); \
	  do echo $$i: ; \
	    xmllint --noent --dropdtd  $$i.xml | rnv $$i.rnc ; \
	done
	xmllint --dropdtd testlite.xml | 	rnv testfrag.rnc 

test-xmllint:
	echo XMLLINT .. for RELAXNG
	echo =======================================
	echo = 
	echo = 
	echo = 
	echo =======================================
	for i  in $(TESTFILES) $(TESTFILESDTD); do xmllint --dropdtd --noout --relaxng $$i.rng $$i.xml; done

test-xsd:
	echo JING .. for XSD....
	echo =======================================
	echo = JING .xsd tests will show errors
	echo = testtagdocs and testns
	echo =======================================
	-for i in $(TESTFILES) $(TESTFILESDTD);do echo $$i: ; jing $$i.xsd $$i.xml; done

test-xsd-valid:
	echo XSDVALID
	echo =======================================
	echo = 
	echo =======================================
	- which xsdvalid && for i in $(TESTFILES) $(TESTFILESDTD);do echo $$i: ; xsdvalid -s $$i.xsd $$i.xml; done

test-xerces:
	echo xerces XSD
	echo =======================================
	echo = Should be no errors from Jing/xerces XSD tests
	echo =======================================
	-for i  in $(TESTFILES) $(TESTFILESDTD) ;do echo $$i: ; java -cp ~/jar/xercesSamples.jar jaxp.SourceValidator -HS -VA -GA -a $$i.xsd -i $$i.xml; done


test-dtd:
	echo xmllint DTD 
	echo =======================================
	echo = 
	echo = expect error in tagdocs about ref, testtite about xmlns:t
	echo = and in tagdocs about non-text nodes
	echo =
	echo =======================================
	-for i in `echo $(TESTFILES) $(TESTFILESDTD)`; do echo $$i: ; xmllint --dropdtd $$i.xml | xmllint --noout --dtdvalid $$i.dtd -; done


# testall, testp4compat, testodds have no DOCTYPE
test-dtd-xmllint:
	echo xmllint DTD using DTD fragments....
	echo =======================================
	echo Should be no errors from xmllint/dtd tests
	echo =======================================
	-for i in  $(TESTFILES) $(TESTFILESDTD); do grep DOCTYPE $$i.xml && xmllint --noout --valid $$i.xml; \
	done

test-extns:
	echo check whether user extensions work ....
	xmllint --noout --valid testextns.xml

test-sgml:
	echo Check SGML extension mechanism
	onsgmls -e -g -s  sgmldecl.tei testextns.sgm 

test-schematron:
	echo Run Schematron tests - these may produce warnings
	for i in $(TESTFILES) $(TESTFILESDTD) $(TESTSPECIALFILES); do \
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
	-for i in $(TESTFILES) $(TESTFILESDTD) $(TESTDOCBOOKFILES) $(TESTSPECIALFILES) $(TESTBADFILES); do rm -f $$i.doc.xml $$i.rnc $$i.dtd $$i.compiled.* $$i.teix.xsd $$i.xsd $$i.rnc $$i.rng $$i.xsl; done
	-rm -rf LOG *~ *.xsd Schema RomaResults DTD names.xml
	-rm -rf *.doc.*
	-rm *-examples.rng *-examples.rnc *test*.nvdl *-ex.odd


