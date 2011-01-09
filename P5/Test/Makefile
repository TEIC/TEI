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
	testmeta2010 \
	testsvg  

default: normal special test-extns test-frag

normal:
	@echo INFO Setup for Roma is $(ROMAOPTS)
	Modules="tei core textstructure figures header linking" 
	for i in $(TESTFILES) $(TESTDTDFILES) ; do \
		@echo BUILD Test case $$i ; \
		@echo Try to validate $$i.odd; \
		rnv ../p5odds.rnc $$i.odd && echo ODD $$i validates OK; \
		saxon -o:$$i-ex.odd $$i.odd ../Utilities/odd2exodd.xsl TEIC=true ;\
		$(ROMA) $(ROMAOPTS) --nodtd --noxsd  --xsl=$(XSL)	$$i-ex.odd . ; \
		@echo check egXML in $$i.odd; \
		perl -p -e "s/testlite-examples/$$i-examples/" ex.nvdl > $$i.nvdl; \
		../run-onvdl $$i.nvdl $$i.odd ; \
		for j in `grep "<schemaSpec" $$i.odd | sed 's/.*ident=.//' | sed 's/\".*//'`; do \
			@echo found schemaSpec $$j in $$i; \
			$(ROMA) $(ROMAOPTS) --xsl=$(XSL) --schema=$$j  $$i.odd .; \
			cp xml.xsd.w3c xml.xsd ; \
			test -f teix.xsd && perl -p -i -e 's+<.*\"xml.xsd\".*++' teix.xsd; \
			test -f spec.xsd && perl -p -i -e 's+<.*\"xml.xsd\".*++' spec.xsd; \
			test -f teix.xsd && mv teix.xsd $$j.teix.xsd ; \
			test -f $$j.teix.xsd && perl -p -i -e "s/teix.xsd/$$j.teix.xsd/" $$j.xsd; \
			@echo Now validating instances documents against $$j schema; \
			@echo JING RelaxNG... ; \
			jing -t $$j.rng $$i.xml; \
			@echo RNV.... ; \
			xmllint --noent --dropdtd  $$i.xml | rnv $$j.rnc ;   \
			@echo xmllint DTD  ; \
			xmllint --dropdtd $$i.xml | xmllint --noout --dtdvalid $$j.dtd - ; \
			@echo JING .. for XSD.... ; \
			jing $$j.xsd $$i.xml; \
			@echo xmllint DTD using DTD fragments.... ; \
			grep -H DOCTYPE $$i.xml && xmllint --noout --valid $$i.xml ; \
			done; \
		done

test-special:
	@echo BUILD testing files which only work to make RELAX NG schemas
	for i in $(TESTSPECIALFILES) ; do \
		@echo "BUILD test of (special) $$i" ; \
		@echo Try  to validate $$i.odd; \
		rnv ../p5odds.rnc $$i.odd && echo ODD $$i validates OK ; \
		$(ROMA) --patternprefix=tei_ $(ROMAOPTS) --xsl=$(XSL)   $$i.odd .; \
		cp xml.xsd.w3c xml.xsd ; \
		test -f teix.xsd && perl -p -i -e 's+<.*\"xml.xsd\".*++' teix.xsd; \
		test -f teix.xsd && mv teix.xsd $$i.teix.xsd ; \
		test -f $$i.teix.xsd && perl -p -i -e "s/teix.xsd/$$i.teix.xsd/" $$i.xsd; \
		echo JING RelaxNG... ; \
		jing -t $$i.rng $$i.xml; \
		xmllint --noent --dropdtd  $$i.xml | rnv $$i.rnc  ; \
		@echo JING .. for XSD.... ; \
		jing $$i.xsd $$i.xml;  \
		done

test-frag:
	@echo BUILD testing files which exercise inclusion of fragments
	test -f testlite.dtd &&	jing -t -c frag.rnc testlite.xml
	xmllint --dropdtd testlite.xml | 	rnv frag.rnc 


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
	@echo BUILD testing files which demonstrate user extensions
	xmllint --noout --valid extns.xml

test-sgml:
	@echo BUILD check SGML extension mechanism
	onsgmls -e -g -s  sgmldecl.tei extns.sgm 

test-schematron:
	@echo BUILD Run Schematron tests - these may produce warnings
	for i in $(TESTFILES) $(TESTDTDFILES) $(TESTSPECIALFILES); do \
	 echo Schematron on $$i: ; \
	 saxon $$i.isosch ../Utilities/iso_schematron_message_xslt2.xsl  > $$i.xsl; \
	 saxon $$i.xml $$i.xsl ; \
	done

test-bad:
	@echo BUILD Now test file with deliberate errors
	@echo =======================================
	@echo Many errors will occur here
	@echo =======================================
	@echo JING RelaxNG...
	-for i in $(TESTBADFILES); do \
	$(ROMA) $(ROMAOPTS) --xsl=$(XSL) $$i.odd . ; \
	jing -t $$i.rng $$i.xml ; \
	echo RNV.... ; \
	xmllint --noent --dropdtd  $$i.xml | rnv $$i.rnc ; \
	echo JING XSD; \
	jing $$i.xsd $$i.xml; done


clean:
	-rm -f test*.doc.xml test*.rnc test*.dtd test*.compiled.* test*.teix.xsd test*.xsd test*.rnc test*.rng test*.xsl test*.isosch
	-rm -rf LOG *~ *.xsd Schema RomaResults DTD 
	-rm -rf *.doc.*
	-rm *-examples.rng *-examples.rnc *test*.nvdl *-ex.odd


