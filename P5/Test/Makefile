LANGUAGE=en
DRIVER=guidelines-${LANGUAGE}.xml
XSL=/usr/share/xml/tei/stylesheet/
ROMAOPTS=--localsource=../Source/Guidelines/${LANGUAGE}/${DRIVER}

TESTFILES= \
	testClasses \
	testbare \
	testchinese \
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
	testi18n \
	testlangs \
	testlite \
	testmav \
	testminimal \
	testms \
	testnames \
	testns \
	testoucs \
	testp4compat \
	testplace \
	testspoken \
	testtagdocs \
	testtite \
	testtranscr \
	tripReport \
	testall 

TESTBADFILES= \
	detest \
	testerrmav

TESTDOCBOOKFILES= 
# testdocbook1 testdocbook2 testdocbook3 testdocbook4 testdocbook5 

TESTSPECIALFILES= \
	testallplus \
	testsvg  \
	testodds \
	testplace-gml \
	testplace-kml \
	testrend \
	testmathml 

default: schemas test

test: test-schema test-dtd test-dtd-xmllint test-extns test-xsd test-bad

schemas:
	xsltproc --xinclude --stringparam Modules \
	"tei core textstructure figures header linking" \
	../Utilities/makenames.xsl \
	../Source/Guidelines/en/guidelines-en.xml > names.xml
	-for i in $(TESTFILES) $(TESTDOCBOOKFILES) $(TESTBADFILES); do \
		rnv ../p5odds.rnc $$i.odd && echo $$i validates OK stage; \
		xsltproc --xinclude ../Utilities/odd2exodd.xsl $$i.odd > $$i-ex.odd; \
		roma $(ROMAOPTS) --nodtd --noxsd  --xsl=$(XSL)	$$i-ex.odd . ; \
		echo check egXML in $$i.odd; \
		perl -p -e "s/testlite-examples/$$i-examples/" ex.nvdl > $$i.nvdl; \
		onvdl $$i.nvdl $$i.odd | grep -v ": error: unfinished element$$" | grep -v "unknown element .egXML" ; \
		roma $(ROMAOPTS) --xsl=$(XSL)   $$i.odd .; \
		cp xml.xsd.w3c xml.xsd ; \
		test -f teix.xsd && perl -p -i -e 's+<.*\"xml.xsd\".*++' teix.xsd; \
		test -f spec.xsd && perl -p -i -e 's+<.*\"xml.xsd\".*++' spec.xsd; \
		test -f teix.xsd && mv teix.xsd $$i.teix.xsd ; \
		test -f $$i.teix.xsd && perl -p -i -e "s/teix.xsd/$$i.teix.xsd/" $$i.xsd; \
	done
	-for i in $(TESTSPECIALFILES) ; do \
		roma --patternprefix=tei_ $(ROMAOPTS) --xsl=$(XSL)   $$i.odd .; \
		cp xml.xsd.w3c xml.xsd ; \
		test -f teix.xsd && perl -p -i -e 's+<.*\"xml.xsd\".*++' teix.xsd; \
		test -f teix.xsd && mv teix.xsd $$i.teix.xsd ; \
		test -f $$i.teix.xsd && perl -p -i -e "s/teix.xsd/$$i.teix.xsd/" $$i.xsd; \
	done

test-schema: test-rnv test-jing 
#test-xmllint

test-jing:
	echo JING RelaxNG...
	for i in $(TESTFILES) $(TESTDOCBOOKFILES) $(TESTSPECIALFILES); do echo $$i: ; jing -t $$i.rng $$i.xml; done
	jing -t -c testfrag.rnc testlite.xml

test-rnv:
	echo RNV....
	for i in $(TESTFILES) $(TESTDOCBOOKFILES) $(TESTSPECIALFILES); do echo $$i: ; xx -P $$i.xml | rnv $$i.rnc ; done
	xmllint --dropdtd testlite.xml | 	rnv testfrag.rnc 

test-xmllint:
	echo XMLLINT ... for DTD ....
	for i in $(TESTFILES); do xmllint --dropdtd --noout --relaxng $$i.rng $$i.xml; done

test-xsd:
	echo JING .. for XSD....
	-for i in $(TESTFILES);do echo $$i: ; jing $$i.xsd $$i.xml; done
	echo XSDVALID
	-for i in $(TESTFILES);do echo $$i: ; xsdvalid -s $$i.xsd $$i.xml; done

test-xerces:
	-for i in $(TESTFILES) ;do echo $$i: ; java -cp ~/jar/xercesSamples.jar jaxp.SourceValidator -HS -VA -GA -a $$i.xsd -i $$i.xml; done


test-dtd:
	echo xmllint DTD 
	-for i in `echo $(TESTFILES)`; do echo $$i: ; xmllint --dropdtd $$i.xml | xmllint --noout --dtdvalid $$i.dtd -; done


# testall, testp4compat, testodds have no DOCTYPE
test-dtd-xmllint:
	echo xmllint DTD using DTD fragments....
	-for i in `echo $(TESTFILES) | sed 's/testlite//' | sed 's/testnames//' | sed 's/testi18n//' | sed 's/testodds//'  | sed 's/testall//' | sed 's/testp4compat//'`; do xmllint --noout --valid $$i.xml; done

test-extns:
	echo check whether user extensions work ....
	xmllint --noout --valid testextns.xml
test-sgml:
	echo Check SGML extension mechanism
	onsgmls -e -g -s  sgmldecl.tei testextns.sgm 

test-bad:
	echo Now test file with deliberate errors
	echo JING RelaxNG...
	-for i in $(TESTBADFILES); do \
	jing -t $$i.rng $$i.xml ; \
	echo RNV.... ; \
	xx -P $$i.xml | rnv $$i.rnc ; \
	echo JING XSD; \
	jing $$i.xsd $$i.xml; done


clean:
	-for i in $(TESTFILES) $(TESTDOCBOOKFILES) $(TESTSPECIALFILES) $(TESTBADFILES); do rm -f $$i.doc.xml $$i.rnc $$i.dtd $$i.compiled.* $$i.teix.xsd $$i.xsd $$i.rnc $$i.rng;done
	-rm -rf LOG *~ *.xsd Schema RomaResults DTD
	-rm -rf *.doc.*
	-rm *-examples.rng *-examples.rnc test*.nvdl


