XSL=/usr/share/xml/tei/stylesheet/
ROMAOPTS=--doc --localsource=../Source-driver.xml

TESTFILES= \
	testchinese \
	testcorpus \
	testdictionaries \
	testdrama \
	testfs \
	testi18n \
	testlite \
	testminimal \
	testms \
	testnames \
	testoucs \
	testp4compat \
	tripReport \
	testall 

TESTBADFILES= \
	detest

TESTDOCBOOKFILES= 

#testdocbook1 \
#testdocbook2 \
#testdocbook3 \
#testdocbook4 \
#testdocbook5 

TESTSPECIALFILES= \
	testsvg  \
	testodds \
	testmathml 

default: schemas test-schema test-dtd test-dtd-xmllint test-xsd test-bad

all: test-schema test-dtd test-xsd

schemas:
	-for i in $(TESTFILES) $(TESTDOCBOOKFILES) $(TESTSPECIALFILES) $(TESTBADFILES); do \
		roma $(ROMAOPTS) --xsl=$(XSL)   $$i.odd .; \
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
	echo XMLLINT DTD ...except testodds ....
	-for i in `echo $(TESTFILES) | sed 's/testodds//'`; do echo $$i: ; xmllint --dropdtd $$i.xml | xmllint --noout --dtdvalid $$i.dtd -; done


# testall, testp4compat, testodds have no DOCTYPE
test-dtd-xmllint:
	echo xmllint DTD ....
	-for i in `echo $(TESTFILES) | sed 's/testi18n//' | sed 's/testodds//'  | sed 's/testall//' | sed 's/testp4compat//'`; do xmllint --noout --valid $$i.xml; done


test-bad:
	echo Now test file with deliberate errors
	echo JING RelaxNG...
	-jing -t $(TESTBADFILES).rng $(TESTBADFILES).xml
	echo RNV....
	-xx -P $(TESTBADFILES).xml | rnv $(TESTBADFILES).rnc
	echo JING XSD
	-jing $(TESTBADFILES).xsd $(TESTBADFILES).xml

clean:
	-for i in $(TESTFILES) $(TESTDOCBOOKFILES) $(TESTSPECIALFILES); do rm -f $$i.doc.xml $$i.rnc $$i.dtd $$i.compiled.* $$i.teix.xsd $$i.xsd $$i.rnc $$i.rng;done
	-rm -rf LOG *~ *.xsd Schema RomaResults DTD
	-rm -rf *.doc.*


