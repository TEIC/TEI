XSL=/usr/share/xml/tei/stylesheet/
ROMAOPTS=--doc --localsource=../Source-driver.xml

TESTFILES= \
	detest \
	testchinese \
	testcorpus \
	testdictionaries \
	testdrama \
	testfs \
	testi18n \
	testlite \
	testminimal \
	testms \
	testoucs \
	testp4compat \
	tripReport \
	testall 

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

default: schemas test-schema test-dtd test-dtd-xmllint test-xsd

all: test-schema test-dtd test-xsd

schemas:
	for i in $(TESTFILES) $(TESTDOCBOOKFILES) $(TESTSPECIALFILES); do roma $(ROMAOPTS) --xsl=$(XSL)   $$i.odd .; done

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
	echo XMLLINT....
	for i in $(TESTFILES); do xmllint --dropdtd --noout --relaxng $$i.rng $$i.xml; done

test-xsd:
	echo JING XSD
	-for i in `echo $(TESTFILES) | sed 's/testall//'`;do echo $$i: ; jing $$i.xsd $$i.xml; done
	echo XSDVALID....
	-for i in $(TESTFILES) ;do echo $$i: ; xsdvalid -s $$i.xsd $$i.xml; done

test-xerces:
	-for i in $(TESTFILES) ;do echo $$i: ; java -cp ~/jar/xercesSamples.jar jaxp.SourceValidator -HS -VA -GA -a $$i.xsd -i $$i.xml; done

test-dtd:
	echo XMLLINT DTD ....
	-for i in $(TESTFILES); do echo $$i: ; xmllint --dropdtd $$i.xml | xmllint --noout --dtdvalid $$i.dtd -; done


test-dtd-xmllint:
	echo RXP DTD ....
	-for i in $(TESTFILES); do xmllint --noout --valid $$i.xml; done

clean:
	-for i in $(TESTFILES) $(TESTDOCBOOKFILES) $(TESTSPECIALFILES); do rm -f $$i.doc.xml $$i.rnc $$i.dtd $$i.compiled.* $$i.xsd $$i.rnc $$i.rng;done
	-rm -rf LOG *~ *.xsd Schema RomaResults DTD
	-rm -rf *.doc.*


