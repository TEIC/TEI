#testdocbook 
TESTFILES= \
	testchinese \
	testcorpus \
	testdictionaries \
	testdrama \
	testfs \
	testi18n \
	testlite \
	testmathml \
	testminimal \
	testms \
	testodds \
	testoucs \
	testp4compat \
	testsvg  \
	testall 


all: test-schema test-dtd

schemas:
	for i in $(TESTFILES); do ../Roma --schema=../Schema/ $$i.odd .; done

test-schema: test-rnv test-jing test-xmllint

test-jing:
	echo JING...
	for i in $(TESTFILES); do echo $$i: ; jing -t $$i.rng $$i.xml; done

test-rnv:
	echo RNV....
	for i in $(TESTFILES); do echo $$i: ; xx -P $$i.xml | rnv $$i.rnc ; done

test-xmllint:
	echo XMLLINT....
	for i in $(TESTFILES); do xmllint --dropdtd --noout --relaxng $$i.rng $$i.xml; done

test-xsd:
	echo XSDVALID....
	for i in $(TESTFILES); do echo $$i: ; xsdvalid -s $$i.xsd $$i.xml; done

test-dtd:
	echo XMLLINT DTD ....
	-for i in $(TESTFILES); do echo $$i: ; xmllint --noout --valid $$i.xml; done
	echo RXP DTD ....
	-for i in $(TESTFILES); do rxp -V -s -x $$i.xml; done

clean:
	-for i in $(TESTFILES); do rm -f $$i.doc.xml $$i.rnc $$i.dtd $$i.compiled.* $$i.xsd $$i.rng;done
