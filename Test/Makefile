#testdocbook 
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
	testoucs \
	testp4compat \
	testall 

TESTSPECIALFILES= \
	testsvg  \
	testodds \
	testmathml 

all: test-schema test-dtd test-xsd

schemas:
	for i in $(TESTFILES) $(TESTSPECIALFILES); do ../Roma --doc --schema=../Schema/ $$i.odd .; done

test-schema: test-rnv test-jing 
#test-xmllint

test-jing:
	echo JING...
	for i in $(TESTFILES) $(TESTSPECIALFILES); do echo $$i: ; jing -t $$i.rng $$i.xml; done

test-rnv:
	echo RNV....
	for i in $(TESTFILES) $(TESTSPECIALFILES); do echo $$i: ; xx -P $$i.xml | rnv $$i.rnc ; done

test-xmllint:
	echo XMLLINT....
	for i in $(TESTFILES); do xmllint --dropdtd --noout --relaxng $$i.rng $$i.xml; done

test-xsd:
	echo JING
	-for i in $(TESTFILES) ;do echo $$i: ; jing $$i.xsd $$i.xml; done
	echo XSDVALID....
	-for i in $(TESTFILES) ;do echo $$i: ; xsdvalid -s $$i.xsd $$i.xml; done

test-dtd:
	echo XMLLINT DTD ....
	-for i in $(TESTFILES); do echo $$i: ; xmllint --dropdtd $$i.xml | xmllint --noout --dtdvalid $$i.dtd -; done


test-dtd-rxp:
	echo RXP DTD ....
	-for i in $(TESTFILES); do rxp -V -s -x $$i.xml; done

clean:
	-for i in $(TESTFILES)  $(TESTSPECIALFILES); do rm -f $$i.doc.xml $$i.rnc $$i.dtd $$i.compiled.* $$i.xsd $$i.rng;done
