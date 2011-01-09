LANGUAGE=en
GOOGLEANALYTICS=""
INPUTLANGUAGE=en
DOCUMENTATIONLANGUAGE=en
LATEX=pdflatex
XELATEXFLAGS=--interaction=batchmode --output-driver="xdvipdfmx -V 5"
XELATEX=xelatex 
VERBOSE=
PREFIX=/usr
SOURCETREE=Source
DRIVER=${SOURCETREE}/guidelines-${INPUTLANGUAGE}.xml
ROMA=roma2
ROMAOPTS="--localsource=`pwd`/p5subset.xml"
XSL=/usr/share/xml/tei/stylesheet
XSLP4=/usr/share/xml/teip4/stylesheet
# If you have not installed the Debian packages, uncomment one
# of the next two lines:
#XSL=../Stylesheets/release/tei-xsl/p5
#XSL=http://www.tei-c.org/stylesheet/release/xml/tei
JING=jing
TRANG=trang
SAXON=saxon
SAXON_ARGS=-ext:on
.PHONY: convert dtds schemas html validate valid test oddschema exampleschema clean dist exemplars

default: validate exemplars test pdf epub html-web validate-html

convert: dtds schemas

dtds: check
	rm -rf DTD
	mkdir DTD
	@echo BUILD generate modular DTDs
	${SAXON} ${SAXON_ARGS}  ${DRIVER} ${XSL}/odds2/odd2dtd.xsl outputDir=DTD 	\
	lang=${LANGUAGE} \
	documentationLanguage=${DOCUMENTATIONLANGUAGE} \
	TEIC=true  ${VERBOSE}

schemas:check schema-relaxng schema-sch

schema-relaxng:
	rm -rf Schema
	mkdir Schema
	@echo BUILD generate modular RELAX NG schemas
	${SAXON} ${SAXON_ARGS}  ${DRIVER}  ${XSL}/odds2/odd2relax.xsl outputDir=Schema \
	lang=${LANGUAGE}  \
	TEIC=true  ${VERBOSE}
	@echo "INFO generate modular RELAX NG (compact) schemas using trang"
	(cd Schema; for i in *rng; do ${TRANG} $$i `basename $$i .rng`.rnc;done)

schema-sch:
	@echo BUILD extract schema rules to make p5.isosch
	${SAXON} ${SAXON_ARGS}  ${DRIVER} ${XSL}/odds2/extract-isosch.xsl > p5.isosch

html-web: check
	@echo BUILD making HTML Guidelines for language ${LANGUAGE}
	perl -p -e \
		"s+http://www.tei-c.org/release/xml/tei/stylesheet+${XSL}+; \
		 s+/usr/share/xml/tei/stylesheet+${XSL}+;" \
		Utilities/guidelines.xsl.model > Utilities/guidelines.xsl
	mkdir -p Guidelines-web
	rm -rf Guidelines-web-tmp 
	mkdir Guidelines-web-tmp
	mkdir -p Guidelines-web-tmp/${LANGUAGE}/html
	cp -r Source/Guidelines/${INPUTLANGUAGE}/Images webnav/* odd.css guidelines.css COPYING.txt guidelines-print.css Guidelines-web-tmp/${LANGUAGE}/html/ 
	${SAXON} ${SAXON_ARGS}  ${DRIVER}  Utilities/guidelines.xsl  outputDir=Guidelines-web-tmp/${LANGUAGE}/html \
		displayMode=both \
		pageLayout=CSS \
	        lang=${LANGUAGE} \
	        doclang=${LANGUAGE} \
		googleAnalytics=${GOOGLEANALYTICS} \
	        documentationLanguage=${DOCUMENTATIONLANGUAGE}  ${VERBOSE}
	(cd Guidelines-web-tmp/${LANGUAGE}/html; for i in *.html; do perl -i ../../../Utilities/cleanrnc.pl $$i;done)
	(cd Guidelines-web-tmp/${LANGUAGE}/html; perl -p -i -e 's+/logos/TEI-glow+TEI-glow+' guidelines.css)
	rm -rf Guidelines-web/${LANGUAGE}
	mv Guidelines-web-tmp/${LANGUAGE} Guidelines-web/${LANGUAGE}
	rmdir Guidelines-web-tmp

validate-html:
	@echo BUILD validate HTML version of Guidelines
	(cd Guidelines-web/${LANGUAGE}/html; \
	for i in *.html; do \
	xmllint --noent --dropdtd $$i > z_$$i; \
	$(JING) -c ../../../xhtml.rnc z_$$i; \
	 rm z_$$i;\
	 done)

xml: check  
	${SAXON} ${SAXON_ARGS}  -o:Guidelines.xml ${DRIVER}  ${XSL}/odds2/odd2lite.xsl displayMode=rnc lang=${LANGUAGE} \
	        doclang=${DOCUMENTATIONLANGUAGE} \
	        documentationLanguage=${DOCUMENTATIONLANGUAGE}	${VERBOSE}

tex: xml
	@echo Checking you have a running ${XELATEX} before trying to make TeX...
	which ${XELATEX} || exit 1
	perl -p -e \
		"s+http://www.tei-c.org/release/xml/tei/stylesheet+${XSL}+; \
		 s+/usr/share/xml/tei/stylesheet+${XSL}+;" \
		Utilities/guidelines-latex.xsl > Utilities/guidelines.xsl
	xelatex --interaction=batchmode Utilities/fonttest 
	if [ -f "missfont.log" ]  ; then  \
	  perl -p -i -e 's/(.*Minion)/%\1/;s/(.*Myriad)/%\1/' Utilities/guidelines.xsl ;\
	  echo "========================="; \
	  echo "WARNING: you do not have Minion or Myriad fonts installed, reverting to Computer Modern " ;\
	  echo "========================="; \
	fi
	rm -f missfont.log fonttest.*
	${SAXON} ${SAXON_ARGS}  Guidelines.xml Utilities/guidelines.xsl > Guidelines.tex
	rm -f Utilities/guidelines.xsl
	for i in Guidelines-REF*tex; \
	  do \
	     perl Utilities/rewrapRNC-in-TeX.pl <$$i>$$i.new; \
		echo NOTE: diff $$i.new $$i; \
		diff $$i.new $$i; \
		mv $$i.new $$i; \
	done

pdf: tex
	@echo Make sure you have Junicode, Arphic and Mincho fonts installed 
	echo '*' | ${XELATEX} ${XELATEXFLAGS} Guidelines
	echo '*' | ${XELATEX} ${XELATEXFLAGS} Guidelines
	makeindex -s p5.ist Guidelines
	echo '*' | ${XELATEX} ${XELATEXFLAGS} Guidelines
	echo '*' | ${XELATEX} ${XELATEXFLAGS} Guidelines
	for i in Guidelines*aux; do perl -p -i -e 's/.*zf@fam.*//' $$i; done


chapterpdfs:
	@echo Checking you have a running ${LATEX} before trying to make PDF...
	which ${XELATEX} || exit 1
	for i in `grep "\\include{" Guidelines.tex | sed 's/.*{\(.*\)}.*/\\1/'`; \
	do echo PDF for chapter $$i; \
	echo  $$i | ${XELATEX} Guidelines; \
	echo  $$i | ${XELATEX} Guidelines ; \
	mv Guidelines.pdf $$i.pdf; \
	perl -p -i -e 's/.*zf@fam.*//' $$i.aux; \
	done

validate: dtds schemas oddschema exampleschema valid 

valid: jing_version=$(wordlist 1,3,$(shell jing))
valid: check
	@echo BUILD check validity with jing
	@echo ${jing_version}
#	We have discovered that jing reports 3-letter language codes
#	from ISO 639-2 as illegal values of xml:lang= even though
#	they are perfectly valid per RFC 3066. We have submitted a
#	bug report, and for now just throw away those error messages
#	with grep -v. Note that we discard *all* such messages, even
#	though fewer than 500 of the 17,576 possible combinations
#	(i.e. < 3%) are valid codes.
	 $(JING) -t p5odds.rng ${DRIVER} 
#\
#	 | grep -v ": error: Illegal xml:lang value \"[A-Za-z][A-Za-z][A-Za-z]\"\.$$"
	xmllint --noent --xinclude ${DRIVER} > Source.xml
	@echo BUILD check validity with rnv
	rnv -v p5odds.rnc Source.xml
	@echo BUILD check validity with nvdl
	-./run-onvdl p5.nvdl ${DRIVER} 
	@echo BUILD check validity with Schematron
	${SAXON} ${SAXON_ARGS}  p5.isosch Utilities/iso_schematron_message_xslt2.xsl > p5.isosch.xsl
	${SAXON} ${SAXON_ARGS}  ${DRIVER} p5.isosch.xsl
	@echo BUILD check validity with local XSLT script
	${SAXON} ${SAXON_ARGS}  ${DRIVER} Utilities/prevalidator.xsl > Utilities/pointerattributes.xsl
	${SAXON} ${SAXON_ARGS}  ${DRIVER} Utilities/validator.xsl
	rm Utilities/pointerattributes.xsl
	rm Source.xml
	#@echo BUILD check validity with xmllint
	#xmllint  --relaxng p5odds.rng --noent --xinclude --noout ${DRIVER}
	@echo BUILD check for places with no examples
	${SAXON} ${DRIVER} Utilities/listspecwithnoexample.xsl

test: subset
	@echo BUILD Run test cases for P5
	(cd Test; make XSL=${XSL})

exemplars: subset
	@echo BUILD Build TEI Exemplars
	(cd Exemplars; make XSL=${XSL} PREFIX=${PREFIX})

oddschema: subset
	@echo Checking you have a running ${ROMA} before trying to make oddschema ...
	which ${ROMA} || exit 1
	${ROMA} ${ROMAOPTS} --nodtd --noxsd --xsl=${XSL}/ p5odds.odd .

exampleschema: subset
	@echo Checking you have a running ${ROMA} before trying to make exampleschema ...
	which ${ROMA} || exit 1
	${ROMA}  ${ROMAOPTS} --nodtd --noxsd --xsl=${XSL}/ p5odds-examples.odd . 
#	 perl -p -i -e 's+org/ns/1.0+org/ns/Examples+' p5examples.rnc && \
#	 perl -p -i -e 's+org/ns/1.0+org/ns/Examples+' p5examples.rng

subset:
	${SAXON} ${SAXON_ARGS}  -o:p5subset.xml  ${DRIVER} Utilities/subset.xsl || echo "failed to extract subset from ${DRIVER}." 

dist: clean dist-source dist-schema dist-doc dist-test dist-database dist-exemplars
	rm -f release/tei-`cat VERSION`.zip
	export V=`cat VERSION`;\
	for i in source schema doc test database exemplars; \
	  do (cd release/tei-p5-$$i-$$V/share; \
	zip -q -r ../../tei-$$V.zip .);done

dist-source: subset
	rm -rf release/tei-p5-source*
	mkdir -p release/tei-p5-source/share/xml/tei/odd
	tar -c -f - --exclude "*~" --exclude .svn 	\
	p5subset.xml \
	COPYING.txt \
	Makefile \
	ReleaseNotes  \
	Source \
	Utilities   \
	VERSION  \
	Exemplars/Makefile  \
	Exemplars/relax*  \
	Exemplars/math*  \
	Exemplars/svg*  \
	fasc-head.xml \
	fasc-tail.xml \
	p5.nvdl \
	p5odds.odd \
	p5odds-examples.odd \
	relax.rng \
	schematron.rng \
	iso-schematron.rng \
	p5sch.xsl \
	schematron1-5.rnc \
	*.css \
	webnav \
	validator.xsl \
	xhtml.rnc \
	| (cd release/tei-p5-source/share/xml/tei/odd; tar xf - )
	(cd release; 	\
	ln -s tei-p5-source tei-p5-source-`cat ../VERSION` ; \
	zip -q -r tei-p5-source-`cat ../VERSION`.zip tei-p5-source-`cat ../VERSION` )

dist-schema: schemas dtds oddschema exampleschema
	rm -rf release/tei-p5-schema*
	mkdir -p release/tei-p5-schema/share/xml/tei/schema/dtd
	mkdir -p release/tei-p5-schema/share/xml/tei/schema/relaxng
	(cd DTD; tar --exclude .svn -c -f - .) \
	| (cd release/tei-p5-schema/share/xml/tei/schema/dtd; tar xf - )
	cp catalog.p5 release/tei-p5-schema/share/xml/tei/schema/catalog.xml
	(cd Schema; tar --exclude .svn -c -f - .) \
	| (cd release/tei-p5-schema/share/xml/tei/schema/relaxng; tar xf - )
	(cd release; 	\
	ln -s tei-p5-schema tei-p5-schema-`cat ../VERSION` ; \
	zip -q -r tei-p5-schema-`cat ../VERSION`.zip tei-p5-schema-`cat ../VERSION` )

dist-doc:  
	make html-web
	make LANGUAGE=es DOCUMENTATIONLANGUAGE=es html-web
	make LANGUAGE=de DOCUMENTATIONLANGUAGE=de html-web
	make LANGUAGE=ja DOCUMENTATIONLANGUAGE=ja html-web
	make LANGUAGE=kr DOCUMENTATIONLANGUAGE=kr html-web
	make LANGUAGE=fr DOCUMENTATIONLANGUAGE=fr html-web
	make LANGUAGE=it DOCUMENTATIONLANGUAGE=it html-web
	make LANGUAGE=zh-tw DOCUMENTATIONLANGUAGE=zh-tw html-web
	rm -rf release/tei-p5-doc*
	mkdir -p release/tei-p5-doc/share/doc/tei-p5-doc
	(cd Guidelines-web; tar --exclude .svn -c -f - . ) \
	| (cd release/tei-p5-doc/share/doc/tei-p5-doc; tar xf - )
	for i in ReleaseNotes/readme*xml; do  \
	saxon \
	$$i ${XSL}/xhtml2/tei.xsl  \
	cssFile=html/guidelines.css \
	> release/tei-p5-doc/share/doc/tei-p5-doc/`basename $$i .xml`.html; \
	done
	make pdf
	cp Guidelines.pdf release/tei-p5-doc/share/doc/tei-p5-doc/en
	(cd release; 	\
	ln -s tei-p5-doc tei-p5-doc-`cat ../VERSION` ; \
	zip -q -r tei-p5-doc-`cat ../VERSION`.zip tei-p5-doc-`cat ../VERSION` )

dist-test:
	rm -rf release/tei-p5-test*
	mkdir -p release/tei-p5-test/share/xml/tei
	(cd Test; make clean)
	tar --exclude "*~" --exclude .svn -c -f - Test \
	| (cd release/tei-p5-test/share/xml/tei; tar xf - )
	(cd release; 	\
	ln -s tei-p5-test tei-p5-test-`cat ../VERSION` ; \
	zip -q -r tei-p5-test-`cat ../VERSION`.zip tei-p5-test-`cat ../VERSION` )

dist-exemplars: 
	(cd Exemplars; make dist)

dist-database: 
	rm -rf release/tei-p5-database*
	mkdir -p release/tei-p5-database/share/xml/tei/xquery
	(cd Query; tar --exclude .svn --exclude "*~" -c -f - . ) \
	| (cd release/tei-p5-database/share/xml/tei/xquery; tar xf - )
	(cd release; 	\
	ln -s tei-p5-database tei-p5-database-`cat ../VERSION` ; \
	zip -q -r tei-p5-database-`cat ../VERSION`.zip tei-p5-database-`cat ../VERSION` )

install-schema: dist-schema
	@echo Making schema release in ${PREFIX}
	(cd release/tei-p5-schema; tar cf - .) | (cd ${PREFIX}; tar xf - )

install-doc: dist-doc
	@echo Making documentation release in ${PREFIX}
	(cd release/tei-p5-doc; tar cf - .) | (cd ${PREFIX}; tar xf - )

install-source: dist-source
	@echo Making source release in ${PREFIX}
	(cd release/tei-p5-source; tar cf - .) | (cd ${PREFIX}; tar xf - )

install-test: dist-test
	@echo Making testfiles release in ${PREFIX}
	(cd release/tei-p5-test; tar cf - .) | (cd ${PREFIX}; tar xf - )

install-exemplars: dist-exemplars
	@echo Making exemplars release in ${PREFIX}
	(cd release/tei-p5-exemplars; tar cf - share) | \
	(cd ${PREFIX}; tar xf -)

install-database: dist-database
	@echo Making database release in ${PREFIX}
	(cd release/tei-p5-database; tar cf - .) | (cd ${PREFIX}; tar xf - )

install: clean install-schema install-doc install-test install-exemplars install-source install-database

check:
	@echo Checking you have running XML tools and Perl before trying to run transform...
	@echo -n Perl: 
	@which perl || exit 1
	@echo -n xmllint: 
	@which xmllint || exit 1
	@echo -n trang: 
	@which ${TRANG} || exit 1
	@echo -n jing: 
	@which ${JING} || exit 1
	@echo -n saxon: 
	@which ${SAXON} || exit 1

epub:
	xmllint --dropdtd --noent Source/guidelines-en.xml > teip5.xml
	${XSL}/teitoepub --profile=tei teip5.xml
	mv teip5.epub Guidelines.epub
	rm teip5.xml

changelog:
	(LastDate=`head -1 ReleaseNotes/ChangeLog | awk '{print $$1}'`; \
	svn log -v -r 'HEAD:{'$$LastDate'}' | perl ../gnuify-changelog.pl | grep -v "^;" > newchanges)
	mv ReleaseNotes/ChangeLog oldchanges
	cat newchanges oldchanges > ReleaseNotes/ChangeLog
	rm newchanges oldchanges


catalogue:
	${SAXON} ${SAXON_ARGS}  -o:catalogue.xml ${DRIVER}  Utilities/catalogue.xsl DOCUMENTATIONLANG=${DOCUMENTATIONLANGUAGE} 
	${SAXON} ${SAXON_ARGS}  catalogue.xml ${XSL}/xhtml2/tei.xsl > catalogue.html
	@echo Made catalogue.html

catalogue-print:
	${SAXON} ${SAXON_ARGS} ${DRIVER}  Utilities/catalogue-print.xsl DOCUMENTATIONLANG=${DOCUMENTATIONLANGUAGE} | xmllint --format - > catalogue.xml

clean:
	rm -rf release Guidelines Guidelines-web Schema DTD dtd Split RomaResults *~ 
	rm -rf Guidelines.??? Guidelines-* \
	p5odds-examples.rng  p5odds-examples.rnc \
	p5odds.rng p5odds.rnc \
	*.xsd \
	p5.sch p5.isosch \
	*.isosch.xsl \
	Test/*.isosch \
	p5subset.xml \
	Utilities/guidelines.xsl Utilities-1/guidelines.xsl
	find . -name "semantic.cache" | xargs rm -f
	(cd Test; make clean)
	(cd Exemplars; make clean)
	rm -rf FASC-*
	rm -rf catalogue.* modList
	rm -f       p5.xml
	rm -f       Guidelines.epub
	rm -f       Test/detest.rnc
	rm -f       Test/detest.rng
	rm -f       Test/detest.dtd
