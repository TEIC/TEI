LANGUAGE=en
OTHERLANGUAGES=fr
LATEX=pdflatex
XELATEX=xelatex
VERBOSE=
PREFIX=/usr
TEISERVER=http://tei.oucs.ox.ac.uk/Query/
SOURCETREE=Source
LANGTREE=${SOURCETREE}/Guidelines/${LANGUAGE}
DRIVER=${LANGTREE}/guidelines-${LANGUAGE}.xml
FASCFILE=${LANGTREE}/FASC-${CHAP}.xml
ROMAOPTS="--localsource=${DRIVER}"
XSL=/usr/share/xml/tei/stylesheet
XSLP4=/usr/share/xml/teip4/stylesheet
CHAPTER=$(shell find ${LANGTREE} -iname ${CHAP}*.xml)
# If you have not installed the Debian packages, uncomment one
# of the next two lines:
#XSL=../Stylesheets/release/tei-xsl/p5
#XSL=http://www.tei-c.org/stylesheet/release/xml/tei
JING=jing
ONVDL=onvdl

.PHONY: convert dtds schemas html validate valid test split oddschema exampleschema fascicule clean dist exemplars

default: dtds schemas exemplars html-web validate-html

convert: dtds schemas

dtds: check
	-mkdir DTD
	-rm DTD/*
	# generate the DTDs
	xmllint --noent --xinclude ${DRIVER} | \
	xsltproc --stringparam outputDir DTD 	\
	--stringparam lang ${LANGUAGE} \
	--stringparam TEIC true \
	${XSL}/odds/odd2dtd.xsl -
	#for i in DTD/* ; do perl -i Utilities/cleandtd.pl $$i; done	

schemas:check
	-mkdir Schema
	-rm Schema/*
	# generate the relaxNG schemas
	xmllint --noent --xinclude ${DRIVER} | \
	xsltproc  \
	--stringparam lang ${LANGUAGE} \
	--stringparam TEIC true \
	${XSL}/odds/odd2relax.xsl -
	# do the indentation better 
	for i in Schema/* ; \
	do echo clean $$i; \
	   xmllint --format $$i \
	     | sed -e '/^ *$$/d' \
	     > x.tmp ; \
	     mv x.tmp $$i;\
	   done
	# convert RelaxNG XML syntax to compact syntax with trang
	(cd Schema; for i in *rng; do trang $$i `basename $$i .rng`.rnc;done)
	# improve the positioning of blank lines in the RelaxNG compact syntax output for human readability
	(for i in Schema/*.rnc; do t=`basename $$i .rnc`.tmp; mv $$i $$t; ./Utilities/fix_rnc_whitespace.perl < $$t > $$i; rm $$t; done)
	xmllint --noent --xinclude ${DRIVER} | xsltproc Utilities/extract-sch.xsl - > p5.sch

html-web: check
	perl -p -e \
		"s+http://www.tei-c.org/release/xml/tei/stylesheet+${XSL}+; \
		 s+/usr/share/xml/tei/stylesheet+${XSL}+;" \
		Utilities/odd2htmlp5.xsl.model > Utilities/odd2htmlp5.xsl
	-rm -rf Guidelines-web-tmp
	-mkdir Guidelines-web-tmp
	for i in ${LANGUAGE} ${OTHERLANGUAGES} ; do \
	echo making HTML Guidelines for language $$i ; \
	mkdir -p Guidelines-web-tmp/$$i/html; \
	cp rightarrow.gif guidelines.css TEI-glow.png Guidelines-web-tmp/$$i/html/ ; \
	xmllint --noent --xinclude ${SOURCETREE}/Guidelines/$$i/guidelines-$$i.xml \
	| xsltproc ${VERBOSE} \
		--stringparam outputDir Guidelines-web-tmp/$$i/html \
		--stringparam displayMode both \
	        --stringparam lang $$i \
	        --stringparam doclang $$i \
	    Utilities/guidelines.xsl - ; \
	cp -r ${SOURCETREE}/Images Guidelines-web-tmp/$$i/html/ ; \
	(cd Guidelines-web-tmp/$$i/html; for i in *.html; do perl -i ../../../Utilities/cleanrnc.pl $$i;done); \
	(cd Guidelines-web-tmp/$$i/html; perl -p -i -e 's+/logos/TEI-glow+TEI-glow+' guidelines.css); \
	done
	-rm -rf Guidelines-web
	-mv Guidelines-web-tmp Guidelines-web

html-web-beta: check
	perl -p -e \
		"s+http://www.tei-c.org/release/xml/tei/stylesheet+${XSL}+; \
		 s+/usr/share/xml/tei/stylesheet+${XSL}+;" \
		Utilities/odd2htmlp5.xsl.model > Utilities/odd2htmlp5.xsl
	-rm -rf Guidelines-web-beta-tmp
	-mkdir Guidelines-web-beta-tmp
	for i in ${LANGUAGE} ${OTHERLANGUAGES} ; do \
	echo making beta HTML Guidelines for language $$i ; \
	mkdir -p Guidelines-web-beta-tmp/$$i/html; \
	cp rightarrow.gif guidelines-beta.css guidelines-print-beta.css TEI-glow.png Guidelines-web-beta-tmp/$$i/html/ ; \
	xmllint --noent --xinclude ${SOURCETREE}/Guidelines/$$i/guidelines-$$i.xml \
	| xsltproc ${VERBOSE} \
		--stringparam outputDir Guidelines-web-beta-tmp/$$i/html \
		--stringparam displayMode both \
	        --stringparam lang $$i \
		--stringparam verbose true \
	        --stringparam doclang $$i \
	    Utilities/guidelines-beta.xsl - ; \
	cp -r ${SOURCETREE}/Images Guidelines-web-beta-tmp/$$i/html/ ; \
	(cd Guidelines-web-beta-tmp/$$i/html; for i in *.html; do perl -i ../../../Utilities/cleanrnc.pl $$i;done); \
	(cd Guidelines-web-beta-tmp/$$i/html; perl -p -i -e 's+/logos/TEI-glow+TEI-glow+' guidelines-beta.css); \
	done; 
	-rm -rf Guidelines-web-beta
	-mv Guidelines-web-beta-tmp Guidelines-web-beta

validate-html:
	for i in ${LANGUAGE} ${OTHERLANGUAGES} ; do \
	(cd Guidelines-web/$$i/html;\
	 for i in *.html; do \
	echo validate $$i; \
	xmllint --dropdtd $$i > z_$$i; \
	$(JING) -c ../../../xhtml.rnc z_$$i; \
	 rm z_$$i;\
	 done); \
	done

validate-html-beta:
	for i in ${LANGUAGE} ${OTHERLANGUAGES} ; do \
	(cd Guidelines-web-beta/$$i/html;\
	 for i in *.html; do \
	echo validate $$i; \
	xmllint --dropdtd $$i > z_$$i; \
	$(JING) -c ../../../xhtml.rnc z_$$i; \
	 rm z_$$i;\
	 done); \
	done

html:check subset
	-rm -rf Guidelines
	-mkdir Guidelines
	perl -p -e "s+/usr/share/xml/tei/stylesheet+${XSL}+" Utilities/odd2htmlp5.xsl.model > Utilities/odd2htmlp5.xsl
	xsltproc \
	    --stringparam outputDir Guidelines \
	    --stringparam localsource `pwd`/p5subset.xml \
	    --stringparam STDOUT false \
	    --stringparam displayMode rnc \
	    --stringparam lang ${LANGUAGE} \
	    --stringparam doclang ${LANGUAGE} \
	    Utilities/guidelines-print.xsl \
	    ${DRIVER} 
	-cp guidelines.css Guidelines
	-cp -r ${SOURCETREE}/Images Guidelines/
	(cd Guidelines; for i in *.html; do perl -i ../Utilities/cleanrnc.pl $$i;done)
	(cd Guidelines; perl -p -i -e 's+ xmlns:html="http://www.w3.org/1999/xhtml"++' index.html)
	-xmllint --noout --valid Guidelines/index.html

xml: check subset 
	xmllint --noent --xinclude ${DRIVER} |  \
	xsltproc  -o Guidelines.xml \
	--stringparam displayMode rnc  \
	${XSL}/odds/odd2lite.xsl -
	@echo Success. Created Guidelines.xml. now attempt to validate
	-rnv Exemplars/teilite.rnc Guidelines.xml

pdf: xml
	@echo Checking you have a running ${LATEX} before trying to make PDF...
	which ${XELATEX} || exit 1
	xsltproc Utilities/guidelines-latex.xsl Guidelines.xml \
	> Guidelines.tex
	cp -r Source/Images .
	-${XELATEX} -interaction=nonstopmode Guidelines
	-${XELATEX} -interaction=nonstopmode Guidelines
	for i in Source/Images/*.png; do rm `basename $$i`;done

validate: schemas oddschema exampleschema valid 

valid: jing_version=$(wordlist 1,3,$(shell jing))
valid: check
	@echo --------- jing
	@echo ${jing_version}
#	We have discovered that jing reports 3-letter language codes
#	from ISO 639-2 as illegal values of xml:lang= even though
#	they are perfectly valid per RFC 3066. We have submitted a
#	bug report, and for now just throw away those error messages
#	with grep -v. Note that we discard *all* such messages, even
#	though fewer than 500 of the 17,576 possible combinations
#	(i.e. < 3%) are valid codes.
	 $(JING) -t p5odds.rng ${DRIVER} \
	 | grep -v ": error: Illegal xml:lang value \"[A-Za-z][A-Za-z][A-Za-z]\"\.$$"
	@echo --------- rnv
	-xmllint --noent --xinclude ${DRIVER} > Source.xml
	-rnv -v p5odds.rnc Source.xml
	rm Source.xml
	@echo --------- nvdl
#	onvdl seems to report an "unfinished element" every
#	time a required child element from another namespace occurs
#	in the instance. In our case, this happens every time there
#	is an <egXML> child of <exemplum>. Since the error message is
#	non-specific (doesn't tell us that <exemplum> is the
#	unfinished element or that one of <eg> or <egXML> would be
#	required to make it finished) we end up throwing out all such
#	messages via the grep -v command so we're not annoyed by the
#	over 800 that are not really problems.
	-${ONVDL} p5.nvdl ${DRIVER} \
	 | grep -v ': error: unfinished element$$'
	@echo --------- Schematron
	-${JING} p5.sch ${DRIVER} 
	@echo --------- XSLT validator
	xsltproc prevalidator.xsl ${DRIVER} > pointerattributes.xsl
	xsltproc validator.xsl ${DRIVER}
	rm pointerattributes.xsl
	@echo --------- xmllint RelaxNG test REMOVED
#	@xmllint --version
#	-xmllint  --relaxng p5odds.rng --noent --xinclude --noout ${DRIVER}

test:
	(cd Test; make XSL=${XSL})

exemplars:
	(cd Exemplars; make XSL=${XSL} PREFIX=${PREFIX})

split:
	(mkdir Split; cd Split; xmllint --noent --xinclude  ../${DRIVER} | xsltproc ../Utilities/divsplit.xsl -)

oddschema: 
	(cd Exemplars;make names)
	roma ${ROMAOPTS} --nodtd --noxsd --xsl=${XSL}/ --teiserver=${TEISERVER} p5odds.odd .


exampleschema:
	(cd Exemplars;make names)
	roma  ${ROMAOPTS} --nodtd --noxsd --xsl=${XSL}/ --teiserver=${TEISERVER} p5odds-ex.odd . 

#	 perl -p -i -e 's+org/ns/1.0+org/ns/Examples+' p5examples.rnc && \
#	 perl -p -i -e 's+org/ns/1.0+org/ns/Examples+' p5examples.rng

subset:
	-xmllint --noent --xinclude ${DRIVER} \
	 | xsltproc -o p5subset.xml Utilities/subset.xsl - || echo "failed to extract subset from ${DRIVER}." 

fascicule: subset
#	fail if we can't find the chapter
	test -n "${CHAPTER}"
#	create fascicule file by appending head- and tail- boilerplate text to chapter
	cp fasc-head.xml ${FASCFILE}
	cat ${CHAPTER} >> ${FASCFILE}
	cat fasc-tail.xml  >> ${FASCFILE}
	xmllint --noent --xinclude ${FASCFILE} | xsltproc \
	-o FASC-${CHAP}-Guidelines/index.html ${VERBOSE} \
	--stringparam localsource `pwd`/p5subset.xml \
	--stringparam cssFile guidelines.css \
	--stringparam displayMode rnc \
	--stringparam lang ${LANGUAGE} \
	--stringparam outputDir . \
	Utilities/guidelines.xsl -
	(cd FASC-${CHAP}-Guidelines; for i in *.html; do perl -i ../Utilities/cleanrnc.pl $$i;done)
	-cp *.gif guidelines.css FASC-${CHAP}-Guidelines
	-jing p5odds.rng ${FASCFILE}
	xsltproc -o FASC-${CHAP}-lite.xml  \
	  --stringparam localsource `pwd`/p5subset.xml \
	  --stringparam displayMode rnc \
	  --stringparam lang ${LANGUAGE} \
	  ${XSL}/odds/odd2lite.xsl ${FASCFILE}
	-perl Utilities/cleanrnc.pl FASC-${CHAP}-lite.xml \
	  | xsltproc ${XSL}/teic/teilatex-teic.xsl - > FASC-${CHAP}.tex
	ln -s Source/Images Images
	-TEXINPUTS=/TEI/Talks/texconfig: pdflatex FASC-${CHAP} 
	-TEXINPUTS=/TEI/Talks/texconfig: pdflatex FASC-${CHAP}
	mv ${FASCFILE} ./
	rm Images

dist: clean dist-source dist-schema dist-doc dist-test dist-database dist-exemplars

dist-source: 
	rm -rf release/tei-p5-source*
	mkdir -p release/tei-p5-source/share/xml/tei/odd
	tar -c -f - --exclude "*~" --exclude .svn 	COPYING \
	Makefile \
	ReleaseNotes  \
	Source  \
	TEI-glow.png \
	Utilities   \
	sitemap.xmap \
	index.xml \
	VERSION  \
	fasc-head.xml \
	fasc-tail.xml \
	p5examples.rnc \
	p5nrl.xml \
	p5odds-ex.odd \
	p5odds.odd \
	p5odds.rnc \
	p5sch.xsl \
	schematron1-5.rnc \
	guidelines.css \
	validator.xsl \
	xhtml.rnc \
	| (cd release/tei-p5-source/share/xml/tei/odd; tar xf - )
	(cd release; 	\
	ln -s tei-p5-source tei-p5-source-`cat ../VERSION` ; \
	zip -r tei-p5-source-`cat ../VERSION`.zip tei-p5-source-`cat ../VERSION` )

dist-schema: schemas dtds oddschema
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
	zip -r tei-p5-schema-`cat ../VERSION`.zip tei-p5-schema-`cat ../VERSION` )

dist-doc:  html-web
	rm -rf release/tei-p5-doc*
	mkdir -p release/tei-p5-doc/share/doc/tei-p5-doc
	(cd Guidelines-web; tar --exclude .svn -c -f - . ) \
	| (cd release/tei-p5-doc/share/doc/tei-p5-doc; tar xf - )
	for i in ReleaseNotes/readme*xml; do  \
	xsltproc \
	--stringparam cssFile html/guidelines.css \
	${XSL}/teic/teihtml-teic.xsl $$i \
	> release/tei-p5-doc/share/doc/tei-p5-doc/`basename $$i .xml`.html; \
	done
	(cd release; 	\
	ln -s tei-p5-doc tei-p5-doc-`cat ../VERSION` ; \
	zip -r tei-p5-doc-`cat ../VERSION`.zip tei-p5-doc-`cat ../VERSION` )

dist-test: 
	rm -rf release/tei-p5-test*
	mkdir -p release/tei-p5-test/share/tei
	(cd Test; make clean)
	tar --exclude "*~" --exclude .svn -c -f - Test \
	| (cd release/tei-p5-test/share/tei; tar xf - )
	(cd release; 	\
	ln -s tei-p5-test tei-p5-test-`cat ../VERSION` ; \
	zip -r tei-p5-test-`cat ../VERSION`.zip tei-p5-test-`cat ../VERSION` )

dist-exemplars: 
	(cd Exemplars; make dist)

dist-database: 
	rm -rf release/tei-p5-database*
	mkdir -p release/tei-p5-database/share/xml/tei/xquery
	(cd Query; tar --exclude .svn --exclude "*~" -c -f - . ) \
	| (cd release/tei-p5-database/share/xml/tei/xquery; tar xf - )
	(cd release; 	\
	ln -s tei-p5-database tei-p5-database-`cat ../VERSION` ; \
	zip -r tei-p5-database-`cat ../VERSION`.zip tei-p5-database-`cat ../VERSION` )

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
	@echo Checking you have a running XML tools and Perl before trying to run transform...
	@echo -n xsltproc: 
	@which xsltproc || exit 1
	@echo -n Perl: 
	@which perl || exit 1
	@echo -n xmllint: 
	@which xmllint || exit 1
	@echo -n trang: 
	@which trang || exit 1
	@echo -n jing: 
	@which jing || exit 1
	@echo -n saxon: 
	@which saxon || exit 1

changelog:
	(LastDate=`head -1 ReleaseNotes/ChangeLog | awk '{print $$1}'`; \
	svn log -v -r 'HEAD:{'$$LastDate'}' | perl ../gnuify-changelog.pl | grep -v "^;" > newchanges)
	mv ReleaseNotes/ChangeLog oldchanges
	cat newchanges oldchanges > ReleaseNotes/ChangeLog
	rm newchanges oldchanges


catalogue:
	xsltproc -o catalogue.xml Utilities/catalogue.xsl ${DRIVER}
	xsltproc --xinclude ${XSL}/html/tei.xsl  catalogue.xml > catalogue.html
	echo Made catalogue.html

clean:
	-rm -rf release Guidelines Guidelines-web Schema DTD dtd Split RomaResults *~ 
	-rm Guidelines.??? \
	p5examples.rng \
	p5odds.rng \
	*.xsd \
	p5.sch
	find . -name "semantic.cache" | xargs rm -f
	(cd Test; make clean)
	(cd Exemplars; make clean)
	rm -rf FASC-*
	rm -rf catalogue.* modList


