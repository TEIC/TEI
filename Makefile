SUFFIX=xml
LANGUAGE=en
PREFIX=/usr
TEISERVER=http://tei.oucs.ox.ac.uk/Query/
SOURCETREE=Source
DRIVER=guidelines-${LANGUAGE}.xml
ROMAOPTS="--localsource=Source/${DRIVER}"
XSL=/usr/share/xml/tei/stylesheet
XSLP4=/usr/share/xml/teip4/stylesheet
#XSL=../Stylesheets
# alternativly, if you have not installed the Debian packages, uncomment the next line:
# XSL=http://www.tei-c.org/stylesheet/release/xml/tei

.PHONY: convert dtds schemas html validate valid test split oddschema exampleschema fascicule clean dist

default: dtds schemas html

convert: dtds schemas

dtds: check
	-mkdir DTD
	-rm DTD/*
	# generate the DTDs
	xmllint --noent --xinclude ${SOURCETREE}/${DRIVER} | \
	xsltproc --stringparam outputDir DTD 	\
	--stringparam lang ${LANGUAGE} \
	--stringparam TEIC true \
	--stringparam verbose true ${XSL}/odds/odd2dtd.xsl -
	for i in DTD/* ; do perl -i Utilities/cleandtd.pl $$i; done	
	# I cannot be bothered to see why these don't work,
	# just hack them by hand.
	perl -p -i -e 's/,\|/\|/' DTD/core.dtd
	perl -p -i -e 's/\| %mix.seg;/%mix.seg;/' DTD/core.dtd
	perl -p -i -e 's/\(\%macro.schemapattern;\)\?/%macro.schemapattern;/' DTD/tagdocs.dtd
	perl -p -i -e 's/\)\*\(/\)\*,\(/' DTD/textstructure.dtd
	(cd DTD; ln -s tei.dtd tei2.dtd)

schemas:check
	-mkdir Schema
	-rm Schema/*
	# generate the relaxNG schemas
	xmllint --noent --xinclude ${SOURCETREE}/${DRIVER} | \
	xsltproc --stringparam verbose true \
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
	xmllint --noent --xinclude ${SOURCETREE}/${DRIVER} | xsltproc Utilities/extract-sch.xsl - > p5.sch

html-web: check
	perl -p -e "s+http://www.tei-c.org/release/xml/tei/stylesheet+${XSL}+" Utilities/odd2htmlp5.xsl.model > Utilities/odd2htmlp5.xsl
	-rm -rf Guidelines-web
	-mkdir Guidelines-web
	xmllint --noent --xinclude ${SOURCETREE}/${DRIVER} \
	| xsltproc \
	    -o Guidelines-web/index.html \
	    --stringparam displayMode rnc \
	    --stringparam lang ${LANGUAGE} \
	    --stringparam outputDir . \
	    Utilities/guidelines.xsl -
	-cp *.css Guidelines-web
	-cp $(SOURCETREE)/Images/* Guidelines-web/
	(cd Guidelines-web; for i in *.html; do perl -i ../Utilities/cleanrnc.pl $$i;done)

html:check subset
	-rm -rf Guidelines
	-mkdir Guidelines
	perl -p -e "s+http://www.tei-c.org/release/xml/tei/stylesheet+${XSL}+" Utilities/odd2htmlp5.xsl.model > Utilities/odd2htmlp5.xsl
	xmllint --noent --xinclude ${SOURCETREE}/${DRIVER} \
	| xsltproc \
	    -o Guidelines/index.html \
	    --stringparam localsource `pwd`/p5subset.xml \
	    --stringparam cssFile tei-print.css \
	    --stringparam displayMode rnc \
	    --stringparam verbose true \
	    --stringparam outputDir . \
	    --stringparam lang ${LANGUAGE} \
	    Utilities/guidelines-print.xsl -
	-cp *.css Guidelines
	-cp $(SOURCETREE)/Images/* Guidelines/
	(cd Guidelines; for i in *.html; do perl -i ../Utilities/cleanrnc.pl $$i;done)

xml: check subset
	xmllint --noent --xinclude ${SOURCETREE}/${DRIVER} | perl Utilities/cleanrnc.pl | \
	xsltproc  -o Guidelines.xml \
	--stringparam displayMode rnc  \
	${XSL}/odds/odd2lite.xsl -
	@echo Success. Created Guidelines.xml

pdf: xml
	@echo Checking you have a running pdfLaTeX before trying to make PDF...
	which pdflatex || exit 1
	test -d /TEI/Talks/texconfig || exit 1
	xsltproc ${XSL}/teic/teilatex-teic.xsl Guidelines.xml \
	> Guidelines.tex
	TEXINPUTS=/TEI/Talks/texconfig: pdflatex Guidelines
	TEXINPUTS=/TEI/Talks/texconfig: pdflatex Guidelines

validate: oddschema exampleschema valid

valid: jing_version=$(wordlist 1,3,$(shell jing))
valid: check
	-xmllint --noent --xinclude ${SOURCETREE}/${DRIVER} > Source.xml
	@echo --------- jing
	@echo ${jing_version}
#	We have discovered that jing reports 3-letter language codes
#	from ISO 639-2 as illegal values of xml:lang= even though
#	they are perfectly valid per RFC 3066. We have submitted a
#	bug report, and for now just throw away those error messages
#	with grep -v. Note that we discard *all* such messages, even
#	though fewer than 500 of the 17,576 possible combinations
#	(i.e. < 3%) are valid codes.
	 jing -t p5odds.rng Source.xml \
	 | grep -v ": error: Illegal xml:lang value \"[A-Za-z][A-Za-z][A-Za-z]\"\.$$"
	@echo --------- rnv
	-rnv -v p5odds.rnc Source.xml
	@echo --------- nrl
#	In addition to erroneously reporting xml:lang= 3-letter
#	values, jing seems to report an "unfinished element" every
#	time a required child element from another namespace occurs
#	in the instance. In our case, this happens every time there
#	is an <egXML> child of <exemplum>. Since the error message is
#	non-specific (doesn't tell us that <exemplum> is the
#	unfinished element or that one of <eg> or <egXML> would be
#	required to make it finished) we end up throwing out all such
#	messages via the grep -v command so we're not annoyed by the
#	over 800 that are not really problems.
	-jing p5nrl.xml Source.xml \
	 | grep -v ": error: Illegal xml:lang value \"[A-Za-z][A-Za-z][A-Za-z]\"\.$$" \
	 | grep -v ': error: unfinished element$$'
	@echo --------- XSLT validator
	xsltproc validator.xsl Source.xml >& tmp && sed 's/TEI...\/text...\/body...\///' tmp && rm tmp
	@echo --------- xmllint RelaxNG test REMOVED
#	@xmllint --version
#	-xmllint  --relaxng p5odds.rng --noent --xinclude --noout ${SOURCETREE}/${DRIVER}
	rm Source.xml

test:
	(cd Test; make)

split:
	(mkdir Split; cd Split; xmllint --noent --xinclude  ../${SOURCETREE}/${DRIVER} | xsltproc ../Utilities/divsplit.xsl -)

oddschema: 
	roma $(ROMAOPTS) --nodtd --noxsd --xsl=$(XSL)/ --teiserver=$(TEISERVER) p5odds.odd .


exampleschema:
	roma  $(ROMAOPTS) --nodtd --noxsd --xsl=$(XSL)/ --teiserver=$(TEISERVER) p5odds-ex.odd . && \
	 perl -p -i -e 's+org/ns/1.0+org/ns/Examples+' p5examples.rnc && \
	 perl -p -i -e 's+org/ns/1.0+org/ns/Examples+' p5examples.rng

subset:
	@echo '<xsl:stylesheet version="1.0"' > subset.xsl
	@echo '  xmlns:tei="http://www.tei-c.org/ns/1.0"' >> subset.xsl
	@echo '  xmlns:xsl="http://www.w3.org/1999/XSL/Transform">' >> subset.xsl
	@echo '  <xsl:template match="/">' >> subset.xsl
	@echo '    <tei:TEI>' >> subset.xsl
	@echo '<xsl:copy-of select=".//tei:elementSpec"/>' >> subset.xsl
	@echo '      <xsl:copy-of select=".//tei:macroSpec"/>' >> subset.xsl
	@echo '      <xsl:copy-of select=".//tei:classSpec"/>' >> subset.xsl
	@echo '      <xsl:copy-of select=".//tei:moduleSpec"/>' >> subset.xsl
	@echo '    </tei:TEI>' >> subset.xsl
	@echo '</xsl:template>' >> subset.xsl
	@echo '</xsl:stylesheet>' >> subset.xsl
	-xmllint --noent --xinclude ${SOURCETREE}/${DRIVER} \
	 | xsltproc -o p5subset.xml subset.xsl - || die "failed to extract subset from ${SOURCETREE}/${DRIVER}."
	rm subset.xsl

fascicule: subset 
	cp fasc-head.xml FASC-$(CHAP).xml
#	perl -p -i -e "s+\"internal-entities.dtd+\"$(SOURCETREE)/$(LANGUAGE)/internal-entities.dtd+" FASC-$(CHAP).xml
#	perl -p -i -e "s+\"external-entities.dtd+\"$(SOURCETREE)/$(LANGUAGE)/external-entities.dtd+" FASC-$(CHAP).xml
	cat `find $(SOURCETREE)/Guidelines/$(LANGUAGE) -iname $(CHAP).$(SUFFIX)`  >> FASC-$(CHAP).xml
	cat fasc-tail.xml  >> FASC-$(CHAP).xml
	export H=`pwd`; xmllint --noent --xinclude FASC-$(CHAP).xml | xsltproc \
	-o FASC-$(CHAP)-Guidelines/index.html \
	--stringparam localsource `pwd`/p5subset.xml \
	--stringparam cssFile tei.css \
	--stringparam verbose true \
	--stringparam displayMode rnc \
	--stringparam lang ${LANGUAGE} \
	--stringparam outputDir . \
	Utilities/guidelines.xsl -
	(cd FASC-$(CHAP)-Guidelines; for i in *.html; do perl -i ../Utilities/cleanrnc.pl $$i;done)
	-cp *.gif *.css FASC-$(CHAP)-Guidelines
	-jing p5odds.rng FASC-$(CHAP).xml 
	export H=`pwd`; \
	xsltproc -o FASC-$(CHAP)-lite.xml  \
	--stringparam localsource `pwd`/p5subset.xml \
	--stringparam displayMode rnc \
	--stringparam lang ${LANGUAGE} \
	$(XSL)/odds/odd2lite.xsl FASC-$(CHAP).xml 
	perl Utilities/cleanrnc.pl FASC-$(CHAP)-lite.xml | \
	xsltproc  \
	 ${XSL}/teic/teilatex-teic.xsl - > FASC-$(CHAP).tex
	TEXINPUTS=/TEI/Talks/texconfig: pdflatex FASC-$(CHAP) 
	TEXINPUTS=/TEI/Talks/texconfig: pdflatex FASC-$(CHAP) 

dist: clean dist-source dist-schema dist-doc dist-test dist-database dist-exemplars

dist-source: 
	rm -rf release/tei-p5-source*
	mkdir -p release/tei-p5-source/share/xml/tei/odd
	tar -c -f - --exclude "*~" --exclude .svn *.* VERSION ChangeLog Source Makefile Utilities  \
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

dist-doc:  html
	rm -rf release/tei-p5-doc*
	mkdir -p release/tei-p5-doc/share/doc/tei-p5-doc/html
	(cd Guidelines; tar --exclude .svn -c -f - . ) \
	| (cd release/tei-p5-doc/share/doc/tei-p5-doc/html; tar xf - )
	for i in readme*xml; do  \
	xsltproc \
	--stringparam cssFile html/teic.css \
	${XSLP4}/teic/teihtml-teic.xsl $$i \
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

changelog:
	(LastDate=`head -1 ReleaseNotes/ChangeLog | awk '{print $$1}'`; \
	svn log -v -r 'HEAD:{'$$LastDate'}' | grep -v "^;" | perl ../gnuify-changelog.pl > newchanges)
	mv ReleaseNotes/ChangeLog oldchanges
	cat newchanges oldchanges > ReleaseNotes/ChangeLog

clean:
	-rm -rf release Guidelines Guidelines-web Schema DTD dtd Split RomaResults *~
	-rm Guidelines.xml core.rnc header.rnc tei.rnc \
	p5subset.xml \
	dictionaries.rnc  linking.rnc  textstructure.rnc \
	figures.rnc       tagdocs.rnc  \
	analysis.rnc \
	certainty.rnc \
	corpus.rnc \
	declarefs.rnc \
	drama.rnc \
	gaiji.rnc \
	iso-fs.rnc \
	msdescription.rnc \
	namesdates.rnc \
	nets.rnc \
	p5examples.rnc \
	p5odds.compiled.rnc \
	p5odds.rnc \
	spoken.rnc \
	textcrit.rnc \
	transcr.rnc \
	verse.rnc \
	mathml*rnc  \
	p5examples.rng \
	p5odds.rng \
	*.xsd \
	p5.sch
	find . -name "semantic.cache" | xargs rm -f
	(cd Test; make clean)
	(cd Exemplars; make clean)
	rm -rf FASC-*

