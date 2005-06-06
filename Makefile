TEISERVER=http://tei.oucs.ox.ac.uk/Query/
PREFIX=/usr
XSL=/usr/share/xml/tei/stylesheet
# alternativly, if you have not installed the Debian packages, uncomment the next line:
# XSL=http://www.tei-c.org/stylesheet
ROMAOPTS=

.PHONY: convert dtds schemas html validate valid test split oddschema exampleschema fascicule exist clean dist

default: dtds schemas html

convert: dtds schemas

dtds: check
	-mkdir DTD
	-rm DTD/*
	# generate the DTDs
	xmllint --noent   Source-driver.xml | \
	xsltproc --stringparam outputDir DTD ${XSL}/odds/odd2dtd.xsl -
	for i in DTD/* ; do perl -i Tools/cleandtd.pl $$i; done	
	# I cannot be bothered to see why these don't work,
	# just hack them by hand.
	perl -p -i -e 's/,\|/\|/' DTD/core.dtd
	perl -p -i -e 's/\| %mix.seg;/%mix.seg;/' DTD/core.dtd
	perl -p -i -e 's/\(\%schemapattern;\)\?/%schemapattern;/' DTD/tagdocs.dtd
	perl -p -i -e 's/\)\*\(/\)\*,\(/' DTD/textstructure.dtd
	(cd DTD; ln -s tei.dtd tei2.dtd)

schemas:check
	-mkdir Schema
	-rm Schema/*
	# generate the relaxNG schemas
	xmllint --noent   Source-driver.xml | xsltproc -stringparam verbose true ${XSL}/odds/odd2relax.xsl -
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
	(for i in Schema/*.rnc; do t=`basename $$i .rnc`.tmp; mv $$i $$t; ./Tools/fix_rnc_whitespace.perl < $$t > $$i; rm $$t; done)
	xmllint --noent   Source-driver.xml | xsltproc extract-sch.xsl - > p5.sch

html-web: check
	-rm -rf Guidelines-web
	-mkdir Guidelines-web
	xmllint --noent    Source-driver.xml | xsltproc \
	-o Guidelines-web/index.html \
	--stringparam displayMode rnc \
	--stringparam outputDir . \
	guidelines.xsl - 
	-cp *.gif *.css Guidelines-web
	(cd Guidelines-web; for i in *.html; do perl -i ../Tools/cleanrnc.pl $$i;done)

html:check
	-rm -rf Guidelines
	-mkdir Guidelines
	(perl -p -e "s+http://www.tei-c.org/stylesheet+${XSL}+" guidelines-print.xsl > tmp$$$$.xsl; \
	xmllint --noent    Source-driver.xml | xsltproc \
	-o Guidelines/index.html \
	--stringparam cssFile tei-print.css \
	--stringparam displayMode rnc \
	--stringparam outputDir . \
	tmp$$$$.xsl - && rm tmp$$$$.xsl)
	(cd Guidelines; for i in *.html; do perl -i ../Tools/cleanrnc.pl $$i;done)
	-cp *.gif *.css Guidelines

xml: check
	xmllint --noent   Source-driver.xml | perl Tools/cleanrnc.pl | \
	xsltproc  -o Guidelines.xml \
	--stringparam displayMode rnc  \
	${XSL}/odds/teixml-odds.xsl -
	@echo Success. Created Guidelines.xml

pdf: xml
	@echo Checking you have a running pdfLaTeX before trying to make PDF...
	which pdflatex || exit 1
	test -d /TEI/Talks/texconfig || exit 1
	xsltproc ${XSL}/../teic/teilatex-teic-P5.xsl Guidelines.xml \
	> Guidelines.tex
	TEXINPUTS=/TEI/Talks/texconfig: pdflatex Guidelines
	TEXINPUTS=/TEI/Talks/texconfig: pdflatex Guidelines

validate: oddschema exampleschema valid

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
	-jing -t p5odds.rng Source-driver.xml \
	 | grep -v ": error: Illegal xml:lang value \"[A-Za-z][A-Za-z][A-Za-z]\"\.$$"
	@echo --------- xx/rnv
	-xmllint --noent  Source-driver.xml > Source.xml
	-rnv -v p5odds.rnc Source.xml && rm Source.xml
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
	-jing p5nrl.xml Source-driver.xml \
	 | grep -v ": error: Illegal xml:lang value \"[A-Za-z][A-Za-z][A-Za-z]\"\.$$" \
	 | grep -v ': error: unfinished element$$'
	@echo --------- XSLT validator
	xsltproc validator.xsl Source-driver.xml >& tmp && sed 's/TEI...\/text...\/body...\///' tmp && rm tmp
	@echo --------- xmllint
	@xmllint --version
	-xmllint  --relaxng p5odds.rng --noent --noout Source-driver.xml

test:
	(cd Test; make)

split:
	(mkdir Split; cd Split; xmllint --noent   ../Source-driver.xml | xsltproc ../divsplit.xsl -)

oddschema: 
	roma $(ROMAOPTS) --nodtd --noxsd --xsl=$(XSL)/ --teiserver=$(TEISERVER) p5odds.odd .


exampleschema:
	roma  --nodtd --noxsd --xsl=$(XSL)/ --teiserver=$(TEISERVER) p5odds-ex.odd . && \
	 perl -p -i -e 's+org/ns/1.0+org/ns/Examples+' p5examples.rnc && \
	 perl -p -i -e 's+org/ns/1.0+org/ns/Examples+' p5examples.rng

fascicule:
	cat fasc-head.xml `find Source -name $(CHAP).odd` fasc-tail.xml > FASC-$(CHAP).xml
	xmllint --noent    FASC-$(CHAP).xml | xsltproc \
	-o FASC-$(CHAP)-Guidelines/index.html \
	--stringparam TEISERVER $(TEISERVER) \
	--stringparam verbose true \
	--stringparam displayMode rnc \
	--stringparam outputDir . \
	${XSL}/odds/odd2html.xsl - 
	(cd FASC-$(CHAP)-Guidelines; for i in *.html; do perl -i ../Tools/cleanrnc.pl $$i;done)
	-jing p5odds.rng FASC-$(CHAP).xml 
	xsltproc -o FASC-$(CHAP)-lite.xml  \
	--stringparam TEISERVER $(TEISERVER) \
	--stringparam displayMode rnc \
	${XSL}/odds/teixml-odds.xsl FASC-$(CHAP).xml 
	perl Tools/cleanrnc.pl FASC-$(CHAP)-lite.xml | \
	xsltproc  \
	 ${XSL}/../teic/teilatex-teic-P5.xsl - > FASC-$(CHAP).tex
	TEXINPUTS=/TEI/Talks/texconfig: pdflatex FASC-$(CHAP) 
	TEXINPUTS=/TEI/Talks/texconfig: pdflatex FASC-$(CHAP) 

dist: clean dist-source dist-schema dist-doc dist-test dist-database

dist-source: 
	rm -rf release/tei-p5/source
	mkdir -p release/tei-p5-source/share/tei
	tar -c -f - --exclude "*~" --exclude CVS *.* VERSION ChangeLog Source Makefile Tools  \
	| (cd release/tei-p5-source/share/tei; tar xf - )
	(cd release; 	\
	ln -s tei-p5-source tei-p5-source-`cat ../VERSION` ; \
	zip -r tei-p5-source-`cat ../VERSION`.zip tei-p5-source-`cat ../VERSION` )

dist-schema: schemas dtds
	rm -rf release/tei-p5-schema
	mkdir -p release/tei-p5-schema/share/xml/tei/schema/dtd/p5
	mkdir -p release/tei-p5-schema/share/xml/tei/schema/relaxng/p5
	(cd DTD; tar --exclude CVS -c -f - .) \
	| (cd release/tei-p5-schema/share/xml/tei/schema/dtd/p5; tar xf - )
	(cd Schema; tar --exclude CVS -c -f - .) \
	| (cd release/tei-p5-schema/share/xml/tei/schema/relaxng/p5; tar xf - )
	(cd release; 	\
	ln -s tei-p5-schema tei-p5-schema-`cat ../VERSION` ; \
	zip -r tei-p5-schema-`cat ../VERSION`.zip tei-p5-schema-`cat ../VERSION` )

dist-doc:  html
	rm -rf release/tei-p5-doc
	mkdir -p release/tei-p5-doc/share/doc/tei/P5
	tar --exclude CVS -c -f - Guidelines \
	| (cd release/tei-p5-doc/share/doc/tei/P5; tar xf - )
	(cd release; 	\
	ln -s tei-p5-doc tei-p5-doc-`cat ../VERSION` ; \
	zip -r tei-p5-doc-`cat ../VERSION`.zip tei-p5-doc-`cat ../VERSION` )

dist-test: 
	rm -rf release/tei-p5-test
	mkdir -p release/tei-p5-test/share/tei
	(cd Test; make clean)
	tar --exclude "*~" --exclude CVS -c -f - Test \
	| (cd release/tei-p5-test/share/tei; tar xf - )
	(cd release; 	\
	ln -s tei-p5-test tei-p5-test-`cat ../VERSION` ; \
	zip -r tei-p5-test-`cat ../VERSION`.zip tei-p5-test-`cat ../VERSION` )

dist-database: 
	rm -rf release/tei-p5-database
	mkdir -p release/tei-p5-database/share/doc/tei/web
	tar --exclude CVS -c -f - Query \
	| (cd release/tei-p5-database/share/doc/tei/web; tar xf - )
	(cd release; 	\
	ln -s tei-p5-database tei-p5-database-`cat ../VERSION` ; \
	zip -r tei-p5-database-`cat ../VERSION`.zip tei-p5-database-`cat ../VERSION` )

install-schema: dist-schema
	echo Making schema release in ${PREFIX}
	(cd release/tei-p5-schema; tar cf - .) | (cd ${PREFIX}; tar xf - )

install-doc: dist-doc
	echo Making documentation release in ${PREFIX}
	(cd release/tei-p5-doc; tar cf - .) | (cd ${PREFIX}; tar xf - )

install-source: dist-source
	echo Making source release in ${PREFIX}
	(cd release/tei-p5-source; tar cf - .) | (cd ${PREFIX}; tar xf - )

install-test: dist-test
	echo Making testfiles release in ${PREFIX}
	(cd release/tei-p5-test; tar cf - .) | (cd ${PREFIX}; tar xf - )

install-database: dist-database
	echo Making database release in ${PREFIX}
	(cd release/tei-p5-database; tar cf - .) | (cd ${PREFIX}; tar xf - )

install: clean install-schema install-doc install-test install-source install-database

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

clean:
	-rm -rf release Guidelines Schema DTD dtd Split RomaResults *~
	-rm Guidelines.xml core.rnc header.rnc tei.rnc \
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

