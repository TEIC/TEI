# Main makefile for TEI P5
# $Id$
ALLLANGUAGES=en
LANGUAGE=en
INPUTLANGUAGE=en
DOCUMENTATIONLANGUAGE=en
LATEX=pdflatex
XELATEXFLAGS=--output-driver="xdvipdfmx -V 5" --interaction=batchmode
XELATEX=xelatex 
VERBOSE=
PREFIX=/usr
SOURCETREE=Source
DRIVER=${SOURCETREE}/guidelines-${INPUTLANGUAGE}.xml
XSL=/usr/share/xml/tei/stylesheet
XSLP4=/usr/share/xml/teip4/stylesheet
# If you have not installed the Debian packages, uncomment one
# of the next two lines:
#XSL=../Stylesheets/release/tei-xsl/p5
#XSL=http://www.tei-c.org/stylesheet/release/xml/tei
JING=jing
TRANG=trang
SAXON=java -Xmx752m -jar Utilities/lib/saxon-9.1.0.8.jar -ext:on
VERSION=`cat VERSION`
UPVERSION=`cat ../VERSION`
ODD2DTD=odds2/odd2dtd.xsl
ODD2RELAX=odds2/odd2relax.xsl
ODD2LITE=odds2/odd2lite.xsl

.PHONY: convert schemas html-web validate valid test clean dist exemplars

default: validate exemplars test html-web

convert: schemas

check: check.stamp  p5.xml

check.stamp: 
	@echo Checking you have running XML tools and Perl before trying to run transform...
	@echo -n Perl: 
	@command -v  perl || exit 1
	@echo -n Java: 
	@command -v  java || exit 1
	@echo -n xmllint: 
	@command -v  xmllint || exit 1
	@echo -n trang: 
	@command -v  ${TRANG} || exit 1
	@echo -n jing: 
	@command -v  ${JING} || exit 1
	@echo -n XeLaTeX: 
	@command -v  xelatex || exit 1
	touch check.stamp

p5.xml: ${DRIVER} Source/Specs/*.xml Source/Guidelines/en/*.xml
	xmllint --xinclude --dropdtd --noent ${DRIVER} > p5.xml

schemas: schemas.stamp

schemas.stamp: check.stamp p5.xml 
	rm -rf DTD Schema
	mkdir DTD Schema
	@echo BUILD: Generate modular DTDs, Schemas, Schematron and miscellaneous outputs
	ant -lib /usr/share/java/jing.jar:/usr/share/saxon/saxon9he.jar -f antbuildschemas.xml -DXSL=${XSL} subset outputs
	@echo "BUILD: Generate modular RELAX NG (compact) schemas using trang"
	(cd Schema; for i in *rng; do ${TRANG} $$i `basename $$i .rng`.rnc;done)
	${TRANG} p5odds.rng p5odds.rnc
	touch schemas.stamp

html-web: check.stamp p5.xml  html-web.stamp

html-web.stamp:  check.stamp p5.xml  Utilities/guidelines.xsl.model
	@echo BUILD: Making HTML Guidelines for language ${LANGUAGE}
	perl -p -e \
		"s+http://www.tei-c.org/release/xml/tei/stylesheet+${XSL}+; \
		 s+/usr/share/xml/tei/stylesheet+${XSL}+;" \
		Utilities/guidelines.xsl.model > Utilities/guidelines.xsl
	rm -rf Guidelines-web
	if [ -n ${GOOGLEANALYTICS} ] ; then curl -s http://www.tei-c.org/index.xml | sed 's/content="text\/html"/content="text\/html; charset=utf-8"/' | xmllint --html --noent --dropdtd --xmlout - > Utilities/teic-index.xml;fi
	echo '<project basedir="." default="html" name="buildweb"><import file="antbuildweb.xml"/><target name="html">' > buildweb.xml
	for i in $(ALLLANGUAGES) ;do \
		mkdir -p Guidelines-web/$$i/html; \
		cp odd.css guidelines.css guidelines-print.css Guidelines-web/$$i/html; \
		(cd Source/Guidelines/${INPUTLANGUAGE}; tar --exclude .svn -c -f - Images) | (cd Guidelines-web/$$i/html; tar xf - );\
		(cd webnav; tar --exclude .svn -c -f - .) | (cd Guidelines-web/$$i/html; tar xf - ); \
		echo "<buildweb lang=\"$$i\"/>" >> buildweb.xml; \
	done
	echo '</target></project>' >> buildweb.xml
	ant -lib /usr/share/java/jing.jar:/usr/share/saxon/saxon9he.jar -f buildweb.xml -DgoogleAnalytics=${GOOGLEANALYTICS}
	rm -f buildweb.xml Utilities/teic-index.xml
	touch html-web.stamp

validate-html:
	@echo BUILD: Validate HTML version of Guidelines
	@ echo SUSPENDED FOR NOW

old-validate-html:
	cd Guidelines-web/${LANGUAGE}/html;for i in *.html; do xmllint --noent --dropdtd $$i > z_$$i; done;ant  -lib /usr/share/java/jing.jar:/usr/share/saxon/saxon9he.jar -f ../../../validatehtml.xml;rm z_*

teiwebsiteguidelines:
	@echo BUILD: make HTML version of Guidelines just for TEI web site
	rm -rf teiwebsiteguidelines.zip 
	rm -f html-web.stamp
	make html-web ALLLANGUAGES="en es de ja ko fr it zh-TW" GOOGLEANALYTICS=UA-4372657-1
	(cd Guidelines-web; zip -r -q ../teiwebsiteguidelines.zip . ) 

pdf: Guidelines.pdf pdf-complete

fontcheck:
	xelatex --interaction=batchmode Utilities/fonttest 
	if [ -f "missfont.log" ]  ; then  \
	  perl -p -i -e 's/(.*Minion)/%\1/;s/(.*Myriad)/%\1/' Utilities/guidelines.xsl ;\
	  echo "========================="; \
	  echo "Note: you do not have Minion or Myriad fonts installed, reverting to Computer Modern " ;\
	  echo "========================="; \
	fi
	rm -f fonttest.*

Guidelines.pdf: check.stamp p5.xml Utilities/guidelines-latex.xsl
	@echo BUILD: build Lite version of Guidelines
	${SAXON} ${SAXON_ARGS}  -o:Guidelines.xml -s:p5.xml -xsl:${XSL}/${ODD2LITE} displayMode=rnc lang=${LANGUAGE} \
	        doclang=${DOCUMENTATIONLANGUAGE} \
	        documentationLanguage=${DOCUMENTATIONLANGUAGE}	${VERBOSE}
	@echo BUILD: build LaTeX version of Guidelines from Lite
	perl -p -e \
		"s+http://www.tei-c.org/release/xml/tei/stylesheet+${XSL}+; \
		 s+/usr/share/xml/tei/stylesheet+${XSL}+;" \
		Utilities/guidelines-latex.xsl > Utilities/guidelines.xsl
	@echo BUILD: check XeLaTeX system works locally
	${SAXON} ${SAXON_ARGS}  Guidelines.xml Utilities/guidelines.xsl > Guidelines.tex
	rm -f Utilities/guidelines.xsl
	for i in Guidelines-REF*tex; \
	  do \
	     perl Utilities/rewrapRNC-in-TeX.pl <$$i>$$i.new; \
		echo NOTE: diff $$i.new $$i; \
		diff $$i.new $$i; \
		mv $$i.new $$i; \
	done
	@echo BUILD: build PDF version of Guidelines from LaTeX using XeLaTeX
	@echo Make sure you have Junicode, Arphic and Mincho fonts installed 
	echo '*' | ${XELATEX}  Guidelines

pdf-complete:
	(echo '*' | ${XELATEX} ${XELATEXFLAGS} Guidelines) 2> pdfbuild.log 1> pdfbuild.log
	grep -v "Failed to convert input string to UTF16" pdfbuild.log
	makeindex -s p5.ist Guidelines 
	(echo '*' | ${XELATEX} ${XELATEXFLAGS} Guidelines) 2> pdfbuild.log 1> pdfbuild.log
	grep -v "Failed to convert input string to UTF16" pdfbuild.log
	(echo '*' | ${XELATEX} ${XELATEXFLAGS} Guidelines) 2> pdfbuild.log 1> pdfbuild.log
	grep -v "Failed to convert input string to UTF16" pdfbuild.log
	rm pdfbuild.log
	rm Guidelines.xml
	for i in Guidelines*aux; do perl -p -i -e 's/.*zf@fam.*//' $$i; done

chapterpdfs: check
	for i in `grep "\\include{" Guidelines.tex | sed 's/.*{\(.*\)}.*/\\1/'`; \
	do echo PDF for chapter $$i; \
	echo  $$i | ${XELATEX} Guidelines; \
	echo  $$i | ${XELATEX} Guidelines ; \
	mv Guidelines.pdf $$i.pdf; \
	perl -p -i -e 's/.*zf@fam.*//' $$i.aux; \
	done

validate: schemas.stamp valid 

#The following line commented out in 10605 by MDH. Restored 2012-10-26 to see if it was the source of 2.2.0 release problems.
valid: jing_version=$(wordlist 1,3,$(shell jing))
valid: check.stamp p5.xml 
#	@echo BUILD: Check validity with jing
#	@echo ${jing_version}
#	We have discovered that jing reports 3-letter language codes
#	from ISO 639-2 as illegal values of xml:lang= even though
#	they are perfectly valid per RFC 3066. We have submitted a
#	bug report, and for now just throw away those error messages
#	with grep -v. Note that we discard *all* such messages, even
#	though fewer than 500 of the 17,576 possible combinations
#	(i.e. < 3%) are valid codes.
#	$(JING) -t p5odds.rng p5.xml 
#
	@echo BUILD: Check validity with nvdl/jing, including all examples with feasible validity
	./run-onvdl p5.nvdl p5.xml 
	@echo BUILD: Check validity with rnv if we have it
	-command -v  rnv && rnv -v p5odds.rnc p5.xml
	@echo BUILD: Check full validity of relevant examples with nvdl
	${SAXON} -s:p5.xml -xsl:Utilities/extractegXML.xsl > v.body
	echo "<!DOCTYPE p [" > v.header
	(cd valid; ls | perl -p -e  "s+(.*)+<\!ENTITY \1 SYSTEM \"valid/\1\">+") >> v.header
	echo "]>" >> v.header
	cat v.header v.body > v.xml
	./run-onvdl  p5valid.nvdl  v.xml
	rm v.body v.header
	@echo BUILD: Check validity with Schematron
	${SAXON} ${SAXON_ARGS}  -s:p5.isosch -xsl:Utilities/iso_schematron_message_xslt2.xsl > p5.isosch.xsl
	${SAXON} ${SAXON_ARGS}  -s:p5.xml -xsl:p5.isosch.xsl
	@echo BUILD: Check validity with local XSLT script
	${SAXON} ${SAXON_ARGS}  -s:p5.xml -xsl:Utilities/prevalidator.xsl > Utilities/pointerattributes.xsl
	${SAXON} ${SAXON_ARGS}  -o:ValidatorLog.xml -s:p5.xml -xsl:Utilities/validator.xsl 
	(grep -q "<ERROR>" ValidatorLog.xml;if [ $$? -eq 1 ] ; then echo No problems ; else echo "Oh dear me. ERROR found"; grep "<ERROR>" ValidatorLog.xml;false; fi)
	diff ValidatorLog.xml expected-results/ValidatorLog.xml
	cat ValidatorLog.xml
	rm ValidatorLog.xml Utilities/pointerattributes.xsl 
	#@echo BUILD: Check validity with xmllint
	#xmllint  --relaxng p5odds.rng --noent --xinclude --noout p5.xml
	@echo BUILD: Check for places with no examples
	${SAXON} -s:p5.xml -xsl:Utilities/listspecwithnoexample.xsl 
	@echo BUILD: do graphics files exist
	${SAXON} -s:p5.xml -xsl:Utilities/listgraphics.xsl | sh

#test: debversion
test: schemas.stamp
	@echo BUILD Run test cases for P5
	(cd Test; make XSL=${XSL})

exemplars:  schemas.stamp 
	@echo BUILD TEI Exemplars
	(cd Exemplars; make XSL=${XSL} PREFIX=${PREFIX})

dist-source.stamp: check.stamp p5.xml  schemas.stamp
	@echo BUILD: Make distribution directory for source
	rm -rf release/tei-p5-source*
	mkdir -p release/tei-p5-source/share/xml/tei/odd
	tar -c -f - --exclude "*~" --exclude .svn 	\
	p5subset.xml \
	p5subset.json \
	p5subset.js \
	stripspace.xsl.model \
	p5attlist.txt \
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
	p5odds.rng \
	p5odds.rnc \
	p5odds-examples.odd \
	p5odds-examples.rng \
	p5odds-examples.rnc \
	relax.rng \
	schematron.rng \
	iso-schematron.rng \
	p5sch.xsl \
	schematron1-5.rnc \
	run-onvdl \
	*.css \
	webnav \
	xhtml.rnc \
	| (cd release/tei-p5-source/share/xml/tei/odd; tar xf - )
	touch dist-source.stamp
	rm p5subset.json p5subset.js p5attlist.txt stripspace.xsl.model

dist-schema.stamp:check.stamp p5.xml schemas.stamp 
	@echo BUILD: Make distribution directory for schema
	rm -rf release/tei-p5-schema*
	mkdir -p release/tei-p5-schema/share/xml/tei/schema/dtd
	mkdir -p release/tei-p5-schema/share/xml/tei/schema/relaxng
	(cd DTD; tar --exclude .svn -c -f - .) \
	| (cd release/tei-p5-schema/share/xml/tei/schema/dtd; tar xf - )
	cp catalog.p5 release/tei-p5-schema/share/xml/tei/schema/catalog.xml
	(cd Schema; tar --exclude .svn -c -f - .) \
	| (cd release/tei-p5-schema/share/xml/tei/schema/relaxng; tar xf - )
	touch dist-schema.stamp

dist-doc.stamp:  check.stamp p5.xml 
	@echo BUILD: Make distribution directory for doc
	rm -rf release/tei-p5-doc*
	mkdir -p release/tei-p5-doc/share/doc/tei-p5-doc/en
	cp VERSION release/tei-p5-doc/share/doc/tei-p5-doc
	@echo BUILD: Make web pages for release notes
	for i in ReleaseNotes/readme*xml; \
	do  teitohtml --css=en/html/guidelines.css --profile=readme $$i  \
		./release/tei-p5-doc/share/doc/tei-p5-doc/`basename $$i .xml`.html; \
	done
	@echo BUILD: Make web guidelines in all supported languages
	make html-web ALLLANGUAGES="en es de ja ko fr it zh-TW"
	(cd Guidelines-web; tar --exclude .svn -c -f - . ) | (cd release/tei-p5-doc/share/doc/tei-p5-doc; tar xf - )
	@echo BUILD: make PDF version of Guidelines
	make pdf
	@echo BUILD: make ePub and Kindle version of Guidelines
	make epub
	make mobi
	cp Guidelines.pdf Guidelines.epub release/tei-p5-doc/share/doc/tei-p5-doc/en
	-test -f Guidelines.mobi  && cp Guidelines.mobi release/tei-p5-doc/share/doc/tei-p5-doc/en
	touch dist-doc.stamp

dist-test.stamp: check.stamp p5.xml 
	@echo BUILD: Make distribution directory for test
	rm -rf release/tei-p5-test*
	mkdir -p release/tei-p5-test/share/xml/tei
	(cd Test; make clean)
	tar --exclude "*~" --exclude .svn -c -f - Test \
	| (cd release/tei-p5-test/share/xml/tei; tar xf - )
	touch dist-test.stamp

dist-exemplars.stamp: check.stamp p5.xml  schemas.stamp
	@echo BUILD: Make distribution directory for exemplars
	(cd Exemplars; make XSL=${XSL} dist)
	tar --exclude "*~" --exclude .svn -c -f - Exemplars \
	| (cd release/tei-p5-exemplars/share/xml/tei; tar xf - )
	touch dist-exemplars.stamp

dist-database.stamp: check.stamp p5.xml 
	@echo BUILD: Make distribution directory for database
	rm -rf release/tei-p5-database*
	mkdir -p release/tei-p5-database/share/xml/tei/xquery
	(cd Query; tar --exclude .svn --exclude "*~" -c -f - . ) \
	| (cd release/tei-p5-database/share/xml/tei/xquery; tar xf - )
	touch dist-database.stamp

dist-source: dist-source.stamp

dist-schema: dist-schema.stamp

dist-doc: dist-doc.stamp

dist-test: dist-test.stamp

dist-exemplars: dist-exemplars.stamp

dist-database: dist-database.stamp

dist: dist-source dist-schema dist-doc dist-test dist-exemplars dist-database
	@echo BUILD: Make overall zip archive
	rm -rf tei-*.zip release/xml release/doc
	(cd release/tei-p5-database/share; tar cf - . | (cd ../../; tar xf - ))
	(cd release/tei-p5-doc/share; tar cf - . | (cd ../../; tar xf - ))
	(cd release/tei-p5-exemplars/share; tar cf - . | (cd ../../; tar xf - ))
	(cd release/tei-p5-schema/share; tar cf - . | (cd ../../; tar xf - ))
	(cd release/tei-p5-source/share; tar cf - . | (cd ../../; tar xf - ))
	(cd release/tei-p5-test/share; tar cf - . | (cd ../../; tar xf - ))
	(cd release; zip -q -r ../tei-${UPVERSION}.zip xml doc)
	@echo BUILD: Make individual zip archives
	(cd release; rm -rf tei-p5-*-${UPVERSION}.zip)
	(cd release; zip -q -r tei-p5-database-${UPVERSION}.zip tei-p5-database )
	(cd release; zip -q -r tei-p5-doc-${UPVERSION}.zip tei-p5-doc )
	(cd release; zip -q -r tei-p5-exemplars-${UPVERSION}.zip tei-p5-exemplars )
	(cd release; zip -q -r tei-p5-source-${UPVERSION}.zip tei-p5-database )
	(cd release; zip -q -r tei-p5-schema-${UPVERSION}.zip tei-p5-schema )
	(cd release; zip -q -r tei-p5-test-${UPVERSION}.zip tei-p5-test )

debversion:
	sh ./mydch debian-tei-p5-database/debian/changelog
	sh ./mydch debian-tei-p5-doc/debian/changelog
	sh ./mydch debian-tei-p5-exemplars/debian/changelog
	sh ./mydch debian-tei-p5-schema/debian/changelog
	sh ./mydch debian-tei-p5-source/debian/changelog
	sh ./mydch debian-tei-p5-test/debian/changelog

deb: debversion
	rm -f tei-p5-*_*deb
	rm -f tei-p5-*_*changes
	rm -f tei-p5-*_*build
	@echo BUILD: make Debian tei-p5-database package
	(cd debian-tei-p5-database; debclean;debuild -eXSL=${XSL} --no-lintian  -nc  -b  -i.svn -I.svn -uc -us)
	@echo BUILD: make Debian tei-p5-doc package
	(cd debian-tei-p5-doc; debclean;debuild -eXSL=${XSL} --no-lintian  -nc  -b  -i.svn -I.svn -uc -us)
	@echo BUILD: make Debian tei-p5-exemplars package
	(cd debian-tei-p5-exemplars; debclean;debuild -eXSL=${XSL} --no-lintian  -nc  -b  -i.svn -I.svn -uc -us)
	@echo BUILD: make Debian tei-p5-schema package
	(cd debian-tei-p5-schema; debclean;debuild -eXSL=${XSL} --no-lintian  -nc  -b  -i.svn -I.svn -uc -us)
	@echo BUILD: make Debian tei-p5-source package
	(cd debian-tei-p5-source; debclean;debuild -eXSL=${XSL} --no-lintian  -nc  -b  -i.svn -I.svn -uc -us)
	@echo BUILD: make Debian tei-p5-test package
	(cd debian-tei-p5-test; debclean;debuild -eXSL=${XSL} --no-lintian  -nc  -b  -i.svn -I.svn -uc -us)

install-schema: dist-schema
	@echo Making schema release in ${PREFIX}
	(cd release/tei-p5-schema; tar cf - .) | (cd ${PREFIX}; tar xf - )

install-doc: dist-doc
	@echo BUILD: Make doc release in ${PREFIX}
	(cd release/tei-p5-doc; tar cf - .) | (cd ${PREFIX}; tar xf - )

install-source: dist-source
	@echo BUILD: Making source release in ${PREFIX}
	(cd release/tei-p5-source; tar cf - .) | (cd ${PREFIX}; tar xf - )

install-test: dist-test
	@echo BUILD: Making testfiles release in ${PREFIX}
	(cd release/tei-p5-test; tar cf - .) | (cd ${PREFIX}; tar xf - )

install-exemplars: dist-exemplars
	@echo BUILD: Making exemplars release in ${PREFIX}
	(cd release/tei-p5-exemplars; tar cf - share) | (cd ${PREFIX}; tar xf -)

install-database: dist-database
	@echo BUILD: Making database release in ${PREFIX}
	(cd release/tei-p5-database; tar cf - .) | (cd ${PREFIX}; tar xf - )

install: clean install-schema install-doc install-test install-exemplars install-source install-database

epub: Guidelines.epub

Guidelines.epub: check.stamp p5.xml 
	@echo BUILD: Make epub version of Guidelines
	teitoepub --profiledir=${XSL}/profiles --coverimage=Utilities/cover.jpg --profile=tei p5.xml Guidelines.epub
	java -jar Utilities/epubcheck-1.2.jar Guidelines.epub
	touch Guidelines.epub

epub3: check.stamp p5.xml 
	teitoepub3 --profiledir=${XSL}/profiles --coverimage=Utilities/cover.jpg --profile=tei p5.xml Guidelines.epub
	java -jar Utilities/epubcheck3.jar Guidelines.epub

mobi:  Guidelines.mobi

Guidelines.mobi: check.stamp p5.xml 
	teitoepub --profiledir=${XSL}/profiles --coverimage=Utilities/cover.jpg --profile=teikindle p5.xml Guidelines-kindle.epub
	-command -v  kindlegen && kindlegen Guidelines-kindle.epub -o Guidelines.mobi
	rm Guidelines-kindle.epub

changelog:
	(LastDate=`head -1 ReleaseNotes/ChangeLog | awk '{print $$1}'`; \
	svn log -v -r 'HEAD:{'$$LastDate'}' | perl ../gnuify-changelog.pl | grep -v "^;" > newchanges)
	mv ReleaseNotes/ChangeLog oldchanges
	cat newchanges oldchanges > ReleaseNotes/ChangeLog
	rm newchanges oldchanges


dependencies:
	@echo to make this thing build under Ubuntu/Debian, here are all the packages you will need:
	@echo	jing
	@echo	msttcorefonts
	@echo	rnv
	@echo	saxon
	@echo	tei-oxygen
	@echo	tei-p5-source
	@echo	tei-p5-xsl
	@echo	tei-p5-xsl2
	@echo	tei-xsl-common
	@echo	trang-java
	@echo	ttf-arphic-ukai
	@echo	ttf-arphic-uming 
	@echo	ttf-baekmuk 
	@echo	ttf-junicode
	@echo	ttf-kochi-gothic
	@echo	ttf-kochi-mincho 
	@echo	zip 

clean:
	rm -rf release Guidelines Guidelines-web Schema DTD dtd Split 
	rm -rf Guidelines.??? Guidelines-* 
	rm -f Guidelines.epub
	rm -f Guidelines.mobi
	rm -f p5odds-examples.rng  p5odds-examples.rnc 
	rm -f p5odds.rng p5odds.rnc 
	rm -f *.xsd 
	rm -f anything buildweb.xml
	rm -f p5.sch p5.isosch 
	rm -f *.isosch.xsl 
	rm -f tei-*.zip 
	rm -f Test/*.isosch 
	rm -f p5subset.xml p5subset.json p5subset.js p5.xml 
	rm -f Utilities/guidelines.xsl Utilities-1/guidelines.xsl
	find . -name "semantic.cache" | xargs rm -f
	(cd Test; make clean)
	(cd Exemplars; make clean)
	rm -rf FASC-*
	rm -rf catalogue.* modList
	rm -f       p5.xml
	rm -f       Test/detest.rnc
	rm -f       Test/detest.rng
	rm -f       Test/detest.dtd
	rm -rf valid v.xml
	rm -f v.body v.header missfont.log 
	rm -f *.stamp
	rm -f tei-p5-*_*deb
	rm -f tei-p5-*_*changes
	rm -f tei-p5-*_*build
	rm -f teiwebsiteguidelines.zip
	rm -rf bin
