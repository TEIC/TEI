# Main makefile for TEI P5
# $Id$
VCS := git
ALLLANGUAGES=en
INJENKINS=false
INPUTLANGUAGE=en
LATEX=pdflatex
XELATEX=xelatex
VERBOSE=
PREFIX=/usr
SOURCETREE=Source
DRIVER=${SOURCETREE}/guidelines-${INPUTLANGUAGE}.xml
XSL=/usr/share/xml/tei/stylesheet
# If you have not installed the Debian packages, uncomment one
# of the next two lines:
#XSL=../Stylesheets/release/tei-xsl/p5
#XSL=http://www.tei-c.org/stylesheet/release/xml/tei
VERSION=`cat VERSION`
UPVERSION=`cat ../VERSION`
SAXONJAR=saxon10he.jar
# Saxon-HE-9.4.0.6.jar is needed by the epubcheck program and is in Utilities/lib
ANT=ANT_OPTS="-Xss2m -Xmx752m -Djava.awt.headless=true" ant -q

# The following will include a local (not in the repo) makefile
# called local.mk (if it exists) that you can use to (e.g.) override
# the variables above. It might include
# XSL=/path/to/my/copy/of/the/Stylesheets
# for example.
sinclude local.mk

.PHONY: convert schemas html-web validate valid test clean dist exemplars

default: validate exemplars test html-web

# Note on construction of this file:
#   For every target that does stuff the target starts by echoing a
#   blank line and a notification line about which target is being
#   run. The “$@” variable is used, which writes out the file name of
#   the target. If the target name contains a ‘%’ (and thus gets run
#   over multiple matching files) the message says “target file”
#   instead of just “target”. Thus the user sees the actual file being
#   worked on, and can figure out the target from that name.
# —Syd, 2025-09-21 (copied from 2021-08-20 msg in P5/Test/Makefile.)

convert: schemas
	@echo " "
	@echo "--------- P5/Makefile work on target $@ ---------"

check: check.stamp  p5.xml
	@echo " "
	@echo "--------- P5/Makefile work on target $@ ---------"

check.stamp:
	@echo " "
	@echo "--------- P5/Makefile work on target $@ ---------"
	@echo Checking you have running XML tools and Perl before trying to run transform...
	@echo -n Ant:
	@command -v  ant || exit 1
	@echo -n Perl:
	@command -v  perl || exit 1
	@echo -n Java:
	@command -v  java || exit 1
	@echo -n xmllint:
	@command -v  xmllint || exit 1
	touch check.stamp

p5.xml: ${DRIVER} Source/Specs/*.xml Source/Guidelines/en/*.xml p5odds.odd check.stamp
	@echo " "
	@echo "--------- P5/Makefile work on target $@ ---------"
	@echo "Creating an XML file with the date last updated and commit number garnered from the VCS system (${VCS}),"
	@echo "if available. (This XML file, repodate.xml, will be inserted into the <editionStmt> of the Guidelines)"
	case ${VCS} in \
	   svn) if [ ${INJENKINS} = "true" ] ; \
	           then svn info --xml svn://svn.code.sf.net/p/tei/code/trunk/P5 ; \
	           else svn info --xml  ; \
           fi > repodate.xml ; \
           ;; \
       git) git log --max-count=1 --pretty=format:"<info type=\"git\"><entry><commit revision=\"%h\"><date>%ai</date></commit></entry></info>" > repodate.xml \
            ;; \
        *) echo "<info type=\"local_build\"><entry><commit revision=\"local_build\"><date>"`date '+%Y-%m-%d %H:%M:%S %z'`"</date></commit></entry></info>" > repodate.xml \
            ;; \
    esac
	@echo BUILD: Generate modular DTDs, Schemas, Schematron and miscellaneous outputs
	${ANT} -lib Utilities/lib/jing.jar:Utilities/lib/${SAXONJAR} -f antbuilder.xml -DXSL=${XSL} -DDRIVER=${DRIVER} base subset outputs
	@echo "BUILD: Generate modular RELAX NG (compact) schemas using trang"
	(cd Schema; for i in *rng; do java -jar ../Utilities/lib/trang.jar $$i `basename $$i .rng`.rnc;done)
	touch schemas.stamp

schemas: schemas.stamp
	@echo " "
	@echo "--------- P5/Makefile work on target $@ ---------"

schemas.stamp: p5.xml
	@echo " "
	@echo "--------- P5/Makefile work on target $@ ---------"

html-web: check.stamp p5.xml  html-web.stamp
	@echo " "
	@echo "--------- P5/Makefile work on target $@ ---------"

html-web.stamp:  check.stamp p5.xml  Utilities/guidelines.xsl.model
	@echo " "
	@echo "--------- P5/Makefile work on target $@ ---------"
	@echo BUILD: Making HTML Guidelines for language ${INPUTLANGUAGE}
	@# remove vestiges of previous run
	rm -rf Guidelines-web
	@# generate XSL stylesheet by modifying paths in our model stylesheet
	java -jar Utilities/lib/${SAXONJAR} -s:Utilities/guidelines.xsl.model -xsl:Utilities/guidelines_model_2_executable.xslt -o:Utilities/guidelines.xsl XSL="${XSL}"
	@# for each language, create a subdirectory and pre-populate it with CSS, source, and web navigation stuff
	for i in $(ALLLANGUAGES) ;do \
		mkdir -p Guidelines-web/$$i/html; \
		cp odd.css guidelines.css guidelines-print.css teic.css Guidelines-web/$$i/html; \
		(cd Source/Guidelines/${INPUTLANGUAGE}; tar --exclude .svn -c -f - Images) | (cd Guidelines-web/$$i/html; tar xf - );\
		(cd webnav; tar --exclude .svn -c -f - .) | (cd Guidelines-web/$$i/html; tar xf - ); \
	done
	@# create a temporary 'buildweb.xml' file for `ant` to run
	echo '<project basedir="." default="html" name="buildweb"><import file="antbuildweb.xml"/><target name="html">' \
		> buildweb.xml
	for i in $(ALLLANGUAGES) ;do \
		echo "<buildweb lang=\"$$i\"/>" >> buildweb.xml; \
	done
	echo '</target></project>' >> buildweb.xml
	@# use `ant` to run the lovely ANT file we just generated
	${ANT} -lib Utilities/lib/jing.jar:Utilities/lib/${SAXONJAR} -f buildweb.xml -DgoogleAnalytics=${GOOGLEANALYTICS}
	@# clean up after ourselves
	rm -f buildweb.xml
	@# let future `make`s know we been there, done that
	touch html-web.stamp

teiwebsiteguidelines:
	@echo " "
	@echo "--------- P5/Makefile work on target $@ ---------"
	@echo BUILD: make HTML version of Guidelines just for TEI web site
	rm -rf teiwebsiteguidelines.zip
	rm -f html-web.stamp
	make html-web ALLLANGUAGES="en es de ja ko fr it zh-TW" GOOGLEANALYTICS=true
	(cd Guidelines-web; zip -r -q ../teiwebsiteguidelines.zip . )

epub: Guidelines.epub

Guidelines.epub: check.stamp p5.xml
	@echo " "
	@echo "--------- P5/Makefile work on target $@ ---------"
	@echo BUILD: Make epub version of Guidelines
	${ANT} -f ${XSL}/epub3/build-to.xml -lib Utilities/lib/${SAXONJAR} -Dprofiledir=${XSL}/profiles -Dprofile=tei -DinputFile=`pwd`/p5.xml -DoutputFile=`pwd`/Guidelines.epub -Dcoverimage=`pwd`/Utilities/cover.jpg
	java -jar Utilities/epubcheck3.jar Guidelines.epub
	touch Guidelines.epub

epub3: check.stamp p5.xml
	@echo " "
	@echo "--------- P5/Makefile work on target $@ ---------"
	${ANT}  -f ${XSL}/epub3/build-to.xml -lib Utilities/lib/${SAXONJAR} -Dprofiledir=${XSL}/profiles -Dprofile=tei -DinputFile=`pwd`/p5.xml -DoutputFile=`pwd`/Guidelines.epub -Dcoverimage=`pwd`/Utilities/cover.jpg
	java -jar Utilities/epubcheck3.jar Guidelines.epub

fonttest:
	@echo " "
	@echo "--------- P5/Makefile work on target $@ ---------"
	-xelatex --interaction=batchmode Utilities/fonttest
	if [ -f "missfont.log" ]  ; then  \
	  perl -p -i -e 's/(.*Minion)/%\1/;s/(.*Myriad)/%\1/' Utilities/guidelines.xsl ;\
	  echo "========================="; \
	  echo "Note: you do not have Minion or Myriad fonts installed, reverting to Computer Modern " ;\
	  echo "========================="; \
	fi
	rm -f fonttest.*

pdf-init: check.stamp p5.xml Utilities/guidelines-latex.xsl
	@echo " "
	@echo "--------- P5/Makefile work on target $@ ---------"
	@echo check if XeLaTeX exist
	@command -v xelatex || exit 1
	java -jar Utilities/lib/${SAXONJAR} -s:Utilities/guidelines-latex.xsl -xsl:Utilities/guidelines_model_2_executable.xslt -o:Utilities/guidelines.xsl XSL="${XSL}"
	@echo BUILD: build Lite version of Guidelines, then LaTeX version of Guidelines from Lite, then run to PDF using XeLaTeX
	@echo Make sure you have Junicode and Noto CJK fonts installed
	${ANT} -lib Utilities/lib/${SAXONJAR} -f antbuilder.xml -DXSL=${XSL} -DXELATEX=${XELATEX} pdfonce

pdf: pdf-init
	@echo " "
	@echo "--------- P5/Makefile work on target $@ ---------"
	${ANT} -lib Utilities/lib/${SAXONJAR} -f antbuilder.xml -DXSL=${XSL} -DXELATEX=${XELATEX} pdfrest 2> pdfbuild.log 1> pdfbuild.log
	grep -v "Failed to convert input string to UTF16" pdfbuild.log
	for auxFile in Guidelines*aux; do perl -p -i -e 's/.*zf@fam.*//' $$auxFile; done

rewraprnc:
	@echo " "
	@echo "--------- P5/Makefile work on target $@ ---------"
	for i in Guidelines-REF*tex; \
	  do \
	     perl Utilities/rewrapRNC-in-TeX.pl <$$i>$$i.new; \
		echo NOTE: diff $$i.new $$i; \
		diff $$i.new $$i; \
		mv $$i.new $$i; \
	done

chapterpdfs: check
	@echo " "
	@echo "--------- P5/Makefile work on target $@ ---------"
	for i in `grep "\\include{" Guidelines.tex | sed 's/.*{\(.*\)}.*/\\1/'`; \
	do echo PDF for chapter $$i; \
	echo  $$i | ${XELATEX} Guidelines; \
	echo  $$i | ${XELATEX} Guidelines ; \
	mv Guidelines.pdf $$i.pdf; \
	perl -p -i -e 's/.*zf@fam.*//' $$i.aux; \
	done

validate: schemas.stamp valid
	@echo " "
	@echo "--------- P5/Makefile work on target $@ ---------"

valid: jing_version=$(wordlist 1,3,$(shell jing))
valid: check.stamp p5.xml
	@echo " "
	@echo "--------- P5/Makefile work on target $@ ---------"
	@echo BUILD: Check validity with rnv \(https://github.com/dtolpin/RNV\) if we have it
	-command -v  rnv && rnv -v p5odds.rnc p5.xml
	@echo BUILD: Check validity with special-purpose XSL code, looking for bad links etc
	${ANT} -lib Utilities/lib/${SAXONJAR} -f antbuilder.xml -DXSL=${XSL} validators
	@grep -v "ARNING: use of deprecated element" ValidatorLog.xml
	(grep -q "<ERROR>" ValidatorLog.xml;if [ $$? -ne 1 ] ; then echo "Oh dear me. ERROR found";diff ValidatorLog.xml expected-results/ValidatorLog.xml;false; fi)
	sh graphics.sh
	@echo BUILD: Check validity with nvdl, first examples with feasible validity, and then the valid ones
	./run-onvdl p5.nvdl p5.xml
	./run-onvdl p5valid.nvdl v.xml

test: schemas.stamp
	@echo " "
	@echo "--------- P5/Makefile work on target $@ ---------"
	@echo BUILD Run test cases for P5
	(cd Test; make XSL=${XSL} VCS=${VCS})

exemplars:  schemas.stamp
	@echo " "
	@echo "--------- P5/Makefile work on target $@ ---------"
	@echo BUILD TEI Exemplars
	(cd Exemplars; make XSL=${XSL} PREFIX=${PREFIX} VCS=${VCS})

dist-source.stamp: check.stamp p5.xml  schemas.stamp
	@echo " "
	@echo "--------- P5/Makefile work on target $@ ---------"
	@echo BUILD: Make distribution directory for source
	rm -rf release/tei-p5-source*
	mkdir -p release/tei-p5-source/share/xml/tei/odd
	tar -c -f - --exclude "*~" --exclude .svn 	\
	p5subset.xml \
	p5subset*.json \
	stripspace.xsl.model \
	p5attlist.txt \
	Makefile \
	ReleaseNotes  \
	Source \
	Utilities   \
	VERSION  \
	Exemplars/Makefile  \
	Exemplars/mathml2-main.rng \
	Exemplars/mathml2-qname-1.mod \
	Exemplars/mathml2-qname-1.mod.rng \
	Exemplars/mathml2.rng \
	Exemplars/relaxng.rng \
	Exemplars/svg-main.rng \
	Exemplars/svg.rng \
	Exemplars/svg11.rng \
	p5.nvdl \
	p5valid.nvdl \
	p5odds.odd \
	p5odds.isosch \
	p5odds.rng \
	p5odds.rnc \
	p5odds-examples.odd \
	p5odds-examples.rng \
	p5odds-examples.rnc \
	relax.rng \
	schematron.rng \
	iso-schematron.rng \
	schematron1-5.rnc \
	run-onvdl \
	*.css \
	webnav \
	xhtml.rnc \
	| (cd release/tei-p5-source/share/xml/tei/odd; tar xf - )
	(cd release/tei-p5-source/share/xml/tei/odd/Source/Specs; for i in *.xml; do sed -i"" -e 's/http:\/\/jenkins.tei-c.org\/job\/TEIP5\/lastSuccessfulBuild\/artifact\/release\/xml\/tei\/odd\/p5.nvdl/..\/..\/p5.nvdl/' $$i;done)
	(cd release/tei-p5-source/share/xml/tei/odd/Source/Guidelines/en; for i in *.xml; do sed -i""  -e 's/http:\/\/jenkins.tei-c.org\/job\/TEIP5\/lastSuccessfulBuild\/artifact\/release\/xml\/tei\/odd\/p5.nvdl/..\/..\/..\/p5.nvdl/' $$i;done)
	(cd release/tei-p5-source/share/xml/tei/odd/Source/Guidelines/fr; for i in *.xml; do sed -i"" -e 's/http:\/\/jenkins.tei-c.org\/job\/TEIP5\/lastSuccessfulBuild\/artifact\/release\/xml\/tei\/odd\/p5.nvdl/..\/..\/..\/p5.nvdl/' $$i;done)
	touch dist-source.stamp
	rm p5subset*.json p5attlist.txt stripspace.xsl.model

dist-schema.stamp:check.stamp p5.xml schemas.stamp
	@echo " "
	@echo "--------- P5/Makefile work on target $@ ---------"
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

readus:
	@echo " "
	@echo "--------- P5/Makefile work on target $@ ---------"
	@echo Make just the README files for testing purposes only
	@echo BUILD: Make distribution directory for doc
	rm -rf release/tei-p5-doc*
	mkdir -p release/tei-p5-doc/share/doc/tei-p5-doc/en
	cp VERSION release/tei-p5-doc/share/doc/tei-p5-doc
	@echo BUILD: Make web pages for release notes
	for i in ReleaseNotes/readme*xml; do \
	${ANT} -f ${XSL}/html/build-to.xml -lib Utilities/lib/${SAXONJAR} -Dprofiledir=${XSL}/profiles -Dprofile=readme \
	-DinputFile=`pwd`/$$i -DoutputFile=`pwd`/release/tei-p5-doc/share/doc/tei-p5-doc/`basename $$i .xml`.html; \
	done

dist-doc.stamp:  check.stamp p5.xml
	@echo " "
	@echo "--------- P5/Makefile work on target $@ ---------"
	@echo BUILD: Make distribution directory for doc
	rm -rf release/tei-p5-doc*
	mkdir -p release/tei-p5-doc/share/doc/tei-p5-doc/en
	cp VERSION release/tei-p5-doc/share/doc/tei-p5-doc
	@echo BUILD: Make web pages for release notes
	for i in ReleaseNotes/readme*xml; do \
	${ANT} -f ${XSL}/html/build-to.xml -lib Utilities/lib/${SAXONJAR} -Dprofiledir=${XSL}/profiles -Dprofile=readme \
	-DinputFile=`pwd`/$$i -DoutputFile=`pwd`/release/tei-p5-doc/share/doc/tei-p5-doc/`basename $$i .xml`.html; \
	done
	@echo BUILD: Make web guidelines in all supported languages
	make html-web ALLLANGUAGES="en es de ja ko fr it zh-TW"
	(cd Guidelines-web; tar --exclude .svn -c -f - . ) | (cd release/tei-p5-doc/share/doc/tei-p5-doc; tar xf - )
	@echo BUILD: make PDF version of Guidelines
	make pdf
	@echo BUILD: make ePub and Kindle version of Guidelines
	make epub
	cp Guidelines.pdf Guidelines.epub release/tei-p5-doc/share/doc/tei-p5-doc/en
	touch dist-doc.stamp

dist-test.stamp: check.stamp p5.xml
	@echo " "
	@echo "--------- P5/Makefile work on target $@ ---------"
	@echo BUILD: Make distribution directory for test
	rm -rf release/tei-p5-test*
	mkdir -p release/tei-p5-test/share/xml/tei
	(cd Test; make clean)
	tar --exclude "*~" --exclude .svn -c -f - Test \
	| (cd release/tei-p5-test/share/xml/tei; tar xf - )
	touch dist-test.stamp

dist-exemplars.stamp: check.stamp p5.xml  schemas.stamp
	@echo " "
	@echo "--------- P5/Makefile work on target $@ ---------"
	@echo BUILD: Make distribution directory for exemplars
	(cd Exemplars; make XSL=${XSL} dist)
	tar --exclude "*~" --exclude .svn -c -f - Exemplars \
	| (cd release/tei-p5-exemplars/share/xml/tei; tar xf - )
	touch dist-exemplars.stamp

dist-database.stamp: check.stamp p5.xml
	@echo " "
	@echo "--------- P5/Makefile work on target $@ ---------"
	@echo BUILD: Make distribution directory for database
	rm -rf release/tei-p5-database*
	mkdir -p release/tei-p5-database/share/xml/tei/xquery
	(cd Query; tar --exclude .svn --exclude "*~" -c -f - . ) \
	| (cd release/tei-p5-database/share/xml/tei/xquery; tar xf - )
	touch dist-database.stamp

dist-source: dist-source.stamp
	@echo " "
	@echo "--------- P5/Makefile work on target $@ ---------"

dist-schema: dist-schema.stamp
	@echo " "
	@echo "--------- P5/Makefile work on target $@ ---------"

dist-doc: dist-doc.stamp
	@echo " "
	@echo "--------- P5/Makefile work on target $@ ---------"

dist-test: dist-test.stamp
	@echo " "
	@echo "--------- P5/Makefile work on target $@ ---------"

dist-exemplars: dist-exemplars.stamp
	@echo " "
	@echo "--------- P5/Makefile work on target $@ ---------"

dist-database: dist-database.stamp
	@echo " "
	@echo "--------- P5/Makefile work on target $@ ---------"

dist: dist-source dist-schema dist-doc dist-test dist-exemplars dist-database
	@echo " "
	@echo "--------- P5/Makefile work on target $@ ---------"
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
	@echo " "
	@echo "--------- P5/Makefile work on target $@ ---------"
	sh ./mydch debian-tei-p5-database/debian/changelog
	sh ./mydch debian-tei-p5-doc/debian/changelog
	sh ./mydch debian-tei-p5-exemplars/debian/changelog
	sh ./mydch debian-tei-p5-schema/debian/changelog
	sh ./mydch debian-tei-p5-source/debian/changelog
	sh ./mydch debian-tei-p5-test/debian/changelog

deb: debversion
	@echo " "
	@echo "--------- P5/Makefile work on target $@ ---------"
	rm -f tei-p5-*_*deb
	rm -f tei-p5-*_*changes
	rm -f tei-p5-*_*build
	@echo BUILD: make Debian tei-p5-database package
	(cd debian-tei-p5-database; debclean;debuild -eXSL=${XSL} -eVCS=${VCS} -eINJENKINS=${INJENKINS} --no-lintian  -nc  -b  -i.svn -I.svn -uc -us)
	@echo BUILD: make Debian tei-p5-doc package
	(cd debian-tei-p5-doc; debclean;debuild -eXSL=${XSL} -eVCS=${VCS} -eINJENKINS=${INJENKINS} --no-lintian  -nc  -b  -i.svn -I.svn -uc -us)
	@echo BUILD: make Debian tei-p5-exemplars package
	(cd debian-tei-p5-exemplars; debclean;debuild -eXSL=${XSL} -eVCS=${VCS} -eINJENKINS=${INJENKINS} --no-lintian  -nc  -b  -i.svn -I.svn -uc -us)
	@echo BUILD: make Debian tei-p5-schema package
	(cd debian-tei-p5-schema; debclean;debuild -eXSL=${XSL} -eVCS=${VCS} -eINJENKINS=${INJENKINS} --no-lintian  -nc  -b  -i.svn -I.svn -uc -us)
	@echo BUILD: make Debian tei-p5-source package
	(cd debian-tei-p5-source; debclean;debuild -eXSL=${XSL} -eVCS=${VCS} -eINJENKINS=${INJENKINS} --no-lintian  -nc  -b  -i.svn -I.svn -uc -us)
	@echo BUILD: make Debian tei-p5-test package
	(cd debian-tei-p5-test; debclean;debuild -eXSL=${XSL} -eVCS=${VCS} -eINJENKINS=${INJENKINS} --no-lintian  -nc  -b  -i.svn -I.svn -uc -us)

install-schema: dist-schema
	@echo " "
	@echo "--------- P5/Makefile work on target $@ ---------"
	@echo Making schema release in ${PREFIX}
	(cd release/tei-p5-schema; tar cf - .) | (cd ${PREFIX}; tar xf - )

install-doc: dist-doc
	@echo " "
	@echo "--------- P5/Makefile work on target $@ ---------"
	@echo BUILD: Make doc release in ${PREFIX}
	(cd release/tei-p5-doc; tar cf - .) | (cd ${PREFIX}; tar xf - )

install-source: dist-source
	@echo " "
	@echo "--------- P5/Makefile work on target $@ ---------"
	@echo BUILD: Making source release in ${PREFIX}
	(cd release/tei-p5-source; tar cf - .) | (cd ${PREFIX}; tar xf - )

install-test: dist-test
	@echo " "
	@echo "--------- P5/Makefile work on target $@ ---------"
	@echo BUILD: Making testfiles release in ${PREFIX}
	(cd release/tei-p5-test; tar cf - .) | (cd ${PREFIX}; tar xf - )

install-exemplars: dist-exemplars
	@echo " "
	@echo "--------- P5/Makefile work on target $@ ---------"
	@echo BUILD: Making exemplars release in ${PREFIX}
	(cd release/tei-p5-exemplars; tar cf - share) | (cd ${PREFIX}; tar xf -)

install-database: dist-database
	@echo " "
	@echo "--------- P5/Makefile work on target $@ ---------"
	@echo BUILD: Making database release in ${PREFIX}
	(cd release/tei-p5-database; tar cf - .) | (cd ${PREFIX}; tar xf - )

install: clean install-schema install-doc install-test install-exemplars install-source install-database
	@echo " "
	@echo "--------- P5/Makefile work on target $@ ---------"

dependencies:
	@echo " "
	@echo "--------- P5/Makefile work on target $@ ---------"
	@echo to make this thing build under Ubuntu/Debian, here are all the packages you will need:
	@echo	msttcorefonts
	@echo	rnv
	@echo	tei-p5-xsl
	@echo	ttf-junicode
	@echo	zip
	@echo   ttf-linux-libertine
	@echo 	fonts-noto-cjk

clean:
	@echo " "
	@echo "--------- P5/Makefile work on target $@ ---------"
	(cd Exemplars; make clean)
	(cd Test; make clean)
	rm -f repodate.xml
	rm -rf release Guidelines Guidelines-web Schema DTD dtd Split
	rm -rf Guidelines.??? Guidelines-*
	rm -f *.isosch.xsl
	rm -f *.stamp
	rm -f *.xsd
	rm -f Exemplars/stdout
	rm -f Test/*.isosch
	rm -f Test/detest.dtd Test/detest.rnc Test/detest.rng Test/detest.xsl
	rm -f Test/stdout
	rm -f Utilities/guidelines.xsl
	rm -f anything buildweb.xml
	rm -f p5.sch p5.isosch p5.xml p5subset.xml p5subset*.json
	rm -f p5attlist.txt
	rm -f p5odds-examples.rng p5odds-examples.rnc p5odds.rng p5odds.rnc p5odds.isosch
	rm -f pdfbuild.log
	rm -f stripspace.xsl.model
	rm -f tei-*.zip
	rm -f tei-p5-*_*build
	rm -f tei-p5-*_*changes
	rm -f tei-p5-*_*deb
	rm -f teiwebsiteguidelines.zip
	rm -rf FASC-*
	rm -rf catalogue.* modList
	rm -rf valid v.xml ValidatorLog.xml Utilities/pointerattributes.xsl graphics.sh missfont.log v.body v.header Schematron1.xml Schematron2.xml
