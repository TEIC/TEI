SFUSER=rahtz
JING=jing
TRANG=trang
SAXON=saxon
SAXON_ARGS=-ext:on
DIRS=odds2 xhtml2 common2 slides2 latex2 fo2 tools2 profiles epub docx oo nlm tite
OLDDIRS=slides fo html common latex
SCRIPTS=teitodocx docxtotei teitoodt odttotei teitolatex teitoepub
PREFIX=/usr
OXY=/usr/share/oxygen/stylesheetDocumentation.sh
TARGETS= \
	latex2/tei.xsl \
	xhtml2/tei.xsl \
	fo2/tei.xsl \
	odds2/odd2odd.xsl \
	odds2/odd2relax.xsl \
	odds2/odd2dtd.xsl \
	slides2/teihtml-slides.xsl \
	slides2/teilatex-slides.xsl \
	profiles/default/csv/to.xsl	\
	profiles/default/docx/to.xsl \
	profiles/default/dtd/to.xsl	\
	profiles/default/epub/to.xsl	\
	profiles/default/fo/to.xsl	\
	profiles/default/html/to.xsl \
	profiles/default/latex/to.xsl \
	profiles/default/lite/to.xsl \
	profiles/default/oo/to.xsl \
	profiles/default/relaxng/to.xsl	\
	profiles/enrich/docx/to.xsl \
	profiles/enrich/fo/to.xsl \
	profiles/enrich/html/to.xsl \
	profiles/enrich/latex/to.xsl \
	profiles/iso/docx/to.xsl \
	profiles/iso/fo/to.xsl \
	profiles/iso/html/to.xsl \
	profiles/iso/latex/to.xsl \
	profiles/default/docx/from.xsl \
	profiles/enrich/docx/from.xsl \
	profiles/default/csv/from.xsl		\
	profiles/default/oo/from.xsl \
	profiles/default/docbook/from.xsl	\
	profiles/default/p4/from.xsl\
	profiles/default/docx/from.xsl


.PHONY: doc release common

default: check test release

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

p5-2:
	@echo BUILD Build for P5, XSLT 2.0
	test -d release/p5-2 || mkdir -p release/p5-2/xml/tei/stylesheet/
	for i in  ${DIRS} ; do \
		tar cf - --exclude .svn $$i | (cd release/p5-2/xml/tei/stylesheet; tar xf - ); \
	done

p4:
	@echo BUILD Build for P4
	test -d release/p4 || mkdir -p release/p4/xml/teip4/stylesheet/
	for i in ${OLDDIRS} ; do \
		test -d release/p4/xml/teip4/stylesheet/$$i || mkdir -p release/p4/xml/teip4/stylesheet/$$i; \
		for j in $$i/*.xsl; do perl toP4.pl < $$j > release/p4/xml/teip4/stylesheet/$$j;done; \
	done
	(cd release/p4/xml/teip4/stylesheet/html/;test -d ../xhtml || mkdir ../xhtml; for i in *.xsl; do \
	cp  $$i ../xhtml/$$i;  \
	perl -p -i -e 's+<xsl:stylesheet+<xsl:stylesheet xmlns=\"http://www.w3.org/1999/xhtml\"+'  ../xhtml/$$i ;  \
	perl -p -i -e 's/>html</>xml</'  ../xhtml/$$i ;  \
	perl -p -i -e 's/ target="_top"//'  ../xhtml/$$i ;  \
	perl -p -i -e 's/>iso-8859-1</>utf-8</'  ../xhtml/$$i ;  \
	perl -p -i -e 's/text\/html/application\/xhtml+xml/' ../xhtml/$$i ; \
	perl -p -i -e 's+outputXHTML\">false<+outputXHTML\">true<+'  ../xhtml/$$i ;  \
	perl -p -i -e 's+-//W3C//DTD HTML 4.0 Transitional//EN+-//W3C//DTD XHTML 1.1//EN+'  ../xhtml/$$i ;  \
	perl -p -i -e 's+http://www.w3.org/TR/html4/loose.dtd+http://www.w3.org/TR/xhtml11/DTD/xhtml11.dtd+'  ../xhtml/$$i ;  \
	 done)
	perl -p -i -e 's/name="xhtml">false</name="xhtml">true</' release/p4/xml/teip4/stylesheet/xhtml/tei-param.xsl

p5: p4
	@echo BUILD Build for P5, XSLT 1.0
	test -d release/p5 || mkdir -p release/p5/xml/tei/stylesheet/
	for i in ${OLDDIRS} ; do \
	test -d release/p5/xml/tei/stylesheet/$$i || mkdir -p release/p5/xml/tei/stylesheet/$$i; cp $$i/*.xsl release/p5/xml/tei/stylesheet/$$i; \
	done
	(cd release/p5/xml/tei/stylesheet/html;test -d ../xhtml || mkdir ../xhtml; for i in *.xsl; do \
	cp  $$i ../xhtml/$$i;  \
	perl -p -i -e 's+<xsl:stylesheet+<xsl:stylesheet xmlns=\"http://www.w3.org/1999/xhtml\"+'  ../xhtml/$$i ;  \
	perl -p -i -e 's/>html</>xml</'  ../xhtml/$$i ;  \
	perl -p -i -e 's/ target="_top"//'  ../xhtml/$$i ;  \
	perl -p -i -e 's/>iso-8859-1</>utf-8</'  ../xhtml/$$i ;  \
	perl -p -i -e 's/text\/html/application\/xhtml+xml/' ../xhtml/$$i ; \
	perl -p -i -e 's+outputXHTML\">false<+outputXHTML\">true<+'  ../xhtml/$$i ;  \
	perl -p -i -e 's+-//W3C//DTD HTML 4.0 Transitional//EN+-//W3C//DTD XHTML 1.1//EN+'  ../xhtml/$$i ;  \
	perl -p -i -e 's+http://www.w3.org/TR/html4/loose.dtd+http://www.w3.org/TR/xhtml11/DTD/xhtml11.dtd+'  ../xhtml/$$i ;  \
	 done)
	perl -p -i -e 's/name="xhtml">false</name="xhtml">true</' release/p5/xml/tei/stylesheet/xhtml/tei-param.xsl

common: 
	@echo BUILD Build for P5, common files and documentation
	test -d release/common/xml/teip4/stylesheet || mkdir -p release/common/xml/teip4/stylesheet
	cp *.css i18n.xml release/common/xml/teip4/stylesheet
	test -d release/common/xml/tei/stylesheet || mkdir -p release/common/xml/tei/stylesheet
	cp *.css i18n.xml release/common/xml/tei/stylesheet

doc:
	test -d release/common/doc/tei-xsl-common || mkdir -p release/common/doc/tei-xsl-common
	saxon -o:customize.xml param.xml doc/param.xsl 
	saxon -o:style.xml param.xml  doc/paramform.xsl 
	saxon -o:release/common/doc/tei-xsl-common/index.html teixsl.xml profiles/default/html/to.xsl 
	saxon -o:release/common/doc/tei-xsl-common/style.html style.xml  profiles/default/html/to.xsl 
	saxon -o:release/common/doc/tei-xsl-common/customize.html customize.xml  profiles/default/html/to.xsl cssFile=tei.css 
	cp doc/teixsl.png teixsl.xml style.xml customize.xml release/common/doc/tei-xsl-common
	cp tei.css ChangeLog LICENSE release/common/doc/tei-xsl-common

oxygendoc:
	test -f $(OXY) || exit 1
	@echo using oXygen stylesheet documentation generator
	for i in ${TARGETS}; do echo process doc for $$i; export ODIR=release/common/doc/tei-xsl-common/`dirname $$i`; ${OXY} $$i -cfg:doc/oxydoc.cfg; (cd `dirname $$i`; tar cf - release) | tar xf -; rm -rf `dirname $$i`/release; done

teioo.jar:
	(cd oo; jar cf ../teioo.jar META-INF/manifest.xml mimetype TypeDetection.xcu *xsl *ott teilite.dtd )

test: clean p4 p5 p5-2 common
	@echo BUILD Run tests
	(cd release/p5/xml/tei/stylesheet; cp ../../../../../i18n.xml .)
	(cd release/p4/xml/teip4/stylesheet; cp ../../../../../i18n.xml .)
	(cd Test2; make)
	(cd Test; make)
	rm release/p5/xml/tei/stylesheet/i18n.xml
	rm release/p4/xml/teip4/stylesheet/i18n.xml


dist: clean release
	-rm tei-xsl-`cat VERSION`.zip
	(cd release/common; zip -r -q ../../tei-xsl-`cat ../../VERSION`.zip .)
	(cd release/p4; zip -r -q ../../tei-xsl-`cat ../../VERSION`.zip .)
	(cd release/p5; zip -r -q ../../tei-xsl-`cat ../../VERSION`.zip .)
	(cd release/p5-2; zip -r -q ../../tei-xsl-`cat ../../VERSION`.zip .)

release: common doc oxygendoc p4 p5 p5-2

installp5-2: p5-2 teioo.jar
	mkdir -p ${PREFIX}/share/xml/tei/stylesheet
	cp teioo.jar ${PREFIX}/share/xml/tei/stylesheet
	(cd release/p5-2; tar cf - .) | (cd ${PREFIX}/share; tar xf  -)
	mkdir -p ${PREFIX}/bin
	for i in $(SCRIPTS); do \
	  cp $$i ${PREFIX}/bin/$$i; \
	  chmod 755 ${PREFIX}/bin/$$i; \
	  perl -p -i -e 's+APPHOME=.*+APPHOME=/usr/share/xml/tei/stylesheet+' ${PREFIX}/bin/$$i; \
	done

installp5: p5
	mkdir -p ${PREFIX}/share
	(cd release/p5; tar cf - .) | (cd ${PREFIX}/share; tar xf  -)

installp4: p4 
	mkdir -p ${PREFIX}/share
	(cd release/p4; tar cf - .) |  (cd ${PREFIX}/share; tar xf  -)


installcommon: doc common
	mkdir -p ${PREFIX}/lib/cgi-bin
	cp stylebear ${PREFIX}/lib/cgi-bin/stylebear
	chmod 755 ${PREFIX}/lib/cgi-bin/stylebear
	mkdir -p ${PREFIX}/share/doc/
	mkdir -p ${PREFIX}/share/xml/
	(cd release/common/doc; tar cf - .) | (cd ${PREFIX}/share/doc; tar xf -)
	(cd release/common/xml; tar cf - .) | (cd ${PREFIX}/share/xml; tar xf -)

install: installp4 installp5 installp5-2 installcommon

debversion:
	(cd debian-tei-xsl-common;  dch -v `cat ../VERSION` new release)
	(cd debian-tei-p5-xsl;  dch -v `cat ../VERSION` new release)
	(cd debian-tei-p5-xsl2;  dch -v `cat ../VERSION` new release)

deb:
	@echo BUILD Make Debian packages
	rm -f tei*xsl*_*deb
	rm -f tei*xsl*_*changes
	rm -f tei*xsl*_*build
	(cd debian-tei-xsl-common; debuild  -nc -b -i.svn -I.svn -uc -us)
	(cd debian-tei-p5-xsl;     debuild  -nc -b -i.svn -I.svn -uc -us)
	(cd debian-tei-p5-xsl2;    debuild  -nc -b -i.svn -I.svn -uc -us)

sfupload:
	rsync -e ssh release/*zip ${SFUSER},tei@frs.sourceforge.net:/home/frs/project/t/te/tei/Stylesheets

log:
	(LastDate=`head -1 ChangeLog | awk '{print $$1}'`; \
	svn log -v -r 'HEAD:{'$$LastDate'}' | perl ../gnuify-changelog.pl | grep -v "^;" > newchanges)
	mv ChangeLog oldchanges
	cat newchanges oldchanges > ChangeLog
	rm newchanges oldchanges

clean:
	echo "" > test~
	find . -name "*~"  | xargs rm
	rm -f tei-xsl-`cat VERSION`.zip	
	rm -f stylebear style.xml customize.xml teixsl.html
	rm -rf release
	rm -rf xhtml
	rm -rf doc/xsltdoc
	(cd Test; make clean)
	(cd Test2; make clean)
	rm -rf tei-p5-xsl_*
	rm -rf tei-p5-xsl2_*
	rm -rf tei-xsl-common_*
	(cd debian-tei-xsl-common/debian;  rm -rf tei-xsl-common)
	(cd debian-tei-p5-xsl/debian;      rm -rf tei-p5-xsl)
	(cd debian-tei-p5-xsl2/debian;     rm -rf tei-p5-xsl2)
	rm -f teioo.jar
