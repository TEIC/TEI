DIRS=odds2 xhtml2 common2 slides2 latex2 fo2 tools2 profiles epub docx oo nlm tite
OLDDIRS=slides fo html common latex odds
SCRIPTS=teitodocx docxtotei teitoodt odttotei teitolatex teitoepub
PREFIX=/usr
OXY=`locate stylesheetDocumentation.sh | head -1`
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

default:
	@echo
	@echo TEI XSL stylesheets
	@echo - install target puts files directly into ${PREFIX} 
	@echo - dist target  makes a release subdirectory of runtime files
	@echo There is no default action
	@echo

p5-2:
	@echo INFO Build for P5, XSLT 2.0
	test -d release/p5-2 || mkdir -p release/p5-2/xml/tei/stylesheet/
	for i in  ${DIRS} ; do \
	tar cf - --exclude .svn $$i | (cd release/p5-2/xml/tei/stylesheet; tar xf - ); \
	done

p4:
	@echo INFO Build for P4
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
	@echo INFO Build for P5, XSLT 1.0
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
	@echo INFO Build for P5, common files and documentation
	test -d release/common || mkdir -p release/common/xml/teip4/stylesheet
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
	cp teixsl.xml style.xml customize.xml release/common/doc/tei-xsl-common
	cp tei.css ChangeLog LICENSE release/common/doc/tei-xsl-common

oxygendoc:
	-locate stylesheetDocumentation.sh || exit 1
	@echo using oXygen stylesheet documentation generator
	for i in ${TARGETS}; do echo process doc for $$i; export ODIR=release/common/doc/tei-xsl-common/`dirname $$i`; ${OXY} $$i -cfg:doc/oxydoc.cfg; (cd `dirname $$i`; tar cf - release) | tar xf -; rm -rf `dirname $$i`/release; done

test: clean p4 p5 p5-2 common
	@echo INFO Run tests
	(cd release/p5/xml/tei/stylesheet; cp ../../../../../i18n.xml .)
	(cd release/p4/xml/teip4/stylesheet; cp ../../../../../i18n.xml .)
	(cd Test2; make)
	(cd Test; make)
	rm release/p5/xml/tei/stylesheet/i18n.xml
	rm release/p4/xml/teip4/stylesheet/i18n.xml


dist: clean release
	-rm release/tei-xsl-`cat VERSION`.zip
	(cd release/common; zip -r ../tei-xsl-`cat ../../VERSION`.zip .)
	(cd release/p4; zip -r ../tei-xsl-`cat ../../VERSION`.zip .)
	(cd release/p5; zip -r ../tei-xsl-`cat ../../VERSION`.zip .)
	(cd release/p5-2; zip -r ../tei-xsl-`cat ../../VERSION`.zip .)

release: common doc oxygendoc p4 p5 p5-2

installp5-2: p5-2
	mkdir -p ${PREFIX}/share
	(cd release/p5-2; tar cf - .) | (cd ${PREFIX}/share; tar xvf  -)
	mkdir -p ${PREFIX}/bin
	for i in $(SCRIPTS); do \
	cp $$i ${PREFIX}/share/xml/tei/stylesheet/$$i; \
	chmod 755 ${PREFIX}/share/xml/tei/stylesheet/$$i; \
	cp $$i ${PREFIX}/bin/$$i; \
	chmod 755 ${PREFIX}/bin/$$i; \
	perl -p -i -e 's+APPHOME=.*+APPHOME=/usr/share/xml/tei/stylesheet+' ${PREFIX}/bin/$$i;done

installp5: p5
	mkdir -p ${PREFIX}/share
	(cd release/p5; tar cf - .) | (cd ${PREFIX}/share; tar xvf  -)

installp4: p4 
	mkdir -p ${PREFIX}/share
	(cd release/p4; tar cf - .) |  (cd ${PREFIX}/share; tar xvf  -)


installcommon: doc common
	mkdir -p ${PREFIX}/lib/cgi-bin
	cp stylebear ${PREFIX}/lib/cgi-bin/stylebear
	chmod 755 ${PREFIX}/lib/cgi-bin/stylebear
	mkdir -p ${PREFIX}/share/doc/
	mkdir -p ${PREFIX}/share/xml/
	(cd release/common/doc; tar cf - .) | (cd ${PREFIX}/share/doc; tar xf -)
	(cd release/common/xml; tar cf - .) | (cd ${PREFIX}/share/xml; tar xf -)

install: installp4 installp5 installp5-2 installcommon

log:
	(LastDate=`head -1 ChangeLog | awk '{print $$1}'`; \
	svn log -v -r 'HEAD:{'$$LastDate'}' | perl ../gnuify-changelog.pl | grep -v "^;" > newchanges)
	mv ChangeLog oldchanges
	cat newchanges oldchanges > ChangeLog
	rm newchanges oldchanges

clean:
	echo "" > test~
	find . -name "*~"  | xargs rm
	rm -f stylebear style.xml customize.xml teixsl.html
	rm -rf release
	rm -rf xhtml
	rm -rf doc/xsltdoc
	(cd Test; make clean)
	(cd Test2; make clean)

