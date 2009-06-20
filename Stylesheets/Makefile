DIRS=odds2 xhtml2 common2 slides2 latex2 fo2 docx profiles tools2
OLDDIRS=slides fo html common latex
PREFIX=/usr
.PHONY: doc release common

default:
	@echo
	@echo TEI XSL stylesheets
	@echo - install target puts files directly into ${PREFIX} 
	@echo - dist target  makes a release subdirectory of runtime files
	@echo There is no default action
	@echo

dist: clean release
	(cd release; 	\
	ln -s tei-xsl tei-xsl-`cat ../VERSION` ; \
	zip -r tei-xsl-`cat ../VERSION`.zip tei-xsl-`cat ../VERSION` )

release: common p4 p5 p5-2

p5-2:
	-mkdir -p release/tei-xsl/p5-2
	for i in  ${DIRS} ; do \
	tar cf - --exclude .svn $$i | (cd release/tei-xsl/p5-2; tar xf - ); \
	done

p4:
	for i in ${OLDDIRS} ; do \
	mkdir -p release/tei-xsl/p4/$$i; \
	for j in $$i/*.xsl; do perl toP4.pl < $$j > release/tei-xsl/p4/$$j;done; \
	done

p5: p4
	for i in ${OLDDIRS} ; do \
	mkdir -p release/tei-xsl/p5/$$i; cp $$i/*.xsl release/tei-xsl/p5/$$i; \
	done
	for j in p4 p5 ; do \
	(cd release/tei-xsl/$$j/html;mkdir ../xhtml; for i in *.xsl; do \
	cp  $$i ../xhtml/$$i;  \
	perl -p -i -e 's+<xsl:stylesheet+<xsl:stylesheet xmlns=\"http://www.w3.org/1999/xhtml\"+'  ../xhtml/$$i ;  \
	perl -p -i -e 's/>html</>xml</'  ../xhtml/$$i ;  \
	perl -p -i -e 's/ target="_top"//'  ../xhtml/$$i ;  \
	perl -p -i -e 's/>iso-8859-1</>utf-8</'  ../xhtml/$$i ;  \
	perl -p -i -e 's/text\/html/application\/xhtml+xml/' ../xhtml/$$i ; \
	perl -p -i -e 's+outputXHTML\">false<+outputXHTML\">true<+'  ../xhtml/$$i ;  \
	perl -p -i -e 's+-//W3C//DTD HTML 4.0 Transitional//EN+-//W3C//DTD XHTML 1.1//EN+'  ../xhtml/$$i ;  \
	perl -p -i -e 's+http://www.w3.org/TR/html4/loose.dtd+http://www.w3.org/TR/xhtml11/DTD/xhtml11.dtd+'  ../xhtml/$$i ;  \
	 done); done
	perl -p -i -e 's/name="xhtml">false</name="xhtml">true</' release/tei-xsl/p5/xhtml/tei-param.xsl
	perl -p -i -e 's/name="xhtml">false</name="xhtml">true</' release/tei-xsl/p4/xhtml/tei-param.xsl

common:
	mkdir -p release/tei-xsl/common
	cp i18n.xml release/tei-xsl/common
	cp *.css release/tei-xsl/common
	mkdir -p release/tei-xsl/doc/xsltdoc
	-test -d xsltdoc && (cd doc; saxon configdoc.xsl xsltdoc.xsl)
	saxon -o customize.xml param.xml doc/param.xsl 
	saxon -o style.xml param.xml  doc/paramform.xsl 
	-test -d xsltdoc && cp -r doc/xsltdoc doc/*.png release/tei-xsl/doc
	-test -d xsltdoc && cp doc/*.css release/tei-xsl/doc/xsltdoc
	cp ChangeLog style.xml customize.xml LICENSE release/tei-xsl/doc
	cp teixsl.xml release/tei-xsl/doc/index.xml


test: p4 p5 p5-2 common
	(cd release/tei-xsl/p4; rm i18n.xml; ln -s ../common/i18n.xml .)
	(cd release/tei-xsl/p5; rm i18n.xml; ln -s ../common/i18n.xml .)
	(cd release/tei-xsl/p5-2; rm i18n.xml; ln -s ../common/i18n.xml .)
	(cd Test; make)
	(cd Test2; make)
	rm release/tei-xsl/p4/i18n.xml
	rm release/tei-xsl/p5/i18n.xml
	rm release/tei-xsl/p5-2/i18n.xml


installp5-2: p5-2
	mkdir -p ${PREFIX}/share/xml/tei/stylesheet
	(cd release/tei-xsl/p5-2; tar cf - .) | \
	(cd ${PREFIX}/share/xml/tei/stylesheet; tar xf -)

installp5: p5
	mkdir -p ${PREFIX}/share/xml/tei/stylesheet
	(cd release/tei-xsl/p5; tar cf - .) | \
	(cd ${PREFIX}/share/xml/tei/stylesheet; tar xf -)

installp4: p4 
	mkdir -p ${PREFIX}/share/xml/teip4/stylesheet
	(cd release/tei-xsl/p4; tar cf - .) | \
	(cd ${PREFIX}/share/xml/teip4/stylesheet; tar xf -)

installcommon: common 
	mkdir -p ${PREFIX}/share/xml/tei/stylesheet
	(cd release/tei-xsl/common; tar cf - .) | (cd ${PREFIX}/share/xml/tei/stylesheet; tar xf -)
	mkdir -p ${PREFIX}/lib/cgi-bin
	cp stylebear ${PREFIX}/lib/cgi-bin/stylebear
	chmod 755 ${PREFIX}/lib/cgi-bin/stylebear
	mkdir -p ${PREFIX}/share/doc/tei-xsl-common
	(cd release/tei-xsl/doc; tar cf - .) | (cd ${PREFIX}/share/doc/tei-xsl-common; tar xf -)

install: installp4 installp5 installp5-2 installcommon

log:
	(LastDate=`head -1 ChangeLog | awk '{print $$1}'`; \
	svn log -v -r 'HEAD:{'$$LastDate'}' | perl ../gnuify-changelog.pl | grep -v "^;" > newchanges)
	mv ChangeLog oldchanges
	cat newchanges oldchanges > ChangeLog
	rm newchanges oldchanges

clean:
	-rm `find . -name "*~" `
	-rm stylebear style.xml customize.xml teixsl.html
	-rm -rf release
	-rm -rf xhtml
	-rm -rf doc/xsltdoc
	(cd Test; make clean)
	(cd Test2; make clean)

