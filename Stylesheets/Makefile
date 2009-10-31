DIRS=odds2 xhtml2 common2 slides2 latex2 fo2 docx profiles tools2
OLDDIRS=slides fo html common latex odds
PREFIX=/usr
.PHONY: doc release common

default:
	@echo
	@echo TEI XSL stylesheets
	@echo - install target puts files directly into ${PREFIX} 
	@echo - dist target  makes a release subdirectory of runtime files
	@echo There is no default action
	@echo

p5-2:
	-mkdir -p release/p5-2/xml/tei/stylesheet/
	for i in  ${DIRS} ; do \
	tar cf - --exclude .svn $$i | (cd release/p5-2/xml/tei/stylesheet; tar xf - ); \
	done

p4:
	-mkdir -p release/p4/xml/teip4/stylesheet/
	for i in ${OLDDIRS} ; do \
		mkdir -p release/p4/xml/teip4/stylesheet/$$i; \
		for j in $$i/*.xsl; do perl toP4.pl < $$j > release/p4/xml/teip4/stylesheet/$$j;done; \
	done
	(cd release/p4/xml/teip4/stylesheet/html/;mkdir ../xhtml; for i in *.xsl; do \
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
	-mkdir -p release/p5/xml/tei/stylesheet/
	for i in ${OLDDIRS} ; do \
	mkdir -p release/p5/xml/tei/stylesheet/$$i; cp $$i/*.xsl release/p5/xml/tei/stylesheet/$$i; \
	done
	(cd release/p5/xml/tei/stylesheet/html;mkdir ../xhtml; for i in *.xsl; do \
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
	mkdir -p release/p4/xml/teip4/stylesheet
	mkdir -p release/p5-2/xml/tei/stylesheet
	cp *.css i18n.xml release/p4/xml/teip4/stylesheet
	cp *.css i18n.xml release/p5-2/xml/tei/stylesheet
	mkdir -p release/p5-2/doc/tei-xsl-common/xsltdoc
	-test -d xsltdoc && (cd doc; saxon configdoc.xsl xsltdoc.xsl)
	saxon -o customize.xml param.xml doc/param.xsl 
	saxon -o style.xml param.xml  doc/paramform.xsl 
	-test -d xsltdoc && cp -r doc/xsltdoc doc/*.png release/p5-2/doc/tei-xsl-common
	-test -d xsltdoc && cp doc/*.css release/p5-2/doc/tei-xsl-common/xsltdoc
	cp ChangeLog style.xml customize.xml LICENSE release/p5-2/doc/tei-xsl-common
	cp teixsl.xml release/p5-2/doc/tei-xsl-common/index.xml

test: p4 p5 p5-2 common
	(cd release/p4/xml/teip4/stylesheet; rm i18n.xml; ln -s ../common/i18n.xml .)
	(cd release/p5/xml/tei/stylesheet; rm i18n.xml; ln -s ../common/i18n.xml .)
	(cd release/p5-2/xml/tei/stylesheet; rm i18n.xml; ln -s ../common/i18n.xml .)
	(cd Test; make)
	(cd Test2; make)
	rm release/xml/teip4/stylesheet/p4/i18n.xml
	rm release/xml/tei/stylesheet/p5/i18n.xml
	rm release/xml/tei/stylesheet/p5-2/i18n.xml


dist: clean release
	-rm release/tei-xsl-`cat VERSION`.zip
	(cd release/p4; zip -r ../tei-xsl-`cat ../../VERSION`.zip .)
	(cd release/p5; zip -r ../tei-xsl-`cat ../../VERSION`.zip .)
	(cd release/p5-2; zip -r ../tei-xsl-`cat ../../VERSION`.zip .)

release: common p4 p5 p5-2

installp5-2: p5-2
	mkdir -p ${PREFIX}/share
	(cd release/p5-2; tar cf - .) | (cd ${PREFIX}/share; tar xvf  -)

installp5: p5
	mkdir -p ${PREFIX}/share
	(cd release/p5; tar cf - .) | (cd ${PREFIX}/share; tar xvf  -)

installp4: p4 
	mkdir -p ${PREFIX}/share
	(cd release/p4; tar cf - .) |  (cd ${PREFIX}/share; tar xvf  -)


installcommon: common 
	mkdir -p ${PREFIX}/lib/cgi-bin
	cp stylebear ${PREFIX}/lib/cgi-bin/stylebear
	chmod 755 ${PREFIX}/lib/cgi-bin/stylebear
	mkdir -p ${PREFIX}/share/doc/

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
	-rm stylebear style.xml customize.xml teixsl.html
	-rm -rf release
	-rm -rf xhtml
	-rm -rf doc/xsltdoc
	(cd Test; make clean)
	(cd Test2; make clean)

