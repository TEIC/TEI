PREFIX=/usr
.PHONY: doc release

default:
	@echo
	@echo TEI XSL2 stylesheets
	@echo - install target puts files directly into ${PREFIX} 
	@echo - dist target  makes a release subdirectory of runtime files
	@echo There is no default action
	@echo

dist: clean xsl release 
	(cd release; 	\
	ln -s tei-xsl2 tei-xsl2-`cat ../VERSION` ; \
	zip -r tei-xsl2-`cat ../VERSION`.zip tei-xsl2-`cat ../VERSION` )

xsl:
	for i in  odds slides fo html common latex ; do \
	mkdir -p release/tei-xsl2/p5/$$i; cp $$i/*.xsl release/tei-xsl2/p5/$$i; \
	done
	for j in p4 p5 ; do \
	(cd release/tei-xsl2/$$j/html;mkdir ../xhtml; for i in *.xsl; do \
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
	perl -p -i -e 's/name="xhtml">false</name="xhtml">true</' release/tei-xsl2/p5/xhtml/tei-param.xsl

log:
	(LastDate=`head -1 ChangeLog | awk '{print $$1}'`; \
	svn log -v -r 'HEAD:{'$$LastDate'}' | perl ../gnuify-changelog.pl | grep -v "^;" > newchanges)
	mv ChangeLog oldchanges
	cat newchanges oldchanges > ChangeLog
	rm newchanges oldchanges

release: doc xsl
	cp i18n.xml release/tei-xsl2/p5
	cp *.css release/tei-xsl2/p5
	mkdir -p release/tei-xsl2/doc
	mkdir -p release/tei-xsl2/doc/xsltdoc
	-test -d xsltdoc && cp -r doc/xsltdoc doc/*.png release/tei-xsl2/doc
	-test -d xsltdoc && cp doc/*.css release/tei-xsl2/doc/xsltdoc
	cp ChangeLog style.xml customize.xml LICENSE release/tei-xsl2/doc
	cp teixsl.xml release/tei-xsl2/doc/index.xml

doc:
	-test -d xsltdoc && (cd doc; saxon configdoc.xsl xsltdoc.xsl)
	xsltproc -o customize.xml param.xsl param.xml
	xsltproc -o style.xml paramform.xsl param.xml 

test: xsl
	(cd Test; make )

clean:
	-rm `find . -name semantic.cache`
	-rm `find . -name "*~" `
	-rm stylebear style.xml customize.xml teixsl.html
	-rm -rf release
	-rm -rf xhtml
	-rm -rf doc/xsltdoc
	(cd Test; make clean)

install: installp4 installp5

installp4: p4 release
	mkdir -p ${PREFIX}/share/xml/teip4/stylesheet
	(cd release/tei-xsl2/p4; tar cf - .) | \
	(cd ${PREFIX}/share/xml/teip4/stylesheet; tar xf -)

installp5: release
	mkdir -p ${PREFIX}/share/xml/tei/stylesheet
	(cd release/tei-xsl2/p5; tar cf - .) | \
	(cd ${PREFIX}/share/xml/tei/stylesheet; tar xf -)
	mkdir -p ${PREFIX}/share/doc/tei-p5-xsl
	(cd release/tei-xsl2/doc; tar cf - .) | (cd ${PREFIX}/share/doc/tei-p5-xsl; tar xf -)
	mkdir -p ${PREFIX}/lib/cgi-bin
	cp stylebear ${PREFIX}/lib/cgi-bin/stylebear
	chmod 755 ${PREFIX}/lib/cgi-bin/stylebear

