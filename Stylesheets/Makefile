PREFIX=/usr
.PHONY: doc release

default:
	@echo
	@echo TEI XSL stylesheets
	@echo - install target puts files directly into ${PREFIX} 
	@echo - dist target  makes a release subdirectory of runtime files
	@echo There is no default action
	@echo

dist: clean p4 p5 release
	(cd release; 	\
	ln -s tei-xsl tei-xsl-`cat ../VERSION` ; \
	zip -r tei-xsl-`cat ../VERSION`.zip tei-xsl-`cat ../VERSION` )



p4:
	for i in slides fo html common latex ; do \
	mkdir -p release/tei-xsl/p4/$$i; \
	for j in $$i/*.xsl; do perl toP4.pl < $$j > release/tei-xsl/p4/$$j;done; \
	done

p5:
	for i in  odds slides fo html common latex ; do \
	mkdir -p release/tei-xsl/p5/$$i; cp $$i/*.xsl release/tei-xsl/p5/$$i; \
	done
	(cd release/tei-xsl/p5/html;mkdir ../xhtml; for i in *.xsl; do \
	cp  $$i ../xhtml/$$i;  \
	perl -p -i -e 's+<xsl:stylesheet+<xsl:stylesheet xmlns=\"http://www.w3.org/1999/xhtml\"+'  ../xhtml/$$i ;  \
	perl -p -i -e 's/>.html</>.xhtml</'  ../xhtml/$$i ;  \
	perl -p -i -e 's/>html</>xml</'  ../xhtml/$$i ;  \
	perl -p -i -e 's/>iso-8859-1</>utf-8</'  ../xhtml/$$i ;  \
	perl -p -i -e 's+outputXHTML\">false<+outputXHTML\">true<+'  ../xhtml/$$i ;  \
	perl -p -i -e 's+-//W3C//DTD HTML 4.0 Transitional//EN+-//W3C//DTD XHTML 1.1//EN+'  ../xhtml/$$i ;  \
	perl -p -i -e 's+http://www.w3.org/TR/html4/loose.dtd+http://www.w3.org/TR/xhtml11/DTD/xhtml11.dtd+'  ../xhtml/$$i ;  \
	 done)

release: doc p4 p5
	cp i18n.xml release/tei-xsl/p4
	cp i18n.xml release/tei-xsl/p5
	cp *.css release/tei-xsl/p4
	cp *.css release/tei-xsl/p5
	mkdir -p release/tei-xsl/doc
	-test -d xsltdoc && cp -r doc/xsltdoc doc/*.png release/tei-xsl/doc
	-test -d xsltdoc && cp doc/*.css release/tei-xsl/doc/xsltdoc
	cp ChangeLog style.xml customize.xml LICENSE release/tei-xsl/doc
	cp teixsl.xml release/tei-xsl/doc/index.xml

doc:
	-test -d xsltdoc && (cd doc; saxon configdoc.xsl xsltdoc.xsl)
	xsltproc -o customize.xml param.xsl param.xml
	xsltproc -o style.xml paramform.xsl param.xml 

test: p4 p5
	(cd Test; make )

clean:
	-rm `find . -name semantic.cache`
	-rm `find . -name "*~" `
	-rm stylebear style.xml customize.xml teixsl.html
	-rm -rf release
	-rm -rf doc/xsltdoc
	(cd Test; make clean)

install: installp4 installp5

installp4: p4 release
	mkdir -p ${PREFIX}/share/xml/teip4/stylesheet
	(cd release/tei-xsl/p4; tar cf - .) | \
	(cd ${PREFIX}/share/xml/teip4/stylesheet; tar xf -)

installp5: release
	mkdir -p ${PREFIX}/share/xml/tei/stylesheet
	(cd release/tei-xsl/p5; tar cf - .) | \
	(cd ${PREFIX}/share/xml/tei/stylesheet; tar xf -)
	mkdir -p ${PREFIX}/share/doc/tei-p5-xsl
	(cd release/tei-xsl/doc; tar cf - .) | (cd ${PREFIX}/share/doc/tei-p5-xsl; tar xf -)
	mkdir -p ${PREFIX}/lib/cgi-bin
	cp stylebear ${PREFIX}/lib/cgi-bin/stylebear
	chmod 755 ${PREFIX}/lib/cgi-bin/stylebear

