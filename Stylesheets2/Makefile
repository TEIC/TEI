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
	for i in  odds xhtml common ; do \
	mkdir -p release/tei-xsl2/$$i; cp $$i/*.xsl release/tei-xsl2/$$i; \
	done

log:
	(LastDate=`head -1 ChangeLog | awk '{print $$1}'`; \
	svn log -v -r 'HEAD:{'$$LastDate'}' | perl ../gnuify-changelog.pl | grep -v "^;" > newchanges)
	mv ChangeLog oldchanges
	cat newchanges oldchanges > ChangeLog
	rm newchanges oldchanges

release: xsl 
	mkdir -p release/tei-xsl2/doc
	cp ChangeLog LICENSE release/tei-xsl2/doc

releasedoc: release
	mkdir -p release/tei-xsl2/doc/xsltdoc
	-test -d xsltdoc && cp -r doc/xsltdoc doc/*.png release/tei-xsl2/doc
	-test -d xsltdoc && cp doc/*.css release/tei-xsl2/doc/xsltdoc
	cp ChangeLog style.xml customize.xml LICENSE release/tei-xsl2/doc
	cp teixsl.xml release/tei-xsl2/doc/index.xml

doc:
	-test -d xsltdoc && (cd doc; saxon configdoc.xsl xsltdoc.xsl)
	saxon -o customize.xml param.xml param.xsl 
	saxon -o style.xml param.xml  paramform.xsl 

test: xsl release
	(cd Test; make )

clean:
	-rm `find . -name semantic.cache`
	-rm `find . -name "*~" `
	-rm stylebear style.xml customize.xml teixsl.html
	-rm -rf release
	-rm -rf doc/xsltdoc
	(cd Test; make clean)

install: release
	mkdir -p ${PREFIX}/share/xml/tei/stylesheet
	(cd release/tei-xsl2; tar cf - .) | \
	(cd ${PREFIX}/share/xml/tei/stylesheet; tar xf -)
	mkdir -p ${PREFIX}/share/doc/tei-xsl2
	(cd release/tei-xsl2/doc; tar cf - .) | (cd ${PREFIX}/share/doc/tei-xsl2; tar xf -)

installdoc:
	mkdir -p ${PREFIX}/lib/cgi-bin
	cp stylebear ${PREFIX}/lib/cgi-bin/stylebear
	chmod 755 ${PREFIX}/lib/cgi-bin/stylebear

