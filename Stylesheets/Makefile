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
	-mkdir -p release/tei-xsl/odds
	-mkdir -p release/tei-xsl/base/p4/fo
	-mkdir -p release/tei-xsl/base/p4/html
	-mkdir -p release/tei-xsl/base/p4/common
	-mkdir -p release/tei-xsl/base/p4/latex
	-mkdir -p release/tei-xsl/slides
	for i in fo/*.xsl html/*xsl common/*xsl latex/*xsl ; do \
	echo do $$i;perl toP4.pl < $$i > release/tei-xsl/base/p4/$$i; \
	done

p5:
	-mkdir -p release/tei-xsl/base/p5/fo
	-mkdir -p release/tei-xsl/base/p5/html
	-mkdir -p release/tei-xsl/base/p5/common
	-mkdir -p release/tei-xsl/base/p5/latex
	for i in  fo/*.xsl html/*xsl common/*xsl latex/*xsl ; do \
	cp $$i release/tei-xsl/base/p5/$$i; \
	done

param: p4 p5
	xsltproc -o customize.xml param.xsl param.xml
	xsltproc -o style.xml paramform.xsl param.xml 

release: param doc
	cp *.css release/tei-xsl
	tar cf - slides/*.xsl slides/*.css odds/*.xsl | (cd release/tei-xsl; tar xf - )
	mkdir -p release/tei-xsl/doc
	cp -r doc/xsltdoc doc/*.png release/tei-xsl/doc
	cp doc/*.css release/tei-xsl/doc/xsltdoc
	cp ChangeLog style.xml customize.xml LICENSE release/tei-xsl/doc
	cp teixsl.xml release/tei-xsl/doc/index.xml

doc:
	(cd doc; saxon configdoc.xsl xsltdoc.xsl)

test: p4 p5
	cd Test; make 

clean:
	-rm `find . -name semantic.cache`
	-rm `find . -name "*~" `
	-rm stylebear style.xml customize.xml teixsl.html
	-rm -rf release
	-rm -rf doc/xsltdoc
	(cd Test; make clean)

install: release
	mkdir -p ${PREFIX}/share/xml/tei/stylesheet
	(cd release/tei-xsl; tar cf - base slides odds *.css) | \
	(cd ${PREFIX}/share/xml/tei/stylesheet; tar xf -)
	mkdir -p ${PREFIX}/share/doc/tei-xsl
	(cd release/tei-xsl/doc; tar cf - .) | (cd ${PREFIX}/share/doc/tei-xsl; tar xf -)
	mkdir -p ${PREFIX}/lib/cgi-bin
	cp stylebear ${PREFIX}/lib/cgi-bin/stylebear
	chmod 755 ${PREFIX}/lib/cgi-bin/stylebear

