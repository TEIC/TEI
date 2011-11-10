SFUSER=rahtz
JING=jing
TRANG=trang
SAXON=saxon
SAXON_ARGS=-ext:on

OLDDIRS=fo html common latex
PREFIX=/usr

.PHONY: doc release common

default: check release

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



dist: clean release
	-rm tei-xslt1-`cat VERSION`.zip
	(cd release/p4; zip -r -q ../../tei-xslt1-`cat ../../VERSION`.zip .)
	(cd release/p5; zip -r -q ../../tei-xslt1-`cat ../../VERSION`.zip .)


release: p4 p5 

installp5: p5
	mkdir -p ${PREFIX}/share
	(cd release/p5; tar cf - .) | (cd ${PREFIX}/share; tar xf  -)

installp4: p4 
	mkdir -p ${PREFIX}/share
	(cd release/p4; tar cf - .) |  (cd ${PREFIX}/share; tar xf  -)


install: installp4 installp5

debversion:
	(cd debian-tei-p5-xsl;  dch -v `cat ../VERSION` new release)

deb:
	@echo BUILD Make Debian packages
	rm -f tei*xsl*_*deb
	rm -f tei*xsl*_*changes
	rm -f tei*xsl*_*build
	(cd debian-tei-p5-xsl;     debclean;debuild --no-lintian  -nc -b -i.svn -I.svn -uc -us)

sfupload:
	rsync -e ssh tei-xsl-`cat VERSION`.zip ${SFUSER},tei@frs.sourceforge.net:/home/frs/project/t/te/tei/Stylesheets

clean:
	echo "" > test~
	find . -name "*~"  | xargs rm
	rm -f tei-xsl*.zip	
	rm -rf tei-p5-xsl_*
	rm -rf release
	rm -rf xhtml
	(cd Test1; make clean)
	rm -rf tei-p5-xsl_*
	(cd debian-tei-p5-xsl/debian;      rm -rf tei-p5-xsl)



