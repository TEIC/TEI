dist: clean p4 p5 other
	(cd release; 	zip -r tei-xsl-`cat ../VERSION`.zip tei-xsl-`cat ../VERSION`)



p4:
	-mkdir -p release/tei-xsl-`cat VERSION`/p4/odds
	-mkdir -p release/tei-xsl-`cat VERSION`/p4/fo
	-mkdir -p release/tei-xsl-`cat VERSION`/p4/html
	-mkdir -p release/tei-xsl-`cat VERSION`/p4/common
	-mkdir -p release/tei-xsl-`cat VERSION`/p4/latex
	-mkdir -p release/tei-xsl-`cat VERSION`/p4/slides
	for i in odds/*xsl fo/*.xsl html/*xsl common/*xsl latex/*xsl slides/*xsl; do \
	echo do $$i;perl toP4.pl --date="`date`" --version=`cat VERSION` < $$i > release/tei-xsl-`cat VERSION`/p4/$$i; \
	done

p5:
	-mkdir -p release/tei-xsl-`cat VERSION`/p5/odds
	-mkdir -p release/tei-xsl-`cat VERSION`/p5/fo
	-mkdir -p release/tei-xsl-`cat VERSION`/p5/slides
	-mkdir -p release/tei-xsl-`cat VERSION`/p5/html
	-mkdir -p release/tei-xsl-`cat VERSION`/p5/common
	-mkdir -p release/tei-xsl-`cat VERSION`/p5/latex
	for i in odds/*xsl fo/*.xsl html/*xsl common/*xsl latex/*xsl slides/*xsl; do \
	perl toP5.pl --date="`date`" --version=`cat VERSION` < $$i > release/tei-xsl-`cat VERSION`/p5/$$i; \
	done

other: param stylebear
	-mkdir -p release/tei-xsl-`cat VERSION`/doc
	-cp ChangeLog param* LICENSE teixsl.* release/tei-xsl-`cat VERSION`/doc
	-mkdir -p release/tei-xsl-`cat VERSION`/css
	-cp *.css release/tei-xsl-`cat VERSION`/css
	-mkdir -p release/tei-xsl-`cat VERSION`/Test
	-cp Test/*.* Test/Makefile release/tei-xsl-`cat VERSION`/Test

param:
	xsltproc param.xsl param.xml  | grep -v masterFile > teihtml-param.xsl

stylebear:
	xsltproc paramform.xsl param.xml > stylebear

test: p4 p5
	cd Test; make

clean:
	-rm -rf release
