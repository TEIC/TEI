all: clean p4 p5 other


p4:
	-mkdir -p dist/p4/odds
	-mkdir -p dist/p4/fo
	-mkdir -p dist/p4/html
	-mkdir -p dist/p4/common
	-mkdir -p dist/p4/latex
	-mkdir -p dist/p4/slides
	for i in odds/*xsl fo/*.xsl html/*xsl common/*xsl latex/*xsl slides/*xsl; do \
	echo do $$i;perl toP4.pl --date="`date`" --version=`cat VERSION` < $$i > dist/p4/$$i; \
	done

p5:
	-mkdir -p dist/p5/odds
	-mkdir -p dist/p5/fo
	-mkdir -p dist/p5/slides
	-mkdir -p dist/p5/html
	-mkdir -p dist/p5/common
	-mkdir -p dist/p5/latex
	for i in odds/*xsl fo/*.xsl html/*xsl common/*xsl latex/*xsl slides/*xsl; do \
	perl toP5.pl --date="`date`" --version=`cat VERSION` < $$i > dist/p5/$$i; \
	done

other: param stylebear
	-mkdir -p dist/doc
	-cp ChangeLog param* LICENSE teixsl.* dist/doc
	-mkdir -p dist/css
	-cp *.css dist/css
	-mkdir -p dist/Test
	-cp Test/*.* Test/Makefile dist/Test

param:
	xsltproc param.xsl param.xml  | grep -v masterFile > teihtml-param.xsl

stylebear:
	xsltproc paramform.xsl param.xml > stylebear

test: p4 p5
	cd Test; make

clean:
	-rm -rf dist
