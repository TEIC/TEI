VERSION=4.0

all: p4 p5 other


p4:
	-mkdir -p dist/p4/odds
	-mkdir -p dist/p4/fo
	-mkdir -p dist/p4/html
	-mkdir -p dist/p4/common
	-mkdir -p dist/p4/latex
	-mkdir -p dist/p4/slides
	for i in odds/*xsl fo/*.xsl html/*xsl common/*xsl latex/*xsl slides/*xsl; do \
	echo do $$i;perl toP4.pl --date="`date`" --version=$(VERSION) < $$i > dist/p4/$$i; \
	done

p5:
	-mkdir -p dist/p5/odds
	-mkdir -p dist/p5/fo
	-mkdir -p dist/p5/slides
	-mkdir -p dist/p5/html
	-mkdir -p dist/p5/common
	-mkdir -p dist/p5/latex
	for i in odds/*xsl fo/*.xsl html/*xsl common/*xsl latex/*xsl slides/*xsl; do \
	perl toP5.pl --date="`date`" --version=$(VERSION) < $$i > dist/p5/$$i; \
	done

other: param stylebear
	-mkdir -p dist/doc
	-cp ChangeLog param* LICENSE teixsl.* dist/doc
	-cp *.css dist

zip:
	chmod -R u+w dist
	(cd dist/p4; zip -r teixsl-fo   fo common ../ChangeLog    ../teixsl.html ../teixsl.xml)
	(cd dist/p4; zip -r teixsl-html html common ../ChangeLog ../teixsl.html ../teixsl.xml)
	(cd dist/p4; zip -r teixsl-latex latex common ../ChangeLog ../teixsl.html ../teixsl.xml)
	(cd dist/p5; zip -r teixsl-fo   fo common ../ChangeLog    ../teixsl.html ../teixsl.xml)
	(cd dist/p5; zip -r teixsl-html html common ../ChangeLog ../teixsl.html ../teixsl.xml)
	(cd dist/p5; zip -r teixsl-latex latex common ../ChangeLog ../teixsl.html ../teixsl.xml)

param:
	xsltproc param.xsl param.xml  | grep -v masterFile > teihtml-param.xsl

stylebear:
	xsltproc paramform.xsl param.xml > stylebear

test: p4 p5
	echo run tests
	xsltproc --stringparam verbose true dist/p5/html/teihtml.xsl test.xml >/dev/null
	saxon test.xml dist/p5/html/teihtml.xsl verbose=true > /dev/null
	xalan -PARAM verbose true -IN test.xml -XSL dist/p5/html/teihtml.xsl > /dev/null
	xsltproc --stringparam verbose true dist/p5/fo/tei.xsl test.xml | xmllint --format -  > test.fo
	-diff test.fo test.fo.ok
	saxon test.xml dist/p5/fo/tei.xsl verbose=true | xmllint --format - > test.fo
	-diff test.fo test.fo.ok
	xalan -PARAM verbose true -IN test.xml -XSL dist/p5/fo/tei.xsl | xmllint --format - >test.fo
	-diff test.fo test.fo.ok
	xsltproc --stringparam verbose true dist/p5/latex/teilatex.xsl test.xml > test.tex
	-diff test.tex test.tex.ok
	saxon test.xml dist/p5/latex/teilatex.xsl verbose=true > test.tex
	-diff test.tex test.tex.ok
	xalan -PARAM verbose true -IN test.xml -XSL dist/p5/latex/teilatex.xsl > test.tex
	-diff test.tex test.tex.ok
	xsltproc --stringparam verbose true dist/p4/html/teihtml.xsl testp4.xml > /dev/null
	saxon testp4.xml dist/p4/html/teihtml.xsl verbose=true > /dev/null
	xalan -PARAM verbose true -IN testp4.xml -XSL dist/p4/html/teihtml.xsl > /dev/null
	xsltproc --stringparam verbose true dist/p4/fo/tei.xsl testp4.xml > /dev/null
	saxon testp4.xml dist/p4/fo/tei.xsl verbose=true > /dev/null
	xalan -PARAM verbose true -IN testp4.xml -XSL dist/p4/fo/tei.xsl > /dev/null
	xsltproc --stringparam verbose true dist/p4/latex/teilatex.xsl testp4.xml > /dev/null
	saxon testp4.xml dist/p4/latex/teilatex.xsl verbose=true > /dev/null
	xalan -PARAM verbose true -IN testp4.xml -XSL dist/p4/latex/teilatex.xsl > /dev/null

clean:
	-rm -rf dist
