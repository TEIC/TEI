VERSION=`cat ../VERSION`
default: test test4 test3 test5

test:
	saxon -o dummy  test.xml ../xhtml2/tei.xsl verbose=true 

testsplit:
	-saxon test6.xml ../xhtml2/tei.xsl STDOUT=false

test3:
	saxon -o dummy -novw  test3.xml ../xhtml2/tei.xsl verbose=true 
	saxon -o dummy4 -novw  test4.xml ../xhtml2/tei.xsl verbose=true 
	echo differences:
	-diff dummy dummy4

test4:
	saxon -o dummy -novw  test3.xml ../xhtml2/tei.xsl verbose=true 
	saxon -o dummy4 -novw  test4.xml ../xhtml2/tei.xsl verbose=true 
	echo differences:
	-diff dummy dummy4

test5:
	saxon -o test5.xhtml test5.xml ../xhtml2/tei.xsl
	-diff test5.xhtml ok/test5.xhtml
	saxon -o test5.tex test5.xml ../latex2/tei.xsl
	-diff test5.tex ok/test5.tex
	saxon -o test5.fo test5.xml ../fo2/tei.xsl
	-diff test5.fo ok/test5.fo

odd:
	saxon testdrama.odd ../odds2/odd2odd.xsl \
	TEIC=true localsource=`pwd`/p5subset.xml > testdrama.compiled.xml

clean:
	-rm test5.tex test5.fo *.html *.xhtml *~ dummy*
