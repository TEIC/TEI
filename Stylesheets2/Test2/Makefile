VERSION=`cat ../VERSION`
default: test test4 test3

test:
	saxon -o dummy -novw  test.xml ../release/tei-xsl2/xhtml2/tei.xsl verbose=true 

testsplit:
	-saxon test6.xml ../release/tei-xsl2/xhtml2/tei.xsl STDOUT=false

test3:
	saxon -o dummy -novw  test3.xml ../release/tei-xsl2/xhtml2/tei.xsl verbose=true 
	saxon -o dummy4 -novw  test4.xml ../release/tei-xsl2/xhtml2/tei.xsl verbose=true 
	echo differences:
	diff dummy dummy4

test4:
	saxon -o dummy -novw  test3.xml ../release/tei-xsl2/xhtml2/tei.xsl verbose=true 
	saxon -o dummy4 -novw  test4.xml ../release/tei-xsl2/xhtml2/tei.xsl verbose=true 
	echo differences:
	diff dummy dummy4

clean:
	-rm test.tex test.fo *.html *~ dummy*
