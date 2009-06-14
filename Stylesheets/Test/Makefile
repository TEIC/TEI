VERSION=`cat ../VERSION`
default: test testp4 test3 test4 testsplit

test:
	xsltproc -o dummy --stringparam verbose true ../release/tei-xsl/p5/html/tei.xsl test.xml
	saxon -o dummy -novw  test.xml ../release/tei-xsl/p5/html/tei.xsl verbose=true 
	xsltproc --stringparam verbose true ../release/tei-xsl/p5/fo/tei.xsl test.xml | xmllint --format -  > test.fo.xsltproc
	-diff test.fo.xsltproc test.fo.ok.xsltproc
	saxon -novw  test.xml ../release/tei-xsl/p5/fo/tei.xsl verbose=true | xmllint --format - > test.fo.saxon
	-diff test.fo.saxon test.fo.ok.saxon
	xsltproc --stringparam verbose true ../release/tei-xsl/p5/latex/tei.xsl test.xml > test.tex.xsltproc
	-diff test.tex.xsltproc test.tex.ok.xsltproc
	saxon -novw  test.xml ../release/tei-xsl/p5/latex/tei.xsl verbose=true > test.tex.saxon
	-diff test.tex.saxon test.tex.ok.saxon

testsplit:
	xsltproc --stringparam STDOUT false ../release/tei-xsl/p5/html/tei.xsl test6.xml
	-saxon test6.xml ../release/tei-xsl/p5/html/tei.xsl STDOUT=false

testp4:
	xsltproc -o dummy --stringparam verbose true ../release/tei-xsl/p4/html/tei.xsl testp4.xml 
	saxon -o dummy -novw  testp4.xml ../release/tei-xsl/p4/html/tei.xsl verbose=true 
	xsltproc -o dummy --stringparam verbose true ../release/tei-xsl/p4/fo/tei.xsl testp4.xml 
	saxon -o dummy -novw  testp4.xml ../release/tei-xsl/p4/fo/tei.xsl verbose=true 
	xsltproc -o dummy --stringparam verbose true ../release/tei-xsl/p4/latex/tei.xsl testp4.xml 
	saxon -o dummy -novw  testp4.xml ../release/tei-xsl/p4/latex/tei.xsl verbose=true 

test3:
	xsltproc -o dummy --stringparam verbose true ../release/tei-xsl/p5/html/tei.xsl test3.xml
	xsltproc -o dummy4 --stringparam verbose true ../release/tei-xsl/p5/html/tei.xsl test4.xml
	echo differences:
	-diff dummy dummy4
	saxon -o dummy -novw  test3.xml ../release/tei-xsl/p5/html/tei.xsl verbose=true 
	saxon -o dummy4 -novw  test4.xml ../release/tei-xsl/p5/html/tei.xsl verbose=true 
	echo differences:
	-diff dummy dummy4


test4:
	xsltproc -o dummy --stringparam verbose true ../release/tei-xsl/p5/xhtml/tei.xsl test3.xml
	xsltproc -o dummy4 --stringparam verbose true ../release/tei-xsl/p5/xhtml/tei.xsl test4.xml
	echo differences:
	-diff dummy dummy4
	saxon -o dummy -novw  test3.xml ../release/tei-xsl/p5/xhtml/tei.xsl verbose=true 
	saxon -o dummy4 -novw  test4.xml ../release/tei-xsl/p5/xhtml/tei.xsl verbose=true 
	echo differences:
	-diff dummy dummy4

clean:
	-rm test.tex test.fo *.html *~ dummy* *.aux *.out *.log
	-rm      test.tex.xsltproc
	-rm      test.fo.xsltproc
	-rm      test.tex.saxon
	-rm      test.fo.saxon
