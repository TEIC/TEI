VERSION=`cat ../VERSION`
default: test testp4 test2 test3

test:
	xsltproc -o dummy --stringparam verbose true ../release/tei-xsl/p5/html/tei.xsl test.xml
	saxon -o dummy -novw  test.xml ../release/tei-xsl/p5/html/tei.xsl verbose=true 
	-xalan -out dummy -param verbose true -in test.xml -xsl ../release/tei-xsl/p5/html/tei.xsl 
	xsltproc --stringparam verbose true ../release/tei-xsl/p5/fo/tei.xsl test.xml | xmllint --format -  > test.fo
	-diff test.fo test.fo.ok
	saxon -novw  test.xml ../release/tei-xsl/p5/fo/tei.xsl verbose=true | xmllint --format - > test.fo
	-diff test.fo test.fo.ok
	-xalan -param verbose true -in test.xml -xsl ../release/tei-xsl/p5/fo/tei.xsl | xmllint --format - >test.fo
	xsltproc --stringparam verbose true ../release/tei-xsl/p5/latex/tei.xsl test.xml > test.tex
	-diff test.tex test.tex.ok
	saxon -novw  test.xml ../release/tei-xsl/p5/latex/tei.xsl verbose=true > test.tex
	-diff test.tex test.tex.ok
	-xalan -param verbose true -in test.xml -xsl ../release/tei-xsl/p5/latex/tei.xsl > test.tex
	-diff test.tex test.tex.ok

testp4:
	xsltproc -o dummy --stringparam verbose true ../release/tei-xsl/p4/html/tei.xsl testp4.xml 
	saxon -o dummy -novw  testp4.xml ../release/tei-xsl/p4/html/tei.xsl verbose=true 
	-xalan -out dummy -param verbose true -in testp4.xml -xsl ../release/tei-xsl/p4/html/tei.xsl 
	xsltproc -o dummy --stringparam verbose true ../release/tei-xsl/p4/fo/tei.xsl testp4.xml 
	saxon -o dummy -novw  testp4.xml ../release/tei-xsl/p4/fo/tei.xsl verbose=true 
	-xalan -out dummy -param verbose true -in testp4.xml -xsl ../release/tei-xsl/p4/fo/tei.xsl 
	xsltproc -o dummy --stringparam verbose true ../release/tei-xsl/p4/latex/tei.xsl testp4.xml 
	saxon -o dummy -novw  testp4.xml ../release/tei-xsl/p4/latex/tei.xsl verbose=true 
	-xalan -out dummy -param verbose true -in testp4.xml -xsl ../release/tei-xsl/p4/latex/tei.xsl 

test3:
	xsltproc -o dummy --stringparam verbose true ../release/tei-xsl/p5/html/tei.xsl test3.xml
	xsltproc -o dummy4 --stringparam verbose true ../release/tei-xsl/p5/html/tei.xsl test4.xml
	echo differences:
	diff dummy dummy4
	saxon -o dummy -novw  test3.xml ../release/tei-xsl/p5/html/tei.xsl verbose=true 
	saxon -o dummy4 -novw  test4.xml ../release/tei-xsl/p5/html/tei.xsl verbose=true 
	echo differences:
	diff dummy dummy4
	-xalan -out dummy -param verbose true -in test3.xml -xsl ../release/tei-xsl/p5/html/tei.xsl 
	-xalan -out dummy4 -param verbose true -in test4.xml -xsl ../release/tei-xsl/p5/html/tei.xsl 
	diff dummy dummy4

test4:
	xsltproc -o dummy --stringparam verbose true ../release/tei-xsl/p5/xhtml/tei.xsl test3.xml
	xsltproc -o dummy4 --stringparam verbose true ../release/tei-xsl/p5/xhtml/tei.xsl test4.xml
	echo differences:
	diff dummy dummy4
	saxon -o dummy -novw  test3.xml ../release/tei-xsl/p5/xhtml/tei.xsl verbose=true 
	saxon -o dummy4 -novw  test4.xml ../release/tei-xsl/p5/xhtml/tei.xsl verbose=true 
	echo differences:
	diff dummy dummy4
	-xalan -out dummy -param verbose true -in test3.xml -xsl ../release/tei-xsl/p5/xhtml/tei.xsl 
	-xalan -out dummy4 -param verbose true -in test4.xml -xsl ../release/tei-xsl/p5/xhtml/tei.xsl 
	diff dummy dummy4


clean:
	-rm test.tex test.fo *.html *~ dummy*
