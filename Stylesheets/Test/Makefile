VERSION=`cat ../VERSION`
default:
	echo run tests
	xsltproc --stringparam verbose true ../release/tei-xsl/p5/html/tei.xsl test.xml >/dev/null
	saxon test.xml ../release/tei-xsl/p5/html/tei.xsl verbose=true > /dev/null
	xalan -PARAM verbose true -IN test.xml -XSL ../release/tei-xsl/p5/html/tei.xsl > /dev/null
	xsltproc --stringparam verbose true ../release/tei-xsl/p5/fo/tei.xsl test.xml | xmllint --format -  > test.fo
	-diff test.fo test.fo.ok
	saxon test.xml ../release/tei-xsl/p5/fo/tei.xsl verbose=true | xmllint --format - > test.fo
	-diff test.fo test.fo.ok
	xalan -PARAM verbose true -IN test.xml -XSL ../release/tei-xsl/p5/fo/tei.xsl | xmllint --format - >test.fo
	xsltproc --stringparam verbose true ../release/tei-xsl/p5/latex/tei.xsl test.xml > test.tex
	-diff test.tex test.tex.ok
	saxon test.xml ../release/tei-xsl/p5/latex/tei.xsl verbose=true > test.tex
	-diff test.tex test.tex.ok
	xalan -PARAM verbose true -IN test.xml -XSL ../release/tei-xsl/p5/latex/tei.xsl > test.tex
	-diff test.tex test.tex.ok
	xsltproc --stringparam verbose true ../release/tei-xsl/base/p4/html/tei.xsl testp4.xml > /dev/null
	saxon testp4.xml ../release/tei-xsl/base/p4/html/tei.xsl verbose=true > /dev/null
	-xalan -PARAM verbose true -IN testp4.xml -XSL ../release/tei-xsl/base/p4/html/tei.xsl > /dev/null
	xsltproc --stringparam verbose true ../release/tei-xsl/base/p4/fo/tei.xsl testp4.xml > /dev/null
	saxon testp4.xml ../release/tei-xsl/base/p4/fo/tei.xsl verbose=true > /dev/null
	-xalan -PARAM verbose true -IN testp4.xml -XSL ../release/tei-xsl/base/p4/fo/tei.xsl > /dev/null
	xsltproc --stringparam verbose true ../release/tei-xsl/base/p4/latex/tei.xsl testp4.xml > /dev/null
	saxon testp4.xml ../release/tei-xsl/base/p4/latex/tei.xsl verbose=true > /dev/null
	-xalan -PARAM verbose true -IN testp4.xml -XSL ../release/tei-xsl/base/p4/latex/tei.xsl > /dev/null

clean:
	-rm test.tex test.fo *.html
