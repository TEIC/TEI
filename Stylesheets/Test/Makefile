VERSION=`cat ../VERSION`
default:
	echo run tests
	xsltproc --stringparam verbose true ../release/tei-xsl-${VERSION}/p5/html/teihtml.xsl test.xml >/dev/null
	saxon test.xml ../release/tei-xsl-${VERSION}/p5/html/teihtml.xsl verbose=true > /dev/null
	xalan -PARAM verbose true -IN test.xml -XSL ../release/tei-xsl-${VERSION}/p5/html/teihtml.xsl > /dev/null
	xsltproc --stringparam verbose true ../release/tei-xsl-${VERSION}/p5/fo/tei.xsl test.xml | xmllint --format -  > test.fo
	-diff test.fo test.fo.ok
	saxon test.xml ../release/tei-xsl-${VERSION}/p5/fo/tei.xsl verbose=true | xmllint --format - > test.fo
	-diff test.fo test.fo.ok
	xalan -PARAM verbose true -IN test.xml -XSL ../release/tei-xsl-${VERSION}/p5/fo/tei.xsl | xmllint --format - >test.fo
	-diff test.fo test.fo.ok
	xsltproc --stringparam verbose true ../release/tei-xsl-${VERSION}/p5/latex/teilatex.xsl test.xml > test.tex
	-diff test.tex test.tex.ok
	saxon test.xml ../release/tei-xsl-${VERSION}/p5/latex/teilatex.xsl verbose=true > test.tex
	-diff test.tex test.tex.ok
	xalan -PARAM verbose true -IN test.xml -XSL ../release/tei-xsl-${VERSION}/p5/latex/teilatex.xsl > test.tex
	-diff test.tex test.tex.ok
	xsltproc --stringparam verbose true ../release/tei-xsl-${VERSION}/p4/html/teihtml.xsl testp4.xml > /dev/null
	saxon testp4.xml ../release/tei-xsl-${VERSION}/p4/html/teihtml.xsl verbose=true > /dev/null
	xalan -PARAM verbose true -IN testp4.xml -XSL ../release/tei-xsl-${VERSION}/p4/html/teihtml.xsl > /dev/null
	xsltproc --stringparam verbose true ../release/tei-xsl-${VERSION}/p4/fo/tei.xsl testp4.xml > /dev/null
	saxon testp4.xml ../release/tei-xsl-${VERSION}/p4/fo/tei.xsl verbose=true > /dev/null
	xalan -PARAM verbose true -IN testp4.xml -XSL ../release/tei-xsl-${VERSION}/p4/fo/tei.xsl > /dev/null
	xsltproc --stringparam verbose true ../release/tei-xsl-${VERSION}/p4/latex/teilatex.xsl testp4.xml > /dev/null
	saxon testp4.xml ../release/tei-xsl-${VERSION}/p4/latex/teilatex.xsl verbose=true > /dev/null
	xalan -PARAM verbose true -IN testp4.xml -XSL ../release/tei-xsl-${VERSION}/p4/latex/teilatex.xsl > /dev/null
