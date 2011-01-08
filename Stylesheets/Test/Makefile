VERSION=`cat ../VERSION`
XSLTOPT=--stringparam verbose true --stringparam useFixedDate true 
SAXONOPT=verbose=true useFixedDate=true

default: test testp4 test3 test4 test5 test6 testsplit

test:
	xsltproc -o dummy $(XSLTOPT) ../release/p5/xml/tei/stylesheet/html/tei.xsl test.xml
	saxon -o:dummy -versionmsg:off  test.xml ../release/p5/xml/tei/stylesheet/html/tei.xsl $(SAXONOPT)
	xsltproc $(XSLTOPT) ../release/p5/xml/tei/stylesheet/fo/tei.xsl test.xml | xmllint --format -  > test.fo.xsltproc
	diff test.fo.xsltproc ok/test.fo.xsltproc
	saxon -versionmsg:off  test.xml ../release/p5/xml/tei/stylesheet/fo/tei.xsl $(SAXONOPT) | xmllint --format - > test.fo.saxon
	diff test.fo.saxon ok/test.fo.saxon
	xsltproc $(XSLTOPT) ../release/p5/xml/tei/stylesheet/latex/tei.xsl test.xml > test.tex.xsltproc
	diff test.tex.xsltproc ok/test.tex.xsltproc
	saxon -versionmsg:off  test.xml ../release/p5/xml/tei/stylesheet/latex/tei.xsl $(SAXONOPT) > test.tex.saxon
	diff test.tex.saxon ok/test.tex.saxon

testsplit:
	xsltproc --stringparam STDOUT false ../release/p5/xml/tei/stylesheet/html/tei.xsl test6.xml

testp4:
	xsltproc -o dummy $(XSLTOPT) ../release/p4/xml/teip4/stylesheet/html/tei.xsl testp4.xml 
	saxon -o:dummy -versionmsg:off  testp4.xml ../release/p4/xml/teip4/stylesheet/html/tei.xsl $(SAXONOPT)
	xsltproc -o dummy $(XSLTOPT) ../release/p4/xml/teip4/stylesheet/fo/tei.xsl testp4.xml 
	saxon -o:dummy -versionmsg:off  testp4.xml ../release/p4/xml/teip4/stylesheet/fo/tei.xsl $(SAXONOPT)
	xsltproc -o dummy $(XSLTOPT) ../release/p4/xml/teip4/stylesheet/latex/tei.xsl testp4.xml 
	saxon -o:dummy -versionmsg:off  testp4.xml ../release/p4/xml/teip4/stylesheet/latex/tei.xsl $(SAXONOPT)

test3:
	xsltproc -o dummy $(XSLTOPT) ../release/p5/xml/tei/stylesheet/html/tei.xsl test3.xml
	xsltproc -o dummy4 $(XSLTOPT) ../release/p5/xml/tei/stylesheet/html/tei.xsl test4.xml
	@echo "differences between numbered and unnumbered divs (xsltproc)"
	diff dummy dummy4

test4:
	saxon -o:dummy -versionmsg:off  test3.xml ../release/p5/xml/tei/stylesheet/html/tei.xsl $(SAXONOPT) 
	saxon -o:dummy4 -versionmsg:off  test4.xml ../release/p5/xml/tei/stylesheet/html/tei.xsl $(SAXONOPT) 
	@echo "differences between numbered and unnumbered divs (saxon)"
	diff dummy dummy4

test5:
	xsltproc -o dummy $(XSLTOPT) ../release/p5/xml/tei/stylesheet/xhtml/tei.xsl test3.xml
	xsltproc -o dummy4 $(XSLTOPT) ../release/p5/xml/tei/stylesheet/xhtml/tei.xsl test4.xml
	@echo "differences between numbered and unnumbered divs (xhtml, xsltproc):"
	diff dummy dummy4

test6:
	saxon -o:dummy -versionmsg:off test3.xml ../release/p5/xml/tei/stylesheet/xhtml/tei.xsl $(SAXONOPT) 
	saxon -o:dummy4 -versionmsg:off test4.xml ../release/p5/xml/tei/stylesheet/xhtml/tei.xsl $(SAXONOPT)
	@echo "differences between numbered and unnumbered divs (xhtml, saxon):"
	diff dummy dummy4

clean:
	rm -f test.tex test.fo *.html *~ dummy* *.aux *.out *.log
	rm -f test.tex.xsltproc
	rm -f test.fo.xsltproc
	rm -f test.tex.saxon
	rm -f test.fo.saxon
