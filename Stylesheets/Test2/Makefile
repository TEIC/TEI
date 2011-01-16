VERSION=`cat ../VERSION`
SAXONOPT=verbose=true useFixedDate=true
default: general test test3 test4 test5 test6 oddbyexample

test:
	saxon -o:dummy  test.xml ../xhtml2/tei.xsl $(SAXONOPT)

testsplit:
	saxon test6.xml ../xhtml2/tei.xsl STDOUT=false verbose=true splitLevel=1 useFixedDate=true

test3:
	saxon -o:dummy -versionmsg:off  test3.xml ../xhtml2/tei.xsl $(SAXONOPT)
	saxon -o:dummy4 -versionmsg:off  test4.xml ../xhtml2/tei.xsl $(SAXONOPT)
	echo differences:
	diff dummy dummy4

test4:
	saxon -o:dummy -versionmsg:off  test3.xml ../xhtml2/tei.xsl $(SAXONOPT)
	saxon -o:dummy4 -versionmsg:off  test4.xml ../xhtml2/tei.xsl $(SAXONOPT)
	echo differences:
	diff dummy dummy4

test5:
	saxon -o:test5.xhtml test5.xml ../xhtml2/tei.xsl  $(SAXONOPT)
	diff test5.xhtml ok/test5.xhtml
	saxon -o:test5.tex test5.xml ../latex2/tei.xsl  $(SAXONOPT)
	diff test5.tex ok/test5.tex
	saxon -o:test5.fo test5.xml ../fo2/tei.xsl  $(SAXONOPT)
	diff test5.fo ok/test5.fo

test6:
	saxon -o:test6.tex test6.xml ../slides2/teilatex-slides.xsl $(SAXONOPT)
	diff test6.tex ok/test6.tex

odd:
	saxon testdrama.odd ../odds2/odd2odd.xsl \
	TEIC=true localsource=`pwd`/p5subset.xml $(SAXONOPT) > testdrama.compiled.xml

oddbyexample:
	saxon -o:oddbyexample.odd ../tools2/oddbyexample.xsl ../tools2/oddbyexample.xsl corpus=`pwd`/bare 
	roma2 oddbyexample.odd .
	jing -c oddbyexample.rnc bare/test2.xml
	jing oddbyexample.xsd bare/test2.xml
	xmllint --noout --dtdvalid oddbyexample.dtd bare/test2.xml
	jing -c oddbyexample.rnc bare/test.xml
	jing oddbyexample.xsd bare/test.xml
	xmllint --noout --dtdvalid oddbyexample.dtd bare/test.xml

general:
	../teitolatex test.xml
	diff test.tex ok/test.tex
	../teitoepub  test.xml 
	../teitoodt test.xml 
	../teitodocx test.xml 
	unzip -t test.xml.docx > test.xml.docx.list
	diff test.xml.docx.list ok/test.xml.docx.list
	unzip -t test.xml.odt > test.xml.odt.list
	diff test.xml.odt.list ok/test.xml.odt.list
	unzip -t test.epub > test.xml.epub.list
	diff test.xml.epub.list ok/test.xml.epub.list

clean:
	rm -f  test6.tex test5.tex test5.fo *.html *.xhtml *~ dummy*
	rm -f  oddbyexample.* xml.xsd index*div*html
	rm -f  test.tex test.xml.odt test.xml.docx test.epub
	rm -f  test8.pdf
	rm -f  test8.log
	rm -f  test5.xml.odt
	rm -f  test8.aux
	rm -f  test7.xml
	rm -f  test8.tex
	rm -f  test7.zip
	rm -f  test8.doc
	rm -f  test8.out
	rm -f  test.xml.*.list
	rm -f  test.aux test.log test.out test.pdf 

