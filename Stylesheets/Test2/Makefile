VERSION=`cat ../VERSION`
default: general test test3 test4 test5 test6 oddbyexample

test:
	saxon -o dummy  test.xml ../xhtml2/tei.xsl verbose=true 

testsplit:
	-saxon test6.xml ../xhtml2/tei.xsl STDOUT=false verbose=true splitLevel=1

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

test6:
	saxon -o test6.tex test6.xml ../slides2/teilatex-slides.xsl 
	saxon  test6.xml ../slides2/teihtml-slides.xsl 
	-diff test6.tex ok/test6.tex

odd:
	saxon testdrama.odd ../odds2/odd2odd.xsl \
	TEIC=true localsource=`pwd`/p5subset.xml > testdrama.compiled.xml

oddbyexample:
	saxon -o oddbyexample.odd ../tools2/oddbyexample.xsl ../tools2/oddbyexample.xsl corpus=`pwd`/bare 
	roma2 oddbyexample.odd .
	jing -c oddbyexample.rnc bare/test2.xml
	jing oddbyexample.xsd bare/test2.xml
	xmllint --noout --dtdvalid oddbyexample.dtd bare/test2.xml
	@echo NOTE: expect errors now about "wicked"
	-jing -c oddbyexample.rnc bare/test.xml
	-jing oddbyexample.xsd bare/test.xml
	-xmllint --noout --dtdvalid oddbyexample.dtd bare/test.xml

general:
	../teitolatex test.xml
	../teitoepub  test.xml 
	../teitoodt test.xml 
	../teitodocx test.xml 

clean:
	-rm test6.tex test5.tex test5.fo *.html *.xhtml *~ dummy*
	-rm oddbyexample.* xml.xsd index*div*html
	-rm test.tex test.xml.odt test.xml.docx test.epub
	-rm test8.pdf
	-rm test8.log
	-rm test5.xml.odt
	-rm test8.aux
	-rm test7.xml
	-rm test8.tex
	-rm test7.zip
	-rm test8.doc
	-rm test8.out

