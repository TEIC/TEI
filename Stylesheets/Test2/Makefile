VERSION=`cat ../VERSION`
SAXONOPT=verbose=true useFixedDate=true
OXY=/usr/share/oxygen
default: conversions-from conversions-to test-html test-latex oddbyexample

test-html:
	saxon -o:dummy  test.xml ../xhtml2/tei.xsl $(SAXONOPT)
	saxon -o:dummy -versionmsg:off  test3.xml ../xhtml2/tei.xsl $(SAXONOPT)
	saxon -o:dummy4 -versionmsg:off  test4.xml ../xhtml2/tei.xsl $(SAXONOPT)
	echo differences:
	-diff dummy dummy4
	saxon -o:test5.xhtml test5.xml ../xhtml2/tei.xsl  $(SAXONOPT)
	diff test5.xhtml ok/test5.xhtml
	jing -c xhtml.rnc test5.xhtml

testsplit:
	saxon test6.xml ../xhtml2/tei.xsl STDOUT=false verbose=true splitLevel=1 useFixedDate=true


test-latex:
	saxon -o:test6.tex test6.xml ../slides2/teilatex-slides.xsl $(SAXONOPT)
	diff test6.tex ok/test6.tex
	saxon -o:test5.tex test5.xml ../latex2/tei.xsl  $(SAXONOPT)
	diff test5.tex ok/test5.tex
	saxon -o:test5.fo test5.xml ../fo2/tei.xsl  $(SAXONOPT)
	diff test5.fo ok/test5.fo

odd:
	saxon testdrama.odd ../odds2/odd2odd.xsl \
	localsource=`pwd`/p5subset.xml $(SAXONOPT) > testdrama.compiled.xml

oddbyexample:
	saxon -o:oddbyexample.odd ../tools2/oddbyexample.xsl ../tools2/oddbyexample.xsl corpus=`pwd`/bare 
	roma2 oddbyexample.odd .
	jing -c oddbyexample.rnc bare/test2.xml
	jing oddbyexample.xsd bare/test2.xml
	xmllint --noout --dtdvalid oddbyexample.dtd bare/test2.xml
	jing -c oddbyexample.rnc bare/test.xml
	jing oddbyexample.xsd bare/test.xml
	xmllint --noout --dtdvalid oddbyexample.dtd bare/test.xml

conversions-from:
	../teitohtml --odd test15.odd test.odd.html
	perl -p -i -e 's/on 20.*Z--/--/' test.odd.html
	diff test.odd.html ok/test.odd.html
	saxon -o:test12.rdf test12.xml ../profiles/default/rdf/to.xsl
	diff test12.rdf ok/test12.rdf
	../teitotxt test14.xml test.text
	diff test.text ok/test.text
	../teitolatex test.xml
	perl -p -i -e 's/Date{[^}]*}/Date{}/' test.tex
	diff test.tex ok/test.tex
	../teitoepub  test.xml test.epub
	unzip -t test.epub > test.epub.listing
	diff test.epub.listing ok/test.epub.listing
	rm test.epub.listing
	../teitoepub  --fileperpage test.xml test-pages.epub
	unzip -t test-pages.epub > test.epub.listing2
	diff test.epub.listing2 ok/test.epub.listing2
	rm test.epub.listing2
	../teitoepub  test8.xml test8.epub
	unzip -t test8.epub > test8.epub.listing
	diff test8.epub.listing ok/test8.epub.listing
	../teitoodt test.xml 
	unzip -t test.xml.odt > test.xml.odt.listing
	diff test.xml.odt.listing ok/test.xml.odt.listing
	rm test.xml.odt.listing
	../teitodocx test.xml 
	unzip -t test.xml.docx > test.xml.docx.listing
	diff test.xml.docx.listing ok/test.xml.docx.listing
	rm test.xml.docx.listing
	../teitoodt test10.xml 
	unzip -t test10.xml.odt > test10.xml.odt.listing
	diff test10.xml.odt.listing ok/test10.xml.odt.listing
	rm test10.xml.odt.listing
	test -d $(OXY) || exit 1
	java -Xmx256m \
	-classpath  "$(OXY)/tools/ant/lib/ant-launcher.jar" \
	-Dant.home=$(OXY)/tools/ant \
	org.apache.tools.ant.launch.Launcher \
	-f ../docx/build-to.xml \
	-lib $(OXY)/lib/saxon9ee.jar \
	-DoutputFile=`pwd`/test9.docx \
	-DinputFile=`pwd`/test9.xml -Dprofile=default 
	java -Xmx256m \
	-classpath  "$(OXY)/tools/ant/lib/ant-launcher.jar" \
	-Dant.home=$(OXY)/tools/ant \
	org.apache.tools.ant.launch.Launcher \
	-f ../odt/build-to.xml \
	-lib $(OXY)/lib/saxon9ee.jar \
	-DoutputFile=`pwd`/test9.odt \
	-DinputFile=`pwd`/test9.xml -Dprofile=default -Dverbose=true -Ddebug=true 

conversions-to:
	roma2 --nodtd --noxsd test.odd .
	../odttotei test7.odt test7.xml
	jing test.rng test7.xml
	diff test7.xml ok/test7.xml
	../docxtotei test11.docx test11.xml
	perl -p -i -e 's/Date:[^<]+</</' test11.xml
	jing test.rng test11.xml
	diff test11.xml ok/test11.xml
	rm test.rng test.rnc

clean:
	rm -f  test6.tex test5.tex test5.fo *.html *.xhtml *~ dummy*
	rm -f  oddbyexample.* xml.xsd index*div*html
	rm -f  test.tex test.xml.odt test.xml.docx test.epub
	rm -f  test8.pdf
	rm -f  test8.log
	rm -f  test5.odt
	rm -f  test10.xml.odt
	rm -f  test8.aux
	rm -f  test7.xml test11.xml
	rm -f  test8.tex
	rm -f  test7.zip
	rm -f  test8.doc
	rm -f  test8.out
	rm -f  test9.docx test9.odt
	rm -f  test*listing*
	rm -f test*epub test*text
	rm -f  test.aux test.log test.out test.pdf 
	rm -rf temp-dir-for-ant Pictures OEBPS META-INF mimetype

