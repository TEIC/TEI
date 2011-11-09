VERSION=`cat ../VERSION`
SAXONOPT=useFixedDate=true
OXY=/usr/share/oxygen
default: conversions-from conversions-to test-html test-latex oddbyexample odd2odd

test-html:
	(mkdir files; cd files; saxon ../testnotes2.xml ../../xhtml2/tei.xsl splitLevel=4 useFixedDate=true STDOUT=false)
	diff -r -x .svn files expected-results/testnotes
	rm -rf files
	(mkdir files; cd files; saxon ../testnotes1.xml ../../xhtml2/tei.xsl splitLevel=4 useFixedDate=true STDOUT=false)
	diff -r -x .svn files expected-results/testnotes
	rm -rf files
	(mkdir files; cd files; saxon ../testnotes2.xml ../../xhtml2/tei.xsl splitLevel=0 useFixedDate=true STDOUT=false)
	diff -r -x .svn files expected-results/testnotes2
	rm -rf files
	saxon -o:dummy  test.xml ../xhtml2/tei.xsl $(SAXONOPT)
	saxon -o:dummy -versionmsg:off  test3.xml ../xhtml2/tei.xsl $(SAXONOPT)
	saxon -o:dummy4 -versionmsg:off  test4.xml ../xhtml2/tei.xsl $(SAXONOPT)
	echo differences:
	diff dummy dummy4
	diff dummy expected-results/dummy
	saxon -o:test5.xhtml test5.xml ../xhtml2/tei.xsl  $(SAXONOPT)
	diff test5.xhtml expected-results/test5.xhtml
	jing -c xhtml.rnc test5.xhtml

testsplit:
	saxon test6.xml ../xhtml2/tei.xsl STDOUT=false verbose=true splitLevel=1 useFixedDate=true


test-latex:
	saxon -o:test6.tex test6.xml ../slides/teilatex-slides.xsl $(SAXONOPT)
	diff test6.tex expected-results/test6.tex
	saxon -o:test5.tex test5.xml ../latex2/tei.xsl  $(SAXONOPT)
	diff test5.tex expected-results/test5.tex
	saxon -o:test5.fo test5.xml ../fo2/tei.xsl  $(SAXONOPT)
	diff test5.fo expected-results/test5.fo

odd2odd:
	saxon -s:testdrama.odd -xsl:../odds2/odd2odd.xsl -o:testdrama.compiled.xml \
	localsource=`pwd`/p5subset.xml $(SAXONOPT) 
	perl -p -i -e 's+ xml:base=".*testdrama.odd"++' testdrama.compiled.xml
	diff testdrama.compiled.xml expected-results/testdrama.compiled.xml

oddbyexample:
	saxon -it:main -o:oddbyexample.odd  -xsl:../tools/oddbyexample.xsl corpus=`pwd`/bare 
	roma2 oddbyexample.odd .
	jing -c oddbyexample.rnc bare/test2.xml
	jing oddbyexample.xsd bare/test2.xml
	xmllint --noout --dtdvalid oddbyexample.dtd bare/test2.xml
	jing -c oddbyexample.rnc bare/test.xml
	jing oddbyexample.xsd bare/test.xml
	xmllint --noout --dtdvalid oddbyexample.dtd bare/test.xml

conversions-from:
	saxon -o:test12.rdf test12.xml ../profiles/default/rdf/to.xsl
	diff test12.rdf expected-results/test12.rdf
	../teitohtml5 test.xml test.html5
	perl -p -i -e 's/on 20.*Z--/--/' test.html5
	perl -p -i -e 's/Date: [^<]+<br/<br/' test.html5
	diff test.html5 expected-results/test.html5
	../teitohtml --profile=tei --odd test15.odd test.odd.html
	perl -p -i -e 's/on 20.*Z--/--/' test.odd.html
	perl -p -i -e 's/Date: [^<]+<br/<br/' test.odd.html
	diff test.odd.html expected-results/test.odd.html
	../teitornc  test15.odd test15.odd.rnc
	perl -p -i -e 's/generated from ODD source.*//' test15.odd.rnc
	diff test15.odd.rnc expected-results/test15.odd.rnc 
	../teitotxt test14.xml test.text
	diff test.text expected-results/test.text
	../teitolatex test.xml
	perl -p -i -e 's/Date{[^}]*}/Date{}/' test.tex
	diff test.tex expected-results/test.tex
	../teitoepub  test.xml test.epub
	unzip -t test.epub > test.epub.listing
	diff test.epub.listing expected-results/test.epub.listing
	rm test.epub.listing
	../teitoepub  --fileperpage test.xml test-pages.epub
	unzip -t test-pages.epub > test.epub.listing2
	diff test.epub.listing2 expected-results/test.epub.listing2
	rm test.epub.listing2
	../teitoepub  test8.xml test8.epub
	unzip -t test8.epub > test8.epub.listing
	diff test8.epub.listing expected-results/test8.epub.listing
	../teitoodt test.xml 
	unzip -t test.xml.odt > test.xml.odt.listing
	diff test.xml.odt.listing expected-results/test.xml.odt.listing
	rm test.xml.odt.listing
	../teitodocx test.xml 
	unzip -t test.xml.docx > test.xml.docx.listing
	diff test.xml.docx.listing expected-results/test.xml.docx.listing
	rm test.xml.docx.listing
	../teitoodt test10.xml 
	unzip -t test10.xml.odt > test10.xml.odt.listing
	diff test10.xml.odt.listing expected-results/test10.xml.odt.listing
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
	diff test7.xml expected-results/test7.xml
	../docxtotei test11.docx test11.xml
	perl -p -i -e 's/Date:[^<]+</</' test11.xml
	jing test.rng test11.xml
	diff test11.xml expected-results/test11.xml
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

