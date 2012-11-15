VERSION=`cat ../VERSION`
SAXONOPT=useFixedDate=true
OXY=/usr/share/oxygen
default: test-html test-latex test-fo test-docx conversions-from oddbyexample p4top5 conversions-to odd2odd 

p4top5:
	saxon -o:test17.tei test17.xml ../profiles/default/p4/from.xsl
	diff test17.tei expected-results/test17.tei

test-html:
	(mkdir files; cd files; saxon ../testnotes1.xml ../../xhtml2/tei.xsl splitLevel=4 useFixedDate=true STDOUT=false cssFile=../../tei.css)
	diff -r -x .svn files expected-results/testnotes
	rm -rf files
	(mkdir files; cd files; saxon ../testnotes2.xml ../../xhtml2/tei.xsl splitLevel=0 useFixedDate=true STDOUT=false cssFile=../../tei.css)
	diff -r -x .svn files expected-results/testnotes2
	rm -rf files
	(mkdir files; cd files; saxon ../testnotes2.xml ../../xhtml2/tei.xsl splitLevel=4 useFixedDate=true STDOUT=false  cssFile=../../tei.css)
	diff -r -x .svn files expected-results/testnotes3
	rm -rf files
	saxon -o:test.html  test.xml ../xhtml2/tei.xsl $(SAXONOPT)
	diff test.html expected-results/test.html
	saxon -o:test20.html  test20.xml ../xhtml2/tei.xsl $(SAXONOPT)
	diff test20.html expected-results/test20.html
	saxon -o:test3.html -versionmsg:off  test3.xml ../xhtml2/tei.xsl $(SAXONOPT)
	saxon -o:test4.html -versionmsg:off  test4.xml ../xhtml2/tei.xsl $(SAXONOPT)
	diff test4.html test3.html
	../teitohtml5 test.xml test.html5
	perl -p -i -e 's/on 20.*Z--/--/' test.html5
	perl -p -i -e 's/Date: [^<]+<br/<br/' test.html5
	diff test.html5 expected-results/test.html5
	echo differences:
	saxon -o:test5.xhtml test5.xml ../xhtml2/tei.xsl  $(SAXONOPT)
	diff test5.xhtml expected-results/test5.xhtml
	jing -c xhtml.rnc test5.xhtml

testsplit:
	saxon test6.xml ../xhtml2/tei.xsl STDOUT=false verbose=true splitLevel=1 useFixedDate=true


test-latex:
	saxon -o:test6.tex test6.xml ../slides/teilatex-slides.xsl $(SAXONOPT)
	perl -p -i -e 's/Date{[^}]*}/Date{}/' test6.tex
	diff test6.tex expected-results/test6.tex
	saxon -o:test5.tex test5.xml ../latex2/tei.xsl  $(SAXONOPT)
	perl -p -i -e 's/Date{[^}]*}/Date{}/' test5.tex
	diff test5.tex expected-results/test5.tex
	saxon -o:testnotes1.tex testnotes1.xml ../latex2/tei.xsl  $(SAXONOPT)
	perl -p -i -e 's/Date{[^}]*}/Date{}/' testnotes1.tex
	diff testnotes1.tex expected-results/testnotes1.tex

test-fo:
	saxon -o:test5.fo test5.xml ../fo2/tei.xsl  $(SAXONOPT)
	diff test5.fo expected-results/test5.fo

odd2odd:
	saxon -s:testdrama.odd -xsl:../odds2/odd2odd.xsl -o:testdrama.compiled.xml \
	localsource=`pwd`/p5subset.xml $(SAXONOPT) 
	perl -p -i -e 's+ xml:base=".*testdrama.odd"++' testdrama.compiled.xml
	diff testdrama.compiled.xml expected-results/testdrama.compiled.xml

oddbyexample:
	saxon -it:main -o:oddbyexample.odd  -xsl:../tools/oddbyexample.xsl corpus=`pwd`/bare 
	roma2 --xsl=`pwd`/.. oddbyexample.odd .
	jing -c oddbyexample.rnc bare/test2.xml
	jing oddbyexample.xsd bare/test2.xml
	xmllint --noout --dtdvalid oddbyexample.dtd bare/test2.xml
	jing -c oddbyexample.rnc bare/test.xml
	jing oddbyexample.xsd bare/test.xml
	xmllint --noout --dtdvalid oddbyexample.dtd bare/test.xml

conversions-from:
	saxon -o:test12.rdf test12.xml ../profiles/default/rdf/to.xsl
	diff test12.rdf expected-results/test12.rdf
	../teitornc  test15.odd test15.odd.rnc
	perl -p -i -e 's/generated from ODD source.*//' test15.odd.rnc
	diff test15.odd.rnc expected-results/test15.odd.rnc 
	../teitornc  test21.odd test21.odd.rnc
	perl -p -i -e 's/generated from ODD source.*//' test21.odd.rnc
	diff test21.odd.rnc expected-results/test21.odd.rnc 
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
	../teitoodt test.xml 
	unzip -t test.xml.odt > test.xml.odt.listing
	diff test.xml.odt.listing expected-results/test.xml.odt.listing
	rm test.xml.odt.listing
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
	../teitohtml --summaryDoc --profile=tei --odd test15.odd test.odd.html
	perl -p -i -e 's/on 20.*Z--/--/' test.odd.html
	perl -p -i -e 's/Date: [^<]+<br/<br/' test.odd.html
	diff test.odd.html expected-results/test.odd.html

test-docx:
	../teitodocx test.xml 
	unzip -p test.xml.docx word/document.xml | xmllint --format - > test.xml.docx.document
	diff test.xml.docx.document expected-results/test.xml.docx.document
	rm test.xml.docx.document
	../teitodocx testnotes1.xml 
	unzip -p testnotes1.xml.docx word/document.xml | xmllint --format - > testnotes1.xml.docx.document
	diff testnotes1.xml.docx.document expected-results/testnotes1.xml.docx.document
	rm testnotes1.xml.docx.document

conversions-to:
	roma2 --xsl=`pwd`/.. --nodtd --noxsd test.odd .
	../odttotei test7.odt test7.xml
	jing test.rng test7.xml
	diff test7.xml expected-results/test7.xml
	../docxtotei test11.docx test11.xml
	perl -p -i -e 's/Date:[^<]+</</' test11.xml
	jing test.rng test11.xml
	diff test11.xml expected-results/test11.xml
	../docxtotei test19.docx test19.xml
	perl -p -i -e 's/Date:[^<]+</</' test19.xml
	jing test.rng test19.xml
	diff test19.xml expected-results/test19.xml
	../docxtotei test18.docx test18.xml
	perl -p -i -e 's/Date:[^<]+</</' test18.xml
	jing test.rng test18.xml
	diff test18.xml expected-results/test18.xml
	rm test.rng test.rnc

clean:
	rm -f  test6.tex test5.tex test5.fo *.html *.xhtml *~ dummy* test.html test3.html test4.html
	rm -f  oddbyexample.* xml.xsd index*div*html
	rm -f  test.tex test.xml.odt test.xml.docx test.epub
	rm -f  test8.pdf
	rm -f  test8.log
	rm -f  test5.odt
	rm -f  test8.aux
	rm -f  test7.xml test11.xml test18.xml test19.xml
	rm -f  test8.tex
	rm -f  test7.zip
	rm -f  test8.doc
	rm -f  test8.out
	rm -f  test12.rdf
	rm -f  test9.docx test9.odt
	rm -f  test*listing*
	rm -f  test*epub test*text
	rm -f  test.aux test.log test.out test.pdf 
	rm -rf temp-dir-for-ant Pictures OEBPS META-INF mimetype

