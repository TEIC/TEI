nERSION=`cat VERSION`
SAXONOPT=useFixedDate=true
OXY=/usr/share/oxygen
SCRIPTS=teitobibtex \
	teitodocbook \
	teitodocx \
	teitoepub \
	teitoepub3 \
	teitofo \
	teitohtml \
	teitohtml5 \
	teitojson \
	teitolatex \
	teitoodt \
	teitopdf \
	teitordf \
	teitoslides \
	teitotxt 

SCHEMASCRIPTS=teitornc \
	teitorelaxng \
	teitoxsd \
	teitodtd 

default: schema test-html test-latex test-fo test-docx test-other test-epub test-rdf test-text test-odt p4top5 conversions-to  oddity pureant scripts

p4top5:
	saxon -o:test17.tei test17.xml ../profiles/default/p4/from.xsl
	diff test17.tei expected-results/test17.tei
	saxon A54031.tcp ../tcp/tcp2tei.xsl | xmllint --format - > A54031.xml
	diff A54031.xml expected-results/A54031.xml

schema:
	../teitorelaxng --odd test.odd test.rng

test-html:
	saxon  test.xml ../xhtml2/tei.xsl cssFile=../tei.css $(SAXONOPT) | xmllint --format - > test.html
	diff test.html expected-results/test.html
	saxon test.html checklinks.xsl
	saxon test.xml ../xhtml2/tei.xsl cssFile=../tei.css pageLayout=CSS  $(SAXONOPT)  | xmllint --format - > test-css.html
	diff test-css.html expected-results/test-css.html
	../teitohtml5 test.xml test.html5
	xmllint --encode utf-8 --format test.html5 > test.$$
	mv test.$$ test.html5
	perl -p -i -e 's/on 20.*Z--/--/' test.html5
	perl -p -i -e 's/Date: [^<]+<br/<br/' test.html5
	diff test.html5 expected-results/test.html5
	saxon -o:test20.html  test20.xml ../xhtml2/tei.xsl $(SAXONOPT)
	diff test20.html expected-results/test20.html
	saxon -o:test3.html -versionmsg:off  test3.xml ../xhtml2/tei.xsl $(SAXONOPT)
	saxon -o:test4.html -versionmsg:off  test4.xml ../xhtml2/tei.xsl $(SAXONOPT)
	diff test4.html test3.html
	saxon test5.xml ../xhtml2/tei.xsl  autoBlockQuote=true $(SAXONOPT)  | xmllint --format - > test5.xhtml
	diff test5.xhtml expected-results/test5.xhtml
	jing -c xhtml.rnc test5.xhtml
	saxon -o:test6.html test6.xml ../xhtml2/tei.xsl  autoBlockQuote=true $(SAXONOPT)
	diff test6.html expected-results/test6.html
	jing -c xhtml.rnc test6.html
	saxon -o:test22.html test22.xml ../xhtml2/tei.xsl $(SAXONOPT)
	diff test22.html expected-results/test22.html
	saxon -o:test23.html test23.xml ../xhtml2/tei.xsl $(SAXONOPT)
	diff test23.html expected-results/test23.html
	(mkdir files; cd files; saxon ../testnotes1.xml ../../xhtml2/tei.xsl splitLevel=4 useFixedDate=true STDOUT=false cssFile=../../tei.css)
	xmllint --format files/index.html > x ; mv x files/index.html
	xmllint --format files/one.html > x ; mv x files/one.html
	xmllint --format files/two.html > x ; mv x files/two.html
	xmllint --format files/three.html > x ; mv x files/three.html
	diff -r -x .svn files expected-results/testnotes
	rm -rf files
	(mkdir files; cd files; saxon ../testnotes2.xml ../../xhtml2/tei.xsl splitLevel=0 useFixedDate=true STDOUT=false cssFile=../../tei.css)
	xmllint --format files/index.html > x ; mv x files/index.html
	xmllint --format files/one.html > x ; mv x files/one.html
	xmllint --format files/three.html > x ; mv x files/three.html
	diff -r -x .svn files expected-results/testnotes2
	rm -rf files
	(mkdir files; cd files; saxon ../testnotes2.xml ../../xhtml2/tei.xsl splitLevel=4 useFixedDate=true STDOUT=false  cssFile=../../tei.css)
	xmllint --format files/index.html > x ; mv x files/index.html
	xmllint --format files/one.html > x ; mv x files/one.html
	xmllint --format files/two.html > x ; mv x files/two.html
	xmllint --format files/three.html > x ; mv x files/three.html
	diff -r -x .svn files expected-results/testnotes3
	saxon -o:test24.html test24.xml ../xhtml2/tei.xsl $(SAXONOPT)
	diff test24.html expected-results/test24.html
	saxon -o:test25.html test25.xml ../xhtml2/tei.xsl $(SAXONOPT)
	diff test25.html expected-results/test25.html
	saxon test27.xml ../xhtml2/tei.xsl $(SAXONOPT) cssFile=../tei.css cssSecondaryFile=../msdescription.css  | xmllint --format - > test27.html
	diff test27.html expected-results/test27.html
	rm -rf files
	saxon test28.xml ../xhtml2/tei.xsl splitLevel=0  STDOUT=false $(SAXONOPT) 
	diff index.html expected-results/test28.html

testsplit:
	saxon test6.xml ../xhtml2/tei.xsl STDOUT=false verbose=true splitLevel=1 useFixedDate=true


test-latex:
	saxon -o:test.tex test.xml ../latex2/tei.xsl  $(SAXONOPT)
	diff test.tex expected-results/test.tex
	saxon -o:testnotes1.tex testnotes1.xml ../latex2/tei.xsl  $(SAXONOPT)
	diff testnotes1.tex expected-results/testnotes1.tex
	saxon -o:test5.tex test5.xml ../latex2/tei.xsl  $(SAXONOPT)
	diff test5.tex expected-results/test5.tex
	saxon -o:test6.tex test6.xml ../slides/teilatex-slides.xsl $(SAXONOPT)
	diff test6.tex expected-results/test6.tex
	saxon -o:test23.tex test23.xml ../latex2/tei.xsl $(SAXONOPT)
	diff test23.tex expected-results/test23.tex
	saxon -o:test24.tex test24.xml ../latex2/tei.xsl $(SAXONOPT)
	diff test24.tex expected-results/test24.tex
	saxon -o:test25.tex test25.xml ../latex2/tei.xsl $(SAXONOPT)
	diff test25.tex expected-results/test25.tex
	saxon -o:test27.tex test27.xml ../latex2/tei.xsl $(SAXONOPT)
	diff test27.tex expected-results/test27.tex

test-fo:
	saxon -o:test.fo test.xml ../fo2/tei.xsl  $(SAXONOPT)
	jing fo.xsd test.fo
	diff test.fo expected-results/test.fo
	saxon  test5.xml ../fo2/tei.xsl  $(SAXONOPT)  | xmllint --format - > test5.fo
	jing fo.xsd test5.fo
	diff test5.fo expected-results/test5.fo
	saxon testnotes1.xml ../fo2/tei.xsl  $(SAXONOPT)   | xmllint --format - > testnotes1.fo
	jing fo.xsd testnotes1.fo
	diff testnotes1.fo expected-results/testnotes1.fo 
	saxon -o:test24.fo test24.xml ../fo2/tei.xsl $(SAXONOPT)
	jing fo.xsd test24.fo 
	diff test24.fo expected-results/test24.fo
	saxon -o:test25.fo test25.xml ../fo2/tei.xsl $(SAXONOPT)
	diff test25.fo expected-results/test25.fo
	saxon -o:test27.fo test27.xml ../fo2/tei.xsl $(SAXONOPT)
	diff test27.fo expected-results/test27.fo

test-epub:
	../teitoepub3  test.xml test.epub
	java -jar epubcheck3.jar test.epub
	../teitoepub  test.xml test.epub
	unzip -t test.epub > test.epub.listing
	diff test.epub.listing expected-results/test.epub.listing
	rm test.epub.listing
	../teitoepub  --fileperpage test.xml test-pages.epub
	unzip -t test-pages.epub > test.epub.listing2
	diff test.epub.listing2 expected-results/test.epub.listing2
	rm test.epub.listing2

test-other:
	../teitornc  test21.odd test21.odd.rnc
	perl -p -i -e 's/generated from ODD source.*//' test21.odd.rnc
	diff test21.odd.rnc expected-results/test21.odd.rnc 
	../teitornc  test15.odd test15.odd.rnc
	perl -p -i -e 's/generated from ODD source.*//' test15.odd.rnc
	diff test15.odd.rnc expected-results/test15.odd.rnc 

test-rdf:
	saxon -o:test12.rdf test12.xml ../profiles/default/rdf/to.xsl
	diff test12.rdf expected-results/test12.rdf

test-text:
	../teitotxt test14.xml test.text
	diff test.text expected-results/test.text

test-odt:
	../teitoodt test.xml 
	unzip -t test.xml.odt > test.xml.odt.listing
	unzip -o -q test.xml.odt content.xml
	xmllint --format content.xml > content.xml.odt
	diff test.xml.odt.listing expected-results/test.xml.odt.listing
	rm test.xml.odt.listing
	rm content.xml
	diff content.xml.odt expected-results/content.xml.odt

test-docx:
	../docxtotei test-indexes.docx temp.xml
	xmllint --format temp.xml > test-indexes.xml  && rm temp.xml
	perl -p -i -e 's/Date:[^<]+</</' test-indexes.xml
	jing test.rng test-indexes.xml
	diff test-indexes.xml expected-results/test-indexes.xml
	../docxtotei test11.docx temp.xml
	xmllint --format temp.xml > test11.xml  && rm temp.xml
	perl -p -i -e 's/Date:[^<]+</</' test11.xml
	jing test.rng test11.xml
	diff test11.xml expected-results/test11.xml
	../docxtotei test19.docx temp.xml
	xmllint --format temp.xml > test19.xml  && rm temp.xml
	perl -p -i -e 's/Date:[^<]+</</' test19.xml
	jing test.rng test19.xml
	diff test19.xml expected-results/test19.xml
	../docxtotei test18.docx temp.xml
	xmllint --format temp.xml > test18.xml  && rm temp.xml
	perl -p -i -e 's/Date:[^<]+</</' test18.xml
	jing test.rng test18.xml
	diff test18.xml expected-results/test18.xml
	../teitodocx test.xml 
	unzip -p test.xml.docx word/document.xml | xmllint --format - > test.xml.docx.document
	unzip -p test.xml.docx docProps/core.xml | xmllint --format - | sed 's/>20.*Z</>20Z</' > test.xml.docx.core
	diff test.xml.docx.document expected-results/test.xml.docx.document
	diff test.xml.docx.core expected-results/test.xml.docx.core
	rm test.xml.docx.document
	../teitodocx test6.xml 
	unzip -p test6.xml.docx word/document.xml | xmllint --format - > test6.xml.docx.document
	diff test6.xml.docx.document expected-results/test6.xml.docx.document
	rm test6.xml.docx.document
	../teitodocx testnotes1.xml 
	unzip -p testnotes1.xml.docx word/document.xml | xmllint --format - > testnotes1.xml.docx.document
	diff testnotes1.xml.docx.document expected-results/testnotes1.xml.docx.document
	rm testnotes1.xml.docx.document

conversions-to: 
	../odttotei test7.odt test7.xml
	jing test.rng test7.xml
	diff test7.xml expected-results/test7.xml

oddity:
	../teitohtml --debug --summaryDoc --profile=tei --odd test15.odd test.odd.html
	xmllint --format test.odd.html > x ; mv x test.odd.html
	perl -p -i -e 's/on 20.*Z--/--/' test.odd.html
	perl -p -i -e 's/Date: [^<]+<br/<br/' test.odd.html
	diff test.odd.html expected-results/test.odd.html
	saxon -it:main -o:oddbyexample.odd  -xsl:../tools/oddbyexample.xsl corpus=`pwd`/bare 
	perl -p -i -e 's+<p>generated by oddbyexample.*</p>+<p></p>+' oddbyexample.odd
	perl -p -i -e 's+<date>.*</date>+<date></date>+' oddbyexample.odd
	diff oddbyexample.odd expected-results/oddbyexample.odd
	roma2 --xsl=`pwd`/.. oddbyexample.odd .
	jing -c oddbyexample.rnc bare/test2.xml
	jing oddbyexample.xsd bare/test2.xml
	xmllint --noout --dtdvalid oddbyexample.dtd bare/test2.xml
	jing -c oddbyexample.rnc bare/test.xml
	jing oddbyexample.xsd bare/test.xml
	xmllint --noout --dtdvalid oddbyexample.dtd bare/test.xml
	saxon -s:testdrama.odd -xsl:../odds2/odd2odd.xsl -o:testdrama.compiled.xml \
	localsource=`pwd`/p5subset.xml $(SAXONOPT) 
	perl -p -i -e 's+ xml:base=".*testdrama.odd"++' testdrama.compiled.xml
	diff testdrama.compiled.xml expected-results/testdrama.compiled.xml
	saxon test.odd ../odds2/odd2odd.xsl > test.odd2odd
	saxon test.odd2odd ../odds2/odd2dtd.xsl > test.odd2dtd
	saxon test.odd2odd ../odds2/odd2html.xsl > test.odd2html
	saxon test.odd2odd ../odds2/odd2json.xsl > test.odd2json
	saxon test.odd2odd ../odds2/odd2lite.xsl > test.odd2lite
	saxon test.odd2odd ../odds2/odd2relax.xsl > test.odd2relax
	saxon test.odd2odd ../odds2/odd2xslstripspace.xsl > test.odd2xslstripspace
	rm test.odd2odd
	rm test.odd2dtd
	rm test.odd2html
	rm test.odd2json
	rm test.odd2lite
	rm test.odd2relax
	rm test.odd2xslstripspace
foo:
	../teitoxsd test.odd test.xsd
	../teitodtd test.odd test.dtd
	../teitornc test.odd test.rnc

scripts:
	for i in $(SCRIPTS); do ../$$i maria.xml $$i.result && rm $$i.result; done
	../teitoodd test16.odd temp.odd
	for i in $(SCHEMASCRIPTS) ; do ../$$i temp.odd $$i.result && rm $$i.result; done
	rm temp.odd

pureant:
	java -Xmx256m \
	-classpath  "$(OXY)/tools/ant/lib/ant-launcher.jar" \
	-Dant.home=$(OXY)/tools/ant \
	org.apache.tools.ant.launch.Launcher \
	-f ../docx/build-to.xml \
	-lib $(OXY)/lib/saxon9ee.jar \
	-DoutputFile=`pwd`/test7.docx \
	-DinputFile=`pwd`/test7.xml -Dprofile=default 

profile:
	saxon -o:/dev/null	-TP test.xml ../xhtml2/tei.xsl >& profile1.html
	saxon -o:/dev/null	-TP test.xml ../latex2/tei.xsl >& profile2.html
	saxon test.odd ../odds2/odd2odd.xsl > test.odd2odd
	saxon -o:/dev/null -TP test.odd2odd ../odds2/odd2relax.xsl >& profile3.html

clean:
	rm -f  test*.odd.rnc test*epub test*pdf test*rdf test*text test*.xml.docx test*.xml.odt test*.tex test*.fo *.html *.xhtml *~ dummy* test.html test3.html test4.html *aux *out *log *nav *ent *snm *toc
	rm -f test.rng test.rnc
	rm -f  oddbyexample.* index*div*html
	rm -f  testdrama.compiled.xml
	rm -f  test5.odt
	rm -f  test7.xml test11.xml test18.xml test19.xml test-index.xml
	rm -f  test7.zip
	rm -f  test9.docx test9.odt
	rm -f  test*listing*
	rm -rf temp-dir-for-ant Pictures OEBPS META-INF mimetype
	rm -f test17.tei
	rm -f test.xml.docx.core
	rm -f test7.docx
	rm -f test.html5
