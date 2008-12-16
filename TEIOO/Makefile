package:
	(cd TEIP4; jar cf ../teioop4-`cat ../VERSION`.jar *)
	(cd TEIP5; jar cf ../teioop5-`cat ../VERSION`.jar *)

u:
	xsltproc unicode.xsl ../unicode.xml > chars.xml
	xmllint --format --noent test11.xml | grep -v chars.xml > test12.xml

log:
	(LastDate=`head -1 ChangeLog | awk '{print $$1}'`; \
	svn log -v -r 'HEAD:{'$$LastDate'}' | perl ../gnuify-changelog.pl | grep -v "^;" > newchanges)
	mv ChangeLog oldchanges
	cat newchanges oldchanges > ChangeLog
	rm newchanges oldchanges

test-from:
	for i in test-from-oo/*.fodt;	do \
	  xsltproc -o test-from-oo/`basename $$i .fodt`.xml TEIP5/TEI/oo-to-teip5.xsl $$i;\
	xmllint --noout test-from-oo/`basename $$i .fodt`.xml ; \
	done

test-from-xalan:
	for i in test-from-oo/*.fodt;	do \
	  xalan -OUT test-from-oo/`basename $$i .fodt`.xml -XSL TEIP5/TEI/oo-to-teip5.xsl -IN $$i;\
	xmllint --noout test-from-oo/`basename $$i .fodt`.xml ; \
	done


clean:
	rm *.jar
