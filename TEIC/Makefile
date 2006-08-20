PREFIX=

install:
	mkdir -p $(PREFIX)/usr/share/xml/tei/stylesheet/teic
	mkdir -p $(PREFIX)/usr/share/xml/teip4/stylesheet/teic
	cp teic.css $(PREFIX)/usr/share/xml/tei/stylesheet
	cp teic.css $(PREFIX)/usr/share/xml/teip4/stylesheet
	cp p5/*.xsl $(PREFIX)/usr/share/xml/tei/stylesheet/teic
	cp p4/*.xsl $(PREFIX)/usr/share/xml/teip4/stylesheet/teic

changelog:
	(LastDate=`head -1 ChangeLog | awk '{print $$1}'`; \
	svn log -v -r 'HEAD:{'$$LastDate'}' | perl ../gnuify-changelog.pl | grep -v "^;" > newchanges)
	mv ChangeLog oldchanges
	cat newchanges oldchanges > ChangeLog
	rm newchanges oldchanges
