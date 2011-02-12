PREFIX=
LOCATION=/usr

FILES=ChangeLog \
	g \
	index.html \
	Makefile \
	notam \
	parser \
	progressbar \
	resource \
	roma \
	roma.sh \
	roma2.sh \
	roma.1 \
	roma.css \
	TEI-glow.png \
	roma.js \
	startroma.php \
	teilogo.jpg \
	VERSION 

.PHONY: release

default:
	@echo
	@echo TEI Roma
	@echo - install target puts files directly into ${PREFIX}${LOCATION}
	@echo - dist target  makes a release subdirectory of runtime files
	@echo There is no default action
	@echo

install: release-stamp
	mkdir -p ${PREFIX}${LOCATION}/share/tei-roma
	(cd release; tar cf - . ) | (cd ${PREFIX}${LOCATION}/share; tar xf - )
	mkdir -p ${PREFIX}${LOCATION}/bin
	cp -p roma.sh ${PREFIX}${LOCATION}/bin/roma
	chmod 755 ${PREFIX}${LOCATION}/bin/roma
	cp -p roma2.sh ${PREFIX}${LOCATION}/bin/roma2
	chmod 755 ${PREFIX}${LOCATION}/bin/roma2
	mkdir -p $(PREFIX)/etc/tei-roma
	cp $(PREFIX)/${LOCATION}/share/tei-roma/roma/config-dist.php $(PREFIX)/etc/tei-roma/config.php
        perl -p -i -e 's+http://www.tei-c.org/release+/usr/share+' $(PREFIX)/etc/tei-roma/config.php
	(cd $(PREFIX)/${LOCATION}/share/tei-roma/roma; rm config.php; ln -s /etc/tei-roma/config.php config.php)


dist:  release-stamp
	(cd release; 	\
	ln -s tei-roma tei-roma-`cat ../VERSION` ; \
	zip -r tei-roma-`cat ../VERSION`.zip tei-roma-`cat ../VERSION` )

deb:	
	debuild -i.svn -I.svn

release: clean release-stamp

release-stamp:
	rm -rf release/tei-roma
	mkdir -p release/tei-roma
	V=`cat VERSION` D=`head -1 ChangeLog | awk '{print $$1}'`;export D V; \
	echo version $$V of date $$D; \
	perl -p -i -e "s+.*define.*roma_date.*+define (\'roma_date\',\'$$D\');+" roma/config.php; \
	perl -p -i -e "s+.*define.*roma_version.*+define (\'roma_version\',\'$$V\');+" roma/config.php; \
	perl -p -i -e "s+.*define.*roma_date.*+define (\'roma_date\',\'$$D\');+" roma/config-dist.php; \
	perl -p -i -e "s+.*define.*roma_version.*+define (\'roma_version\',\'$$V\');+" roma/config-dist.php; \
	tar --exclude=.svn -c  -f - $(FILES) | (cd release/tei-roma; tar xf -); \
	perl -p -i -e "s/{roma_version}/$$V/;s/{roma_date}/$$D/" release/tei-roma/roma/templates/main.tem
	(cd roma; ../roma2.sh --nodtd --noxsd oddschema.odd .)
	touch release-stamp

clean:
	rm -rf release
	rm -f release-stamp roma/oddschema.rnc
	-`which debclean` && debclean
	rm -f tei-roma_*

log:
	(LastDate=`head -1 ChangeLog | awk '{print $$1}'`; \
	svn log -v -r 'HEAD:{'$$LastDate'}' | perl ../gnuify-changelog.pl | grep -v "^;" > newchanges)
	mv ChangeLog oldchanges
	cat newchanges oldchanges > ChangeLog
	rm newchanges oldchanges
