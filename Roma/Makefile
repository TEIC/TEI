PREFIX=/usr
FILES=ChangeLog \
	g \
	index.html \
	Makefile \
	notam \
	parser \
	progressbar \
	release \
	resource \
	roma \
	roma.sh \
	roma.1 \
	roma.css \
	roma.js \
	startroma.php \
	teilogo.jpg \
	VERSION 

.PHONY: release

default:
	@echo
	@echo TEI Roma
	@echo - install target puts files directly into ${PREFIX} 
	@echo - dist target  makes a release subdirectory of runtime files
	@echo There is no default action
	@echo

install: release
	mkdir -p ${PREFIX}/share/tei-roma
	(cd release; tar cf - . ) | (cd ${PREFIX}/share; tar xf - )
	mkdir -p ${PREFIX}/bin
	cp -p roma.sh ${PREFIX}/bin/roma
	chmod 755 ${PREFIX}/bin/roma

dist:  release
	(cd release; 	\
	ln -s tei-roma tei-roma-`cat ../VERSION` ; \
	zip -r tei-roma-`cat ../VERSION`.zip tei-roma-`cat ../VERSION` )

release: clean
	mkdir -p release/tei-roma
	V=`cat VERSION` D=`head -1 ChangeLog | awk '{print $$1}'`;export D V; \
	perl -p -i -e "s+(define \(.roma_date.,).*(\'.*)+\1\'$$D\2+;s+(define \(.roma_version.,).*(\'.*)+\1\'$$V\2+" \
	roma/config.php; perl -p -i -e "s/{roma_version}/$$V/;s/{roma_date}/$$D/" roma/templates/main.tem
	tar --exclude=.svn -c -f - $(FILES) | (cd release/tei-roma; tar xf -)

clean:
	-rm -rf release
	-find . -name "*~" | xargs rm -f
	-find . -name semantic.cache | xargs rm -f

changelog:
	(LastDate=`head -1 ChangeLog | awk '{print $$1}'`; \
	svn log -v -r 'HEAD:{'$$LastDate'}' | perl ../gnuify-changelog.pl | grep -v "^;" > newchanges)
	mv ChangeLog oldchanges
	cat newchanges oldchanges > ChangeLog
	rm newchanges oldchanges


