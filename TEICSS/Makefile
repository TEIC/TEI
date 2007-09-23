PREFIX=/usr
.PHONY: release

default:
	@echo
	@echo TEI CSS stylesheets
	@echo - install target puts files directly into ${PREFIX} 
	@echo - dist target  makes a release subdirectory of runtime files
	@echo There is no default action
	@echo

changelog:
	(LastDate=`head -1 ChangeLog | awk '{print $$1}'`; \
	svn log -v -r 'HEAD:{'$$LastDate'}' | perl ../gnuify-changelog.pl | grep -v "^;" > newchanges)
	mv ChangeLog oldchanges
	cat newchanges oldchanges > ChangeLog
	rm newchanges oldchanges

dist: clean release
	(cd release; 	\
	ln -s tei-css tei-css-`cat ../VERSION` ; \
	zip -r tei-css-`cat ../VERSION`.zip tei-css-`cat ../VERSION` )

release: 
	-mkdir -p release/tei-css/css
	cp tei.css release/tei-css/css/tei.css

clean:

install: release
	mkdir -p ${PREFIX}/share/xml/css
	(cd release/tei-css/css; tar cf - .) | \
	(cd ${PREFIX}/share/xml/css; tar xf -)

