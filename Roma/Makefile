FILES=ChangeLog g index.html Makefile notam parser progressbar ressource roma roma.css roma.js startroma.php teilogo.jpg VERSION xquery 

release: 
	rm -rf dist
	mkdir -p dist/tei-roma-`cat VERSION`
	tar --exclude=CVS -c -f - $(FILES) | (cd dist/tei-roma-`cat VERSION`; tar xf -)
	(cd dist; zip -r tei-roma-`cat ../VERSION`.zip tei-roma-`cat ../VERSION`)


