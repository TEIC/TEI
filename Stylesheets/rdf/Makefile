default: build compile 

build:
	saxon -o:rdf.expanded -s:rdf.odd -xsl:/usr/share/xml/tei/stylesheet/odds/odd2odd.xsl

compile:
	saxon rdf.expanded  make-acdc.xsl > tei2rdf.xsl 

clean:
	-rm rdf.expanded *.rdf
