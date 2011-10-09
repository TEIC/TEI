default: build compile run

build:
	saxon -o:rdf.expanded -s:rdf.odd -xsl:/usr/share/xml/tei/stylesheet/odds2/odd2odd.xsl

compile:
	saxon rdf.expanded  make-acdc.xsl > tei2rdf.xsl 

run:
	saxon diary.xml tei2rdf.xsl > diary.rdf
	saxon test.xml tei2rdf.xsl > test.rdf
	saxon testota.xml tei2rdf.xsl > testota.rdf

talk:
	saxon talk.xml ../../Talks/2010-11-10_TEIMM_ODD/slides.xsl latexLogo=logo > talk.tex
	xelatex talk
	xelatex talk

clean:
	-rm rdf.expanded *.rdf