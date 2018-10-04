html:
	xmllint --xinclude oddmanual.xml | xsltproc teidoc-html.xsl - | perl include.pl > oddmanual.html 

pdf:
	xmllint --xinclude oddmanual.xml | xsltproc -o oddmanual.tex teidoc-latex.xsl -
	pdflatex oddmanual
	pdflatex oddmanual

schema: 
	roma --xsl=/usr/share/xml/tei/stylesheet --localsource=/home/rahtz/TEI/Sourceforge/trunk/P5/Source/Guidelines/en/guidelines-en.xml teidoc.odd . 

validate:
	jing teidoc.rng oddmanual.xml
	rnv teidoc.rnc oddmanual.xml

clean:
	-rm *~ */*~
	-rm *.tex *.aux *.log oddmanual.pdf *.out *.toc oddmanual.html teidoc.compiled.odd
	-rm teidoc.rng teidoc.rnc teidoc.dtd teidoc.xsd
