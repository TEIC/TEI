all: index.html


index.html: 
	xsltproc ../../stylesheet/teic/teihtml-teic.xsl index.xml > index.html


clean:
	-rm *.aux *.log *.dvi *.out *.fo *.toc *.html
	-(cd test; rm *.aux *.log *.dvi *.pdf *.out *.fo *.toc *.html)

distrib:
	export X=`date '+%Y%m%d'`; ( cd ..; \
	zip -r passivetex/passivetex-$$X \
	passivetex/test \
	passivetex/index.xml \
	passivetex/LICENSE \
	passivetex/index.html \
	passivetex/README.passivetex \
	passivetex/dummyels.sty \
	passivetex/fotex.sty \
	passivetex/fotex.xmt \
	passivetex/nomult*sty \
	passivetex/mlnames.sty \
	passivetex/ucharacters.sty \
	passivetex/unicode.sty ); \
	p4 add passivetex-$$X.zip ; \
	p4 edit passivetex.zip ; \
	rm -f passivetex.zip ; \
	ln -s passivetex-$$X.zip passivetex.zip
