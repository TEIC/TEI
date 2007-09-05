FILES=poems

default:
	latex graphs.ltx
	for i in 1 2 3 4 5 6 7 8 9 10 11 12 13 14; do \
	dvips -E -j0 -p $$i -n 1 graphs -o graph$$i.eps; \
	gs -dCompatibilityLevel=1.4 -dSAFER \
	-q -dNOPAUSE -dBATCH -sDEVICE=pdfwrite -dEPSCrop \
	"-sOutputFile=graph$$i.pdf" \
	-c .setpdfwrite -f graph$$i.eps;\
	gs -dSAFER \
	-dTextAlphaBits=4 \
	-dGraphicsAlphaBits=4 \
	-q -dNOPAUSE -dBATCH -sDEVICE=jpeg -r150 -dEPSCrop \
	"-sOutputFile=graph$$i.jpg" \
	-f graph$$i.eps;\
	done

trees:
	for i in ${FILES}; do \
	xsltproc -o $$i.tex xml2tree.xsl $$i.xml; \
	latex $$i; \
	dvips -E -j0 $$i.dvi -o $$i.eps; \
	gs -dCompatibilityLevel=1.4 -dSAFER \
	-q -dNOPAUSE -dBATCH -sDEVICE=pdfwrite -dEPSCrop \
	"-sOutputFile=$$i.pdf" \
	-c .setpdfwrite -f $$i.eps;\
	gs -dSAFER \
	-dTextAlphaBits=4 \
	-dGraphicsAlphaBits=4 \
	-q -dNOPAUSE -dBATCH -sDEVICE=jpeg -r150 -dEPSCrop \
	"-sOutputFile=$$i.jpg" \
	-f $$i.eps;\
	done
clean:
	-rm *.eps *.log *.aux graphs.dvi graphs.ps graphs.pdf



