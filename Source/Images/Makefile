default:
	latex graphs.ltx
	for i in 1 2 3 4 5 6 7 8 9 10 11 12 13 14; do \
	dvips -E -j0 -p $$i -n 1 graphs -o graph$$i.eps; \
	gs -dCompatibilityLevel=1.4 -dSAFER \
	-q -dNOPAUSE -dBATCH -sDEVICE=pdfwrite -dEPSCrop \
	"-sOutputFile=graph$$i.pdf" \
	-c .setpdfwrite -f graph$$i.eps;\
	gs -dSAFER \
	-q -dNOPAUSE -dBATCH -sDEVICE=jpeg -r200 -dEPSCrop \
	"-sOutputFile=graph$$i.jpg" \
	-f graph$$i.eps;\
	done

clean:
	-rm *.eps *.log *.aux graphs.dvi graphs.ps graphs.pdf



