all: teiu5 teimath exercise darkness test

teiu5:
	xsltproc teiu5-print.xsl teiu5.xml   > teiu5.fo 
	TEXINPUTS=..: pdfxmltex teiu5.fo
	TEXINPUTS=..: pdfxmltex teiu5.fo

teimath:
	xsltproc teimath-print.xsl teimath.xml   > teimath.fo 
	TEXINPUTS=..: pdfxmltex teimath.fo
	TEXINPUTS=..: pdfxmltex teimath.fo

exercise:
	xsltproc exercise-print.xsl exercise.xml    > exercise.fo 
	TEXINPUTS=..: pdfxmltex exercise.fo
	TEXINPUTS=..: pdfxmltex exercise.fo


darkness:
	xsltproc darkness.xsl darkness.xml    > darkness.fo 
	TEXINPUTS=..: pdfxmltex darkness.fo

test:
	TEXINPUTS=..: pdfxmltex test1.fot
	TEXINPUTS=..: pdfxmltex test2.fot


clean:
	-rm *.dvi *.aux *.log *.out

realclean: clean
	-rm *.html *.pdf

