VERSION=3.1

all: p4 p5 zip


p4:
	-mkdir -p web/P4/fo
	-mkdir -p web/P4/html
	-mkdir -p web/P4/common
	-mkdir -p web/P4/latex
	-mkdir -p web/P4/slides
	for i in fo/*.xsl html/*xsl common/*xsl latex/*xsl slides/*xsl; do \
	perl toP4.pl --date="`date`" --version=$(VERSION) < $$i > web/P4/$$i; \
	done

p5:
	-mkdir -p web/P5/fo
	-mkdir -p web/P5/slides
	-mkdir -p web/P5/html
	-mkdir -p web/P5/common
	-mkdir -p web/P5/latex
	for i in fo/*.xsl html/*xsl common/*xsl latex/*xsl slides/*xsl; do \
	perl toP5.pl --date="`date`" --version=$(VERSION) < $$i > web/P5/$$i; \
	done

zip:
	chmod -R u+w web
	-cp ChangeLog teixsl.* web
	(cd web/P4; zip -r teixsl-fo   fo common ../ChangeLog    ../teixsl.html ../teixsl.xml)
	(cd web/P4; zip -r teixsl-html html common ../ChangeLog ../teixsl.html ../teixsl.xml)
	(cd web/P4; zip -r teixsl-latex latex common ../ChangeLog ../teixsl.html ../teixsl.xml)
	(cd web/P5; zip -r teixsl-fo   fo common ../ChangeLog    ../teixsl.html ../teixsl.xml)
	(cd web/P5; zip -r teixsl-html html common ../ChangeLog ../teixsl.html ../teixsl.xml)
	(cd web/P5; zip -r teixsl-latex latex common ../ChangeLog ../teixsl.html ../teixsl.xml)

install:
	(cd web;  tar cf - .) | (cd ../web/Stylesheets; tar xf - )

param:
	saxon param.xml param.xsl | grep -v masterFile > teihtml-param.xsl

stylebear:
	saxon param.xml paramform.xsl > stylebear

