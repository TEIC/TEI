S=/usr/share/xml/tei/stylesheet

default: dtds schemas html

convert: dtds schemas

dtds: 
	-mkdir DTD
	-rm DTD/*
	# generate the DTDs
	xmllint --noent   Source-driver.xml | xsltproc ${S}/base/p5/odds/odd2dtd.xsl -
	for i in DTD/* ; do perl -i Tools/cleandtd.pl $$i; done	
	# I cannot be bothered to see why these don't work,
	# just hack them by hand.
	perl -p -i -e 's/,\|/\|/' DTD/core.dtd
	perl -p -i -e 's/\| %mix.seg;/%mix.seg;/' DTD/core.dtd
	perl -p -i -e 's/\(\%schemapattern;\)\?/%schemapattern;/' DTD/tagdocs.dtd
	perl -p -i -e 's/\)\*\(/\)\*,\(/' DTD/textstructure.dtd
	(cd DTD; ln -s tei.dtd tei2.dtd)

schemas:
	-mkdir Schema
	-rm Schema/*
	# generate the relaxNG schemas
	xmllint --noent   Source-driver.xml | xsltproc -stringparam verbose true ${S}/base/p5/odds/odd2relax.xsl -
	# do the indentation better 
	for i in Schema/* ; \
	do echo clean $$i; \
	 xmllint --format $$i \
	   | sed -e '/^ *$$/d' \
	   > x.tmp ; \
	   mv x.tmp $$i;\
	 done
	 (cd Schema; for i in *rng; do trang $$i `basename $$i .rng`.rnc;done)
	xmllint --noent   Source-driver.xml | xsltproc extract-sch.xsl - > p5.sch

html: 
	-rm -rf Guidelines
	-mkdir Guidelines 
	xmllint --noent    Source-driver.xml | xsltproc \
	-o Guidelines/index.html \
	--stringparam verbose true \
	--stringparam displayMode rnc \
	--stringparam ODDROOT `pwd`/ \
	--stringparam outputDir . \
	${S}/base/p5/odds/odd2html.xsl - 
	(cd Guidelines; for i in *.html; do perl -i ../Tools/cleanrnc.pl $$i;done)
	-cp *.gif *.css Guidelines

xml:
	xmllint --noent   Source-driver.xml | perl Tools/cleanrnc.pl | \
	xsltproc  -o Guidelines.xml \
	--stringparam displayMode rnc  \
	${S}/base/p5/odds/teixml-odds.xsl -

pdf: xml
	xsltproc ${S}/teic/teilatex-teic-P5.xsl Guidelines.xml \
	> Guidelines.tex
	TEXINPUTS=/TEI/Talks/texconfig: pdflatex Guidelines
	TEXINPUTS=/TEI/Talks/texconfig: pdflatex Guidelines

validate: oddschema exampleschema
	echo JING version: `jing `
#	We have discovered that jing reports 3-letter language codes
#	from ISO 639-2 as illegal values of xml:lang= even though
#	they are perfectly valid per RFC 3066. We have submitted a
#	bug report, and for now just throw away those error messages
#	with grep -v. Note that we discard *all* such messages, even
#	though fewer than 500 of the 17,576 possible combinations
#	(i.e. < 3%) are valid codes.
	-jing -t p5odds.rng Source-driver.xml \
	 | grep -v ": error: Illegal xml:lang value \"[A-Za-z][A-Za-z][A-Za-z]\"\.$$"
	echo xx/rnv
	-xmllint --noent  Source-driver.xml > Source.xml
	-rnv -v p5odds.rnc Source.xml && rm Source.xml
	echo NRL
#	In addition to erroneously reporting xml:lang= 3-letter
#	values, jing seems to report an "unfinished element" every
#	time a required child element from another namespace occurs
#	in the instance. In our case, this happens every time there
#	is an <egXML> child of <exemplum>. Since the error message is
#	non-specific (doesn't tell us that <exemplum> is the
#	unfinished element or that one of <eg> or <egXML> would be
#	required to make it finished) we end up throwing out all such
#	messages via the grep -v command so we're not annoyed by the
#	over 800 that are not really problems.
	-jing p5nrl.xml Source-driver.xml \
	 | grep -v ": error: Illegal xml:lang value \"[A-Za-z][A-Za-z][A-Za-z]\"\.$$" \
	 | grep -v ': error: unfinished element$$'
	echo XSLT validator
	xsltproc validator.xsl Source-driver.xml >& tmp && sed 's/TEI...\/text...\/body...\///' tmp && rm tmp
	echo xmllint version: ` xmllint --version`
	-xmllint  --relaxng p5odds.rng --noent --noout Source-driver.xml

test:
	(cd Test; make)

split:
	(mkdir Split; cd Split; xmllint --noent   ../Source-driver.xml | xsltproc ../divsplit.xsl -)

oddschema:
	./Roma --simple --xsl=$S --schema=./Schema/ p5odds.odd .


exampleschema:
	./Roma --simple --xsl=$S --schema=./Schema/ p5odds-ex.odd . && \
	 mv p5examples.compiled.rng p5examples.rng && \
	 mv p5examples.compiled.rnc p5examples.rnc && \
	 perl -p -i -e 's/org\/ns\/1.0/org\/ns\/Examples/' p5examples.rnc && \
	 perl -p -i -e 's/org\/ns\/1.0/org\/ns\/Examples/' p5examples.rng

fascicule:
	cat fasc-head.xml `find Source -name $(CHAP).odd` fasc-tail.xml > FASC-$(CHAP).xml
	xmllint --noent    FASC-$(CHAP).xml | xsltproc \
	-o FASC-$(CHAP)-Guidelines/index.html \
	--stringparam verbose true \
	--stringparam displayMode rnc \
	--stringparam outputDir . \
	${S}/base/p5/odds/odd2html.xsl - 
	(cd FASC-$(CHAP)-Guidelines; for i in *.html; do perl -i ../Tools/cleanrnc.pl $$i;done)
	-jing p5odds.rng FASC-$(CHAP).xml 
	xsltproc -o FASC-$(CHAP)-lite.xml  \
	--stringparam displayMode rnc \
	--stringparam ODDROOT `pwd`/ \
	${S}/base/p5/odds/teixml-odds.xsl FASC-$(CHAP).xml 
	perl Tools/cleanrnc.pl FASC-$(CHAP)-lite.xml | \
	xsltproc  \
	 ${S}/teic/teilatex-teic-P5.xsl - > FASC-$(CHAP).tex
	TEXINPUTS=/TEI/Talks/texconfig: pdflatex FASC-$(CHAP) 
	TEXINPUTS=/TEI/Talks/texconfig: pdflatex FASC-$(CHAP) 

exist: split
	perl rmcol.pl /db/TEI
	perl addcol.pl /db/TEI
	perl updateexist.pl Split /db/TEI
	perl updateexist.pl teinames.xml /db/TEI
	perl updateexist.pl datatypes.xml /db/TEI

clean:
	-rm -rf Guidelines Schema DTD dtd Split RomaResults *~
	-rm Guidelines.xml core.rnc header.rnc tei.rnc \
	dictionaries.rnc  linking.rnc  textstructure.rnc \
	figures.rnc       tagdocs.rnc  \
	analysis.rnc \
	certainty.rnc \
	corpus.rnc \
	declarefs.rnc \
	drama.rnc \
	gaiji.rnc \
	iso-fs.rnc \
	msdescription.rnc \
	namesdates.rnc \
	nets.rnc \
	p5examples.rnc \
	p5odds.compiled.rnc \
	p5odds.rnc \
	spoken.rnc \
	textcrit.rnc \
	transcr.rnc \
	verse.rnc 
