INPUTLANGUAGE=en
DRIVER=guidelines-${INPUTLANGUAGE}.xml
XSL=/usr/share/xml/tei/stylesheet/
TRANG=java -jar ../Utilities/lib/trang.jar
JING=java -jar ../Utilities/lib/jing.jar
SAXON=java -Xmx752m -jar ../Utilities/lib/saxon10he.jar -ext:on
ANT=ANT_OPTS="-Xss2m -Xmx752m -Djava.awt.headless=true" ant -lib \
  ../Utilities/lib/saxon10he.jar:../Utilities/lib/jing.jar:../Utilities/lib/xml-resolver-1.2.jar \
	-Dtrang=../Utilities/lib/trang.jar  -DdefaultSource=`pwd`/../p5subset.xml -DXSL=${XSL}

NORMALFILES= \
	testbare.dtd \
	testbasic.xsd \
	testall.xsd \
	testplus.special

EXTENDEDFILES = \
	testjustfs.xsd \
	testp4compat.xsd \
	testplace-kml.special
#testi18n.xsd \


default: normal special testmeta2010 test-frag detest

# Note on construction of this file:
#   For every target that does stuff the target starts by echoing a
#   blank line and a notification line about which target is being
#   run. The “$@” variable is used, which writes out the file name of
#   the target. If the target name contains a ‘%’ (and thus gets run
#   over multiple matching files) the message says “target file”
#   instead of just “target”. Thus the user sees the actual file being
#   worked on, and can figure out the target from that name.
# —Syd, 2021-08-20

normal: $(NORMALFILES)
special: $(EXTENDEDFILES)

checkrnv:
	@command -v rnv || echo WARNING: rnv is not installed. Some targets may fail.

testall.examples: testall.xsd
	@echo " "
	@echo "--------- work on target $@ ---------"
	$(SAXON) -o:testall-ex.odd -s:testall.odd -xsl:../Utilities/odd2exodd.xsl
	$(ANT) -f antruntest.xml -Doutputname=testall-examples -DoddFile=testall-ex.odd compileodd rng cleanup
	@echo check egXML in testall.odd
	perl -p -e "s/testlite-examples/testall-examples/" ex.nvdl > testall.nvdl
	../run-onvdl testall.nvdl testall.odd

%.dtd: %.odd
	@echo " "
	@echo "--------- work on target file $@ ---------"
	$(ANT) -f antruntest.xml -Doutputname=$* -Dtestfile=$*.xml -DoddFile=$*.odd validateodd compileodd dtd rng validaterng cleanup
	@echo Check file using DOCTYPE in instance
	xmllint --noout --valid $*.xml

%.xsd: %.odd
	@echo " "
	@echo "--------- work on target file $@ ---------"
	$(ANT) -f antruntest.xml -Doutputname=$* -Dtestfile=$*.xml -DoddFile=$*.odd validateodd compileodd rng validaterng  isoschematron validateschematron cleanup
	${TRANG}   -o disable-abstract-elements $*.rng $*.xsd
	-test -f xml.xsd && cp xml.xsd $*_xml.xsd    && perl -p -i -e "s+xml.xsd+$*_xml.xsd+;s+ns1.xsd+$*_ns1.xsd+;s+teix.xsd+$*_teix.xsd+" $*.xsd
	-test -f teix.xsd && mv teix.xsd $*_teix.xsd && perl -p -i -e "s+xml.xsd+$*_xml.xsd+;s+ns1.xsd+$*_ns1.xsd+" $*_teix.xsd
	-test -f spec.xsd && mv spec.xsd $*_spec.xsd && perl -p -i -e "s+xml.xsd+$*_xml.xsd+;s+ns1.xsd+$*_ns1.xsd+;s+teix.xsd+$*_teix.xsd+" $*_spec.xsd && perl -p -i -e "s/spec.xsd/$*_spec.xsd/" $*.xsd
	-test -f ns1.xsd && mv ns1.xsd $*_ns1.xsd    && perl -p -i -e "s+xml.xsd+$*_xml.xsd+" $*_ns1.xsd 
	@echo Validate XSD using Jing ...
	${JING} $*.xsd $*.xml

%.special: %.odd checkrnv
	@echo " "
	@echo "--------- work on target file $@ ---------"
	@echo "BUILD:  Test schema from (special) $<"
	xmllint --xinclude $< > xi_$<
	$(ANT) -f antruntest.xml -Dtestfile=$*.xml -DoddFile=xi_$*.odd -Doutputname=$* validateodd compileodd rng validaterng cleanup
	${TRANG} $*.rng $*.rnc
	xmllint --noent --dropdtd  $*.xml | rnv $*.rnc
	rm xi_$*.odd

testmeta2010:
	@echo " "
	@echo "--------- work on target $@ ---------"
	@echo check that TEI web site is up and responding ...
	-TEISITE=`curl -s --head http://www.tei-c.org/Vault/P5/1.5.0/xml/tei/odd/p5subset.xml`
ifdef TEISITE
	@echo ... it is, so run tests
	$(ANT) -f antruntest.xml -DoddFile=testmeta2010.odd -Dtestfile=testmeta2010.xml validateodd
	for j in `grep "<schemaSpec" testmeta2010.odd | sed 's/.*ident=.//' | sed 's/\".*//' | grep _old `; do \
		echo found schemaSpec $$j in testmeta2010; \
		$(ANT) -f antruntest.xml -DoddFile=testmeta2010.odd -Dtestfile=testmeta2010.xml -Doutputname=$$j -DautoGlobal=true -Dschema=$$j  compileodd rng validaterng cleanup; \
	done
	for j in `grep "<schemaSpec" testmeta2010.odd | sed 's/.*ident=.//' | sed 's/\".*//' | grep _new `; do \
		echo found schemaSpec $$j in testmeta2010; \
		$(ANT) -f antruntest.xml -DoddFile=testmeta2010.odd -Dtestfile=testmeta2010.xml -Doutputname=$$j -Dschema=$$j  compileodd rng validaterng cleanup; \
	done
endif

test-frag: checkrnv
	@echo " "
	@echo "--------- work on target $@ ---------"
	@echo BUILD: Testing files which exercise inclusion of fragments
	${JING} -t -c frag.rnc testfrag.xml
	rnv frag.rnc testfrag.xml

detest: checkrnv
	@echo " "
	@echo "--------- work on target $@ ---------"
	@echo BUILD: Test file with deliberate mistakes
	@echo "--- run ant to validate ODD file (./detest.odd):"
#	Note that output below is called 'detest_odd_schematron' because if there
#	is a RelaxNG error the build dies and no output is generated
	$(ANT) -f antruntest.xml -Doutputname=detest -DoddFile=detest.odd validateodd > detest_odd_schematron.log 2>&1
	@echo "--- run ant to compile ODD file inoto RelaxNG and Schematron"
	@echo "(detest.odd → detest.rng & detest.isosch):"
	$(ANT) -f antruntest.xml -Doutputname=detest -DoddFile=detest.odd compileodd rng isoschematron cleanup
	@echo "--- convert RelaxNG to compact syntax with trang (detest.rng → detest.rnc):"
	${TRANG} detest.rng detest.rnc
	@echo "-- validate XML file (detest.xml) against RelaxNG schema (detest.rnc) using rnv:"
	-rnv detest.rnc detest.xml > detest_xml_relaxng.log 2>&1
	@echo "--- validate XML file against Schematron using Saxon:"
	$(SAXON) -s:detest.xml -xsl:detest.xsl > detest_xml_schematron.log 2>&1
	@echo "--- compare each of the 3 log files just created to the corresponding one in expected-results/:"
#	Note on following somewhat complicated 'diff' command:
#	The log file contains some lines (like "Processing…" and "Loading…") that include filepaths that are different
#	when executed in different environments (like the Travis server, e.g.). Thus rather than comparing the files
#	directly, we delete those lines first, and compare the results of deletions.
	bash -c "diff -C0 -bBw  \
	  <(grep -F '[xslt]' expected-results/detest_odd_schematron.log | grep -Ev 'xslt\] Processing.*detest.odd|xslt\] Loading.*message.isosch.xsl')  \
	  <(grep -F '[xslt]'                ./detest_odd_schematron.log | grep -Ev 'xslt\] Processing.*detest.odd|xslt\] Loading.*message.isosch.xsl')"
	diff -C0 -bBw expected-results/detest_xml_relaxng.log    ./
	diff -C0 -bBw expected-results/detest_xml_schematron.log ./

clean:
	@echo " "
	@echo "--------- clean up time! ---------"
	rm -f test*.doc.xml test*.rnc test*.dtd test*.compiled.* test*.xsd test*.rnc test*.rng test*.xsl test*.isosch
	rm -rf LOG *~ *.xsd Schema RomaResults DTD
	rm -rf *.doc.*
	rm -f detest.log detest.log.all
	rm -f *-examples.rng *-examples.rnc *test*.nvdl *-ex.odd
	rm -f detest.rnc    detest.rng   detest.dtd   detest.isosch
