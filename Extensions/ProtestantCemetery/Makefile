default:
	-rm Schema/*
	/TEI/Sourceforge/P5/Roma cem.odd .

test:
	xmllint --noent /TEI/Projects/ProtestantCemetery/cem.xml | xmllint --noout --relaxng teicem.rng - 

test2:
	xmllint --noent /TEI/Projects/ProtestantCemetery/cem.xml > cemall.xml
	jing -c teicem.rnc cemall.xml

test3:
	xmllint --noent /TEI/Projects/ProtestantCemetery/cem.xml > cemall.xml
	jing -c teicem.compiled.rnc cemall.xml

clean:
	-rm *.rng *.rnc *.xsd *.dtd *.doc.xml *~



