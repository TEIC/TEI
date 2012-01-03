default: ms schema validate

schema: 
	@echo "BUILD: Test schema object"
	@echo Validate $< itself
	rnv ../../P5/p5odds.rnc object.odd
	@echo "generate examples schema"
	saxon -o:object-ex.odd object.odd ../../P5/Utilities/odd2exodd.xsl 
	roma --nodtd --noxsd object-ex.odd . 
	@echo "generate schema" 
	roma --docFlags="summaryDoc=true" --dochtml --isoschematron object.odd .

validate:
	@echo check egXML in object.odd
	../../P5/run-onvdl object.nvdl object.odd 
	@echo Validate using Jing for RELAX NG... 
	jing -t object.rng object.xml
	@echo Validate using RNV.... 
	xmllint --noent --dropdtd  object.xml | rnv object.rnc 
	@echo Validate using xmllint for DTD  
	xmllint --noent --dropdtd object.xml | xmllint --noout --dtdvalid object.dtd -
	@echo Validate using Schematron
	saxon object.isosch ../../P5/Utilities/iso_schematron_message_xslt2.xsl  > object.xsl
	saxon object.xml object.xsl

ms:
	perl ms.pl < ms.dat > ms.xml
