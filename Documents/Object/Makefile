default: object.rng

object.rng: object.odd
	@echo "BUILD: Test schema object"
	@echo Validate $< itself
	rnv ../../P5/p5odds.rnc $< 
	saxon -o:object-ex.odd $< ../../P5/Utilities/odd2exodd.xsl 
	roma --nodtd --noxsd object-ex.odd . 
	@echo check egXML in $<
	../../P5/run-onvdl object.nvdl $< 
	@echo "Run Roma on $<" 
	roma $< .
	@echo Validate using Jing for RELAX NG... 
	jing -t object.rng object.xml
	@echo Validate using RNV.... 
	xmllint --noent --dropdtd  object.xml | rnv object.rnc 
	@echo Validate using xmllint for DTD  
	xmllint --dropdtd object.xml | xmllint --noout --dtdvalid object.dtd -
	@echo Validate using Schematron
	saxon object.isosch ../../P5/Utilities/iso_schematron_message_xslt2.xsl  > object.xsl
	saxon object.xml object.xsl


