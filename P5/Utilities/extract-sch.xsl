<xsl:stylesheet version="1.0"
  xmlns:rng="http://relaxng.org/ns/structure/1.0"
  xmlns:tei="http://www.tei-c.org/ns/1.0"
  xmlns:s="http://www.ascc.net/xml/schematron"
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
  <xsl:template match="/">
    <s:schema xmlns:s="http://www.ascc.net/xml/schematron" >
      <s:title>Schematron rules for TEI</s:title>
      <s:ns prefix="tei" uri="http://www.tei-c.org/ns/1.0"/>
      <s:ns prefix="rng" uri="http://relaxng.org/ns/structure/1.0"/>
      <xsl:for-each select=".//s:pattern">
	<xsl:copy-of select="."/>
      </xsl:for-each>
    </s:schema>
</xsl:template>
</xsl:stylesheet>

