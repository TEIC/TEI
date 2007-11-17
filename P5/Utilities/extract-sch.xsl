<xsl:stylesheet version="1.0"
  xmlns:rng="http://relaxng.org/ns/structure/1.0"
  xmlns:tei="http://www.tei-c.org/ns/1.0"
  xmlns:s="http://www.ascc.net/xml/schematron"
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
  <xsl:key name="SCHEMATRON" match="s:ns|s:pattern" use="1"/>
  <xsl:template match="/">
    <s:schema xmlns:s="http://www.ascc.net/xml/schematron" >
      <s:title>Schematron rules for TEI</s:title>
      <xsl:copy-of select="key('SCHEMATRON',1)"/>
    </s:schema>
</xsl:template>
</xsl:stylesheet>

