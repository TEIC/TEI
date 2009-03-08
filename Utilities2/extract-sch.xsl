<xsl:stylesheet version="2.0"
  xmlns:rng="http://relaxng.org/ns/structure/1.0"
  xmlns:tei="http://www.tei-c.org/ns/1.0"
  xmlns="http://www.ascc.net/xml/schematron"
  xmlns:s="http://www.ascc.net/xml/schematron"
  exclude-result-prefixes="rng tei s"
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
  <xsl:output encoding="utf-8" indent="yes" method="xml"/>
  <xsl:key name="SCHEMATRON" match="s:ns|s:pattern" use="1"/>
  <xsl:template match="/">
    <schema xmlns:s="http://www.ascc.net/xml/schematron" >
      <title>Schematron rules for TEI</title>
      <xsl:copy-of select="key('SCHEMATRON',1)"/>
    </schema>
</xsl:template>
</xsl:stylesheet>

