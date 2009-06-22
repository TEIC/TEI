<xsl:stylesheet version="1.0"
  xmlns:tei="http://www.tei-c.org/ns/1.0"
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
  <xsl:key name="ALL" use="1" 
    match="tei:elementSpec|tei:macroSpec|tei:classSpec|tei:moduleSpec"/>
  <xsl:template match="/">
    <tei:TEI>
      <xsl:copy-of select="tei:TEI/tei:teiHeader"/>
      <xsl:copy-of select="key('ALL',1)"/>
    </tei:TEI>
</xsl:template>
</xsl:stylesheet>