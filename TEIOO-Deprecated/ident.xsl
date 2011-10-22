<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
  <xsl:output method="xml"/>
  <xsl:template match="/">
      <xsl:copy-of select="."/>
  </xsl:template>

<xsl:template match="mot">
<xsl:element name="w">
<xsl:attribute name="lemma">
<xsl:value-of select="LemmeDMF"/>
</xsl:attribute>
<xsl:value-of select="."/>
</xsl:element>
</xsl:template>

</xsl:stylesheet>