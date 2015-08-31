<xsl:stylesheet version="1.0"
	  xmlns:rng="http://relaxng.org/ns/structure/1.0"
	  xmlns:tei="http://www.tei-c.org/ns/1.0"
	  exclude-result-prefixes="rng tei"
	  xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
<xsl:output method="text" encoding="utf-8" indent="yes"/>
<xsl:key name="PI" match="processing-instruction()" use="1"/>
<xsl:key name="PIS" match="processing-instruction()" use="."/>
<xsl:template match="/">
  <xsl:for-each select="key('PI',1)">
    <xsl:if test="generate-id(.)=generate-id(key('PIS',.)[1])">
      <xsl:value-of select="normalize-space(.)"/>
      <xsl:text>&#10;</xsl:text>
    </xsl:if>
  </xsl:for-each>
</xsl:template>
</xsl:stylesheet>
