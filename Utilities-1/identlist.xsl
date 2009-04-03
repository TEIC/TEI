<xsl:stylesheet 
  xmlns:tei="http://www.tei-c.org/ns/1.0"
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform" 
  version="1.0"
>
<xsl:output method="xml" indent="yes"/>
<xsl:key name="MEMBERS" match="tei:classSpec" use="tei:classes/tei:memberOf/@key"/>

<xsl:template match="/">
  <xsl:for-each select="//tei:elementSpec">
    <xsl:value-of select="@ident"/><xsl:text> : </xsl:text>
    <xsl:value-of select="tei:classes/tei:memberOf/@key"/>
</xsl:text>
  </xsl:for-each>
</xsl:template>
</xsl:stylesheet>
