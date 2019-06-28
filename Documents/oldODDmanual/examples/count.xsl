<xsl:stylesheet 
  xmlns:tei="http://www.tei-c.org/ns/1.0"
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform" 
  exclude-result-prefixes="tei" 
  version="1.0"
>
<xsl:output method="text" encoding="utf-8" indent="yes"/>

<xsl:template match="/">
Elements: <xsl:value-of select="count(.//tei:elementSpec)"/>
Classes: <xsl:value-of select="count(.//tei:classSpec)"/>
Macros: <xsl:value-of select="count(.//tei:macroSpec)"/>
</xsl:template>
</xsl:stylesheet>
