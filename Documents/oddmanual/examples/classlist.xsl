<xsl:stylesheet 
  xmlns:tei="http://www.tei-c.org/ns/1.0"
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform" 
  version="1.0"
  exclude-result-prefixes="tei" 
>
<xsl:key name="MEMBERS" match="tei:elementSpec|tei:classSpec" use="tei:classes/tei:memberOf/@key"/>

<xsl:output method="xml" encoding="utf-8" indent="yes"/>

<xsl:template match="/">
<tei:glossList>
  <xsl:for-each select="//tei:classSpec">
    <xsl:sort select="@ident"/>
    <xsl:variable name="X" select="@ident"/>
      <tei:label><xsl:value-of select="@ident"/></tei:label>
      <tei:item>(<xsl:value-of select="@type"/>): <xsl:apply-templates
      select="tei:desc"/>
      <xsl:text> [</xsl:text>
      <xsl:for-each       select="key('MEMBERS',@ident)">
      <xsl:sort select="@ident"/>
      <xsl:if
	  test="not(position()=1)">, </xsl:if>
      <tei:ident><xsl:value-of select="@ident"/></tei:ident>
    </xsl:for-each>
      <xsl:text>]</xsl:text>
      </tei:item>
  </xsl:for-each>
</tei:glossList>
</xsl:template>
</xsl:stylesheet>
