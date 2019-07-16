<xsl:stylesheet 
  xmlns:tei="http://www.tei-c.org/ns/1.0"
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform" 
  exclude-result-prefixes="tei" 
  version="1.0"
>
<xsl:output method="xml" encoding="utf-8" indent="yes"/>

<xsl:template match="/">
<tei:glossList>
  <xsl:for-each select="//tei:macroSpec">
    <xsl:sort select="@ident"/>
    <xsl:choose>
      <xsl:when test="starts-with(@ident,'TEI.')"/>
      <xsl:when test="starts-with(@ident,'mix.')"/>
      <xsl:otherwise>
	<tei:label><xsl:value-of select="@ident"/></tei:label>
	<tei:item>
	  <xsl:apply-templates select="tei:desc"/>
	  <tei:input>
	    <xsl:text disable-output-escaping="yes">&lt;![CDATA[</xsl:text>
	    <xsl:copy-of select="tei:content/*"/>
	    <xsl:text disable-output-escaping="yes">]]&gt;</xsl:text>
	  </tei:input>
	</tei:item>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:for-each>
</tei:glossList>
</xsl:template>
</xsl:stylesheet>
