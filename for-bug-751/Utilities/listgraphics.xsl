<xsl:stylesheet 
    version="2.0"
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform" 
    xpath-default-namespace="http://www.tei-c.org/ns/1.0">
  
  <xsl:output method="text" indent="yes" encoding="utf-8"/>
  
  
  <xsl:template match="/">
    <xsl:text>OK=0&#10;</xsl:text>
    <xsl:for-each select="//graphic">
      <xsl:choose>
	<xsl:when test="starts-with(@url,'http')">
	  <xsl:text>curl -I </xsl:text>
	  <xsl:value-of select="@url"/>
	</xsl:when>
	<xsl:otherwise>
	  <xsl:text>if [ ! -f </xsl:text>
	  <xsl:value-of select="@url"/>
	  <xsl:text> ] &#10; then&#10; echo ERROR FILE DOES NOT EXIST: </xsl:text>
	  <xsl:value-of select="@url"/>
	  <xsl:text>&amp;&amp; OK=1; fi</xsl:text>
	</xsl:otherwise>
      </xsl:choose>
      <xsl:text>&#10;</xsl:text>
    </xsl:for-each>
    <xsl:text>exit $OK&#10;</xsl:text>
  </xsl:template>
</xsl:stylesheet>
