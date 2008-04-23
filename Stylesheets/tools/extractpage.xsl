<xsl:stylesheet 
    xmlns:tei="http://www.tei-c.org/ns/1.0"
    exclude-result-prefixes="tei"
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform" 
    version="1.0"
>
<xsl:output method="xml" indent="yes"/>
<xsl:param name="start">1</xsl:param>
<xsl:param name="end">2</xsl:param>

<xsl:template match="tei:pb">
  <xsl:if test="@n=$start">
    <xsl:copy-of select="."/>
  </xsl:if>
</xsl:template>

<xsl:template match="text()">
  <xsl:choose>
    <xsl:when test="preceding::tei:pb[@n=$end]">
    </xsl:when>
    <xsl:when test="following::tei:pb[@n=$start]">
    </xsl:when>
    <xsl:otherwise>
      <xsl:copy-of select="."/>
    </xsl:otherwise>
  </xsl:choose>
</xsl:template>

<xsl:template match="*">
  <xsl:choose>
    <xsl:when test=".//tei:pb[@n=$start or @n=$end]">
      <xsl:copy>
	<xsl:copy-of select="@*"/>
	<xsl:apply-templates/>
      </xsl:copy>
    </xsl:when>
    <xsl:when test="following::tei:pb[@n=$end] and
		    preceding::tei:pb[@n=$start]">
      <xsl:copy-of select="."/>
    </xsl:when>
  </xsl:choose>
</xsl:template>

</xsl:stylesheet>