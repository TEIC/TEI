<xsl:stylesheet
    xpath-default-namespace="http://www.tei-c.org/ns/1.0"
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    version="2.0"
    >
<!-- cf http://www.bramstein.com/projects/xsltjson/ for better
coverage -->
<xsl:strip-space elements="*"/>
<xsl:output method="text" encoding="utf-8" />
<xsl:variable name="inq">"</xsl:variable>
<xsl:variable name="outq">\\"</xsl:variable>
<xsl:template match="/">
<xsl:text>{"TEI": [</xsl:text>
  <xsl:for-each select="TEI/text/body/*">
    <xsl:text>{  "</xsl:text>
    <xsl:value-of select="local-name()"/>
    <xsl:value-of select="position()"/>
  <xsl:text>" : [ &#10; </xsl:text>
  <xsl:for-each select="*">
    <xsl:text> [ </xsl:text>
    <xsl:for-each select="*">
      <xsl:text>"</xsl:text>
      <xsl:apply-templates/>
      <xsl:text>"</xsl:text>
      <xsl:if test="not(position() = last())">,</xsl:if>
    </xsl:for-each>
    <xsl:text> ] </xsl:text>
    <xsl:if test="not(position() = last())">,</xsl:if>
  </xsl:for-each>
  <xsl:text>]&#10;}</xsl:text>
  <xsl:if test="not(position() = last())">,</xsl:if>
  </xsl:for-each>
<xsl:text>
] }
</xsl:text>

</xsl:template>

<xsl:template match="text()">
  <xsl:value-of select="replace(replace(normalize-space(.),'\\','\\\\'),$inq,$outq)"/>
</xsl:template>

</xsl:stylesheet>
