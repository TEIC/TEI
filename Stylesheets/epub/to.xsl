<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet                 xmlns:dc="http://purl.org/dc/elements/1.1/"
                xmlns:html="http://www.w3.org/1999/xhtml"
                xmlns:tei="http://www.tei-c.org/ns/1.0"
                xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                version="2.0"
                exclude-result-prefixes="tei dc html">

  <xsl:import href="../xhtml2/tei.xsl"/>
  <xsl:param name="splitLevel">0</xsl:param>
  <xsl:param name="STDOUT">false</xsl:param>
  <xsl:param name="outputDir">OEBPS</xsl:param>
  <xsl:param name="cssFile">tei.css</xsl:param>
  <xsl:param name="cssPrintFile"/>

  <xsl:template match="/">
    <xsl:apply-templates mode="split"/>
    <xsl:result-document method="text" href="tei.css">
      <xsl:value-of select="unparsed-text('../tei.css')"/>
    </xsl:result-document>
  </xsl:template>

</xsl:stylesheet>