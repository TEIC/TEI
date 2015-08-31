<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet 
    xmlns:xd="http://www.pnp-software.com/XSLTdoc"
    xmlns:s="http://www.ascc.net/xml/schematron" 
    xmlns:a="http://relaxng.org/ns/compatibility/annotations/1.0"
    xmlns:xs="http://www.w3.org/2001/XMLSchema" 
    xmlns:rng="http://relaxng.org/ns/structure/1.0"
    xmlns:teix="http://www.tei-c.org/ns/Examples"
    xmlns:tei="http://www.tei-c.org/ns/1.0"
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    xmlns:fo="http://www.w3.org/1999/XSL/Format"
    xmlns:edate="http://exslt.org/dates-and-times"
    xmlns:exsl="http://exslt.org/common"
    xmlns:estr="http://exslt.org/strings"
    exclude-result-prefixes="exsl estr edate teix fo a tei xs rng s xd" 
    extension-element-prefixes="edate exsl estr"
    version="1.0">

<xsl:output 
   method="xml"
   indent="yes"
   encoding="utf-8"
   cdata-section-elements="tei:eg"
   omit-xml-declaration="yes"/>

<xsl:template match="tei:valItem/tei:gloss">
  <xsl:choose>
    <xsl:when test="parent::valItem/tei:desc">
      <xsl:copy-of select="."/>
    </xsl:when>
    <xsl:otherwise>
      <desc xmlns="http://www.tei-c.org/ns/1.0">
	<xsl:apply-templates select="*|@*|processing-instruction()|comment()|text()"/>
      </desc>
    </xsl:otherwise>
  </xsl:choose>

</xsl:template>

<xsl:template match="*">
 <xsl:copy>
  <xsl:apply-templates select="*|@*|processing-instruction()|comment()|text()"/>
 </xsl:copy>
</xsl:template>

<xsl:template match="@*|processing-instruction()|comment()|text()">
    <xsl:copy-of select="."/>
</xsl:template>


</xsl:stylesheet>
