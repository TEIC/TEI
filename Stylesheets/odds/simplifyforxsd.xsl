<?xml version="1.0" encoding="utf-8"?>
<!-- 
Text Encoding Initiative Consortium XSLT stylesheet family
$Date$, $Revision$, $Author$

XSL stylesheet to process TEI documents using ODD markup

 
##LICENSE
--> 
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
  xmlns:s="http://www.ascc.net/xml/schematron"
  xmlns:exsl="http://exslt.org/common"
  extension-element-prefixes="exsl"
  xmlns:xsp="http://apache.org/xsp/core/v1"
  xmlns:xs="http://www.w3.org/2001/XInclude"
  xmlns:a="http://relaxng.org/ns/compatibility/annotations/1.0" 
  xmlns:f="http://axkit.org/NS/xsp/perform/v1" 
  xmlns:tei="http://www.tei-c.org/ns/1.0" 
  exclude-result-prefixes="exsl rng a f tei s" 
  xmlns:rng="http://relaxng.org/ns/structure/1.0" 
  version="1.0">

<xsl:output indent="yes"/>

<xsl:key name="DEFS" match="rng:define" use="@name"/>

<xsl:template match="*|@*|text()|comment()">
  <xsl:copy>
    <xsl:apply-templates select="*|@*|text()|comment()"/>
  </xsl:copy>
</xsl:template>

<!-- copy the xxx.content body into place -->
<xsl:template match="rng:define[contains(@name,'.content')]" mode="stage2"/>
<xsl:template match="rng:ref[contains(@name,'.content')]" mode="stage2">
<xsl:choose>
  <xsl:when test="key('DEFS',@name)">
    <xsl:for-each
	select="key('DEFS',@name)">
      <xsl:copy-of select="rng:*"/>
    </xsl:for-each>
  </xsl:when>
  <xsl:otherwise>
    <xsl:copy-of select="."/>
  </xsl:otherwise>
</xsl:choose>
</xsl:template>

</xsl:stylesheet>

