<!-- 
TEI XSLT stylesheet family
$Date$, $Revision$, $Author$

XSL FO stylesheet to format TEI XML documents 

#include LICENSE
-->

<xsl:stylesheet
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0"
  xmlns:tei="http://www.tei-c.org/ns/1.0"
  xmlns:fo="http://www.w3.org/1999/XSL/Format"
  >

<xsl:template match="tei:bibl">
  <xsl:apply-templates/>
</xsl:template>

<xsl:template match="tei:listBibl/tei:bibl">
 <fo:block>
     <xsl:call-template name="addID"/>
     <xsl:attribute name="space-before.optimum">
<xsl:value-of select="$spaceBeforeBibl"/></xsl:attribute>
     <xsl:attribute name="space-after.optimum">
<xsl:value-of select="$spaceAfterBibl"/></xsl:attribute>
     <xsl:attribute name="text-indent">-<xsl:value-of select="$indentBibl"/>
</xsl:attribute>
     <xsl:attribute name="start-indent"><xsl:value-of select="$indentBibl"/>
</xsl:attribute>
   <xsl:apply-templates/>
 </fo:block>
</xsl:template>

<xsl:template match="tei:listBibl">
<xsl:choose>
<!-- is it in the back matter? -->
<xsl:when test="ancestor::tei:back">
 <fo:page-sequence>
  <fo:block>
  <xsl:call-template name="listBiblSetup"/>
  </fo:block>
  <xsl:apply-templates/>
 </fo:page-sequence>
</xsl:when>
<xsl:otherwise>
  <xsl:apply-templates/>
</xsl:otherwise>
</xsl:choose>
</xsl:template>

<xsl:template name="listBiblSetup">
    <xsl:call-template name="setupDiv0"/>
     <xsl:call-template name="addID"/>
     <xsl:value-of select="$biblioWords"/>
</xsl:template>

</xsl:stylesheet>
