<!-- to deal with

THROUGH argument
THROUGH bibl   
THROUGH epigraph
THROUGH group   
THROUGH name    
THROUGH salute
THROUGH signed  
-->
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
<!-- special purpose
 domain-specific elements, whose interpretation
 is open to all sorts of questions -->


<xsl:template match="tei:div[@type='frontispiece']">
 <xsl:apply-templates/>
</xsl:template>

<xsl:template match="tei:div[@type='epistle']">
 <xsl:apply-templates/>
</xsl:template>

<xsl:template match="tei:div[@type='illustration']">
 <xsl:apply-templates/>
</xsl:template>

<xsl:template match="tei:div[@type='canto']">
  <xsl:variable name="divlevel" select="count(ancestor::tei:div)"/>
  <xsl:call-template name="NumberedHeading">
    <xsl:with-param name="level"><xsl:value-of select="$divlevel"/></xsl:with-param>
  </xsl:call-template>
  <xsl:apply-templates/>
</xsl:template>

<xsl:template match="tei:div[@type='dedication']">
  <xsl:variable name="divlevel" select="count(ancestor::tei:div)"/>
  <xsl:call-template name="NumberedHeading">
    <xsl:with-param name="level"><xsl:value-of select="$divlevel"/></xsl:with-param>
  </xsl:call-template>
  <xsl:apply-templates/>
</xsl:template>

<xsl:template match="tei:byline">
 <fo:block text-align="center">
   <xsl:apply-templates/>
 </fo:block>
</xsl:template>

<xsl:template match="tei:epigraph">
 <fo:block 	
	text-align="center"
	space-before.optimum="4pt"
	space-after.optimum="4pt"
	start-indent="{$exampleMargin}">
   <xsl:apply-templates/>
 </fo:block>
</xsl:template>

<xsl:template match="tei:closer">
 <fo:block 	
	space-before.optimum="4pt"
	space-after.optimum="4pt">
   <xsl:apply-templates/>
 </fo:block>
</xsl:template>

<xsl:template match="tei:salute">
 <fo:block  text-align="left">
   <xsl:apply-templates/>
 </fo:block>
</xsl:template>

<xsl:template match="tei:signed">
 <fo:block  text-align="left">
   <xsl:apply-templates/>
 </fo:block>
</xsl:template>

<xsl:template match="tei:epigraph/tei:lg">
    <fo:block 
	text-align="center"
	space-before.optimum="4pt"
	space-after.optimum="4pt"
	>
      <xsl:apply-templates/>
    </fo:block>
</xsl:template>


<xsl:template match="tei:l">
 <fo:block 	
	space-before.optimum="0pt"
	space-after.optimum="0pt">
   <xsl:choose>
   <xsl:when test="starts-with(@rend,'indent(')">
    <xsl:attribute name="text-indent">
      <xsl:value-of select="concat(substring-before(substring-after(@rend,'('),')'),'em')"/>
    </xsl:attribute>
  </xsl:when>
  <xsl:when test="starts-with(@rend,'indent')">
    <xsl:attribute name="text-indent">1em</xsl:attribute>
  </xsl:when>
  </xsl:choose>
  <xsl:apply-templates/>
 </fo:block> 
</xsl:template>

<xsl:template match="tei:lg">
    <fo:block 
	text-align="start"
	space-before.optimum="4pt"
	space-after.optimum="4pt"
	>
      <xsl:apply-templates/>
    </fo:block>
</xsl:template>

</xsl:stylesheet>
