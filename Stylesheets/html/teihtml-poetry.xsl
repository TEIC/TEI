<!-- $Date: 
Text Encoding Initiative Consortium XSLT stylesheet family
2001/10/01 $, $Revision$, $Author$

XSL HTML stylesheet to format TEI XML documents 

 
##LICENSE
-->

<xsl:stylesheet
  xmlns:tei="http://www.tei-c.org/ns/1.0"

  xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0"
  xmlns:fo="http://www.w3.org/1999/XSL/Format"
  >

<xsl:template match="tei:div[@type='frontispiece']">
 <xsl:apply-templates/>
</xsl:template>

<xsl:template match="tei:div[@type='epistle']">
 <xsl:apply-templates/>
</xsl:template>

<xsl:template match="tei:div[@type='illustration']">
 <xsl:apply-templates/>
</xsl:template>

<!--
<xsl:template match="tei:div[@type='canto']">
  <xsl:variable name="divlevel" select="count(ancestor::tei:div)"/>
  <xsl:call-template name="NumberedHeading">
    <xsl:with-param name="level"><xsl:value-of select="$divlevel"/></xsl:with-param>
  </xsl:call-template>
  <xsl:apply-templates/>
</xsl:template>

-->

<xsl:template match="tei:byline">
 <p align="center">
   <xsl:apply-templates/>
 </p>
</xsl:template>

<xsl:template match="tei:epigraph">
 <p 		align="center">
   <xsl:apply-templates/>
 </p>
</xsl:template>

<xsl:template match="tei:closer">
 <p 	
	space-before.optimum="4pt"
	space-after.optimum="4pt">
   <xsl:apply-templates/>
 </p>
</xsl:template>

<xsl:template match="tei:salute">
 <p  align="left">
   <xsl:apply-templates/>
 </p>
</xsl:template>

<xsl:template match="tei:signed">
 <p  align="left">
   <xsl:apply-templates/>
 </p>
</xsl:template>

<xsl:template match="tei:epigraph/lg">
    <table>
      <xsl:apply-templates/>
    </table>
</xsl:template>

<xsl:template match="tei:l">
  <xsl:apply-templates/><br/>
</xsl:template>

<xsl:template match="tei:lg/l">
  <tr><td>
  <xsl:choose>
    <xsl:when test="@rend='Alignr'">
      <xsl:attribute name="align">right</xsl:attribute>
    </xsl:when>
    <xsl:when test="@rend='Alignc'">
     <xsl:attribute name="align">center</xsl:attribute>
    </xsl:when>
    <xsl:when test="@rend='Alignl'">
      <xsl:attribute name="align">left</xsl:attribute>
      <xsl:text>&#xA0;&#xA0;</xsl:text>
    </xsl:when>
    <xsl:otherwise>
      <xsl:attribute name="align"><xsl:value-of select="$cellAlign"/></xsl:attribute>
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
</xsl:otherwise>
</xsl:choose>
  <xsl:apply-templates/>
</td></tr> 
</xsl:template>

<xsl:template match="tei:lg">
    <table class="lg">
      <xsl:apply-templates/>
    </table>
</xsl:template>

</xsl:stylesheet>
