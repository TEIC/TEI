<!-- 
TEI XSLT stylesheet family
$Date$, $Revision$, $Author$

XSL FO stylesheet to format TEI XML documents 

##LICENSE
-->

<xsl:stylesheet
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0"
  xmlns:tei="http://www.tei-c.org/ns/1.0"
  xmlns:fo="http://www.w3.org/1999/XSL/Format"
  >

<xsl:template match="tei:sp">
<fo:block 
	text-align="justify" 
	start-indent="1em"
	text-indent="-1em"
 	space-before="3pt"
	>
      <xsl:apply-templates/>
</fo:block>
</xsl:template>

<!-- paragraphs inside speeches do very little-->
 <xsl:template match="tei:sp/tei:p">
  <fo:inline> 
    <xsl:apply-templates/>
  </fo:inline>
</xsl:template>


<xsl:template match="tei:speaker">
<fo:inline>
 <xsl:call-template name="rend">
   <xsl:with-param name="defaultvalue" select="string('italic')"/>
   <xsl:with-param name="defaultstyle" select="string('font-style')"/>
 </xsl:call-template>
      <xsl:apply-templates/><xsl:text> </xsl:text>
</fo:inline>
</xsl:template>

<xsl:template match="tei:p/tei:stage">
<fo:inline>
 <xsl:call-template name="rend">
   <xsl:with-param name="defaultvalue" select="string('normal')"/>
   <xsl:with-param name="defaultstyle" select="string('font-style')"/>
 </xsl:call-template>
      <xsl:apply-templates/>
</fo:inline>
</xsl:template>

<xsl:template match="tei:stage">
<fo:block>
 <xsl:attribute name="text-indent">1em</xsl:attribute>
 <xsl:call-template name="rend">
   <xsl:with-param name="defaultvalue" select="string('normal')"/>
   <xsl:with-param name="defaultstyle" select="string('font-style')"/>
 </xsl:call-template>
      <xsl:apply-templates/>
</fo:block>
</xsl:template>

</xsl:stylesheet>
