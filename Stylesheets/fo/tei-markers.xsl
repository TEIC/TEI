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

<xsl:template match="tei:milestone">
    <fo:block>
    <xsl:text>******************</xsl:text>
    <xsl:value-of select="@unit"/>
    <xsl:text> </xsl:text><xsl:value-of select="@n"/>
    <xsl:text>******************</xsl:text>
    </fo:block>
</xsl:template>

<xsl:template match="tei:pb">
<xsl:choose>
  <xsl:when test="$activePagebreaks">
     <fo:block break-before="page">
     </fo:block>
  </xsl:when>
  <xsl:otherwise>
     <fo:block text-align="center">
      <xsl:text>&#x2701;[</xsl:text>
      <xsl:value-of select="@unit"/>
      <xsl:text> Page </xsl:text>
      <xsl:value-of select="@n"/>
      <xsl:text>]&#x2701;</xsl:text>
     </fo:block>
  </xsl:otherwise>
</xsl:choose>
</xsl:template>

<xsl:template match="tei:eg[@rend='kwic']/lb"/>

<xsl:template match="tei:cell//tei:lb">
 <xsl:text>&#x2028;</xsl:text>
</xsl:template>

<xsl:template match="tei:lb">
<xsl:choose>
  <xsl:when test="$activeLinebreaks">
<!-- this is a *visible* linebreak character 

PassiveTeX implements it as a real line break
-->
       <xsl:text>&#x2028;</xsl:text>
  </xsl:when>
  <xsl:otherwise>
    <fo:inline font-size="8pt">
      <xsl:text>&#x2761;</xsl:text>
    </fo:inline>
  </xsl:otherwise>
</xsl:choose>
<!-- JT's suggestion:
<fo:inline
 xml:space="preserve"
 white-space-collapse="false">&#xA;</fo:inline>
-->
</xsl:template>



</xsl:stylesheet>
