<!-- 
Text Encoding Initiative Consortium XSLT stylesheet family
$Date$, $Revision$, $Author$

XSL stylesheet to format TEI XML documents to HTML or XSL FO

 
#include LICENSE
--> 
<xsl:stylesheet
  xmlns:tei="http://www.tei-c.org/ns/1.0"

  xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0">



<!-- top-level stuff -->

<xsl:template match="tei:docImprint"/>


<xsl:template match="tei:front|titlePart">
  <xsl:apply-templates/>
</xsl:template>

<xsl:template match="tei:titlePage">
  <hr/>
  <table>
    <tr><td><b><xsl:apply-templates mode="print" select="tei:docTitle"/></b></td></tr>
    <tr><td><i><xsl:apply-templates mode="print" select="tei:docAuthor"/></i></td></tr>
    <tr><td><xsl:apply-templates mode="print" select="tei:docDate"/></td></tr>
  </table>
  <hr/>
</xsl:template>

<xsl:template match="tei:body|back" mode="split">
  <xsl:for-each select="*">
   <xsl:choose>
    <xsl:when test="starts-with(name(.),'div')">
       <xsl:apply-templates select="." mode="split"/>
    </xsl:when>
    <xsl:otherwise>
       <xsl:apply-templates select="."/>
    </xsl:otherwise>
   </xsl:choose>
  </xsl:for-each>
</xsl:template>

<xsl:template match="tei:teiHeader"/>

<!-- author and title -->
<xsl:template match="tei:docTitle"/>
<xsl:template match="tei:docAuthor"/>
<xsl:template match="tei:docDate"/>

<xsl:template match="tei:docDate" mode="print">
    <xsl:apply-templates/>
</xsl:template>

<xsl:template match="tei:docAuthor" mode="author">
     <xsl:if test="preceding-sibling::tei:docAuthor">
	<xsl:text>, </xsl:text>
     </xsl:if>
    <xsl:apply-templates/>
</xsl:template>



</xsl:stylesheet>
