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


<!-- ignore the header -->
<xsl:template match="tei:teiHeader">
<!--
  <fo:block>
    <xsl:for-each select="@*"> 
      <xsl:text>Value of </xsl:text><xsl:value-of select="name(.)"/>
      <xsl:text> is </xsl:text><xsl:value-of select="."/>
    </xsl:for-each>
  </fo:block>
-->
</xsl:template>

<xsl:template name="textTitle">
  <xsl:apply-templates select="tei:front"/>  
</xsl:template>

<!-- author and title -->
<xsl:template match="tei:docTitle">
    <fo:block text-align="left" font-size="{$titleSize}" >
      <xsl:if test="ancestor::tei:group/tei:text/tei:front">
        <xsl:attribute name="id">
      <xsl:choose>
        <xsl:when test="ancestor::tei:text/@id">
         <xsl:value-of select="translate(ancestor::tei:text/@id,'_','-')"/>
       </xsl:when>
        <xsl:when test="ancestor::tei:text/@xml:id">
         <xsl:value-of select="translate(ancestor::tei:text/@xml:id,'_','-')"/>
       </xsl:when>
       <xsl:otherwise>
          <xsl:value-of select="generate-id()"/>
       </xsl:otherwise>
       </xsl:choose>
        </xsl:attribute>
      </xsl:if>
	<fo:inline font-weight="bold">
	<xsl:apply-templates select="tei:titlePart"/></fo:inline>
    </fo:block>
</xsl:template>

<xsl:template match="tei:docImprint"/>

<xsl:template match="tei:docAuthor" mode="heading">
  <xsl:if test="preceding-sibling::tei:docAuthor">
   <xsl:choose>
     <xsl:when test="not(following-sibling::tei:docAuthor)">
	<xsl:text> and </xsl:text>
     </xsl:when>
     <xsl:otherwise>
	<xsl:text>, </xsl:text>
     </xsl:otherwise>
   </xsl:choose>
 </xsl:if>
 <xsl:apply-templates/>
</xsl:template>

<xsl:template match="tei:docAuthor">
    <fo:block font-size="{$authorSize}">
       <fo:inline font-style="italic">
        <xsl:apply-templates/>
       </fo:inline>
    </fo:block>
</xsl:template>

<xsl:template match="tei:docDate">
    <fo:block font-size="{$dateSize}">
	<xsl:apply-templates/></fo:block>
</xsl:template>

<!-- omit if found outside front matter -->
<xsl:template match="tei:div/tei:docDate"/>
<xsl:template match="tei:div/tei:docAuthor"/>
<xsl:template match="tei:div/tei:docTitle"/>
</xsl:stylesheet>
