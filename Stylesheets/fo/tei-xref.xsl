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

<!-- cross-referencing -->
<xsl:template match="tei:ptr">
 <fo:basic-link>
   <xsl:call-template name="linkStyle"/>
  <xsl:attribute name="internal-destination"><xsl:value-of select="translate(@target,'_','-')"/></xsl:attribute>
  <xsl:apply-templates mode="xref" select="key('IDS',@target)" />
 </fo:basic-link>
</xsl:template>

<xsl:template match="tei:ref">
 <fo:basic-link>
   <xsl:call-template name="linkStyle"/>
  <xsl:attribute name="internal-destination"><xsl:value-of select="translate(@target,'_','-')"/></xsl:attribute>
  <xsl:apply-templates/>
 </fo:basic-link>
</xsl:template>

<xsl:template match="tei:xref">
  <xsl:variable name="dest">
       <xsl:choose>
    <xsl:when test="@doc">
     <xsl:value-of select="unparsed-entity-uri(@doc)"/>
    </xsl:when>
    <xsl:otherwise>
      <xsl:value-of select="@url"/>
    </xsl:otherwise>
   </xsl:choose>
  </xsl:variable>
  <fo:basic-link external-destination="{$dest}">
      <xsl:call-template name="linkStyle"/>
      <xsl:apply-templates/>
   </fo:basic-link>
   <xsl:call-template name="showXrefURL">
       <xsl:with-param name="dest" select="$dest"/>
   </xsl:call-template>
</xsl:template>

<xsl:template match="tei:xptr">
<fo:basic-link font-family="{$typewriterFont}">
   <xsl:call-template name="linkStyle"/>
   <xsl:attribute name="external-destination">
    <xsl:choose>
     <xsl:when test="@doc">
      <xsl:value-of select="unparsed-entity-uri(@doc)"/>
     </xsl:when>
     <xsl:otherwise>
       <xsl:value-of select="@url"/>
     </xsl:otherwise>
    </xsl:choose>
   </xsl:attribute>
   <xsl:choose>
    <xsl:when test="@doc">
     <xsl:value-of select="unparsed-entity-uri(@doc)"/>
    </xsl:when>
    <xsl:otherwise>
      <xsl:value-of select="@url"/>
    </xsl:otherwise>
   </xsl:choose>
</fo:basic-link>
</xsl:template>

</xsl:stylesheet>
