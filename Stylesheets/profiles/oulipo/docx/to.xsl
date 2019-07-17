<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                xmlns:tei="http://www.tei-c.org/ns/1.0"
                xmlns:teix="http://www.tei-c.org/ns/Examples"
                xmlns:iso="http://www.iso.org/ns/1.0"
                xmlns:xs="http://www.w3.org/2001/XMLSchema"
                xmlns:ve="http://schemas.openxmlformats.org/markup-compatibility/2006"
                xmlns:o="urn:schemas-microsoft-com:office:office"
                xmlns:r="http://schemas.openxmlformats.org/officeDocument/2006/relationships"
                xmlns:m="http://schemas.openxmlformats.org/officeDocument/2006/math"
                xmlns:v="urn:schemas-microsoft-com:vml"
                xmlns:fn="http://www.w3.org/2005/02/xpath-functions"
                xmlns:wp="http://schemas.openxmlformats.org/drawingml/2006/wordprocessingDrawing"
                xmlns:a="http://schemas.openxmlformats.org/drawingml/2006/main"
                xmlns:w10="urn:schemas-microsoft-com:office:word"
                xmlns:w="http://schemas.openxmlformats.org/wordprocessingml/2006/main"
                xmlns:wne="http://schemas.microsoft.com/office/word/2006/wordml"
                xmlns:mml="http://www.w3.org/1998/Math/MathML"
                xmlns:tbx="http://www.lisa.org/TBX-Specification.33.0.html"
                xmlns:pic="http://schemas.openxmlformats.org/drawingml/2006/picture"
                
                xmlns:teidocx="http://www.tei-c.org/ns/teidocx/1.0"
                version="2.0"
                exclude-result-prefixes="teix ve o r m v wp w10 w wne mml tbx iso tei a xs pic fn tei teidocx">
    <!-- import conversion style -->
    <xsl:import href="../../default/docx/to.xsl"/>
    
    <xsl:param name="lang">fr</xsl:param>
    <xsl:param name="pagebreakStyle">active</xsl:param>
    <xsl:template name="document-title"/>
    
    <xsl:template match="tei:p[@rend]" mode="get-style">
      <xsl:variable name="this" select="@rend"/>
      <xsl:for-each select="document($styleDoc,/)">
      <xsl:for-each select="key('Styles',$this)">
        <xsl:value-of select="parent::w:style/@w:styleId"/>
      </xsl:for-each>
    </xsl:for-each>
    </xsl:template>

   <xsl:template match="tei:title|tei:q|tei:said|tei:quote">
    <xsl:call-template name="makeInline">
      <xsl:with-param name="style" select="local-name()"/>
    </xsl:call-template>
   </xsl:template>

  <xsl:template match="tei:byline|tei:quotation|tei:opener|tei:closer">
    <xsl:call-template name="block-element">
      <xsl:with-param name="style" select="concat('tei',local-name())"/>
    </xsl:call-template>
  </xsl:template>

  <xsl:template match="tei:date" mode="get-style">teidate</xsl:template>

   <xsl:template match="tei:persName[@type='oulipen']">
    <xsl:call-template name="makeInline">
      <xsl:with-param name="style">Oulipen</xsl:with-param>
    </xsl:call-template>
   </xsl:template>

   <xsl:template match="tei:pb[parent::tei:text]"/>

   <xsl:template match="tei:persName[@type='other']">
    <xsl:call-template name="makeInline">
      <xsl:with-param name="style">Personne</xsl:with-param>
    </xsl:call-template>
   </xsl:template>

</xsl:stylesheet>
