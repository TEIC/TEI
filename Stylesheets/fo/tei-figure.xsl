<!-- 
TEI XSLT stylesheet family
$Date$, $Revision$, $Author$

XSL FO stylesheet to format TEI XML documents 

#include LICENSE
-->

<xsl:stylesheet
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0"
  xmlns:tei="http://www.tei-c.org/ns/1.0"
  xmlns:fo="http://www.w3.org/1999/XSL/Format">

<xsl:template match='tei:figure'>
 <fo:float>
   <xsl:attribute name="id">
        <xsl:call-template name="idLabel"/>
   </xsl:attribute>
 <fo:block text-align="center">
 <xsl:variable name="File">
  <xsl:variable name="ent">
   <xsl:value-of select="unparsed-entity-uri(@entity)"/>
  </xsl:variable>
  <xsl:choose> 
  <xsl:when test="@file">
   <xsl:value-of select="@file"/>
  </xsl:when>
  <xsl:when test="@url">
   <xsl:value-of select="@url"/>
  </xsl:when>
  <xsl:when test="starts-with($ent,'file:')">
<!-- some XSL processors, eg xt, turn plain names into a full file: URL -->
 <xsl:value-of select="substring-after($ent,'file:')"/>
  </xsl:when>
  <xsl:otherwise>
 <xsl:value-of select="$ent"/>
  </xsl:otherwise>
  </xsl:choose>
 </xsl:variable>
 <xsl:if test="not($File='')">
  <fo:external-graphic>
    <xsl:attribute name="src">
      <xsl:if test="not(starts-with($File,'./'))">
        <xsl:value-of select="$graphicsPrefix"/>
      </xsl:if>
      <xsl:value-of select="$File"/>
      <xsl:if test="not(contains($File,'.'))">
        <xsl:value-of select="$graphicsSuffix"/>
      </xsl:if>
    </xsl:attribute>
 <xsl:choose>
  <xsl:when test="@scale">
      <xsl:attribute name="content-width">
         <xsl:value-of select="@scale * 100"/><xsl:text>%</xsl:text>
      </xsl:attribute>
  </xsl:when>
  <xsl:when test="@width and not(@width=0)">
      <xsl:attribute name="content-width">
         <xsl:value-of select="@width"/>
      </xsl:attribute>
  </xsl:when>
  <xsl:when test="@height and not(@height=0)">
      <xsl:attribute name="content-height">
         <xsl:value-of select="@height"/>
      </xsl:attribute>
  </xsl:when>
  <xsl:when test="$autoScaleFigures">
      <xsl:attribute name="content-width">
          <xsl:value-of select="$autoScaleFigures"/></xsl:attribute>
  </xsl:when>
  </xsl:choose>
 </fo:external-graphic>
 </xsl:if>
 </fo:block>
 <fo:block>
   <xsl:call-template name="figureCaptionstyle"/>
     <xsl:value-of select="$figureWord"/>
     <xsl:call-template name="calculateFigureNumber"/>
     <xsl:text>. </xsl:text>
      <xsl:apply-templates select="tei:head"/>
  </fo:block>
 </fo:float>
</xsl:template>

<xsl:template match='tei:figure' mode="xref">
   <xsl:if test="$xrefShowTitle">
     <xsl:value-of select="$figureWord"/>
     <xsl:text> </xsl:text>
   </xsl:if>
   <xsl:call-template name="calculateFigureNumber"/>
   <xsl:if test="$xrefShowHead='true'">
     <xsl:if test="head">
       <xsl:text> (</xsl:text>
         <xsl:apply-templates select="tei:head"/>
       <xsl:text>)</xsl:text>
     </xsl:if>
   </xsl:if>
   <xsl:if test="$xrefShowPage='true'">
    on page
   <fo:page-number-citation>
    <xsl:attribute name="ref-id">
      <xsl:call-template name="idLabel"/>     
    </xsl:attribute>
    </fo:page-number-citation> 
   </xsl:if>
</xsl:template>

<xsl:template match="tei:figure[@rend='inline']">
 <xsl:variable name="File">
  <xsl:choose> 
  <xsl:when test="@file">
   <xsl:value-of select="@file"/>
  </xsl:when>
  <xsl:when test="@url">
   <xsl:value-of select="@url"/>
  </xsl:when>
  <xsl:otherwise>
 <xsl:value-of select="substring-after(unparsed-entity-uri(@entity),'file:')"/>
  </xsl:otherwise>
  </xsl:choose>
 </xsl:variable>
  <fo:external-graphic text-align="relative">
   <xsl:attribute name="id">
        <xsl:call-template name="idLabel"/>
   </xsl:attribute>
    <xsl:attribute name="src">
      <xsl:if test="not(starts-with($File,'./'))">
        <xsl:value-of select="$graphicsPrefix"/>
      </xsl:if>
      <xsl:value-of select="$File"/>
      <xsl:if test="not(contains($File,'.'))">
        <xsl:value-of select="$graphicsSuffix"/>
      </xsl:if>
    </xsl:attribute>
 <xsl:choose>
  <xsl:when test="@scale">
      <xsl:attribute name="content-width">
        <xsl:value-of select="@scale * 100"/><xsl:text>%</xsl:text>
      </xsl:attribute>
  </xsl:when>
  <xsl:when test="@width">
      <xsl:attribute name="content-width">
        <xsl:value-of select="@width"/>
      </xsl:attribute>
  </xsl:when> 
  <xsl:when test="@height">
      <xsl:attribute name="content-height">
        <xsl:value-of select="@height"/>
      </xsl:attribute>
  </xsl:when>
 <xsl:when test="$autoScaleFigures">
      <xsl:attribute name="content-width">
          <xsl:value-of select="$autoScaleFigures"/></xsl:attribute>
  </xsl:when>
 </xsl:choose>
 </fo:external-graphic>
<xsl:choose>
 <xsl:when test="$captionInlinefigures">
 <fo:block>
     <xsl:call-template name="figureCaptionstyle"/>
     <xsl:text>Figure </xsl:text>
     <xsl:call-template name="calculateFigureNumber"/>
      <xsl:text>. </xsl:text>
      <xsl:apply-templates select="tei:head"/>
  </fo:block>
 </xsl:when>
 <xsl:otherwise>
  <xsl:if test="head">
   <fo:block text-align="center">
      <xsl:apply-templates select="tei:head"/>
  </fo:block>
 </xsl:if>
 </xsl:otherwise>
</xsl:choose>
</xsl:template>


<xsl:template name="calculateFigureNumber">
     <xsl:number  from="text" level="any"/>
</xsl:template>

<xsl:template match="tei:figDesc"/>

</xsl:stylesheet>
