<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet exclude-result-prefixes="a t xd tei fo rng xs"
  version="2.0"
  xmlns:a="http://relaxng.org/ns/compatibility/annotations/1.0"
  xmlns:fo="http://www.w3.org/1999/XSL/Format"
  xmlns:rng="http://relaxng.org/ns/structure/1.0"
  xmlns:t="http://www.thaiopensource.com/ns/annotations"
  xmlns:tei="http://www.tei-c.org/ns/1.0"
  xmlns="http://www.tei-c.org/ns/1.0"
  xmlns:xd="http://www.pnp-software.com/XSLTdoc"
  xmlns:xs="http://www.w3.org/2001/XMLSchema"
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform">

<xsl:template match="tei:table[@rend='wovenodd' or @rend='attDef']" >
  <list type="gloss">
    <xsl:apply-templates/>
  </list>
</xsl:template>

<xsl:template match="tei:table[@rend='valList' 
     or @rend='attList' 
     or @rend='specDesc']">
  <list type="gloss">
    <xsl:apply-templates/>
  </list>
</xsl:template>

<xsl:template match="tei:table[@rend='wovenodd' 
    or @rend='attList' 
    or @rend='valList' 
    or @rend='attDef' 
    or @rend='specDesc']/tei:row">
  <xsl:apply-templates/>
</xsl:template>

<xsl:template match="tei:table[@rend='wovenodd' 
     or @rend='attList' 
     or @rend='specDesc' 
     or @rend='valList' 
     or @rend='attDef']/tei:row/tei:cell[1]">

<xsl:choose>
  <xsl:when test="parent::tei:row/parent::tei:table[@rend='attList']">
    <label>@<xsl:apply-templates/></label>
  </xsl:when>
  <xsl:when test="ancestor::tei:table[@rend='valList']">
    <label><xsl:apply-templates/></label>
  </xsl:when>
  <xsl:when test="ancestor::tei:table[@rend='specDesc']">
    <label>@<xsl:apply-templates/></label>
  </xsl:when>
  <xsl:when test="@cols='2' and not(parent::tei:row/preceding-sibling::tei:row)">
   <label></label>
   <hi><xsl:value-of select="ancestor::tei:div[1]/@xml:id"/></hi>
    <xsl:apply-templates/>
  </xsl:when>
  <xsl:when test="@cols='2'">
    <label></label>
    <xsl:apply-templates/>
  </xsl:when>
  <xsl:otherwise>
    <label>
      <xsl:apply-templates/>
    </label>
  </xsl:otherwise>
</xsl:choose>
</xsl:template>

<xsl:template match="tei:div[@type='refdoc']/tei:head"/>

<xsl:template match="tei:div[@type='refdoc']">
<xsl:apply-templates/>
</xsl:template>

<xsl:template match="tei:table[@rend='wovenodd' 
      or @rend='attList' 
      or @rend='valList' 
      or @rend='specDesc' 
      or @rend='attDef']/tei:row/tei:cell[2]">
  <item>
    <xsl:apply-templates/>
  </item>
</xsl:template>

<!-- identity transform -->
<xsl:output method="xml" indent="yes"/>

<xsl:template match="@*|text()|comment()|processing-instruction()">
 <xsl:copy-of select="."/>
</xsl:template>


<xsl:template match="*">
  <xsl:copy>
    <xsl:apply-templates 
	select="*|@*|processing-instruction()|comment()|text()"/>
  </xsl:copy>
</xsl:template>



</xsl:stylesheet>