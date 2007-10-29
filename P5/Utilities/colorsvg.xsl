<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet 
    xmlns:xlink="http://www.w3.org/1999/xlink"
    xmlns:svg="http://www.w3.org/2000/svg" 
    xmlns="http://www.w3.org/2000/svg" 
    xmlns:dbk="http://docbook.org/ns/docbook"
    xmlns:html="http://www.w3.org/1999/xhtml"
    xmlns:its="http://www.w3.org/2005/11/its"
    xmlns:eg="http://www.tei-c.org/ns/Examples"
    xmlns:xd="http://www.pnp-software.com/XSLTdoc"
    xmlns:a="http://relaxng.org/ns/compatibility/annotations/1.0"
    xmlns:rng="http://relaxng.org/ns/structure/1.0"
    xmlns:xs="http://www.w3.org/2001/XMLSchema" 
    xmlns:tei="http://www.tei-c.org/ns/1.0"
    xmlns:teix="http://www.tei-c.org/ns/Examples"
    xmlns:local="http://www.pantor.com/ns/local"
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    xmlns:fo="http://www.w3.org/1999/XSL/Format"
    xmlns:edate="http://exslt.org/dates-and-times"
    xmlns:exsl="http://exslt.org/common"
    xmlns:estr="http://exslt.org/strings"
    xmlns:spec="http://example.com/xmlspec"
    exclude-result-prefixes="exsl estr edate fo a xd tei rng local teix xs eg its html dbk xlink" 
    extension-element-prefixes="edate exsl estr spec"
    version="1.0">

<xsl:output method="xml" omit-xml-declaration="yes" encoding="utf-8"/>

<xsl:key name="E" match="tei:*" use="@ident"/>
<xsl:template match="@*|processing-instruction()|comment()|text()">
  <xsl:copy/>
</xsl:template>

<xsl:template match="*">
  <xsl:copy>
    <xsl:apply-templates
	select="*|@*|processing-instruction()|comment()|text()"/>
  </xsl:copy>
</xsl:template>


<xsl:template match="svg:a">
<xsl:apply-templates/>
</xsl:template>
<xsl:template match="svg:text">
    <xsl:variable name="me">
      <xsl:choose>
	<xsl:when test="contains(.,'_')">
	  <xsl:value-of select="substring-before(normalize-space(.),'_')"/>
	</xsl:when>
	<xsl:otherwise>
	  <xsl:value-of select="normalize-space(.)"/>
	</xsl:otherwise>
      </xsl:choose>
    </xsl:variable>

    <xsl:variable name="module">
      <xsl:variable name="file">
	<xsl:value-of select="concat($me,'.xml')"/>
      </xsl:variable>
      <xsl:choose>
	<xsl:when test="starts-with($me,'high')"/>
	<xsl:when test="starts-with($me,'medium')"/>
	<xsl:when test="starts-with($me,'low')"/>
	<xsl:when test="starts-with($me,'unknown')"/>
	<xsl:when test="starts-with($me,'0')"/>
	<xsl:when test="starts-with($me,'1')"/>
	<xsl:when test="starts-with($me,'2')"/>
	<xsl:when test="starts-with($me,'9')"/>
	<xsl:when test="starts-with($me,'inapplicable')"/>
	<xsl:when test="$me='empty'"/>
	<xsl:when test="$me='cdata'"/>
	<xsl:when test="starts-with($me,'XSD')"/>
	<xsl:otherwise>
	  <xsl:for-each select="document($file)/*">
	  <xsl:value-of select="@module"/>
	</xsl:for-each>
	</xsl:otherwise>
      </xsl:choose>
    </xsl:variable>

<xsl:variable name="t">
<xsl:copy>
  <xsl:copy-of select="@x"/>
  <xsl:copy-of select="@y"/>
  <xsl:attribute name="style">
    <xsl:text>fill:#000000;text-anchor:start;</xsl:text>
    <xsl:text>font-family:Times</xsl:text>
    <xsl:text>;</xsl:text>
    <xsl:text>font-style:</xsl:text>
    <xsl:choose>
      <xsl:when test="starts-with($me,'model.')">normal</xsl:when>
      <xsl:when test="starts-with($me,'atts.')">normal</xsl:when>
      <xsl:when test="starts-with($me,'macro.')">italic</xsl:when>
      <xsl:when test="$module=''">normal</xsl:when>
      <xsl:otherwise>normal</xsl:otherwise>
    </xsl:choose>
    <xsl:text>;</xsl:text>
    <xsl:text>font-weight:</xsl:text>
    <xsl:choose>
      <xsl:when test="starts-with($me,'model.')">normal</xsl:when>
      <xsl:when test="starts-with($me,'atts.')">normal</xsl:when>
      <xsl:when test="starts-with($me,'macro.')">normal</xsl:when>
      <xsl:when test="$module=''">normal</xsl:when>
      <xsl:otherwise>bold</xsl:otherwise>
    </xsl:choose>
    <xsl:text>;</xsl:text>
    <xsl:text>font-size:200pt;</xsl:text>
    <xsl:text>fill:</xsl:text>
    <xsl:choose>
      <xsl:when test="$module='core'">black</xsl:when>
      <xsl:when test="$module='tei'">black</xsl:when>
      <xsl:when test="$module='textstructure'">blue</xsl:when>
      <xsl:when test="$module='header'">green</xsl:when>
      <xsl:when test="$module=''">#B0B0B0</xsl:when>
      <xsl:otherwise>red</xsl:otherwise>
    </xsl:choose>
    <xsl:text>;stroke:</xsl:text>
    <xsl:choose>
      <xsl:when test="$module='core'">black</xsl:when>
      <xsl:when test="$module='tei'">black</xsl:when>
      <xsl:when test="$module='textstructure'">blue</xsl:when>
      <xsl:when test="$module='header'">green</xsl:when>
      <xsl:when test="$module=''">#B0B0B0</xsl:when>
      <xsl:otherwise>red</xsl:otherwise>
    </xsl:choose>
  </xsl:attribute>
  <xsl:apply-templates/>
</xsl:copy>
</xsl:variable>
<xsl:choose>
<xsl:when test="starts-with($me,'high')">
  <xsl:copy-of select="$t"/>
</xsl:when>
<xsl:when test="starts-with($me,'medium')">
  <xsl:copy-of select="$t"/>
</xsl:when>
<xsl:when test="starts-with($me,'low')">
  <xsl:copy-of select="$t"/>
</xsl:when>
<xsl:when test="starts-with($me,'unknown')">
  <xsl:copy-of select="$t"/>
</xsl:when>
<xsl:when test="starts-with($me,'0')">
  <xsl:copy-of select="$t"/>
</xsl:when>
<xsl:when test="starts-with($me,'1')">
  <xsl:copy-of select="$t"/>
</xsl:when>
<xsl:when test="starts-with($me,'2')">
  <xsl:copy-of select="$t"/>
</xsl:when>
<xsl:when test="starts-with($me,'9')">
  <xsl:copy-of select="$t"/>
</xsl:when>
<xsl:when test="starts-with($me,'inapplicable')">
  <xsl:copy-of select="$t"/>
</xsl:when>
<xsl:when test="starts-with($me,'XSD')">
  <xsl:copy-of select="$t"/>
</xsl:when>
<xsl:when test="$me='empty'">
  <xsl:copy-of select="$t"/>
</xsl:when>
<xsl:when test="$me='cdata'">
  <xsl:copy-of select="$t"/>
</xsl:when>
<xsl:otherwise>
  <svg:a xlink:href="{$me}.svg">
    <xsl:copy-of select="$t"/>
  </svg:a>
</xsl:otherwise>
</xsl:choose>
</xsl:template>

</xsl:stylesheet>
