<?xml version="1.0" encoding="utf-8"?>
<!-- 
Text Encoding Initiative Consortium XSLT stylesheet family
$Date$, $Revision$, $Author$

XSL stylesheet to format TEI XML documents to HTML or XSL FO

 
##LICENSE
--> 
<xsl:stylesheet
  xmlns:tei="http://www.tei-c.org/ns/1.0"

  xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0"
  >
<!-- elaborated by Nick  Nicholas <nicholas@uci.edu>, March 2001 -->
<xsl:template match="tei:sp">
<dl>
 <dt>
   <xsl:if test="@id|@xml:id"><a name="{@id|@xml:id}"/></xsl:if>
   <xsl:apply-templates select="tei:speaker"/>
 </dt>
<dd><xsl:apply-templates select="tei:p | tei:l | tei:lg | tei:seg | tei:ab | tei:stage"/></dd>
</dl>
</xsl:template>

<!-- paragraphs inside speeches do very little-->
 <xsl:template match="tei:sp/tei:p">
    <xsl:apply-templates/>
</xsl:template>


<xsl:template match="tei:p/tei:stage">
<em><xsl:apply-templates/></em>
</xsl:template>

<xsl:template match="tei:stage">
<p><em>
      <xsl:apply-templates/>
</em></p>
</xsl:template>

<xsl:template match="tei:castList">
<ul>
	<xsl:apply-templates/>
</ul>
</xsl:template>

<xsl:template match="tei:castGroup">
<ul>
	<xsl:apply-templates/>
</ul>
</xsl:template>

<xsl:template match="tei:castItem">
<li>
	<xsl:apply-templates/>
</li>
</xsl:template>

<xsl:template match="tei:role">
<strong>
	<xsl:apply-templates/>
</strong>
</xsl:template>

<xsl:template match="tei:roleDesc">
<blockquote>
	<xsl:apply-templates/>
</blockquote>
</xsl:template>

<xsl:template match="tei:actor">
<em>
	<xsl:apply-templates/>
</em>
</xsl:template>

<xsl:template match="tei:set">
<em>
	<xsl:apply-templates/>
</em>
</xsl:template>

<xsl:template match="tei:view">
<em>
	<xsl:apply-templates/>
</em>
</xsl:template>

<xsl:template match="tei:camera">
<em>
	<xsl:apply-templates/>
</em>
</xsl:template>

<xsl:template match="tei:caption">
<em>
	<xsl:apply-templates/>
</em>
</xsl:template>

<xsl:template match="tei:sound">
<em>
	<xsl:apply-templates/>
</em>
</xsl:template>

<xsl:template match="tei:tech">
<em>
	<xsl:apply-templates/>
</em>
</xsl:template>


</xsl:stylesheet>


