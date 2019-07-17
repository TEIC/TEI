<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" 
                xmlns="http://www.tei-c.org/ns/1.0"
                xmlns:tei="http://www.tei-c.org/ns/1.0"
		exclude-result-prefixes="tei"
		version="2.0"
		xpath-default-namespace="http://www.tei-c.org/ns/tite/1.0">

   <xsl:output cdata-section-elements="eg"/>

<xsl:template match="tei:text">
  <TEI>
    <teiHeader>
    <fileDesc>
      <titleStmt>
        <title>[converted from Tite]</title>
      </titleStmt>
      <publicationStmt>
        <p>unknown</p>
      </publicationStmt>
      <sourceDesc>
        <p>converted from Tite on 
	<date><xsl:value-of select="format-dateTime(current-dateTime(),'[Y]-[M02]-[D02]T[H02]:[m02]:[s02]Z')"/></date></p>
      </sourceDesc>
    </fileDesc>
      
    </teiHeader>
    <text>
    <xsl:apply-templates 
	select="@*|*|text()|comment()|processing-instruction()"/>  
    </text>
  </TEI>
</xsl:template>

<xsl:template
    match="tei:div1|tei:div2|tei:div3|tei:div4|tei:div5|tei:div6|tei:div7">
  <div>
    <xsl:apply-templates 
	select="@*|*|text()|comment()|processing-instruction()"/>  
  </div>
</xsl:template>

<xsl:template name="b">
  <hi rend="bold">
    <xsl:apply-templates 
	select="@*|*|text()|comment()|processing-instruction()"/>  
  </hi>
</xsl:template>

<xsl:template name="i">
  <hi rend="italic">
    <xsl:apply-templates 
	select="@*|*|text()|comment()|processing-instruction()"/>  
  </hi>
</xsl:template>

<xsl:template name="ul">
  <hi rend="underline">
    <xsl:apply-templates 
	select="@*|*|text()|comment()|processing-instruction()"/>  
  </hi>
</xsl:template>

<xsl:template name="sup">
  <hi rend="sup">
    <xsl:apply-templates 
	select="@*|*|text()|comment()|processing-instruction()"/>  
  </hi>
</xsl:template>

<xsl:template name="sub">
  <hi rend="sub">
    <xsl:apply-templates 
	select="@*|*|text()|comment()|processing-instruction()"/>  
  </hi>
</xsl:template>

<xsl:template name="smcap">
  <hi rend="smcap">
    <xsl:apply-templates 
	select="@*|*|text()|comment()|processing-instruction()"/>  
  </hi>
</xsl:template>

<xsl:template name="ornament">
  <figure place="inline">
    <xsl:apply-templates 
	select="@*"/>
    <xsl:choose>
      <xsl:when test="*">
	<xsl:apply-templates 
	    select="*|comment()|processing-instruction()"/>  
      </xsl:when>
      <xsl:when test="string-length(.)&gt;0">
	<p>
	<xsl:apply-templates 
	    select="text()|comment()|processing-instruction()"/>  
	</p>
      </xsl:when>
      <xsl:otherwise>
	<xsl:apply-templates 
	    select="comment()|processing-instruction()"/>  
      </xsl:otherwise>
    </xsl:choose>
  </figure>
</xsl:template>

<xsl:template name="cols">
  <milestone unit="column">
    <xsl:apply-templates 
	select="@*|*|text()|comment()|processing-instruction()"/>  
  </milestone>
</xsl:template>

   <xsl:template match="*">
      <xsl:copy>
         <xsl:apply-templates select="*|@*|processing-instruction()|comment()|text()"/>
      </xsl:copy>
   </xsl:template>
   <xsl:template match="text()|comment()|@*|processing-instruction()">
      <xsl:copy/>
   </xsl:template>
   <xsl:template match="b">
      <xsl:call-template name="b"/>
   </xsl:template>
   <xsl:template match="i">
      <xsl:call-template name="i"/>
   </xsl:template>
   <xsl:template match="ul">
      <xsl:call-template name="ul"/>
   </xsl:template>
   <xsl:template match="sub">
      <xsl:call-template name="sub"/>
   </xsl:template>
   <xsl:template match="sup">
      <xsl:call-template name="sup"/>
   </xsl:template>
   <xsl:template match="smcap">
      <xsl:call-template name="smcap"/>
   </xsl:template>
   <xsl:template match="cols|colShift">
      <xsl:call-template name="cols"/>
   </xsl:template>
   <xsl:template match="ornament">
      <xsl:call-template name="ornament"/>
   </xsl:template>
</xsl:stylesheet>
