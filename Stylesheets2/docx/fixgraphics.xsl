<?xml version="1.0"?>
<xsl:stylesheet version="2.0" xmlns:tei="http://www.tei-c.org/ns/1.0"
    xmlns:xs="http://www.w3.org/2001/XMLSchema"
    xmlns:tiff='http://ns.adobe.com/tiff/1.0/'
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
  <xsl:output method="xml" indent="yes"/>
  <xsl:param name="DIR"/>
  <xsl:key name="W" match="tiff:ImageWidth" use="1"/>
  <xsl:key name="H" match="tiff:ImageLength" use="1"/>
  <xsl:template match="@*|text()|comment()|processing-instruction()">
    <xsl:copy-of select="."/>
  </xsl:template>
  
  
  <xsl:template match="*">
    <xsl:copy>
      <xsl:apply-templates 
	  select="*|@*|processing-instruction()|comment()|text()"/>
    </xsl:copy>
  </xsl:template>
  
  
  <xsl:template match="tei:graphic">
    <xsl:copy>
	<xsl:variable name="newName">
	  <xsl:choose>
	    <xsl:when test="contains(@url,'/')">
	      <xsl:value-of select="tokenize(@url, '/')[last()]"/>
	    </xsl:when>
	    <xsl:otherwise>
	      <xsl:value-of select="@url"/>
	    </xsl:otherwise>
	  </xsl:choose>
	</xsl:variable>
      <xsl:attribute name="url">
	<xsl:text>media/</xsl:text>
	<xsl:value-of select="$newName"/>
      </xsl:attribute>
 
      <xsl:attribute name="height">
	<xsl:variable name="h">
	  <xsl:for-each
	      select="document(concat($DIR,'/media/',$newName,'.xmp'),/)">
	    <xsl:value-of select="number(key('H',1))"/>
	  </xsl:for-each>
	</xsl:variable>
	<xsl:choose>
	  <xsl:when test="contains(@height,'%')">
	    <xsl:value-of select="($h *
				  (number(substring-before(@height,'%'))
				  div 100)) cast as xs:integer"/>
	    <xsl:text>pt</xsl:text>
	  </xsl:when>
	  <xsl:when test="@height">
	    <xsl:value-of select="@height"/>
	  </xsl:when>
	  <xsl:when test="contains(@width,'%')">
	    <xsl:value-of select="($h *
	      (number(substring-before(@width,'%')) div 100)) cast as xs:integer"/>
	    <xsl:text>pt</xsl:text>
	  </xsl:when>
	  <xsl:otherwise>
	    <xsl:value-of select="$h cast as xs:integer"/>
	    <xsl:text>pt</xsl:text>
	  </xsl:otherwise>
	</xsl:choose>
      </xsl:attribute>
 
     <xsl:attribute name="width">
	<xsl:variable name="h">
	  <xsl:for-each
	      select="document(concat($DIR,'/media/',$newName,'.xmp'),/)">
	    <xsl:value-of select="number(key('W',1))"/>
	  </xsl:for-each>
	</xsl:variable>
	<xsl:variable name="new">
	<xsl:choose>
	  <xsl:when test="contains(@width,'%')">
	    <xsl:value-of select="($h *
	      (number(substring-before(@width,'%')) div 100)) cast as xs:integer"/>
	    <xsl:text>pt</xsl:text>
	  </xsl:when>
	  <xsl:when test="@width">
	    <xsl:value-of select="@width"/>
	  </xsl:when>
	  <xsl:otherwise>
	    <xsl:value-of select="$h  cast as xs:integer"/>
	    <xsl:text>pt</xsl:text>
	  </xsl:otherwise>
	</xsl:choose>
	</xsl:variable>
	<xsl:message><xsl:value-of select="@width"/>, <xsl:value-of
	select="$h"/>, <xsl:value-of select="$new"/></xsl:message>
	<xsl:value-of select="$new"/>
      </xsl:attribute>
    </xsl:copy>
  </xsl:template>
</xsl:stylesheet>
