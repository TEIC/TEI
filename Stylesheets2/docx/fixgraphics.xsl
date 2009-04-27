<?xml version="1.0"?>
<xsl:stylesheet version="2.0" xmlns:tei="http://www.tei-c.org/ns/1.0"
    xmlns:xs="http://www.w3.org/2001/XMLSchema"
    xmlns:tiff='http://ns.adobe.com/tiff/1.0/'
    xmlns:teidocx="http://www.tei-c.org/ns/teidocx/1.0"
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
	  <xsl:text>media/image</xsl:text>
	  <xsl:number level="any"/>
	  <xsl:text>.</xsl:text>
	  <xsl:value-of select="tokenize(@url,'\.')[last()]"/>
	</xsl:variable>
	<xsl:attribute name="url">
	  <xsl:value-of select="$newName"/>
	</xsl:attribute>
	<xsl:copy-of select="@height"/>
	<xsl:copy-of select="@width"/>
	<xsl:copy-of select="@scale"/>

	<xsl:attribute name="teidocx:width">
	  <xsl:for-each
	      select="document(concat($DIR,'/',$newName,'.xmp'),/)">
	    <xsl:value-of select="(number(key('W',1)) div 72) * 9144"/>
	  </xsl:for-each>
	</xsl:attribute>

	<xsl:attribute name="teidocx:height">
	  <xsl:for-each
	      select="document(concat($DIR,'/',$newName,'.xmp'),/)">
	    <xsl:value-of select="(number(key('H',1)) div 72) * 9144"/>
	  </xsl:for-each>
	</xsl:attribute>
    </xsl:copy>
  </xsl:template>
</xsl:stylesheet>
