<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet xmlns="http://www.tei-c.org/ns/1.0" xmlns:teix="http://www.tei-c.org/ns/Examples"
                xmlns:s="http://www.ascc.net/xml/schematron" xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                xmlns:tei="http://www.tei-c.org/ns/1.0" xmlns:t="http://www.thaiopensource.com/ns/annotations"
                xmlns:xs="http://www.w3.org/2001/XMLSchema"
                xmlns:a="http://relaxng.org/ns/compatibility/annotations/1.0"
                xmlns:rng="http://relaxng.org/ns/structure/1.0"
                exclude-result-prefixes="#all" version="2.0">
  <!--
      Look for <classRef> elements which need to generate parenthesized parameter entity references in a DTD 
      and wrap them in a <sequence> element so that teitodtd will add the parentheses. 
      
      Parens are added 
      (a) if the classRef is a direct child of <content> (e.g. vNot) 
      (b) if the classRef is directly inside a sequence but does not itself repeat
      
      LB, 2015-08-31 -->
  
  <xsl:template match="tei:content/tei:classRef | tei:content//tei:sequence/tei:classRef">
    <xsl:variable name="maxOccurs" select="( @maxOccurs, '1' )[1]"/>
    <xsl:choose>
      <xsl:when test="$maxOccurs eq 'unbounded'">
        <xsl:copy-of select="."/>
      </xsl:when>
      <xsl:when test="$maxOccurs cast as xs:integer gt 1">
        <xsl:copy-of select="."/>
      </xsl:when>
      <xsl:otherwise>
        <sequence>
          <xsl:copy-of select="."/>
        </sequence>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>

  <xsl:template match="*">
    <xsl:copy>
      <xsl:apply-templates select="@* | node()"/>
    </xsl:copy>
  </xsl:template>

  <xsl:template match="@* | text() | comment() | processing-instruction()">
    <xsl:copy/>
  </xsl:template>

</xsl:stylesheet>
