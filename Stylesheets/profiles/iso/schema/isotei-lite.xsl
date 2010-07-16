<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                xmlns:tei="http://www.tei-c.org/ns/1.0"
                xmlns:saxon="http://icl.com/saxon"
                xmlns:sch="http://www.ascc.net/xml/schematron"
                xmlns:xj="http://xml.apache.org/xalan/java"
                xmlns:loc="http://www.thaiopensource.com/ns/location"
                xmlns:err="http://www.thaiopensource.com/ns/error"
                version="2.0">
   <xsl:output method="text"/>
   <xsl:template match="/">
      <xsl:apply-templates select="/" mode="all"/>
   </xsl:template>
   <xsl:template match="*|/" mode="all">
      <xsl:apply-templates select="*" mode="all"/>
   </xsl:template>
   <xsl:template name="location"/>
   <xsl:template match="node() | @*" mode="schematron-get-full-path-2">
      <xsl:text>

* section </xsl:text>
      <xsl:for-each select="ancestor-or-self::tei:div">/<xsl:number level="multiple"/>
         <xsl:text> - </xsl:text>
         <xsl:value-of select="translate(substring(tei:head,1,20),' ',' ')"/>
      </xsl:for-each>
      <xsl:text> (element </xsl:text>
      <xsl:value-of select="local-name()"/>
      <xsl:text>)

</xsl:text>
   </xsl:template>
</xsl:stylesheet>