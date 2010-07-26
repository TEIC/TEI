<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet xmlns:tei="http://www.tei-c.org/ns/1.0"
                xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                version="2.0">
  <xsl:output method="text"/>
  <xsl:param name="DIR"/>
  <xsl:param name="ORIG">.</xsl:param>

  <xsl:key match="tei:graphic|tei:pb[@facs]" use="1" name="G"/>

  <xsl:template match="/">
      <xsl:for-each select="key('G',1)">
         <xsl:variable name="F">
            <xsl:value-of select="@url|@facs"/>
         </xsl:variable>
	 <xsl:variable name="target">
	   <xsl:value-of select="$DIR"/>
	   <xsl:text>/image</xsl:text>
	   <xsl:if test="self::tei:pb">pb</xsl:if>
	   <xsl:number level="any"/>
	   <xsl:text>.</xsl:text>
	   <xsl:value-of select="tokenize($F,'\.')[last()]"/>
	 </xsl:variable>
         <xsl:choose>
	   <xsl:when test="starts-with($F,'http')">
	     <xsl:text>curl -s -o </xsl:text>
	     <xsl:value-of select="$target"/>
	     <xsl:text> </xsl:text>
	     <xsl:value-of select="$F"/> 
	   </xsl:when>
	   <xsl:when test="starts-with($F,'/')">
	     <xsl:text>cp </xsl:text>
	     <xsl:value-of select="$F"/> 
	     <xsl:text> </xsl:text>
	     <xsl:value-of select="$target"/>
	   </xsl:when>
	   <xsl:otherwise> 
	     <xsl:text>cp </xsl:text>
	     <xsl:value-of select="$ORIG"/>
	     <xsl:text>/</xsl:text>
	     <xsl:value-of select="$F"/> 
	     <xsl:text> </xsl:text>
	     <xsl:value-of select="$target"/>
	   </xsl:otherwise>
         </xsl:choose>
         <xsl:text>&#10;</xsl:text>
      </xsl:for-each>
  </xsl:template>
</xsl:stylesheet>