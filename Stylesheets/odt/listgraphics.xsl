<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet xmlns:tei="http://www.tei-c.org/ns/1.0"
                xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                version="2.0">
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" scope="stylesheet" type="stylesheet">
      <desc>
         <p> TEI stylesheet dealing with elements from the core module. </p>
         <p>This software is dual-licensed:

1. Distributed under a Creative Commons Attribution-ShareAlike 3.0
Unported License http://creativecommons.org/licenses/by-sa/3.0/ 

2. http://www.opensource.org/licenses/BSD-2-Clause
		


Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are
met:

* Redistributions of source code must retain the above copyright
notice, this list of conditions and the following disclaimer.

* Redistributions in binary form must reproduce the above copyright
notice, this list of conditions and the following disclaimer in the
documentation and/or other materials provided with the distribution.

This software is provided by the copyright holders and contributors
"as is" and any express or implied warranties, including, but not
limited to, the implied warranties of merchantability and fitness for
a particular purpose are disclaimed. In no event shall the copyright
holder or contributors be liable for any direct, indirect, incidental,
special, exemplary, or consequential damages (including, but not
limited to, procurement of substitute goods or services; loss of use,
data, or profits; or business interruption) however caused and on any
theory of liability, whether in contract, strict liability, or tort
(including negligence or otherwise) arising in any way out of the use
of this software, even if advised of the possibility of such damage.
</p>
         <p>Author: See AUTHORS</p>
         
         <p>Copyright: 2013, TEI Consortium</p>
      </desc>
   </doc>

  <xsl:output method="text"/>
  <xsl:param name="DIR"/>
  <xsl:param name="ORIG">.</xsl:param>

  <xsl:key match="tei:graphic|tei:media|tei:pb[@facs]" use="1" name="G"/>

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
