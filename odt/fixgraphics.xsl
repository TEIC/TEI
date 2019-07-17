<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet xmlns:tei="http://www.tei-c.org/ns/1.0" xmlns:xs="http://www.w3.org/2001/XMLSchema"
                xmlns:tiff="http://ns.adobe.com/tiff/1.0/"
                xmlns:teidocx="http://www.tei-c.org/ns/teidocx/1.0"
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

  <xsl:output method="xml" indent="yes"/>
  <xsl:param name="DIR"/>
  <xsl:key name="W" match="tiff:ImageWidth" use="1"/>
  <xsl:key name="H" match="tiff:ImageLength" use="1"/>
  <xsl:template match="@*|text()|comment()|processing-instruction()">
      <xsl:copy-of select="."/>
  </xsl:template>
  
  
  <xsl:template match="*">
      <xsl:copy>
         <xsl:apply-templates select="*|@*|processing-instruction()|comment()|text()"/>
      </xsl:copy>
  </xsl:template>
  
  
  <xsl:template match="tei:graphic|tei:media">
      <xsl:copy>
	        <xsl:variable name="newName">
	           <xsl:text>Pictures/resource</xsl:text>
	           <xsl:number level="any"/>
	           <xsl:text>.</xsl:text>
	           <xsl:value-of select="tokenize(@url,'\.')[last()]"/>
	        </xsl:variable>
	        <xsl:attribute name="url">
	           <xsl:value-of select="$newName"/>
	        </xsl:attribute>
	        <xsl:copy-of select="@n"/>
	        <xsl:copy-of select="@height"/>
	        <xsl:copy-of select="@width"/>
	        <xsl:copy-of select="@scale"/>
		
	        <xsl:if test="doc-available(concat($DIR,'/',$newName,'.xmp'))">
	           <xsl:attribute name="tei:width">
	              <xsl:for-each select="document(concat($DIR,'/',$newName,'.xmp'),/)">
	                 <xsl:value-of select="(number(key('W',1)) div 72) * 9144"/>
	              </xsl:for-each>
	           </xsl:attribute>
	  
	           <xsl:attribute name="tei:height">
	              <xsl:for-each select="document(concat($DIR,'/',$newName,'.xmp'),/)">
	                 <xsl:value-of select="(number(key('H',1)) div 72) * 9144"/>
	              </xsl:for-each>
	           </xsl:attribute>
	        </xsl:if>
      </xsl:copy>
  </xsl:template>
</xsl:stylesheet>
