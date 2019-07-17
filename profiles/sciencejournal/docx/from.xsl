<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                xmlns="http://www.tei-c.org/ns/1.0"
                xmlns:xs="http://www.w3.org/2001/XMLSchema"
                xmlns:iso="http://www.iso.org/ns/1.0"
                xmlns:teidocx="http://www.tei-c.org/ns/teidocx/1.0"
                xmlns:tei="http://www.tei-c.org/ns/1.0"
                xmlns:ve="http://schemas.openxmlformats.org/markup-compatibility/2006"
                xmlns:o="urn:schemas-microsoft-com:office:office"
                xmlns:r="http://schemas.openxmlformats.org/officeDocument/2006/relationships"
                xmlns:rel="http://schemas.openxmlformats.org/package/2006/relationships"
                xmlns:m="http://schemas.openxmlformats.org/officeDocument/2006/math"
                xmlns:v="urn:schemas-microsoft-com:vml"
                xmlns:wp="http://schemas.openxmlformats.org/drawingml/2006/wordprocessingDrawing"
                xmlns:a="http://schemas.openxmlformats.org/drawingml/2006/main"
                xmlns:pic="http://schemas.openxmlformats.org/drawingml/2006/picture"
                xmlns:w10="urn:schemas-microsoft-com:office:word"
                xmlns:w="http://schemas.openxmlformats.org/wordprocessingml/2006/main"
                xmlns:wne="http://schemas.microsoft.com/office/word/2006/wordml"
                xmlns:mml="http://www.w3.org/1998/Math/MathML"
                xmlns:tbx="http://www.lisa.org/TBX-Specification.33.0.html"
                version="2.0"
                exclude-result-prefixes="#all">
    <!-- import base conversion style -->


    <xsl:import href="../../default/docx/from.xsl"/>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" scope="stylesheet" type="stylesheet">
    <desc>
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
    
  <xsl:param name="preserveEffects">true</xsl:param>
  <xsl:param name="preserveSpace">true</xsl:param>
  <xsl:param name="preserveFontSizeChanges">true</xsl:param>

 <xsl:template match="tei:TEI" mode="pass2">
  <xsl:variable name="pass2">
   <xsl:copy>
    <xsl:apply-templates mode="pass2"/>
   </xsl:copy>
  </xsl:variable>
  <xsl:apply-templates select="$pass2" mode="pass3"/>
 </xsl:template>

  <xsl:template name="fromDocxEffectsHook">
    <xsl:if test="w:rPr/w:rStyle/@w:val='Heading 2 Char' and not(w:rPr/w:b/@w:val='0')">
      <n>bold</n>
    </xsl:if>
  </xsl:template>

   
    <doc type="function" xmlns="http://www.oxygenxml.com/ns/doc/xsl"  >
      <desc>Defines whether or not a word paragraph is a  heading 
      </desc>
    </doc>
    
    <xsl:function name="tei:is-heading" as="xs:boolean">
      <xsl:param name="p"/>
      <xsl:variable name="s" select="$p/w:pPr/w:pStyle/@w:val"/>
      <xsl:choose>
	<xsl:when test="$s=''">false</xsl:when>
	<xsl:when test="starts-with($s,'heading')">true</xsl:when>
	<xsl:when test="starts-with($s,'Heading')">true</xsl:when>
	<xsl:when test="$s='Subtitle'">true</xsl:when>
	<xsl:otherwise>false</xsl:otherwise>
      </xsl:choose>
    </xsl:function>

    <xsl:function name="tei:heading-level" as="xs:string">
      <xsl:param name="p"/>
      <xsl:analyze-string select="$p/w:pPr/w:pStyle/@w:val" regex="[^0-9]*([0-9])">
	<xsl:matching-substring>
	  <xsl:value-of select="number(regex-group(1))"/>
	</xsl:matching-substring>
	<xsl:non-matching-substring>
	  <xsl:choose>
	    <xsl:when test="regex-group(0)='Subtitle'">2</xsl:when>
	    <xsl:otherwise>1</xsl:otherwise>
	  </xsl:choose>
	</xsl:non-matching-substring>
      </xsl:analyze-string>
    </xsl:function>
    
    <doc type="function" xmlns="http://www.oxygenxml.com/ns/doc/xsl"  >
      <desc>Is given a header style and returns the style for the next level
	header 
      </desc>
    </doc>
    
    <xsl:function name="tei:get-nextlevel-header" as="xs:string">
      <xsl:param name="current-header"/>
      <xsl:choose>
	<xsl:when test="$current-header='Title'">
	  <xsl:text>Subtitle</xsl:text>
	</xsl:when>
	<xsl:otherwise>
	  <xsl:value-of select="translate($current-header,'12345678','23456789')"/>
	</xsl:otherwise>
      </xsl:choose>
    </xsl:function>

   <xsl:template match="tei:teiHeader/tei:fileDesc/tei:titleStmt/tei:title" mode="pass2">
     <xsl:copy>
    <xsl:choose>
      <xsl:when test="string-length(.)=0">
      <xsl:for-each
	  select="ancestor::tei:TEI/tei:text/tei:body/tei:p[@rend =
		  'Title'][1]" >
	<xsl:apply-templates mode="pass2"/>
      </xsl:for-each>
      </xsl:when>
      <xsl:otherwise>
	<xsl:apply-templates mode="pass2"/>
      </xsl:otherwise>
    </xsl:choose>
     </xsl:copy>
   </xsl:template>

   <!-- try to remove unnecessary anchor elements -->
   <xsl:template match="tei:anchor" mode="pass3">
     <xsl:choose>
       <xsl:when test="preceding-sibling::node()  or parent::*/@xml:id">
	 <xsl:copy-of select="."/>
       </xsl:when>
       <xsl:otherwise>
	 <xsl:copy-of select="@xml:id"/>
       </xsl:otherwise>
     </xsl:choose>
   </xsl:template>
 <!-- and copy everything else -->

 <xsl:template match="@*|comment()|processing-instruction()|text()" mode="pass3">
  <xsl:copy-of select="."/>
 </xsl:template>
 <xsl:template match="*" mode="pass3">
  <xsl:copy>
   <xsl:apply-templates select="*|@*|processing-instruction()|comment()|text()" mode="pass3"/>
  </xsl:copy>
 </xsl:template>

  </xsl:stylesheet>
  
