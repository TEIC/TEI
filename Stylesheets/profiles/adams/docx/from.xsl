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
                exclude-result-prefixes="ve o r m v wp w10 w wne mml tbx pic rel a         tei teidocx xs iso">
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
    

    <xsl:template match="w:p[w:pPr/w:pStyle/@w:val='ITLP Caption']" mode="paragraph">
        <head>
	  <xsl:apply-templates/>
        </head>
    </xsl:template>

    <xsl:template match="w:p[w:pPr/w:pStyle/@w:val='ITLP Table Heading']" mode="paragraph">
        <head>
	  <xsl:apply-templates/>
        </head>
    </xsl:template>
    <xsl:template match="w:p[w:pPr/w:pStyle/@w:val='ITLP Ex Tasks Bulleted']" mode="paragraph">
        <item>
	  <xsl:apply-templates/>
        </item>
    </xsl:template>
    <xsl:template match="w:p[w:pPr/w:pStyle/@w:val='ITLP BodyText Bulletted']" mode="paragraph">
        <item>
	  <xsl:apply-templates/>
        </item>
    </xsl:template>

    <doc type="function" xmlns="http://www.oxygenxml.com/ns/doc/xsl"  >
      <desc>Returns a listtype for a given stylename (return empty string to
	figure it out dynamically)
      </desc>
    </doc>
    
    <xsl:function name="tei:get-listtype" as="xs:string">
      <xsl:param name="style"/>
      <xsl:choose>
	<xsl:when test="$style='ITLP BodyText Bulletted'">
	  <xsl:text>unordered</xsl:text>
	</xsl:when>
	<xsl:otherwise>
	  <xsl:text/>
	</xsl:otherwise>
      </xsl:choose>
    </xsl:function>

    <doc type="function" xmlns="http://www.oxygenxml.com/ns/doc/xsl"  >
      <desc>Defines whether or not a word paragraph is a first level heading 
      </desc>
    </doc>
    
    <xsl:function name="tei:isFirstlevel-heading" as="xs:boolean">
      <xsl:param name="p"/>      
      <xsl:variable name="s" select="$p/w:pPr/w:pStyle/@w:val"/>
      <xsl:choose>
	<xsl:when test="$s='heading 1'">true</xsl:when>
	<xsl:when test="$s='Heading 1'">true</xsl:when>
	<xsl:when test="$s='Heading1'">true</xsl:when>
	<xsl:when test="$s='ITLP H1'">true</xsl:when>
	<xsl:when test="$s='ITLP Anonymous Heading 1'">true</xsl:when>
	<xsl:otherwise>false</xsl:otherwise>
      </xsl:choose>
    </xsl:function>
    
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
	<xsl:when test="$s='ITLP Anonymous Heading 1'">true</xsl:when>
	<xsl:when test="$s='ITLP Anonymous Heading 2'">true</xsl:when>
	<xsl:when test="$s='ITLP H1'">true</xsl:when>
	<xsl:when test="$s='ITLP H2'">true</xsl:when>
	<xsl:when test="$s='ITLP H3'">true</xsl:when>
	<xsl:when test="$s='Heading1'">true</xsl:when>
	<xsl:when test="$s='Heading2'">true</xsl:when>
	<xsl:when test="$s='Heading3'">true</xsl:when>
	<xsl:when test="$s='Heading4'">true</xsl:when>
	<xsl:when test="$s='Heading5'">true</xsl:when>
	<xsl:when test="$s='Heading6'">true</xsl:when>
	<xsl:when test="$s='Heading7'">true</xsl:when>
	<xsl:when test="$s='Heading8'">true</xsl:when>
	<xsl:when test="$s='Heading9'">true</xsl:when>
	<xsl:when test="$s='heading 1'">true</xsl:when>
	<xsl:when test="$s='heading 2'">true</xsl:when>
	<xsl:when test="$s='heading 3'">true</xsl:when>
	<xsl:when test="$s='heading 4'">true</xsl:when>
	<xsl:when test="$s='heading 5'">true</xsl:when>
	<xsl:when test="$s='heading 6'">true</xsl:when>
	<xsl:when test="$s='heading 7'">true</xsl:when>
	<xsl:when test="$s='heading 8'">true</xsl:when>
	<xsl:when test="$s='heading 9'">true</xsl:when>
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
	  <xsl:text>1</xsl:text>
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
      <xsl:value-of select="translate($current-header,'12345678','23456789')"/>
    </xsl:function>
   
    <doc type="function" xmlns="http://www.oxygenxml.com/ns/doc/xsl"  >
      <desc>Defines whether or not a word paragraph is a list element.</desc>
    </doc>
   
    <xsl:function name="tei:is-list" as="xs:boolean">
        <xsl:param name="p"/>        
        <xsl:choose>
            <xsl:when test="$p[contains(w:pPr/w:pStyle/@w:val,'List')]">true</xsl:when>
            <xsl:when test="$p[contains(w:pPr/w:pStyle/@w:val,'Bulletted')]">true</xsl:when>
            <xsl:when
		test="$p[contains(w:pPr/w:pStyle/@w:val,'Bulleted')]">true</xsl:when>
	    <xsl:when test="$p/w:pPr/w:numPr">true</xsl:when>
            <xsl:otherwise>false</xsl:otherwise>
        </xsl:choose>
    </xsl:function>

    <doc type="function" xmlns="http://www.oxygenxml.com/ns/doc/xsl"  >
      <desc>Override default behaviour for a styled paragraph</desc>
    </doc>

    <xsl:template match="w:p[w:pPr/w:pStyle/@w:val='SIG1']"
		  mode="paragraph">
      <closer>
	<xsl:apply-templates/>
      </closer>
    </xsl:template>

    <xsl:template match="w:p[w:pPr/w:pStyle/@w:val='RC']"
		  mode="paragraph">
      <bibl>
	<xsl:apply-templates/>
      </bibl>
    </xsl:template>

    <xsl:template match="w:p[w:pPr/w:pStyle/@w:val='SALU1']"
		  mode="paragraph">
      <dateline>
	<xsl:apply-templates/>
      </dateline>
    </xsl:template>

    <xsl:template match="w:p[w:pPr/w:pStyle/@w:val='DATE1']"
		  mode="paragraph">
      <date>
	<xsl:apply-templates/>
      </date>
    </xsl:template>


    <xsl:template match="w:p[w:pPr/w:pStyle/@w:val='CAP_1']"
		  mode="paragraph">
      <head>
	<xsl:apply-templates/>
      </head>
    </xsl:template>


    <xsl:template match="w:p[w:pPr/w:pStyle/@w:val='FNOT']"
		  mode="paragraph">
      <note>
	<xsl:apply-templates/>
      </note>
    </xsl:template>

    <xsl:template match="w:r[w:rPr/w:rStyle/@w:val='FNOT_REF']">
      <xsl:choose>
	<xsl:when test="ancestor::w:p[w:pPr/w:pStyle/@w:val='FNOT']">
	  <anchor xml:id="note{.}"/>
	</xsl:when>
	<xsl:otherwise>
	  <ref target="#note{.}">
	    <xsl:apply-templates/>
	  </ref>
	</xsl:otherwise>
      </xsl:choose>
    </xsl:template>

  </xsl:stylesheet>
  
