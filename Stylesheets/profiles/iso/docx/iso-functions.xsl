<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                xmlns:tei="http://www.tei-c.org/ns/1.0"
                xmlns:iso="http://www.iso.org/ns/1.0"
                xmlns:cals="http://www.oasis-open.org/specs/tm9901"
                xmlns:tbx="http://www.lisa.org/TBX-Specification.33.0.html"
                xmlns:xs="http://www.w3.org/2001/XMLSchema"
                xmlns:ve="http://schemas.openxmlformats.org/markup-compatibility/2006"
                xmlns:o="urn:schemas-microsoft-com:office:office"
                xmlns:r="http://schemas.openxmlformats.org/officeDocument/2006/relationships"
                xmlns:m="http://schemas.openxmlformats.org/officeDocument/2006/math"
                xmlns:v="urn:schemas-microsoft-com:vml"
                xmlns:fn="http://www.w3.org/2005/02/xpath-functions"
                xmlns:wp="http://schemas.openxmlformats.org/drawingml/2006/wordprocessingDrawing"
                xmlns:a="http://schemas.openxmlformats.org/drawingml/2006/main"
                xmlns:w10="urn:schemas-microsoft-com:office:word"
                xmlns:w="http://schemas.openxmlformats.org/wordprocessingml/2006/main"
                xmlns:wne="http://schemas.microsoft.com/office/word/2006/wordml"
                xmlns:mml="http://www.w3.org/1998/Math/MathML"
                xmlns:pic="http://schemas.openxmlformats.org/drawingml/2006/picture"
                xmlns:teidocx="http://www.tei-c.org/ns/teidocx/1.0"
                version="2.0"
                exclude-result-prefixes="cals ve o r m v wp w10 w wne mml tbx iso tei a xs pic fn">
    <xsl:import href="iso-variables.xsl"/>
    

  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" scope="stylesheet" type="stylesheet">
    <desc>
      <p> TEI stylesheet for simplifying TEI ODD markup </p>
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

    <doc type="function" xmlns="http://www.oxygenxml.com/ns/doc/xsl"  >
      <desc>Returns a listtype for a given stylename (return empty string to
	figure it out dynamically)
      </desc>
    </doc>
    
    
    <doc type="function" xmlns="http://www.oxygenxml.com/ns/doc/xsl"  >
      <desc>Returns the correct heading style (return empty string to figure it
	out dynamically)
      </desc>
    </doc>
    
    <xsl:function name="tei:get-headingstyle" as="xs:string">
      <xsl:param name="element"/>
      <xsl:param name="level"/>
      <xsl:choose>
	<xsl:when test="$element/../@type='annex'">
	  <xsl:value-of select="$ANNEX"/>
	</xsl:when>
	<xsl:when test="$element/../@type='termHeading'">
	  <xsl:value-of select="concat('termHeading',$level)"/>
	</xsl:when>
	<xsl:when test="$element/parent::tei:div/@type='annexSection'">
	  <xsl:value-of select="concat('a',$level)"/>
	</xsl:when>
	<xsl:when test="$element/../@type='bibliography'">
	  <xsl:value-of select="$BibliographyHeading"/>
	</xsl:when>
	<xsl:when test="$element/../@type='foreword'">
	  <xsl:value-of select="$ForewordHeading"/>
	</xsl:when>
	<xsl:when test="$element/../@type='introduction'">
	  <xsl:value-of select="$IntroductionHeading"/>
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
          <xsl:variable name="Heading">heading</xsl:variable>
	  <xsl:variable name="Heading1">heading 1</xsl:variable>      
      <xsl:choose>
	<xsl:when test="$p[w:pPr/w:pStyle/@w:val=$Heading1]">true</xsl:when>
	<xsl:when test="$p[w:pPr/w:pStyle/@w:val=$ANNEX]">true</xsl:when>
	<xsl:when test="$p[w:pPr/w:pStyle/@w:val=$ForewordHeading]">true</xsl:when>
	<xsl:when test="$p[w:pPr/w:pStyle/@w:val=$zzIntroductionHeading]">true</xsl:when>
	<xsl:when test="$p[w:pPr/w:pStyle/@w:val=$IntroductionHeading]">true</xsl:when>
	<xsl:when test="$p[w:pPr/w:pStyle/@w:val=$BibliographyHeading]">true</xsl:when>
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
	<xsl:when test="$s=$ANNEX">true</xsl:when>
	<xsl:when test="$s=$a2">true</xsl:when>
	<xsl:when test="$s=$a3">true</xsl:when>
	<xsl:when test="$s=$a4">true</xsl:when>
	<xsl:when test="$s=$a5">true</xsl:when>
	<xsl:when test="$s=$a6">true</xsl:when>
	<xsl:when test="$s=$pA2">true</xsl:when>
	<xsl:when test="$s=$pA3">true</xsl:when>
	<xsl:when test="$s=$pA4">true</xsl:when>
	<xsl:when test="$s=$pA5">true</xsl:when>
	<xsl:when test="$s=$pA6">true</xsl:when>
	<xsl:when test="$s='p2'">true</xsl:when>
	<xsl:when test="$s='p3'">true</xsl:when>
	<xsl:when test="$s='p4'">true</xsl:when>
	<xsl:when test="$s='p5'">true</xsl:when>
	<xsl:when test="$s='p6'">true</xsl:when>
	<xsl:when test="$s='Heading1'">true</xsl:when>
	<xsl:when test="$s='Heading2'">true</xsl:when>
	<xsl:when test="$s='Heading3'">true</xsl:when>
	<xsl:when test="$s='Heading4'">true</xsl:when>
	<xsl:when test="$s='Heading5'">true</xsl:when>
	<xsl:when test="$s='Heading6'">true</xsl:when>
	<xsl:when test="$s='Heading7'">true</xsl:when>
	<xsl:when test="$s='Heading8'">true</xsl:when>
	<xsl:when test="$s='Heading9'">true</xsl:when>
	<xsl:when test="$s='termHeading2'">true</xsl:when>
	<xsl:when test="$s='termHeading3'">true</xsl:when>
	<xsl:when test="$s='termHeading4'">true</xsl:when>
	<xsl:when test="$s='termHeading5'">true</xsl:when>
	<xsl:when test="$s='termHeading6'">true</xsl:when>
	<xsl:when test="$s='heading 1'">true</xsl:when>
	<xsl:when test="$s='heading 2'">true</xsl:when>
	<xsl:when test="$s='heading 3'">true</xsl:when>
	<xsl:when test="$s='heading 4'">true</xsl:when>
	<xsl:when test="$s='heading 5'">true</xsl:when>
	<xsl:when test="$s='heading 6'">true</xsl:when>
	<xsl:when test="$s='heading 7'">true</xsl:when>
	<xsl:when test="$s='heading 8'">true</xsl:when>
	<xsl:when test="$s='heading 9'">true</xsl:when>
	<xsl:when test="$s=$BibliographyHeading">true</xsl:when>
	<xsl:when test="$s=$ForewordHeading">true</xsl:when>
	<xsl:when test="$s=$IntroductionHeading">true</xsl:when>
	<xsl:when test="$s=$zzIntroductionHeading">true</xsl:when>
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
      <xsl:choose>
	<xsl:when test="$current-header=$ANNEX">
	  <xsl:text>a2</xsl:text>
	</xsl:when>
	<xsl:otherwise>
	  <xsl:value-of select="translate($current-header,'12345678','23456789')"/>
	</xsl:otherwise>
      </xsl:choose>
    </xsl:function>
    
    <doc type="function" xmlns="http://www.oxygenxml.com/ns/doc/xsl"  >
      <desc>Define special rendering 
      </desc>
    </doc>
    
    <xsl:function name="tei:render-bold" as="xs:boolean">
      <xsl:param name="element"/>
      <xsl:for-each select="$element">
	<xsl:choose>
	  <xsl:when test="starts-with(@rend,'specList-')">true</xsl:when>
	  <xsl:when test="starts-with(parent::tei:hi/@rend,'specList-')">true</xsl:when>
	  <xsl:when test="tei:match(@rend,'label')">true</xsl:when>
	  <xsl:when test="tei:match(@rend,'bold')">true</xsl:when>
	  <xsl:when test="parent::tei:hi[tei:match(@rend,'bold')]">true</xsl:when>
	  <xsl:when test="self::tei:term">true</xsl:when>
	  <xsl:otherwise>false</xsl:otherwise>
	</xsl:choose>
      </xsl:for-each>
    </xsl:function>
    
    <doc type="function" xmlns="http://www.oxygenxml.com/ns/doc/xsl"  >
      <desc>Whether to render an element in italic 
      </desc>
    </doc>
    
    <xsl:function name="tei:render-italic" as="xs:boolean">
      <xsl:param name="element"/>
      <xsl:for-each select="$element">
	<xsl:choose>
	  <xsl:when test="tei:match(@rend,'italic')">true</xsl:when>
	  <xsl:when test="self::tei:emph">true</xsl:when>
	  <xsl:when test="self::tbx:hi[@style='italics']">true</xsl:when>
	  <xsl:when test="parent::tei:item and self::tei:gloss">false</xsl:when>
	  <xsl:when test="parent::tei:item and self::tei:term">false</xsl:when>
	  <xsl:when test="self::tei:gloss">true</xsl:when>
	  <xsl:when test="self::tei:term">true</xsl:when>
	  <xsl:otherwise>false</xsl:otherwise>
	</xsl:choose>
      </xsl:for-each>
    </xsl:function>
    
    
    <!-- whether an element is block-level or inline -->
    <xsl:function name="tei:isInline" as="xs:boolean">
      <xsl:param name="element"/>
      <xsl:for-each select="$element">
	<xsl:choose>
	  <xsl:when test="self::tbx:term">true</xsl:when>
	  <xsl:when test="self::tbx:admin[@type='source']">true</xsl:when>
	  <xsl:when test="self::tbx:hi">true</xsl:when>
	  <xsl:when test="self::tbx:descrip">true</xsl:when>
	  <xsl:when test="self::tbx:termGrp">true</xsl:when>
	  <xsl:when test="self::m:oMath">true</xsl:when>
	  <xsl:when test="self::mml:math">true</xsl:when>
	  <xsl:when
	      test="self::tei:dynamicContent">true</xsl:when>
	  <xsl:when test="self::w:drawing">true</xsl:when>
	  <xsl:when
	      test="self::tei:formula[parent::cals:entry]">true</xsl:when>
	  <xsl:when
	      test="self::tei:formula[parent::tei:title]">true</xsl:when>
	  <xsl:when test="self::tei:abbr">true</xsl:when>
	  <xsl:when test="self::tei:add">true</xsl:when>
	  <xsl:when test="self::tei:affiliation">true</xsl:when>
	  <xsl:when test="self::tei:anchor">true</xsl:when>	
	  <xsl:when test="self::tei:analytic">true</xsl:when>	
	  <xsl:when test="self::tei:att">true</xsl:when>
	  <xsl:when test="self::tei:author">true</xsl:when>
	  <xsl:when test="self::tei:br">true</xsl:when>
	  <xsl:when test="self::tei:cit[ancestor::tei:p]">true</xsl:when>
	  <xsl:when test="self::tei:c">true</xsl:when>
	  <xsl:when test="self::tei:g">true</xsl:when>
	  <xsl:when test="self::tei:code">true</xsl:when>
	  <xsl:when test="self::tei:del">true</xsl:when>
	  <xsl:when test="self::tei:editionStmt">true</xsl:when>
	  <xsl:when test="self::tei:edition">true</xsl:when>
	  <xsl:when test="self::tei:date">true</xsl:when>
	  <xsl:when test="self::tei:emph">true</xsl:when>
	  <xsl:when test="self::tei:foreign">true</xsl:when>
	  <xsl:when test="self::tei:forename">true</xsl:when>
	  <xsl:when test="self::tei:graphic">true</xsl:when>
	  <xsl:when test="self::tei:gi">true</xsl:when>
	  <xsl:when
	      test="self::tei:gloss[not(ancestor-or-self::*/@type='termsAndDefinitions')]">true</xsl:when>
	  <xsl:when test="self::tei:hi[not(w:*)]">true</xsl:when>
	  <xsl:when test="self::tei:idno">true</xsl:when>
	  <xsl:when test="self::tei:ident">true</xsl:when>
	  <xsl:when test="self::tei:lb">true</xsl:when>
	  <xsl:when test="self::tei:imprint">true</xsl:when>
	  <xsl:when test="self::tei:monogr">true</xsl:when>
	  <xsl:when test="self::tei:series">true</xsl:when>
	  <xsl:when test="self::tei:name">true</xsl:when>
	  <xsl:when
	      test="self::tei:note[parent::tei:biblStruct]">true</xsl:when>
	  <xsl:when test="self::tei:note[parent::tei:bibl]">true</xsl:when>
	  <xsl:when
	      test="self::tei:note[@type='remark']">true</xsl:when>
	  <xsl:when test="self::tei:note[@type='emphasize']">true</xsl:when>
	  <xsl:when test="self::tei:note[tei:isEndNote(.)]">true</xsl:when>
	  <xsl:when test="self::tei:note[tei:isFootNote(.)]">true</xsl:when>
	  <xsl:when test="self::tei:note[@place='comment']">true</xsl:when>
	  <xsl:when test="self::tei:note[@place='inline' and parent::tei:q]">true</xsl:when>
	  <xsl:when test="self::tei:note[@place='inline' and not(parent::tei:div or
			parent::tei:list)]">true</xsl:when>
	  <xsl:when test="self::tei:num">true</xsl:when>
	  <xsl:when test="self::tei:mentioned">true</xsl:when>
	  <xsl:when test="self::tei:orgName">true</xsl:when>
	  <xsl:when test="self::tei:placeName">true</xsl:when>
	  <xsl:when test="self::tei:persName">true</xsl:when>
	  <xsl:when test="self::tei:publisher">true</xsl:when>
	  <xsl:when test="self::tei:pb">true</xsl:when>
	  <xsl:when test="self::tei:ptr">true</xsl:when>
	  <xsl:when test="self::tei:q[parent::tei:p]">true</xsl:when>
	  <xsl:when test="self::tei:q[parent::tei:note]">true</xsl:when>
	  <xsl:when test="self::tei:q[parent::tei:cell]">true</xsl:when>
	  <xsl:when test="self::tei:q[parent::tei:item]">true</xsl:when>
	  <xsl:when test="self::tei:q[@type='sdt']">false</xsl:when>
	  <xsl:when test="self::tei:ref">true</xsl:when>
	  <xsl:when test="self::tei:termRef">true</xsl:when>
	  <xsl:when test="self::tei:seg">true</xsl:when>
	  <xsl:when test="self::tei:surname">true</xsl:when>
	  <xsl:when test="self::tei:soCalled">true</xsl:when>
	  <xsl:when
	      test="self::tei:term[not(ancestor-or-self::*/@type='termsAndDefinitions')]">true</xsl:when>
	  <xsl:when test="self::tei:val">true</xsl:when>
	  <xsl:when test="self::tei:title">true</xsl:when>
	  <xsl:otherwise>
	    <xsl:choose>
	      <xsl:when test="empty($element/..)">false</xsl:when>
	      <xsl:when test="tei:isInline($element/..)">true</xsl:when>
	      <xsl:otherwise>false</xsl:otherwise>
	    </xsl:choose>
	  </xsl:otherwise>
	</xsl:choose>
      </xsl:for-each>
    </xsl:function>
    
    <!-- whether a Word style is supported -->
    <xsl:function name="tei:is-supported-style" as="xs:boolean">
      <xsl:param name="style"/>
      <xsl:variable name="styles" select="document('styles.xml')"/>
      
      <xsl:choose>
	<xsl:when test="$styles/styles[style=$style]">
	  true
	</xsl:when>
	<xsl:otherwise>false</xsl:otherwise>
      </xsl:choose>
    </xsl:function>
    
    
  </xsl:stylesheet>
  
