<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xmlns:cals="http://www.oasis-open.org/specs/tm9901" xmlns:tei="http://www.tei-c.org/ns/1.0" xmlns:iso="http://www.iso.org/ns/1.0" xmlns:xs="http://www.w3.org/2001/XMLSchema" xmlns:ve="http://schemas.openxmlformats.org/markup-compatibility/2006" xmlns:o="urn:schemas-microsoft-com:office:office" xmlns:r="http://schemas.openxmlformats.org/officeDocument/2006/relationships" xmlns:m="http://schemas.openxmlformats.org/officeDocument/2006/math" xmlns:v="urn:schemas-microsoft-com:vml" xmlns:fn="http://www.w3.org/2005/02/xpath-functions" xmlns:wp="http://schemas.openxmlformats.org/drawingml/2006/wordprocessingDrawing" xmlns:a="http://schemas.openxmlformats.org/drawingml/2006/main" xmlns:w10="urn:schemas-microsoft-com:office:word" xmlns:w="http://schemas.openxmlformats.org/wordprocessingml/2006/main" xmlns:wne="http://schemas.microsoft.com/office/word/2006/wordml" xmlns:mml="http://www.w3.org/1998/Math/MathML" xmlns:tbx="http://www.lisa.org/TBX-Specification.33.0.html" xmlns:pic="http://schemas.openxmlformats.org/drawingml/2006/picture" xmlns:teidocx="http://www.tei-c.org/ns/teidocx/1.0" version="2.0" exclude-result-prefixes="cals ve o r m v wp w10 w wne mml tbx iso tei a xs pic fn">
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" scope="stylesheet" type="stylesheet">
    <desc>
      <p> TEI Utility stylesheet defining functions for use in all
	 output formats.</p>
      <p>This software is dual-licensed:

1. Distributed under a Creative Commons Attribution-ShareAlike 3.0
Unported License http://creativecommons.org/licenses/by-sa/3.0/ 

2. http://www.opensource.org/licenses/BSD-2-Clause
		
All rights reserved.

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
      <p>Id: $Id$</p>
      <p>Copyright: 2008, TEI Consortium</p>
    </desc>
  </doc>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc>Whether a section is "identifiable"</desc>
  </doc>
  <xsl:function name="tei:is-identifiable" as="xs:boolean">
    <xsl:param name="element"/>
    <xsl:for-each select="$element">
      <xsl:choose>
        <xsl:when test="self::tei:div">true</xsl:when>
        <xsl:when test="self::tei:div1">true</xsl:when>
        <xsl:when test="self::tei:div2">true</xsl:when>
        <xsl:when test="self::tei:div3">true</xsl:when>
        <xsl:when test="self::tei:div4">true</xsl:when>
        <xsl:when test="self::tei:div5">true</xsl:when>
        <xsl:when test="self::tei:div6">true</xsl:when>
        <xsl:when test="self::tei:p[@xml:id]">true</xsl:when>
        <xsl:when test="self::tei:index[@xml:id]">true</xsl:when>
        <xsl:otherwise>false</xsl:otherwise>
      </xsl:choose>
    </xsl:for-each>
  </xsl:function>

  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc>Whether a section is "transcribable"</desc>
  </doc>
  <xsl:function name="tei:is-transcribable" as="xs:boolean">
    <xsl:param name="element"/>
    <xsl:for-each select="$element">
      <xsl:choose>
	<xsl:when test="self::tei:p and parent::tei:sp">true</xsl:when>
        <xsl:when test="self::tei:l">true</xsl:when>
        <xsl:otherwise>false</xsl:otherwise>
      </xsl:choose>
    </xsl:for-each>
  </xsl:function>

  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc>Whether to render text in small caps.</desc>
  </doc>
  <xsl:function name="tei:render-smallcaps" as="xs:boolean">
    <xsl:param name="element"/>
    <xsl:for-each select="$element">
      <xsl:choose>
        <xsl:when test="contains(@rend,'smallcaps')">true</xsl:when>
        <xsl:when test="@rend='sc'">true</xsl:when>
        <xsl:otherwise>false</xsl:otherwise>
      </xsl:choose>
    </xsl:for-each>
  </xsl:function>

  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc>Whether to render text in smart quotes.</desc>
  </doc>
  <xsl:function name="tei:render-quotes" as="xs:boolean">
    <xsl:param name="element"/>
    <xsl:for-each select="$element">
      <xsl:choose>
        <xsl:when test="self::tei:soCalled">true</xsl:when>
        <xsl:when test="contains(@rend,'quotes')">true</xsl:when>
        <xsl:otherwise>false</xsl:otherwise>
      </xsl:choose>
    </xsl:for-each>
  </xsl:function>


  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc>Whether to render text in bold.</desc>
  </doc>
  <xsl:function name="tei:render-bold" as="xs:boolean">
    <xsl:param name="element"/>
    <xsl:for-each select="$element">
      <xsl:choose>
        <xsl:when test="parent::tei:hi[starts-with(@rend,'specList-')]">true</xsl:when>
        <xsl:when test="parent::tei:hi[@rend='bold']">true</xsl:when>
        <xsl:when test="contains(@rend,'bold')">true</xsl:when>
        <xsl:when test="@rend='label'">true</xsl:when>
        <xsl:when test="ancestor-or-self::tei:cell[@role='label']">true</xsl:when>
        <xsl:when test="ancestor-or-self::tei:cell[@rend='wovenodd-col1']">true</xsl:when>
        <xsl:when test="self::tei:cell and parent::tei:row[@role='label']">true</xsl:when>
        <xsl:when test="self::tei:label[following-sibling::tei:item]">true</xsl:when>
        <xsl:when test="self::tei:term">true</xsl:when>
        <xsl:when test="self::tei:unclear">true</xsl:when>
        <xsl:otherwise>false</xsl:otherwise>
      </xsl:choose>
    </xsl:for-each>
  </xsl:function>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc>Whether to render something in italic.</desc>
  </doc>
  <xsl:function name="tei:render-italic" as="xs:boolean">
    <xsl:param name="element"/>
    <xsl:for-each select="$element">
      <xsl:choose>
        <xsl:when test="self::tei:ref and tei:render-italic(..)">true</xsl:when>
        <xsl:when test="contains(@rend,'italic')">true</xsl:when>
        <xsl:when test="self::tei:emph">true</xsl:when>
        <xsl:when test="self::tei:hi[not(@rend)]">true</xsl:when>
        <xsl:when test="self::tbx:hi[@style='italics']">true</xsl:when>
        <xsl:when test="@rend='ital'">true</xsl:when>
        <xsl:when test="@rend='it'">true</xsl:when>
        <xsl:when test="@rend='i'">true</xsl:when>
        <xsl:when test="@rend='att'">true</xsl:when>
        <xsl:when test="self::tei:att">true</xsl:when>
        <xsl:when test="self::tei:speaker">true</xsl:when>
        <xsl:when test="self::tei:gloss">true</xsl:when>
        <xsl:when test="self::tei:title">true</xsl:when>
        <xsl:when test="self::tei:name">true</xsl:when>
        <xsl:otherwise>false</xsl:otherwise>
      </xsl:choose>
    </xsl:for-each>
  </xsl:function>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc>Whether to render something in typewriter-like code.</desc>
  </doc>
  <xsl:function name="tei:render-typewriter" as="xs:boolean">
    <xsl:param name="element"/>
    <xsl:for-each select="$element">
      <xsl:choose>
        <xsl:when test="self::tei:gi">true</xsl:when>
        <xsl:when test="self::tei:val">true</xsl:when>
        <xsl:when test="self::tei:code">true</xsl:when>
        <xsl:when test="self::tei:ident">true</xsl:when>
        <xsl:otherwise>false</xsl:otherwise>
      </xsl:choose>
    </xsl:for-each>
  </xsl:function>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc>Is given an element and defines whether or not this element is to be rendered inline.</desc>
  </doc>
  <xsl:function name="tei:is-inline" as="xs:boolean">
    <xsl:param name="element"/>
    <xsl:for-each select="$element">
      <xsl:choose>
        <xsl:when test="self::mml:math">true</xsl:when>
        <xsl:when test="self::tei:abbr">true</xsl:when>
        <xsl:when test="self::tei:affiliation">true</xsl:when>
        <xsl:when test="self::tei:altIdentifier">true</xsl:when>
        <xsl:when test="self::tei:analytic">true</xsl:when>
        <xsl:when test="self::tei:add">true</xsl:when>
        <xsl:when test="self::tei:am">true</xsl:when>
        <xsl:when test="self::tei:att">true</xsl:when>
        <xsl:when test="self::tei:author">true</xsl:when>
        <xsl:when test="self::tei:bibl and tei:is-inline($element/..)">true</xsl:when>
        <xsl:when test="self::tei:biblScope">true</xsl:when>
        <xsl:when test="self::tei:br">true</xsl:when>
        <xsl:when test="self::tei:byline">true</xsl:when>
        <xsl:when test="self::tei:c">true</xsl:when>
        <xsl:when test="self::tei:caesura">true</xsl:when>
        <xsl:when test="self::tei:choice">true</xsl:when>
        <xsl:when test="self::tei:code">true</xsl:when>
        <xsl:when test="self::tei:collection">true</xsl:when>
        <xsl:when test="self::tei:country">true</xsl:when>
        <xsl:when test="self::tei:damage">true</xsl:when>
        <xsl:when test="self::tei:date">true</xsl:when>
        <xsl:when test="self::tei:del">true</xsl:when>
        <xsl:when test="self::tei:desc">true</xsl:when>
        <xsl:when test="self::tei:depth">true</xsl:when>
        <xsl:when test="self::tei:dim">true</xsl:when>
        <xsl:when test="self::tei:dimensions">true</xsl:when>
        <xsl:when test="self::tei:editor">true</xsl:when>
        <xsl:when test="self::tei:editionStmt">true</xsl:when>
        <xsl:when test="self::tei:emph">true</xsl:when>
        <xsl:when test="self::tei:ex">true</xsl:when>
        <xsl:when test="self::tei:expan">true</xsl:when>
        <xsl:when test="self::tei:figure[@place='inline']">true</xsl:when>
        <xsl:when test="self::tei:figure[parent::tei:head]">true</xsl:when>
        <xsl:when test="self::tei:foreign">true</xsl:when>
        <xsl:when test="self::tei:forename">true</xsl:when>
        <xsl:when test="self::tei:gap">true</xsl:when>
        <xsl:when test="self::tei:genName">true</xsl:when>
        <xsl:when test="self::tei:geogName">true</xsl:when>
        <xsl:when test="self::tei:gi">true</xsl:when>
        <xsl:when test="self::tei:gloss">true</xsl:when>
        <xsl:when test="self::tei:graphic">true</xsl:when>
        <xsl:when test="self::tei:height">true</xsl:when>
        <xsl:when test="self::tei:hi[not(w:*)]">true</xsl:when>
        <xsl:when test="self::tei:ident">true</xsl:when>
        <xsl:when test="self::tei:idno">true</xsl:when>
        <xsl:when test="self::tei:imprint">true</xsl:when>
        <xsl:when test="self::tei:institution">true</xsl:when>
        <xsl:when test="self::tei:lb">true</xsl:when>
        <xsl:when test="self::tei:locus">true</xsl:when>
        <xsl:when test="self::tei:mentioned">true</xsl:when>
        <xsl:when test="self::tei:monogr">true</xsl:when>
        <xsl:when test="self::tei:series">true</xsl:when>
        <xsl:when test="self::tei:msName">true</xsl:when>
        <xsl:when test="self::tei:name">true</xsl:when>
        <xsl:when test="self::tei:note[@place='margin']">true</xsl:when>
        <xsl:when test="self::tei:note[@place='marginOuter']">true</xsl:when>
        <xsl:when test="self::tei:note[@place='marginLeft']">true</xsl:when>
        <xsl:when test="self::tei:note[@place='marginRight']">true</xsl:when>
        <xsl:when test="self::tei:note[@place='bottom']">true</xsl:when>
        <xsl:when test="self::tei:note[@place='comment']">true</xsl:when>
        <xsl:when test="self::tei:note[@place='end']">true</xsl:when>
        <xsl:when test="self::tei:note[@place='foot']">true</xsl:when>
        <xsl:when test="self::tei:note[@place='inline']">true</xsl:when>
        <xsl:when test="self::tei:note[parent::tei:biblStruct]">true</xsl:when>
        <xsl:when test="self::tei:note[parent::tei:bibl]">true</xsl:when>
        <xsl:when test="self::tei:num">true</xsl:when>
        <xsl:when test="self::tei:orgName">true</xsl:when>
        <xsl:when test="self::tei:orig">true</xsl:when>
        <xsl:when test="self::tei:origDate">true</xsl:when>
        <xsl:when test="self::tei:origPlace">true</xsl:when>
        <xsl:when test="self::tei:origPlace">true</xsl:when>
        <xsl:when test="self::tei:pc">true</xsl:when>
        <xsl:when test="self::tei:pb">true</xsl:when>
        <xsl:when test="self::tei:persName">true</xsl:when>
        <xsl:when test="self::tei:placeName">true</xsl:when>
        <xsl:when test="self::tei:ptr">true</xsl:when>
        <xsl:when test="self::tei:publisher">true</xsl:when>
        <xsl:when test="self::tei:pubPlace">true</xsl:when>
        <xsl:when test="self::tei:q[tei:l]">false</xsl:when>
        <xsl:when test="self::tei:q[tei:figure or tei:p or tei:note or
			tei:bibl or tei:sp or tei:floatingText]">false</xsl:when>
        <xsl:when test="self::tei:q">true</xsl:when>
        <xsl:when test="self::tei:said">true</xsl:when>
        <xsl:when test="self::tei:ref">true</xsl:when>
        <xsl:when test="self::tei:region">true</xsl:when>
        <xsl:when test="self::tei:repository">true</xsl:when>
        <xsl:when test="self::tei:roleName">true</xsl:when>
        <xsl:when test="self::tei:rubric">true</xsl:when>
        <xsl:when test="self::tei:seg">true</xsl:when>
        <xsl:when test="self::tei:sic">true</xsl:when>
        <xsl:when test="self::tei:settlement">true</xsl:when>
        <xsl:when test="self::tei:soCalled">true</xsl:when>
        <xsl:when test="self::tei:summary">true</xsl:when>
        <xsl:when test="self::tei:supplied">true</xsl:when>
        <xsl:when test="self::tei:surname">true</xsl:when>
        <xsl:when test="self::tei:term">true</xsl:when>
        <xsl:when test="self::tei:textLang">true</xsl:when>
        <xsl:when test="self::tei:title">true</xsl:when>
        <xsl:when test="self::tei:unclear">true</xsl:when>
        <xsl:when test="self::tei:val">true</xsl:when>
        <xsl:when test="self::tei:width">true</xsl:when>
        <xsl:when test="self::tei:dynamicContent">true</xsl:when>
        <xsl:when test="self::w:drawing">true</xsl:when>
        <xsl:when test="self::m:oMath">true</xsl:when>
        <xsl:otherwise>
          <xsl:choose>
            <xsl:when test="empty($element/..)">false</xsl:when>
            <xsl:when test="tei:is-inline($element/..)">true</xsl:when>
            <xsl:otherwise>false</xsl:otherwise>
          </xsl:choose>
        </xsl:otherwise>
      </xsl:choose>
    </xsl:for-each>
  </xsl:function>

  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc>Is given an element and says whether the context is at the
    level of a block</desc>
  </doc>
  <xsl:function name="tei:blockContext" as="xs:boolean">
    <xsl:param name="element"/>
    <xsl:for-each select="$element">
      <xsl:choose>
	<xsl:when test="parent::tei:note[@place='foot'] and self::tei:gap">false</xsl:when>
	<xsl:when test="parent::tei:note[@place='foot' or @place='bottom']">true</xsl:when>
	<xsl:when test="parent::tei:body">true</xsl:when>
	<xsl:when test="tei:floatingText">true</xsl:when>
	<xsl:when test="parent::tei:div">true</xsl:when>
	<xsl:when test="parent::tei:titlePage">true</xsl:when>
	<xsl:otherwise>false</xsl:otherwise>
      </xsl:choose>
    </xsl:for-each>
  </xsl:function>


</xsl:stylesheet>
