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
      <p>Copyright: 2013, TEI Consortium</p>
    </desc>
  </doc>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" class="misc" type="boolean">
    <desc>Whether to attempt to work out a current date (set to true
    for test results which won't differ</desc>
  </doc>
  <xsl:param name="useFixedDate">false</xsl:param>
  <xsl:param name="wordDirectory"/>
  <xsl:param name="splitLevel">-1</xsl:param>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" class="misc" type="boolean">
    <desc>Title, author and date is taken from the
    &lt;teiHeader&gt; rather than looked for in the front matter</desc>
  </doc>
  <xsl:param name="useHeaderFrontMatter">false</xsl:param>

  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" class="output" type="boolean">
    <desc>Whether it should be attempted to make quotes into block
      quotes if they are over a certain length</desc>
  </doc>
  <xsl:param name="autoBlockQuote">false</xsl:param>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" class="output" type="integer">
    <desc>Length beyond which a quote is a block quote</desc>
  </doc>
  <xsl:param name="autoBlockQuoteLength">150</xsl:param>
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
	<xsl:when test="ancestor-or-self::*[@rend][contains(@rend,'smallcaps')]">true</xsl:when>
	<xsl:when test="ancestor-or-self::*[@rend][contains(@rend,'sc')]">true</xsl:when>
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
	<xsl:when test="ancestor-or-self::*[@rend][contains(@rend,'quotes')]">true</xsl:when>
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
	<xsl:when test="@rend='label'">true</xsl:when>
	<xsl:when test="@rend='odd_label'">true</xsl:when>
	<xsl:when test="parent::tei:hi[starts-with(@rend,'specList-')]">true</xsl:when>
	<xsl:when test="self::tei:docAuthor">true</xsl:when>
	<xsl:when test="self::tei:label[following-sibling::tei:item]">true</xsl:when>
	<xsl:when test="starts-with(@rend,'specList-')">true</xsl:when>
	<xsl:when test="starts-with(parent::tei:hi/@rend,'specList-')">true</xsl:when>
        <xsl:when test="@rend='label'">true</xsl:when>
        <xsl:when test="ancestor-or-self::tei:cell[@rend='wovenodd-col1']">true</xsl:when>
        <xsl:when test="ancestor-or-self::tei:cell[@role='label']">true</xsl:when>
	<xsl:when test="ancestor-or-self::*[@rend][contains(@rend,'bold')]">true</xsl:when>
        <xsl:when test="parent::tei:hi[starts-with(@rend,'specList-')]">true</xsl:when>
        <xsl:when test="self::tei:cell and parent::tei:row[@role='label']">true</xsl:when>
        <xsl:when test="self::tei:label[following-sibling::tei:item]">true</xsl:when>
        <xsl:when test="self::tei:term">true</xsl:when>
        <xsl:when test="self::tei:unclear">true</xsl:when>
        <xsl:otherwise>false</xsl:otherwise>
      </xsl:choose>
    </xsl:for-each>
  </xsl:function>

  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc>Whether to render something in color.</desc>
  </doc>
  <xsl:function name="tei:render-color" as="xs:string*">
    <xsl:param name="element"/>
    <xsl:for-each select="$element">
      <xsl:for-each
	  select="ancestor-or-self::*[@rend][matches(@rend,'color\(')]">
	<xsl:analyze-string select="@rend" regex="color\(([^\)]+)">
	  <xsl:matching-substring>
	    <xsl:value-of select="regex-group(1)"/>
	  </xsl:matching-substring>
	</xsl:analyze-string>
      </xsl:for-each>
    </xsl:for-each>
  </xsl:function>

  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc>Whether to render something with background color.</desc>
  </doc>
  <xsl:function name="tei:render-backgroundcolor" as="xs:string*">
    <xsl:param name="element"/>
    <xsl:for-each select="$element">
      <xsl:for-each
	  select="ancestor-or-self::*[@rend][matches(@rend,'background\(')]">
	<xsl:analyze-string select="@rend" regex="background\(([^\)]+)">
	  <xsl:matching-substring>
	    <xsl:value-of select="regex-group(1)"/>
	  </xsl:matching-substring>
	</xsl:analyze-string>
      </xsl:for-each>
    </xsl:for-each>
  </xsl:function>

  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc>Whether to render something in italic.</desc>
  </doc>
  <xsl:function name="tei:render-italic" as="xs:boolean">
    <xsl:param name="element"/>
    <xsl:for-each select="$element">
      <xsl:choose>
	<xsl:when test="ancestor-or-self::*[@rend][contains(@rend,'italic')]">true</xsl:when>
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
	<xsl:when test="ancestor-or-self::*[@rend][contains(@rend,'typewriter')]">true</xsl:when>
        <xsl:otherwise>false</xsl:otherwise>
      </xsl:choose>
    </xsl:for-each>
  </xsl:function>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc>Is given an element and defines whether or not this element is to be rendered inline.</desc>
  </doc>
  <xsl:function name="tei:is-inline" as="xs:boolean">
    <xsl:param name="element"/>
    <xsl:choose>
      <xsl:when test="empty($element)">true</xsl:when>
      <xsl:otherwise>	
	<xsl:for-each select="$element">
	  <xsl:choose>
        <xsl:when test="not(self::*)">true</xsl:when>
        <xsl:when test="contains(@rend,'inline') and not(tei:p or tei:l)">true</xsl:when>
        <xsl:when test="self::tei:note[@place='display']">false</xsl:when>
        <xsl:when test="self::tei:note[tei:isEndNote(.)]">true</xsl:when>
        <xsl:when test="self::tei:note[tei:isFootNote(.)]">true</xsl:when>
        <xsl:when test="@rend='display' or @rend='block'">false</xsl:when>
        <xsl:when test="tei:table or tei:figure or tei:list or tei:lg
			or tei:q/tei:l or tei:l or tei:p or tei:biblStruct or tei:sp or tei:floatingText">false</xsl:when>
        <xsl:when test="parent::tei:div">false</xsl:when>
        <xsl:when test="parent::tei:titlePage">false</xsl:when>
        <xsl:when test="self::tei:cit[not(@rend)]">true</xsl:when>
        <xsl:when test="parent::tei:cit[@rend='display']">false</xsl:when>
        <xsl:when test="parent::tei:cit and (tei:p or tei:l)">false</xsl:when>
        <xsl:when test="parent::tei:cit and parent::cit/tei:bibl">false</xsl:when>
        <xsl:when test="parent::tei:body">false</xsl:when>
        <xsl:when test="parent::tei:titlePage">false</xsl:when>
        <xsl:when test="self::tei:docAuthor and parent::tei:byline">true</xsl:when>
        <xsl:when test="self::tei:note[tei:cit/tei:bibl]">false</xsl:when>
        <xsl:when test="self::tei:note[parent::tei:biblStruct]">true</xsl:when>
        <xsl:when test="self::tei:note[parent::tei:bibl]">true</xsl:when>
        <xsl:when test="self::tei:note">true</xsl:when>
        <xsl:when test="self::mml:math">true</xsl:when>
        <xsl:when test="self::tei:abbr">true</xsl:when>
        <xsl:when test="self::tei:affiliation">true</xsl:when>
        <xsl:when test="self::tei:altIdentifier">true</xsl:when>
        <xsl:when test="self::tei:analytic">true</xsl:when>
        <xsl:when test="self::tei:add">true</xsl:when>
        <xsl:when test="self::tei:am">true</xsl:when>
        <xsl:when test="self::tei:att">true</xsl:when>
        <xsl:when test="self::tei:author">true</xsl:when>
        <xsl:when test="self::tei:bibl and not (tei:is-inline(preceding-sibling::*[1]))">false</xsl:when>
        <xsl:when test="self::tei:bibl and not (parent::tei:listBibl)">true</xsl:when>
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
        <xsl:when test="self::tei:depth">true</xsl:when>
        <xsl:when test="self::tei:dim">true</xsl:when>
        <xsl:when test="self::tei:dimensions">true</xsl:when>
        <xsl:when test="self::tei:editor">true</xsl:when>
        <xsl:when test="self::tei:editionStmt">true</xsl:when>
        <xsl:when test="self::tei:emph">true</xsl:when>
        <xsl:when test="self::tei:ex">true</xsl:when>
        <xsl:when test="self::tei:expan">true</xsl:when>
        <xsl:when test="self::tei:figure[@place='inline']">true</xsl:when>
        <xsl:when test="self::tei:figure">false</xsl:when> 
        <xsl:when test="self::tei:floatingText">false</xsl:when>
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
        <xsl:when test="self::tei:list">false</xsl:when>
        <xsl:when test="self::tei:locus">true</xsl:when>
        <xsl:when test="self::tei:mentioned">true</xsl:when>
        <xsl:when test="self::tei:monogr">true</xsl:when>
        <xsl:when test="self::tei:series">true</xsl:when>
        <xsl:when test="self::tei:msName">true</xsl:when>
        <xsl:when test="self::tei:name">true</xsl:when>
        <xsl:when test="self::tei:num">true</xsl:when>
        <xsl:when test="self::tei:orgName">true</xsl:when>
        <xsl:when test="self::tei:orig">true</xsl:when>
        <xsl:when test="self::tei:origDate">true</xsl:when>
        <xsl:when test="self::tei:origPlace">true</xsl:when>
        <xsl:when test="self::tei:pc">true</xsl:when>
        <xsl:when test="self::tei:persName">true</xsl:when>
        <xsl:when test="self::tei:placeName">true</xsl:when>
        <xsl:when test="self::tei:ptr">true</xsl:when>
        <xsl:when test="self::tei:publisher">true</xsl:when>
        <xsl:when test="self::tei:pubPlace">true</xsl:when>
        <xsl:when test="self::tei:lb or self::pb">true</xsl:when>
        <xsl:when test="self::tei:quote and tei:lb">false</xsl:when>
        <xsl:when test="self::tei:quote and $autoBlockQuote='true' and string-length(.)&gt;$autoBlockQuoteLength">false</xsl:when>
        <xsl:when test="self::tei:q">true</xsl:when>
        <xsl:when test="self::tei:quote">true</xsl:when>
        <xsl:when test="self::tei:ref">true</xsl:when>
        <xsl:when test="self::tei:region">true</xsl:when>
        <xsl:when test="self::tei:repository">true</xsl:when>
        <xsl:when test="self::tei:roleName">true</xsl:when>
        <xsl:when test="self::tei:rubric">true</xsl:when>
        <xsl:when test="self::tei:rs">true</xsl:when>
        <xsl:when test="self::tei:said">true</xsl:when>
        <xsl:when test="self::tei:seg">true</xsl:when>
        <xsl:when test="self::tei:sic">true</xsl:when>
        <xsl:when test="self::tei:settlement">true</xsl:when>
        <xsl:when test="self::tei:soCalled">true</xsl:when>
        <xsl:when test="self::tei:summary">true</xsl:when>
        <xsl:when test="self::tei:supplied">true</xsl:when>
        <xsl:when test="self::tei:surname">true</xsl:when>
        <xsl:when test="self::tei:tag">true</xsl:when>
        <xsl:when test="self::tei:term">true</xsl:when>
        <xsl:when test="self::tei:textLang">true</xsl:when>
        <xsl:when test="self::tei:title">true</xsl:when>
        <xsl:when test="self::tei:unclear">true</xsl:when>
        <xsl:when test="self::tei:val">true</xsl:when>
        <xsl:when test="self::tei:width">true</xsl:when>
        <xsl:when test="self::tei:dynamicContent">true</xsl:when>
        <xsl:when test="self::w:drawing">true</xsl:when>
        <xsl:when test="self::m:oMath">true</xsl:when>
        <xsl:when test="parent::tei:note[tei:isEndNote(.)]">false</xsl:when>
	<xsl:when test="empty($element/..)">false</xsl:when>
	<xsl:when test="not(self::tei:p) and tei:is-inline($element/..)">true</xsl:when>
	<xsl:otherwise>false</xsl:otherwise>
      </xsl:choose>
    </xsl:for-each>
  </xsl:otherwise>
</xsl:choose>
  </xsl:function>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc>Returns the current date.</desc>
  </doc>
  <xsl:function name="tei:whatsTheDate">
    <xsl:value-of select="format-dateTime(current-dateTime(),'[Y]-[M02]-[D02]T[H02]:[m02]:[s02]Z')"/>
  </xsl:function>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc>Whether an element has any more (useful) text in its parent</desc>
  </doc>
  <xsl:function name="tei:is-last" as="xs:boolean">
    <xsl:param name="element"/>
    <xsl:for-each select="$element">
      <xsl:choose>
        <xsl:when test="count(following-sibling::node())=1 and normalize-space(following-sibling::node())=''">true</xsl:when>
        <xsl:when test="not(following-sibling::node())">true</xsl:when>
        <xsl:otherwise>false</xsl:otherwise>
      </xsl:choose>
    </xsl:for-each>
  </xsl:function>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc>Whether an element has any  (useful) text before it</desc>
  </doc>
  <xsl:function name="tei:is-first" as="xs:boolean">
    <xsl:param name="element"/>
    <xsl:for-each select="$element">
      <xsl:choose>
        <xsl:when test="count(preceding-sibling::node())=1 and normalize-space(preceding-sibling::node())=''">true</xsl:when>
        <xsl:when test="not(preceding-sibling::node())">true</xsl:when>
        <xsl:otherwise>false</xsl:otherwise>
      </xsl:choose>
    </xsl:for-each>
  </xsl:function>



  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>[common] handle character data.
      Following http://wiki.tei-c.org/index.php/XML_Whitespace#XSLT_Normalization_Code,
      the algorithm to normalize space in mixed content is:
      Collapse all white space, then
      trim leading space on the first text node in an element and
      trim trailing space on the last text node in an element,
      trim both if a text node is both first and last, i.e., is the only text node in the element.</desc>
  </doc>
    <xsl:template match="text()">
      <xsl:choose>
	<xsl:when
	    test="ancestor::*[@xml:space][1]/@xml:space='preserve'">
	  <xsl:value-of select="tei:escapeChars(.,parent::*)"/>
	</xsl:when>
	<xsl:otherwise>
        <!-- Retain one leading space if node isn't first, has
	     non-space content, and has leading space.-->
	<xsl:if test="position()!=1 and 
		      matches(.,'^\s') and 
		      normalize-space()!=''">
	  <xsl:call-template name="space"/>
	</xsl:if>
	<xsl:value-of select="tei:escapeChars(normalize-space(.),parent::*)"/>
	<xsl:choose>
	  <!-- node is an only child, and has content but it's all space -->
	  <xsl:when test="last()=1 and string-length()!=0 and
			  normalize-space()=''">
	  <xsl:call-template name="space"/>
	  </xsl:when>
	  <!-- node isn't last, isn't first, and has trailing space -->
	  <xsl:when test="position()!=1 and position()!=last() and matches(.,'\s$')">
	  <xsl:call-template name="space"/>
	  </xsl:when>
	  <!-- node isn't last, is first, has trailing space, and has non-space content   -->
	  <xsl:when test="position()=1 and matches(.,'\s$') and normalize-space()!=''">
	  <xsl:call-template name="space"/>
	  </xsl:when>
	</xsl:choose>
	</xsl:otherwise>
      </xsl:choose>
    </xsl:template>

    <xsl:template name="space">
      <xsl:text> </xsl:text>
    </xsl:template>

  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>[common] allow for further handling of text. By default,
      just normalize, but some formats may escape some characters.</desc></doc>
  <xsl:function name="tei:escapeChars" as="xs:string">
    <xsl:param name="letters"/>
    <xsl:param name="context"/>
    <xsl:value-of select="$letters"/>
  </xsl:function>

  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>[common] process target urls, looking for magic patterns</desc></doc>
  <xsl:function name="tei:resolveURI" as="xs:string">
    <xsl:param name="context"/>
    <xsl:param name="target"/>
     <xsl:analyze-string select="normalize-space($target)" regex="^(\w+):(.+)$">
       <xsl:matching-substring>
	 <xsl:variable name="prefix" select="regex-group(1)"/>
	 <xsl:variable name="value" select="regex-group(2)"/>
	 <xsl:choose>
	   <xsl:when
	       test="$context/ancestor::*/tei:teiHeader/tei:encodingDesc/tei:listPrefixDef/tei:prefixDef[@ident=$prefix]">
	     <xsl:variable name="result">
	       <xsl:for-each
		   select="($context/ancestor::*/tei:teiHeader/tei:encodingDesc/tei:listPrefixDef/tei:prefixDef[@ident=$prefix])[1]">
		 <xsl:sequence select="replace($value,@matchPattern,@replacementPattern)"/>
	       </xsl:for-each>
	     </xsl:variable>
	     <xsl:choose>
	       <xsl:when test="$result=''">
		   <xsl:message terminate="yes">prefix pattern/replacement applied to <xsl:value-of
		   select="$value"/> returns an empty result</xsl:message>
	       </xsl:when>
	       <xsl:otherwise>
		 <xsl:sequence select="$result"/>
	       </xsl:otherwise>
	     </xsl:choose>
	   </xsl:when>
	   <xsl:otherwise>
	     <xsl:sequence select="."/>
	   </xsl:otherwise>
	 </xsl:choose>
       </xsl:matching-substring>
       <xsl:non-matching-substring>
	 <xsl:value-of select="$target"/>
       </xsl:non-matching-substring>
     </xsl:analyze-string>
  </xsl:function>


  <xsl:key match="entry" name="KEYS" use="key"/>
  <xsl:param name="documentationLanguage">en</xsl:param>

  <xsl:variable name="i18n"
		select="document('../i18n.xml',document(''))"/>

  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>[common] give language-specific version of a word or phrase<param name="word">the word(s) to translate</param>
      </desc>
   </doc>
  <xsl:function name="tei:i18n" as="xs:string">
      <xsl:param name="word"/>
      <xsl:variable name="Word">
         <xsl:value-of select="normalize-space($word)"/>
      </xsl:variable>
      <xsl:variable name="local">
         <xsl:call-template name="myi18n">
	   <xsl:with-param name="word">
	     <xsl:value-of select="$word"/>
	   </xsl:with-param>
         </xsl:call-template>
      </xsl:variable>
      <xsl:choose>
	<xsl:when test="string-length($local)&gt;0">
	  <xsl:value-of select="$local"/>
	</xsl:when>
	<xsl:otherwise>
	  <xsl:for-each select="$i18n">
	    <xsl:choose>
	      <xsl:when test="key('KEYS',$Word)/text[@xml:lang=$documentationLanguage]">
		  <xsl:value-of select="key('KEYS',$Word)/text[@xml:lang=$documentationLanguage]"/>
	      </xsl:when>
	      <xsl:when test="key('KEYS',$Word)/text[@lang3=$documentationLanguage]">
		<xsl:value-of select="key('KEYS',$Word)/text[lang3=$documentationLanguage]"/>
	      </xsl:when>
	      <xsl:otherwise>
		<!--
		    <xsl:if test="$verbose='true'">
		    <xsl:message>NO TRANSLATION for <xsl:value-of 
		    select="$word"/> in <xsl:value-of select="$documentationLanguage"/></xsl:message>
		      </xsl:if>
		  -->
		  <xsl:value-of select="key('KEYS',$Word)/text[@xml:lang='en']"/>
		</xsl:otherwise>
	      </xsl:choose>
            </xsl:for-each>
         </xsl:otherwise>
      </xsl:choose>
</xsl:function>

  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>[localisation] dummy template for overriding in a local system<param name="word">the word(s) to translate</param>
      </desc>
   </doc>
  <xsl:template name="myi18n">
    <xsl:param name="word"/>
  </xsl:template>

  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>[localisation] wrapper for legacy support</desc>
   </doc>
  <xsl:template name="i18n">
    <xsl:param name="word"/>
    <xsl:sequence select="tei:i18n($word)"/>
  </xsl:template>

  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>[common] Generate a title</desc>
   </doc>
  <xsl:function name="tei:generateTitle">
    <xsl:param name="context"/>
    <xsl:for-each select="$context">
      <xsl:choose>
         <xsl:when test="$useHeaderFrontMatter='true' and ancestor-or-self::tei:TEI/tei:text/tei:front//tei:docTitle">
            <xsl:apply-templates
		select="ancestor-or-self::tei:TEI/tei:text/tei:front//tei:docTitle/tei:titlePart"/>
         </xsl:when>

         <xsl:when test="$useHeaderFrontMatter='true' and ancestor-or-self::tei:teiCorpus/tei:text/tei:front//tei:docTitle">
            <xsl:apply-templates select="ancestor-or-self::tei:teiCorpus/tei:text/tei:front//tei:docTitle/tei:titlePart"/>
         </xsl:when>

         <xsl:when test="self::tei:teiCorpus">	
            <xsl:apply-templates select="tei:teiHeader/tei:fileDesc/tei:titleStmt/tei:title[not(@type='subordinate')]"/>
         </xsl:when>

         <xsl:otherwise>
            <xsl:for-each
		select="ancestor-or-self::tei:TEI/tei:teiHeader/tei:fileDesc/tei:titleStmt">
	      <xsl:choose>
		<xsl:when test="tei:title[@type='main']">
		  <xsl:apply-templates select="tei:title[@type='main']"/>
		</xsl:when>
		<xsl:otherwise>
		  <xsl:apply-templates select="tei:title"/>
		</xsl:otherwise>
	      </xsl:choose>
	    </xsl:for-each>
         </xsl:otherwise>
      </xsl:choose>
    </xsl:for-each>
  </xsl:function>

  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>[common] Find a plausible editor name</desc>
   </doc>
  <xsl:function name="tei:generateEditor" as="node()*">
    <xsl:param name="context"/>
    <xsl:for-each select="$context">
    <xsl:choose>
      <xsl:when test="ancestor-or-self::tei:TEI/tei:teiHeader/tei:fileDesc/tei:titleStmt/tei:editor">
        <xsl:for-each
          select="ancestor-or-self::tei:TEI/tei:teiHeader/tei:fileDesc/tei:titleStmt/tei:editor">
          <xsl:apply-templates/>
          <xsl:choose>
            <xsl:when test="count(following-sibling::tei:editor)=1">
              <xsl:if test="count(preceding-sibling::tei:editor)>=1">
                <xsl:text>, </xsl:text>
              </xsl:if>
	      <xsl:sequence select="tei:i18n('and')"/>
            </xsl:when>
            <xsl:when test="following-sibling::tei:editor">, </xsl:when>
          </xsl:choose>
        </xsl:for-each>
      </xsl:when>
      <xsl:when
        test="ancestor-or-self::tei:TEI/tei:teiHeader/tei:revisionDesc/tei:change/tei:respStmt[tei:resp='editor']">
        <xsl:apply-templates
          select="ancestor-or-self::tei:TEI/tei:teiHeader/tei:revisionDesc/tei:change/tei:respStmt[tei:resp='editor'][1]/tei:name"
        />
      </xsl:when>
    </xsl:choose>
    </xsl:for-each>
  </xsl:function>
  
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>[common] Find a plausible main author name</desc>
   </doc>
  <xsl:function name="tei:generateAuthor">
    <xsl:param name="context"/>
    <xsl:for-each select="$context">
      <xsl:choose>
         <xsl:when test="$useHeaderFrontMatter='true' and ancestor-or-self::tei:TEI/tei:text/tei:front//tei:docAuthor">
            <xsl:apply-templates mode="author"
                                 select="ancestor-or-self::tei:TEI/tei:text/tei:front//tei:docAuthor"/>
         </xsl:when>
         <xsl:when test="ancestor-or-self::tei:TEI/tei:teiHeader/tei:fileDesc/tei:titleStmt/tei:author">
	   <xsl:for-each select="ancestor-or-self::tei:TEI/tei:teiHeader/tei:fileDesc/tei:titleStmt/tei:author">
	     <xsl:apply-templates select="*[not(self::tei:email)]|text()"/>
	     <xsl:choose>
	       <xsl:when test="count(following-sibling::tei:author)=1">
		 <xsl:if test="count(preceding-sibling::tei:author)>1">
		   <xsl:text>,</xsl:text>
		 </xsl:if>
		 <xsl:text> </xsl:text>
		 <xsl:sequence select="tei:i18n('and')"/>
		 <xsl:text> </xsl:text>
	       </xsl:when>
	       <xsl:when test="following-sibling::tei:author">, </xsl:when>
	     </xsl:choose>
	   </xsl:for-each>
         </xsl:when>
         <xsl:when test="ancestor-or-self::tei:TEI/tei:teiHeader/tei:revisionDesc/tei:change/tei:respStmt[tei:resp='author']">
            <xsl:apply-templates select="ancestor-or-self::tei:TEI/tei:teiHeader/tei:revisionDesc/tei:change/tei:respStmt[tei:resp='author'][1]/tei:name"/>
         </xsl:when>
         <xsl:when test="ancestor-or-self::tei:TEI/tei:text/tei:front//tei:docAuthor">
            <xsl:apply-templates mode="author"
                                 select="ancestor-or-self::tei:TEI/tei:text/tei:front//tei:docAuthor"/>
         </xsl:when>
      </xsl:choose>
    </xsl:for-each>
  </xsl:function>

  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>[common] Find a plausible name of person responsible for current revision</desc>
   </doc>
  <xsl:function name="tei:generateRevAuthor" as="node()*">
    <xsl:param name="context"/>
    <xsl:for-each select="$context">
      <xsl:variable name="who">
         <xsl:choose>
            <xsl:when test="ancestor-or-self::tei:TEI/tei:teiHeader/tei:revisionDesc/@vcwho">
               <xsl:apply-templates select="ancestor-or-self::tei:TEI/tei:teiHeader/tei:revisionDesc/@vcwho"/>
            </xsl:when>
            <xsl:when test="ancestor-or-self::tei:TEI/tei:teiHeader/tei:revisionDesc/tei:change[1]/tei:respStmt/tei:name">
               <xsl:value-of select="ancestor-or-self::tei:TEI/tei:teiHeader/tei:revisionDesc/tei:change[1]/tei:respStmt/tei:name/text()"/>
            </xsl:when>
         </xsl:choose>
      </xsl:variable>
      <xsl:choose>
         <xsl:when test="normalize-space($who)=concat('$Author', '$')"/>
         <xsl:when test="starts-with($who,'$Author')">
        <!-- it's RCS -->
        <xsl:value-of select="normalize-space(substring-before(substring-after($who,'Author'),'$'))"/>
         </xsl:when>
         <xsl:when test="starts-with($who,'$LastChangedBy')">
        <!-- it's Subversion -->
        <xsl:value-of select="normalize-space(substring-before(substring-after($who,'LastChangedBy:'),'$'))"/>
         </xsl:when>
         <xsl:otherwise>
            <xsl:value-of select="$who"/>
         </xsl:otherwise>
      </xsl:choose>
    </xsl:for-each>
  </xsl:function>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>[common] Work out the last revision date of the document </desc>
   </doc>
  <xsl:function name="tei:generateRevDate" as="node()*">
    <xsl:param name="context"/>
    <xsl:for-each select="$context">
      <xsl:variable name="when">
         <xsl:choose>
            <xsl:when test="ancestor-or-self::tei:TEI/tei:teiHeader/tei:revisionDesc/@vcdate">
               <xsl:apply-templates select="ancestor-or-self::tei:TEI/tei:teiHeader/tei:revisionDesc/@vcdate"/>
            </xsl:when>
            <xsl:when test="ancestor-or-self::tei:TEI/tei:teiHeader/tei:revisionDesc/descendant::tei:date">
               <xsl:value-of select="ancestor-or-self::tei:TEI/tei:teiHeader/tei:revisionDesc/descendant::tei:date[1]"/>
            </xsl:when>
            <xsl:when
		test="ancestor-or-self::tei:TEI/tei:teiHeader/tei:fileDesc/descendant::tei:date">
	      <xsl:value-of select="ancestor-or-self::tei:TEI/tei:teiHeader/tei:fileDesc/descendant::tei:date"/>
	    </xsl:when>	    
         </xsl:choose>
      </xsl:variable>
      <xsl:choose>
         <xsl:when test="starts-with($when,'$Date')">
	   <!-- it's RCS -->
	   <xsl:value-of select="substring($when,16,2)"/>
	   <xsl:text>/</xsl:text>
	   <xsl:value-of select="substring($when,13,2)"/>
	   <xsl:text>/</xsl:text>
	   <xsl:value-of select="substring($when,8,4)"/>
         </xsl:when>
         <xsl:when test="starts-with($when,'$LastChangedDate')">
	   <!-- it's Subversion-->
	   <xsl:value-of select="substring-before(substring-after($when,'('),')')"/>
         </xsl:when>
         <xsl:when test="not($when='')">
	   <xsl:value-of select="$when"/>
	 </xsl:when>
	 <xsl:otherwise>
	   <xsl:value-of select="format-dateTime(current-dateTime(),'[Y]-[M02]-[D02]T[H02]:[m02]:[s02]Z')"/>
         </xsl:otherwise>
      </xsl:choose>
    </xsl:for-each>
  </xsl:function>
  
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>[common] Work out the publish date of the document </desc>
   </doc>
  <xsl:function name="tei:generateDate">
    <xsl:param name="context"/>
    <xsl:for-each select="$context">
      <xsl:choose>
	 <xsl:when test="$useFixedDate='true'">1970-01-01</xsl:when>
         <xsl:when test="$useHeaderFrontMatter='true' and ancestor-or-self::tei:TEI/tei:text/tei:front//tei:docDate">
            <xsl:apply-templates mode="date" select="ancestor-or-self::tei:TEI/tei:text/tei:front//tei:docDate"/>
         </xsl:when>
         <xsl:when
	     test="ancestor-or-self::tei:TEI/tei:teiHeader/tei:fileDesc/tei:editionStmt/descendant::tei:date[@when]">
            <xsl:value-of select="ancestor-or-self::tei:TEI/tei:teiHeader/tei:fileDesc/tei:editionStmt/descendant::tei:date[@when][1]/@when"/>
         </xsl:when>
         <xsl:when test="ancestor-or-self::tei:TEI/tei:teiHeader/tei:fileDesc/tei:editionStmt/descendant::tei:date">
            <xsl:value-of select="ancestor-or-self::tei:TEI/tei:teiHeader/tei:fileDesc/tei:editionStmt/descendant::tei:date[1]"/>
         </xsl:when>
         <xsl:when test="ancestor-or-self::tei:TEI/tei:teiHeader/tei:fileDesc/tei:publicationStmt/tei:date">
            <xsl:value-of select="ancestor-or-self::tei:TEI/tei:teiHeader/tei:fileDesc/tei:publicationStmt/tei:date"/>
         </xsl:when>
         <xsl:when test="ancestor-or-self::tei:TEI/tei:teiHeader/tei:fileDesc/tei:editionStmt/tei:edition">
            <xsl:apply-templates select="ancestor-or-self::tei:TEI/tei:teiHeader/tei:fileDesc/tei:editionStmt/tei:edition"/>
         </xsl:when>
	 <xsl:when
	     test="ancestor-or-self::tei:TEI/tei:teiHeader/tei:revisionDesc/tei:change[@when
		   or tei:date]">
            <xsl:for-each
		select="ancestor-or-self::tei:TEI/tei:teiHeader/tei:revisionDesc/tei:change[1]">
	      <xsl:choose>
		<xsl:when test="@when">
		  <xsl:value-of select="@when"/>
		</xsl:when>
		<xsl:when test="tei:date/@when">
		  <xsl:value-of select="tei:date/@when"/>
		</xsl:when>
		<xsl:when test="tei:date">
		  <xsl:value-of select="tei:date"/>
		</xsl:when>
		<xsl:otherwise>
		  <xsl:value-of select="format-dateTime(current-dateTime(),'[Y]-[M02]-[D02]')"/>
		</xsl:otherwise>
	      </xsl:choose>
	    </xsl:for-each>
	 </xsl:when>
	 <xsl:otherwise>
	   <xsl:value-of select="format-dateTime(current-dateTime(),'[Y]-[M02]-[D02]')"/>
	 </xsl:otherwise>
      </xsl:choose>
    </xsl:for-each>
  </xsl:function>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>
         <p>[common] </p>
         <p>Generate simple title with no markup</p>
      </desc>
   </doc>
  <xsl:function name="tei:generateSimpleTitle" >
    <xsl:param name="context"/>
    <xsl:for-each select="$context">
      <xsl:choose>
         <xsl:when test="$useHeaderFrontMatter='true' and ancestor-or-self::tei:TEI/tei:text/tei:front//tei:docTitle">
            <xsl:apply-templates select="ancestor-or-self::tei:TEI/tei:text/tei:front//tei:docTitle"
                                 mode="simple"/>
         </xsl:when>
         <xsl:when test="ancestor-or-self::tei:teiCorpus">	
            <xsl:apply-templates
		select="tei:teiHeader/tei:fileDesc/tei:titleStmt/tei:title[not(@type='subordinate')]"
		mode="simple"/>
         </xsl:when>
         <xsl:otherwise>
            <xsl:for-each
		select="ancestor-or-self::tei:TEI/tei:teiHeader/tei:fileDesc/tei:titleStmt">
	      <xsl:choose>
		<xsl:when test="tei:title[@type='main']">
		  <xsl:apply-templates
		      select="tei:title[@type='main']" mode="simple"/>
		</xsl:when>
		<xsl:otherwise>
		  <xsl:apply-templates select="tei:title[1]" mode="simple"/>
		</xsl:otherwise>
	      </xsl:choose>
	    </xsl:for-each>
         </xsl:otherwise>
      </xsl:choose>
    </xsl:for-each>
  </xsl:function>

  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>[common] Generate subtitle </desc>
   </doc>
  <xsl:function name="tei:generateSubTitle">
    <xsl:param name="context"/>
    <xsl:for-each select="$context">
      <xsl:choose>
         <xsl:when test="$useHeaderFrontMatter='true' and ancestor-or-self::tei:TEI/tei:text/tei:front//tei:docTitle">
            <xsl:apply-templates select="ancestor-or-self::tei:TEI/tei:text/tei:front//tei:docTitle"/>
         </xsl:when>
         <xsl:when test="$useHeaderFrontMatter='true' and ancestor-or-self::tei:teiCorpus/tei:text/tei:front//tei:docTitle">
            <xsl:apply-templates select="ancestor-or-self::tei:teiCorpus/tei:text/tei:front//tei:docTitle"/>
         </xsl:when>
         <xsl:otherwise>
            <xsl:for-each select="ancestor-or-self::tei:TEI|ancestor-or-self::tei:teiCorpus">
               <xsl:apply-templates select="tei:teiHeader/tei:fileDesc/tei:titleStmt/tei:title[@type='subordinate']"/>
            </xsl:for-each>
         </xsl:otherwise>
      </xsl:choose>
    </xsl:for-each>
  </xsl:function>


  <xsl:function name="tei:generateEdition">
    <xsl:param name="context"/>
    <xsl:for-each select="$context">
    <xsl:value-of
	select="tei:TEI/tei:teiHeader/tei:fileDesc/tei:editionStmt/tei:edition"/>
    </xsl:for-each>
  </xsl:function>


  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc>[epub] Set name of publisher</desc>
  </doc>
  <xsl:function name="tei:generatePublisher" as="node()*">
    <xsl:param name="context"/>
    <xsl:param name="default"/>
    <xsl:for-each select="$context">
    <xsl:choose>
      <xsl:when test="not($default='')">
        <xsl:value-of select="$default"/>
      </xsl:when>
      <xsl:otherwise>
        <xsl:for-each
	    select="ancestor-or-self::tei:TEI/tei:teiHeader/tei:fileDesc/tei:publicationStmt">
	  <xsl:for-each
	      select="tei:authority|tei:publisher|tei:distributor|tei:p">
	    <xsl:value-of select="normalize-space(.)"/>
	    <xsl:if
		test="following-sibling::tei:authority|tei:publisher|tei:distributor|tei:p">
	      <xsl:text>, </xsl:text>
	    </xsl:if>
	  </xsl:for-each>
	</xsl:for-each>
      </xsl:otherwise>
    </xsl:choose>
    </xsl:for-each>
  </xsl:function>


  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc>whether a note is a footnote</desc>
  </doc>
  <xsl:function name="tei:isFootNote" as="xs:boolean">
    <xsl:param name="context"/>
    <xsl:for-each select="$context">
      <xsl:choose>
	<xsl:when test="@place='foot' or @place='bottom' or @place='parend' or @place='tablefoot'
		  and not(parent::tei:bibl or  ancestor::tei:teiHeader)">true</xsl:when>
	<xsl:otherwise>false</xsl:otherwise>
      </xsl:choose>
    </xsl:for-each>
  </xsl:function>

  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc>whether a note is an endnote</desc>
  </doc>
  <xsl:function name="tei:isEndNote" as="xs:boolean">
    <xsl:param name="context"/>
    <xsl:for-each select="$context">
      <xsl:choose>
	<xsl:when test="@place='end'">true</xsl:when>
	<xsl:otherwise>false</xsl:otherwise>
      </xsl:choose>
    </xsl:for-each>
  </xsl:function>

  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc>whether a list is to be rendered as ordered</desc>
  </doc>
  <xsl:function name="tei:isOrderedList" as="xs:boolean">
    <xsl:param name="context"/>
    <xsl:for-each select="$context">
      <xsl:choose>
	<xsl:when test="@rend='numbered'">true</xsl:when>
	<xsl:when test="@rend='ordered'">true</xsl:when>
	<xsl:when test="@type='ordered'">true</xsl:when>
	<xsl:otherwise>false</xsl:otherwise>
      </xsl:choose>
    </xsl:for-each>
  </xsl:function>

  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc>whether a list is to be rendered as unordered</desc>
  </doc>
  <xsl:function name="tei:isUnorderedList" as="xs:boolean">
    <xsl:param name="context"/>
    <xsl:for-each select="$context">
      <xsl:choose>
	<xsl:when test="not(@rend or @type)">true</xsl:when>
	<xsl:when test="@rend='unordered'">true</xsl:when>
	<xsl:when test="@type='unordered'">true</xsl:when>
	<xsl:when test="@type='simple'">true</xsl:when>
	<xsl:otherwise>false</xsl:otherwise>
      </xsl:choose>
    </xsl:for-each>
  </xsl:function>

  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc>whether a list is to be rendered as a gloss list</desc>
  </doc>
  <xsl:function name="tei:isGlossList" as="xs:boolean">
    <xsl:param name="context"/>
    <xsl:for-each select="$context">
      <xsl:choose>
	<xsl:when test="@rend='valList'">true</xsl:when>
	<xsl:when test="@rend='gloss'">true</xsl:when>
	<xsl:when test="@type='gloss'">true</xsl:when>
	<xsl:when test="tei:label">true</xsl:when>
	<xsl:otherwise>false</xsl:otherwise>
      </xsl:choose>
    </xsl:for-each>
  </xsl:function>

  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc>whether a list is to be rendered as a gloss table</desc>
  </doc>
  <xsl:function name="tei:isGlossTable" as="xs:boolean">
    <xsl:param name="context"/>
    <xsl:for-each select="$context">
      <xsl:choose>
	<xsl:when test="@type='valList'">true</xsl:when>
	<xsl:when test="@rend='glosstable'">true</xsl:when>
	<xsl:when test="@type='glosstable'">true</xsl:when>
	<xsl:otherwise>false</xsl:otherwise>
      </xsl:choose>
    </xsl:for-each>
  </xsl:function>

  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc>whether a list is to be rendered inline</desc>
  </doc>
  <xsl:function name="tei:isInlineList" as="xs:boolean">
    <xsl:param name="context"/>
    <xsl:for-each select="$context">
      <xsl:choose>
	<xsl:when test="@rend='inline'">true</xsl:when>
	<xsl:when test="@type='inline'">true</xsl:when>
	<xsl:when test="ancestor::tei:head or parent::tei:label">true</xsl:when>
	<xsl:otherwise>false</xsl:otherwise>
      </xsl:choose>
    </xsl:for-each>
  </xsl:function>

</xsl:stylesheet>
