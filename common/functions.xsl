<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet 
    xmlns:ann="http://relaxng.org/ns/compatibility/annotations/1.0"
 xmlns:sch="http://purl.oclc.org/dsdl/schematron"
xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
xmlns:cals="http://www.oasis-open.org/specs/tm9901"
xmlns:tei="http://www.tei-c.org/ns/1.0"
xmlns:iso="http://www.iso.org/ns/1.0"
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
xmlns:tbx="http://www.lisa.org/TBX-Specification.33.0.html"
xmlns:pic="http://schemas.openxmlformats.org/drawingml/2006/picture"
xmlns:teidocx="http://www.tei-c.org/ns/teidocx/1.0" version="2.0"
    exclude-result-prefixes="#all"
>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" scope="stylesheet" type="stylesheet">
    <desc>
      <p> TEI Utility stylesheet defining functions for use in all
         output formats.</p>
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
  <xsl:key match="tei:schemaSpec" name="LISTSCHEMASPECS" use="@ident"/>
  <xsl:key match="tei:schemaSpec" name="SCHEMASPECS" use="1"/>
  <xsl:param name="oddmode">tei</xsl:param>
  <xsl:param name="ignoreXmlBase">false</xsl:param>
  <xsl:param name="selectedSchema"/>
  <xsl:param name="doclang"/>
  <xsl:variable name="top" select="/"/>
  <xsl:variable name="whichSchemaSpec">
    <xsl:choose>
      <xsl:when test="$selectedSchema">
        <xsl:value-of select="$selectedSchema"/>
      </xsl:when>
      <xsl:otherwise>
        <xsl:value-of select="key('SCHEMASPECS',1)[1]/@ident"/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:variable>
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
  <xsl:function name="tei:isIdentifiable" as="xs:boolean">
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
  <xsl:function name="tei:isTranscribable" as="xs:boolean">
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
        <xsl:when test="ancestor-or-self::*[@rend][tei:match(@rend,'smallcaps')]">true</xsl:when>
        <xsl:when test="ancestor-or-self::*[@rend][tei:match(@rend,'sc')]">true</xsl:when>
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
        <xsl:when test="ancestor-or-self::*[@rend][tei:match(@rend,'quotes')]">true</xsl:when>
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
        <xsl:when test="tei:match(@rend,'odd_label')">true</xsl:when>
        <xsl:when test="parent::tei:hi[starts-with(@rend,'specList-')]">true</xsl:when>
        <xsl:when test="self::tei:docAuthor">true</xsl:when>
        <xsl:when test="self::tei:label[following-sibling::tei:item]">true</xsl:when>
        <xsl:when test="starts-with(@rend,'specList-')">true</xsl:when>
        <xsl:when test="starts-with(parent::tei:hi/@rend,'specList-')">true</xsl:when>
        <xsl:when test="tei:match(@rend,'label')">true</xsl:when>
        <xsl:when test="tei:match(@rend,'wovenodd')">true</xsl:when>
        <xsl:when test="tei:match(@rend,'important')">true</xsl:when>
        <xsl:when test="tokenize(@rend,' ')=('Heading_2_Char')">true</xsl:when>
        <xsl:when test="tei:match(@rend,'specChildModule')">true</xsl:when>
        <xsl:when test="ancestor-or-self::tei:cell[tei:match(@rend,'wovenodd-col1')]">true</xsl:when>
        <xsl:when test="ancestor-or-self::tei:cell[@role='label']">true</xsl:when>
        <xsl:when test="ancestor-or-self::*[@rend][tei:match(@rend,'bold')]">true</xsl:when>
        <xsl:when test="parent::tei:hi[starts-with(@rend,'specList-')]">true</xsl:when>
        <xsl:when test="self::tei:cell and parent::tei:row[@role='label']">true</xsl:when>
        <xsl:when test="self::tei:label[following-sibling::tei:item]">true</xsl:when>
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
      <xsl:for-each select="ancestor-or-self::*[@rend][matches(@rend,'color\(')]">
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
      <xsl:for-each select="ancestor-or-self::*[@rend][matches(@rend,'background\(')]">
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
        <xsl:when test="ancestor-or-self::*[@rend][tei:match(@rend,'italic')]">true</xsl:when>
        <xsl:when test="self::tei:emph">true</xsl:when>
        <xsl:when test="self::tei:hi[not(@rend)]">true</xsl:when>
        <xsl:when test="self::tbx:hi[@style='italics']">true</xsl:when>
        <xsl:when test="tei:match(@rend,'ital')">true</xsl:when>
        <xsl:when test="tei:match(@rend,'it')">true</xsl:when>
        <xsl:when test="tei:match(@rend,'i')">true</xsl:when>
        <xsl:when test="tei:match(@rend,'att')">true</xsl:when>
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
        <xsl:when test="ancestor-or-self::*[@rend][tei:match(@rend,'typewriter')]">true</xsl:when>
        <xsl:when test="ancestor-or-self::*[@rend][tei:match(@rend,'code')]">true</xsl:when>
        <xsl:otherwise>false</xsl:otherwise>
      </xsl:choose>
    </xsl:for-each>
  </xsl:function>

  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc>Whether to render something in underline.</desc>
  </doc>
  <xsl:function name="tei:render-underline" as="xs:boolean">
    <xsl:param name="element"/>
    <xsl:for-each select="$element">
      <xsl:choose>
        <xsl:when test="ancestor-or-self::*[@rend][tei:match(@rend,'ul')]">true</xsl:when>
        <xsl:when test="ancestor-or-self::*[@rend][tei:match(@rend,'underline')]">true</xsl:when>
        <xsl:when test="ancestor-or-self::*[@rend][tei:match(@rend,'UL')]">true</xsl:when>
        <xsl:otherwise>false</xsl:otherwise>
      </xsl:choose>
    </xsl:for-each>
  </xsl:function>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc>Whether to render something in superscript.</desc>
  </doc>
  <xsl:function name="tei:render-superscript" as="xs:boolean">
    <xsl:param name="element"/>
    <xsl:for-each select="$element">
      <xsl:choose>
        <xsl:when test="ancestor-or-self::*[@rend][tei:match(@rend,'sup')]">true</xsl:when>
        <xsl:when test="ancestor-or-self::*[@rend][tei:match(@rend,'superscript')]">true</xsl:when>
        <xsl:otherwise>false</xsl:otherwise>
      </xsl:choose>
    </xsl:for-each>
  </xsl:function>

  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc>Whether to render something in subscript.</desc>
  </doc>
  <xsl:function name="tei:render-subscript" as="xs:boolean">
    <xsl:param name="element"/>
    <xsl:for-each select="$element">
      <xsl:choose>
        <xsl:when test="ancestor-or-self::*[@rend][tei:match(@rend,'sup')]">true</xsl:when>
        <xsl:when test="ancestor-or-self::*[@rend][tei:match(@rend,'subscript')]">true</xsl:when>
        <xsl:otherwise>false</xsl:otherwise>
      </xsl:choose>
    </xsl:for-each>
  </xsl:function>

  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc>Whether to render something in strikethrough.</desc>
  </doc>
  <xsl:function name="tei:render-strike" as="xs:boolean">
    <xsl:param name="element"/>
    <xsl:for-each select="$element">
      <xsl:choose>
        <xsl:when test="ancestor-or-self::*[@rend][tei:match(@rend,'strikethrough')]">true</xsl:when>
        <xsl:otherwise>false</xsl:otherwise>
      </xsl:choose>
    </xsl:for-each>
  </xsl:function>

  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc>Is given an element and defines whether or not this element is to be rendered inline.</desc>
  </doc>
  <xsl:function name="tei:isInline" as="xs:boolean">
    <xsl:param name="element"/>
    <xsl:choose>
      <xsl:when test="empty($element)">true</xsl:when>
      <xsl:otherwise>
        <xsl:for-each select="$element">
          <xsl:choose>
            <xsl:when test="parent::tei:div">false</xsl:when>
            <xsl:when test="parent::tei:titlePage">false</xsl:when>
            <xsl:when test="parent::tei:body">false</xsl:when>
            <xsl:when test="parent::tei:front">false</xsl:when>
            <xsl:when test="parent::tei:back">false</xsl:when>
            <xsl:when test="self::tei:body">false</xsl:when>
            <xsl:when test="self::tei:front">false</xsl:when>
            <xsl:when test="self::tei:back">false</xsl:when>
            <xsl:when test="not(self::*)">true</xsl:when>
            <xsl:when test="parent::tei:bibl/parent::tei:q">true</xsl:when>
            <xsl:when test="tei:match(@rend,'inline') and not(tei:p or tei:l)">true</xsl:when>
            <xsl:when test="self::tei:note[@place='display']">false</xsl:when>
            <xsl:when test="self::tei:note[@place='block']">false</xsl:when>
            <xsl:when test="self::tei:note[tei:isEndNote(.)]">true</xsl:when>
            <xsl:when test="self::tei:note[tei:isFootNote(.)]">true</xsl:when>
            <xsl:when test="tei:match(@rend,'display') or tei:match(@rend,'block')">false</xsl:when>
            <xsl:when test="@type='display' or @type='block'">false</xsl:when>
            <xsl:when test="tei:table or tei:figure or tei:list or tei:lg    or tei:q/tei:l or tei:l or tei:p or tei:biblStruct or tei:sp or tei:floatingText">false</xsl:when>
            <xsl:when test="self::tei:cit[not(@rend)]">true</xsl:when>
            <xsl:when test="parent::tei:cit[tei:match(@rend,'display')]">false</xsl:when>
            <xsl:when test="parent::tei:cit and (tei:p or tei:l)">false</xsl:when>
            <xsl:when test="parent::tei:cit and parent::cit/tei:bibl">false</xsl:when>
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
            <xsl:when test="self::tei:bibl and not (tei:isInline(preceding-sibling::*[1]))">false</xsl:when>
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
            <xsl:when test="self::tei:g">true</xsl:when>
            <xsl:when test="self::tei:gap">true</xsl:when>
            <xsl:when test="self::tei:genName">true</xsl:when>
            <xsl:when test="self::tei:geogName">true</xsl:when>
            <xsl:when test="self::tei:gi">true</xsl:when>
            <xsl:when test="self::tei:gloss">true</xsl:when>
            <xsl:when test="self::tei:graphic">true</xsl:when>
            <xsl:when test="self::tei:media">true</xsl:when>
            <xsl:when test="self::tei:height">true</xsl:when>
            <xsl:when test="self::tei:hi[not(w:*)]">true</xsl:when>
            <xsl:when test="self::tei:ident">true</xsl:when>
            <xsl:when test="self::tei:idno">true</xsl:when>
            <xsl:when test="self::tei:imprint">true</xsl:when>
            <xsl:when test="self::tei:institution">true</xsl:when>
            <xsl:when test="self::tei:label[not(parent::tei:list)]">true</xsl:when>
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
            <xsl:when test="not(self::tei:p) and tei:isInline($element/..)">true</xsl:when>
            <xsl:otherwise>false</xsl:otherwise>
          </xsl:choose>
        </xsl:for-each>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:function>
  <xsl:function name="tei:isLast" as="xs:boolean">
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
  <xsl:function name="tei:isFirst" as="xs:boolean">
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
  <xsl:template match="text()" mode="#default plain">
    <xsl:choose>
      <xsl:when test="ancestor::*[@xml:space][1]/@xml:space='preserve'">
        <xsl:value-of select="tei:escapeChars(.,parent::*)"/>
      </xsl:when>
      <xsl:otherwise>
        <!-- Retain one leading space if node isn't first, has
             non-space content, and has leading space.-->
        <xsl:variable name="context" select="name(parent::*)"/>
        <xsl:if test="matches(.,'^\s') and  normalize-space()!=''">
          <!-- if the text is first thing in a note, zap it,  definitely -->
          <xsl:choose>
            <xsl:when test="(tei:isFootNote(..) or tei:isEndNote(..))
                            and position()=1"/>
            <!-- but if its in a run of inline objects with the same
            name (like a sequence of <hi>), then the space needs
            keeping -->
            <xsl:when test="(tei:isInline(parent::*)  and
                            parent::*/preceding-sibling::node()[1][name()=$context])">
                      <xsl:call-template name="space"/>
              <xsl:call-template name="space"/>
            </xsl:when>
            <xsl:when test="position()=1"/>
            <xsl:otherwise>
              <xsl:call-template name="space"/>
            </xsl:otherwise>
          </xsl:choose>
        </xsl:if>
        <xsl:value-of select="tei:escapeChars(normalize-space(.),parent::*)"/>
        <xsl:choose>
          <!-- node is an only child, and has content but it's all space -->
          <xsl:when test="last()=1 and string-length()!=0 and      normalize-space()=''">
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
      just convert long s to short s, but some formats may escape some characters.</desc>
  </doc>
  <xsl:function name="tei:escapeChars" as="xs:string">
    <xsl:param name="letters"/>
    <xsl:param name="context"/>
    <xsl:value-of select="$letters"/>
  </xsl:function>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc>[common] process target urls, looking for magic patterns</desc>
  </doc>
  <xsl:function name="tei:resolveURI" as="xs:string">
    <xsl:param name="context"/>
    <xsl:param name="target"/>
    <xsl:analyze-string select="normalize-space($target)" regex="^(\w+):(.+)$">
      <xsl:matching-substring>
        <xsl:variable name="prefix" select="regex-group(1)"/>
        <xsl:variable name="value" select="regex-group(2)"/>
        <xsl:choose>
          <xsl:when test="$context/ancestor::*/tei:teiHeader/tei:encodingDesc/tei:listPrefixDef/tei:prefixDef[@ident=$prefix]">
            <xsl:variable name="result">
              <xsl:for-each select="($context/ancestor::*/tei:teiHeader/tei:encodingDesc/tei:listPrefixDef/tei:prefixDef[@ident=$prefix])[1]">
                <xsl:sequence select="replace($value,@matchPattern,@replacementPattern)"/>
              </xsl:for-each>
            </xsl:variable>
            <xsl:choose>
              <xsl:when test="$result=''">
                <xsl:message terminate="yes">prefix pattern/replacement applied to <xsl:value-of select="$value"/> returns an empty result</xsl:message>
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
        <xsl:variable name="base" select="$context/ancestor-or-self::*[@xml:base][1]/@xml:base"/>
        <xsl:sequence select="if (starts-with($base,'file:') or $ignoreXmlBase='true') then
                              $target else concat(replace($base,'/[^/]+$','/'),$target)"/>
      </xsl:non-matching-substring>
    </xsl:analyze-string>
  </xsl:function>
  <xsl:key match="entry" name="KEYS" use="key"/>
  <xsl:param name="documentationLanguage">en</xsl:param>
  <xsl:variable name="i18n" select="document('../i18n.xml',document(''))"/>
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
  <xsl:function name="tei:generateTitle"  as="node()*">
    <xsl:param name="context"/>
    <xsl:for-each select="$context">
      <xsl:choose>
        <xsl:when test="$useHeaderFrontMatter='true' and ancestor-or-self::tei:TEI/tei:teiHeader/tei:fileDesc/tei:titleStmt">
          <xsl:apply-templates
              select="ancestor-or-self::tei:TEI/tei:teiHeader/tei:fileDesc/tei:titleStmt"/>
        </xsl:when>
        <xsl:when test="ancestor-or-self::tei:TEI/tei:text/tei:front//tei:docTitle">
          <xsl:apply-templates select="ancestor-or-self::tei:TEI/tei:text/tei:front//tei:docTitle/tei:titlePart"/>
        </xsl:when>
        <xsl:when test="ancestor-or-self::tei:teiCorpus/tei:text/tei:front//tei:docTitle">
          <xsl:apply-templates select="ancestor-or-self::tei:teiCorpus/tei:text/tei:front//tei:docTitle/tei:titlePart"/>
        </xsl:when>
        <xsl:when test="ancestor-or-self::tei:teiCorpus">
          <xsl:apply-templates select="tei:teiHeader/tei:fileDesc/tei:titleStmt/tei:title[not(@type='subordinate')]"/>
        </xsl:when>
        <xsl:otherwise>
          <xsl:for-each select="ancestor-or-self::tei:TEI/tei:teiHeader/tei:fileDesc/tei:titleStmt">
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
    <desc>[common] Generate a title for metadata context</desc>
  </doc>
  <xsl:function name="tei:generateMetadataTitle"  as="node()*">
    <xsl:param name="context"/>
    <xsl:variable name="r">
      <xsl:for-each select="$context">
        <xsl:choose>
          <xsl:when test="ancestor-or-self::tei:teiCorpus">
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
    </xsl:variable>
    <xsl:value-of select="normalize-space($r)"/>
  </xsl:function>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc>[common] Find a plausible editor name</desc>
  </doc>
  <xsl:function name="tei:generateEditor" as="node()*">
    <xsl:param name="context"/>
    <xsl:for-each select="$context">
      <xsl:choose>
        <xsl:when test="ancestor-or-self::tei:TEI/tei:teiHeader/tei:fileDesc/tei:titleStmt/tei:editor">
          <xsl:for-each select="ancestor-or-self::tei:TEI/tei:teiHeader/tei:fileDesc/tei:titleStmt/tei:editor">
            <xsl:apply-templates/>
            <xsl:choose>
              <xsl:when test="count(following-sibling::tei:editor)=1">
                <xsl:if test="count(preceding-sibling::tei:editor)&gt;=1">
                  <xsl:text>, </xsl:text>
                </xsl:if>
                <xsl:value-of select="tei:i18n('and')"/>
                <xsl:text> </xsl:text>
              </xsl:when>
              <xsl:when test="following-sibling::tei:editor">, </xsl:when>
            </xsl:choose>
          </xsl:for-each>
        </xsl:when>
        <xsl:when test="ancestor-or-self::tei:TEI/tei:teiHeader/tei:revisionDesc/tei:change/tei:respStmt[tei:resp='editor']">
          <xsl:apply-templates select="ancestor-or-self::tei:TEI/tei:teiHeader/tei:revisionDesc/tei:change/tei:respStmt[tei:resp='editor'][1]/tei:name"/>
        </xsl:when>
      </xsl:choose>
    </xsl:for-each>
  </xsl:function>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc>[common] Find a plausible main author name</desc>
  </doc>
  <xsl:function name="tei:generateAuthor"  as="node()*">
    <xsl:param name="context"/>
      <xsl:variable name="result">
    <xsl:for-each select="$context">
      <xsl:choose>
        <xsl:when test="$useHeaderFrontMatter='true' and
                        ancestor-or-self::tei:TEI/tei:teiHeader/tei:fileDesc/tei:titleStmt/tei:author">
          <xsl:apply-templates mode="author" select="ancestor-or-self::tei:TEI/tei:teiHeader/tei:fileDesc/tei:titleStmt/tei:author"/>
        </xsl:when>
        <xsl:when
            test="ancestor-or-self::tei:TEI/tei:text/tei:front//tei:docAuthor">
          <xsl:apply-templates mode="author"
                               select="ancestor-or-self::tei:TEI/tei:text/tei:front//tei:docAuthor"/>
        </xsl:when>
        <xsl:when
            test="ancestor-or-self::tei:TEI/tei:teiHeader/tei:fileDesc/tei:titleStmt/tei:author">
          <xsl:apply-templates mode="author" select="ancestor-or-self::tei:TEI/tei:teiHeader/tei:fileDesc/tei:titleStmt/tei:author"/>
        </xsl:when>
        <xsl:when
            test="ancestor-or-self::tei:teiCorpus/tei:teiHeader/tei:fileDesc/tei:titleStmt/tei:author">
          <xsl:apply-templates mode="author" select="ancestor-or-self::tei:teiCorpus/tei:teiHeader/tei:fileDesc/tei:titleStmt/tei:author"/>
        </xsl:when>
        <xsl:when
            test="ancestor-or-self::tei:TEI/tei:teiHeader/tei:revisionDesc/tei:change/tei:respStmt[tei:resp='author']">
          <xsl:apply-templates select="ancestor-or-self::tei:TEI/tei:teiHeader/tei:revisionDesc/tei:change/tei:respStmt[tei:resp='author'][1]/tei:name"/>
        </xsl:when>
      </xsl:choose>
    </xsl:for-each>
  </xsl:variable>
  <xsl:sequence select="$result"/>
  </xsl:function>


  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc>[common] Find a plausible main author name in metadata context</desc>
  </doc>
  <xsl:function name="tei:generateMetadataAuthor"  as="node()*">
    <xsl:param name="context"/>
    <xsl:variable name="r">
      <xsl:for-each select="$context">
        <xsl:choose>
          <xsl:when test="ancestor-or-self::tei:TEI/tei:teiHeader/tei:fileDesc/tei:titleStmt/tei:author">
            <xsl:apply-templates mode="author" select="ancestor-or-self::tei:TEI/tei:teiHeader/tei:fileDesc/tei:titleStmt/tei:author"/>
          </xsl:when>
          <xsl:when test="ancestor-or-self::tei:teiCorpus/tei:teiHeader/tei:fileDesc/tei:titleStmt/tei:author">
            <xsl:apply-templates mode="author" select="ancestor-or-self::tei:teiCorpus/tei:teiHeader/tei:fileDesc/tei:titleStmt/tei:author"/>
          </xsl:when>
        <xsl:when test="ancestor-or-self::tei:TEI/tei:teiHeader/tei:revisionDesc/tei:change/tei:respStmt[tei:resp='author']">
          <xsl:apply-templates select="ancestor-or-self::tei:TEI/tei:teiHeader/tei:revisionDesc/tei:change/tei:respStmt[tei:resp='author'][1]/tei:name"/>
        </xsl:when>
        </xsl:choose>
      </xsl:for-each>
    </xsl:variable>
    <xsl:value-of select="normalize-space($r)"/>
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
          <xsl:when test="ancestor-or-self::tei:TEI/tei:teiHeader/tei:fileDesc/descendant::tei:date">
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
  <xsl:function name="tei:checkNormalizedDate">
    <xsl:param name="context"/>
    <xsl:value-of select="if ($context/@when) then $context/@when else $context"/>
  </xsl:function>

  <xsl:function name="tei:generateDate">
    <xsl:param name="context"/>
    <xsl:for-each select="$context">
      <xsl:choose>
        <xsl:when test="$useFixedDate='true'">1970-01-01</xsl:when>
        <xsl:when test="$useHeaderFrontMatter='true' and ancestor-or-self::tei:TEI/tei:text/tei:front//tei:docDate">
          <xsl:apply-templates mode="date" select="tei:checkNormalizedDate(ancestor-or-self::tei:TEI/tei:text/tei:front//tei:docDate)"/>
        </xsl:when>
        <xsl:when test="ancestor-or-self::tei:TEI/tei:teiHeader/tei:fileDesc/tei:editionStmt/descendant::tei:date">
          <xsl:value-of select="tei:checkNormalizedDate(ancestor-or-self::tei:TEI/tei:teiHeader/tei:fileDesc/tei:editionStmt/descendant::tei:date[1])"/>
        </xsl:when>
        <xsl:when test="ancestor-or-self::tei:TEI/tei:teiHeader/tei:fileDesc/tei:publicationStmt/tei:date">
          <xsl:value-of select="tei:checkNormalizedDate(ancestor-or-self::tei:TEI/tei:teiHeader/tei:fileDesc/tei:publicationStmt/tei:date)"/>
        </xsl:when>
        <xsl:when test="ancestor-or-self::tei:TEI/tei:teiHeader/tei:fileDesc/tei:editionStmt/tei:edition">
          <xsl:apply-templates select="ancestor-or-self::tei:TEI/tei:teiHeader/tei:fileDesc/tei:editionStmt/tei:edition"/>
        </xsl:when>
        <xsl:when test="ancestor-or-self::tei:TEI/tei:teiHeader/tei:revisionDesc/tei:change[@when      or tei:date]">
          <xsl:for-each select="ancestor-or-self::tei:TEI/tei:teiHeader/tei:revisionDesc/tei:change[1]">
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
  <xsl:function name="tei:generateSimpleTitle">
    <xsl:param name="context"/>
    <xsl:for-each select="$context">
      <xsl:choose>
        <xsl:when test="$useHeaderFrontMatter='true' and ancestor-or-self::tei:TEI/tei:text/tei:front//tei:docTitle">
          <xsl:apply-templates select="ancestor-or-self::tei:TEI/tei:text/tei:front//tei:docTitle" mode="simple"/>
        </xsl:when>
        <xsl:when test="ancestor-or-self::tei:teiCorpus">
          <xsl:apply-templates select="ancestor-or-self::tei:teiCorpus/tei:teiHeader/tei:fileDesc/tei:titleStmt/tei:title[not(@type='subordinate')]" mode="simple"/>
        </xsl:when>
        <xsl:otherwise>
          <xsl:for-each select="ancestor-or-self::tei:TEI/tei:teiHeader/tei:fileDesc/tei:titleStmt">
            <xsl:choose>
              <xsl:when test="tei:title[@type='main']">
                <xsl:apply-templates select="tei:title[@type='main']" mode="simple"/>
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
      <xsl:value-of select="normalize-space(tei:TEI/tei:teiHeader/tei:fileDesc/tei:editionStmt/tei:edition)"/>
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
          <xsl:for-each select="/*/tei:teiHeader/tei:fileDesc/tei:publicationStmt">
            <xsl:for-each select="tei:authority|tei:publisher|tei:distributor|tei:p">
              <xsl:value-of select="normalize-space(.)"/>
              <xsl:if test="following-sibling::tei:authority|tei:publisher|tei:distributor|tei:p">
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
        <xsl:when test="ancestor::tei:listBibl">false</xsl:when>
        <xsl:when test="@place='foot'">true</xsl:when>
        <xsl:when test="@place='bottom'">true</xsl:when>
        <xsl:when test="@place='parend'">true</xsl:when>
        <xsl:when test="@place='tablefoot'">true</xsl:when>
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
        <xsl:when test="tei:match(@rend,'numbered')">true</xsl:when>
        <xsl:when test="tei:match(@rend,'ordered')">true</xsl:when>
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
        <xsl:when test="tei:match(@rend,'unordered')">true</xsl:when>
        <xsl:when test="tei:match(@rend,'bulleted')">true</xsl:when>
        <xsl:when test="@type='unordered'">true</xsl:when>
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
        <xsl:when test="tei:match(@rend,'valList')">true</xsl:when>
        <xsl:when test="tei:match(@rend,'gloss')">true</xsl:when>
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
        <xsl:when test="tei:match(@rend,'glosstable')">true</xsl:when>
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
        <xsl:when test="tei:match(@rend,'inline')">true</xsl:when>
        <xsl:when test="@type='inline'">true</xsl:when>
        <xsl:when test="ancestor::tei:head or parent::tei:label">true</xsl:when>
        <xsl:otherwise>false</xsl:otherwise>
      </xsl:choose>
    </xsl:for-each>
  </xsl:function>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc>extract description for an ODD</desc>
  </doc>
  <xsl:function name="tei:makeDescription" as="node()*">
    <xsl:param name="context"/>
    <xsl:param name="showListRef"/>
<!-- MDH 2018-01-21: added this param so we can stop building
     ugly and superfluous lists in Guidelines ref pages. See 
     issue #296. -->
    <xsl:param name="makeMiniList" as="xs:boolean"/>
    <xsl:variable name="D">
    <xsl:for-each select="$context">
      <xsl:variable name="langs"
                    select="tei:generateDocumentationLang(.)"/>
      <xsl:variable name="firstLang" select="($langs)[1]"/>
      <!-- first the gloss -->
      <xsl:sequence select="tei:makeGloss(.,$langs)"/>
      <!-- now the description -->
      <xsl:choose>
        <xsl:when test="not(tei:desc)"> </xsl:when>
        <xsl:when test="count(tei:desc)=1">
          <xsl:for-each select="tei:desc">
            <xsl:apply-templates select="." mode="inLanguage"/>
          </xsl:for-each>
        </xsl:when>
        <xsl:when test="tei:desc[@xml:lang=$firstLang]">
          <xsl:for-each select="tei:desc[@xml:lang=$firstLang]">
            <xsl:apply-templates select="." mode="inLanguage"/>
          </xsl:for-each>
        </xsl:when>
        <xsl:otherwise>
          <xsl:variable name="D">
            <xsl:for-each select="tei:desc">
              <xsl:variable name="currentLang"   select="tei:findLanguage(.)"/>
              <xsl:if test="$currentLang=($langs)">
                <xsl:apply-templates select="." mode="inLanguage"/>
              </xsl:if>
            </xsl:for-each>
          </xsl:variable>
          <xsl:choose>
            <xsl:when test="$D='' and tei:desc[(not(@xml:lang) or @xml:lang='en')]">
              <xsl:for-each select="tei:desc[(not(@xml:lang) or @xml:lang='en')]">
                <xsl:apply-templates select="." mode="inLanguage"/>
              </xsl:for-each>
            </xsl:when>
            <xsl:when test="not($oddmode='tei')">
              <xsl:sequence select="$D/text()"/>
            </xsl:when>
            <xsl:otherwise>
              <xsl:copy-of select="$D"/>
            </xsl:otherwise>
          </xsl:choose>
        </xsl:otherwise>
      </xsl:choose>
      <xsl:choose>
        <xsl:when test="$oddmode='tei'"/>
        <!--
          The original code, which had separate templates for tei:valList[@type=open] and
          tei:valList[@type=semi], was very redundant. However, it may have been very clever
          in how it handled the case of multiple child <valList>s. Or, it may have obliviously
          worked in that caes, producing passable, if not ideal, outupt. Depends on your point
          of view, in part.
          I believe we reproduce what it did, whether you like it or not, by using '=' instead
          of 'eq' in the "if" comparison in the definition of $msg.
          —Syd, 2018-01-19
        -->
        <!-- MDH 2018-01-21: using $makeMiniList param so we can stop building
     ugly and superfluous lists in Guidelines ref pages. See 
     issue #296. -->
        <xsl:when test="tei:valList[ @type = ('open','semi')] and $makeMiniList = true()">
          <xsl:variable name="msg"
            select="tei:i18n( concat( if (tei:valList/@type = 'open') then 'Sample' else 'Suggested', '&#x20;values include' ) )"/>
          <xsl:value-of select="concat('&#x0A;', $msg, ':&#x20;')"/>
          <xsl:for-each select="tei:valList/tei:valItem">
            <xsl:number/>
            <xsl:text>] </xsl:text>
            <xsl:choose>
              <xsl:when test="tei:altIdent=@ident">
                <xsl:value-of select="@ident"/>
              </xsl:when>
              <xsl:when test="tei:altIdent">
                <xsl:value-of select="normalize-space(tei:altIdent)"/>
              </xsl:when>
              <xsl:otherwise>
                <xsl:value-of select="@ident"/>
              </xsl:otherwise>
            </xsl:choose>
            <xsl:variable name="langs">
              <xsl:value-of select="concat(normalize-space(tei:generateDocumentationLang(.)),' ')"/>
            </xsl:variable>
            <xsl:variable name="firstLang" select="($langs)[1]"/>
            <xsl:variable name="gloss"
                          select="normalize-space( string-join( tei:makeGloss(.,$langs),'') )"/>
            <xsl:value-of select="concat(
                                    if ($gloss ne '') then '&#x20;' else '',
                                    $gloss,
                                    if (following-sibling::tei:valItem) then ';' else '',
                                    if (position() ne last()) then '&#x20;' else ''
                                    )"/>
          </xsl:for-each>
        </xsl:when>
      </xsl:choose>
      <xsl:if test="tei:listRef and $showListRef">
        <xsl:text> [</xsl:text>
        <xsl:for-each select="tei:listRef/tei:*">
          <xsl:apply-templates select="." mode="weave"/>
          <xsl:if test="following-sibling::tei:*">
            <xsl:text> </xsl:text>
          </xsl:if>
        </xsl:for-each>
        <xsl:text>]</xsl:text>
      </xsl:if>
    </xsl:for-each>
  </xsl:variable>
  <xsl:copy-of select="$D"/>
  </xsl:function>

  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc>[localisation]work out the language for documentation</desc>
  </doc>
  <xsl:function name="tei:generateDocumentationLang" as="node()*">
    <xsl:param name="context"/>
    <xsl:for-each select="$context">
      <xsl:choose>
        <xsl:when test="key('LISTSCHEMASPECS',$whichSchemaSpec)/@docLang">
          <xsl:value-of select="key('LISTSCHEMASPECS',$whichSchemaSpec)/@docLang"/>
        </xsl:when>
        <xsl:when test="string-length($doclang)&gt;0">
          <xsl:value-of select="$doclang"/>
        </xsl:when>
        <xsl:otherwise>
          <xsl:text>en</xsl:text>
        </xsl:otherwise>
      </xsl:choose>
    </xsl:for-each>
  </xsl:function>

  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc>work out which &lt;gloss&gt; to use</desc>
  </doc>

  <xsl:function name="tei:makeGloss" as="node()*">
    <xsl:param name="context"/>
    <xsl:param name="langs"/>

    <!-- This function returns a sequence of strings that may include
         leading or trailing whitespace. -->
    <xsl:variable name="firstLang" select="($langs)[1]"/>
    <xsl:for-each select="$context">
      <xsl:choose>
        <xsl:when test="not(tei:gloss)"/>
        <xsl:when test="string-length(tei:gloss[1])=0"/>
        <xsl:when test="count(tei:gloss)=1 and not(tei:gloss[@xml:lang])">
          <xsl:text>(</xsl:text>
          <xsl:apply-templates select="tei:gloss" mode="inLanguage"/>
          <xsl:text>) </xsl:text>
        </xsl:when>
        <xsl:when test="tei:gloss[@xml:lang=$firstLang]">
          <xsl:if test="not(tei:gloss[@xml:lang=$firstLang]='')">
            <xsl:text>(</xsl:text>
            <xsl:apply-templates select="tei:gloss[@xml:lang=$firstLang]" mode="inLanguage"/>
            <xsl:text>) </xsl:text>
          </xsl:if>
        </xsl:when>
        <xsl:otherwise>
          <xsl:variable name="G">
            <xsl:for-each select="tei:gloss">
              <xsl:variable name="currentLang" select="tei:findLanguage(.)"/>
              <xsl:if test="$currentLang=($langs)">
                <xsl:text>(</xsl:text>
                <xsl:apply-templates select="." mode="inLanguage"/>
                <xsl:text>) </xsl:text>
              </xsl:if>
            </xsl:for-each>
          </xsl:variable>
          <xsl:choose>
            <xsl:when test="$G='' and tei:gloss[(not(@xml:lang) or @xml:lang='en')]">
              <xsl:text>(</xsl:text>
              <xsl:apply-templates select="tei:gloss[(not(@xml:lang) or @xml:lang='en')]" mode="inLanguage"/>
              <xsl:text>) </xsl:text>
            </xsl:when>
            <xsl:otherwise>
              <xsl:copy-of select="$G"/>
            </xsl:otherwise>
          </xsl:choose>
        </xsl:otherwise>
      </xsl:choose>
    </xsl:for-each>
  </xsl:function>

  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc>which prefix for schematron</desc>
  </doc>
  <!--* tei:generate-nsprefix-schematron($e)
      * Calculate a namespace prefix for a given element, or fail. *-->
  <xsl:function name="tei:generate-nsprefix-schematron" as="xs:string">
    <xsl:param name="e" as="element()"/>
    <xsl:for-each select="$e">
      <xsl:variable name="myns" select="normalize-space( ancestor::tei:elementSpec/@ns )"/>
      <xsl:variable name="prefixes" select="in-scope-prefixes(.)" as="xs:string*"/>
      <xsl:variable name="uris" select="for $pre in $prefixes return namespace-uri-for-prefix( $pre,$e)" as="xs:anyURI*"/>
      <xsl:choose>
        <!--* if ns is not specified or is tei, use 'tei'
            * Note that $myns will be undefined both when the ancestor
            * <elementSpec> does not have an @ns, and when there is no
            * ancestor <elementSpec>. *-->
        <xsl:when test="not($myns)  or  $myns eq 'http://www.tei-c.org/ns/1.0'">
          <xsl:text>tei:</xsl:text>
        </xsl:when>
        <!--* otherwise, if an ancestor has a suitable <sch:ns> element, 
            * use the one belonging to the nearest such ancestor *-->
        <xsl:when test="ancestor::*/sch:ns[ normalize-space(@uri) eq $myns ]">
          <xsl:variable name="a" as="element()"
                        select="ancestor::*[sch:ns[ normalize-space(@uri) eq $myns ]][1]"/>
          <xsl:variable name="prefix" as="xs:string*"
                        select="$a/sch:ns[ normalize-space(@uri) eq $myns ]/normalize-space(@prefix)"/>
          <xsl:value-of select="concat( $prefix[1],':')"/>
        </xsl:when>
        <!--* Otherwise, if the actual declaration has a suitable
            * namespace node, use the current prefix. Or, to be
            * exact, any one of the current non-null prefixes.
            * We'll take the first one presented.
            *-->
        <xsl:when test="$uris = $myns  and  $myns ne ''">
          <xsl:variable name="indx" select="index-of( $uris, $myns )[1]"/>
          <xsl:value-of select="concat( $prefixes[$indx],':')"/>
        </xsl:when>
        <!--* If no ancestor has a suitable sch:ns child, and we don't
            * have any local namespace bindings with non-zero
            * prefixes, then we are desperate and we will take any 
            * sch:ns in the schemaSpec (this is getting a bit desperate) *-->   
        <xsl:when test="ancestor::tei:schemaSpec//sch:ns[ normalize-space(@uri) eq $myns ]">
          <xsl:variable name="NSs" select="ancestor::tei:schemaSpec//sch:ns[ normalize-space(@uri) eq $myns ]"/>
          <xsl:value-of select="concat($NSs[1]/normalize-space(@prefix),':')"/>
        </xsl:when>
        <xsl:otherwise>
          <xsl:message terminate="yes"
                       select="concat('schematron rule cannot work out prefix for ', ancestor::tei:elementSpec/@ident, '.')"/>
        </xsl:otherwise>
      </xsl:choose>
    </xsl:for-each>
  </xsl:function>


  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc>find version of stylesheets</desc>
  </doc>

  <xsl:function name="tei:stylesheetVersion" as="xs:string">
    <xsl:param name="context"/>
    <xsl:choose>
      <xsl:when test="$useFixedDate='true'">0</xsl:when>
      <xsl:otherwise><xsl:value-of select="normalize-space(unparsed-text('../VERSION'))"/></xsl:otherwise>
    </xsl:choose>
  </xsl:function>

  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc>look up witness</desc>
  </doc>
  <xsl:function name="tei:getWitness" as="xs:string*">
    <xsl:param name="witness"/>
      <xsl:variable name="r">
    <xsl:for-each select="tokenize($witness,' ')">
      <xsl:variable name="wit" select="."/>
      <xsl:for-each select="$top">
        <xsl:choose>
          <xsl:when test="starts-with($wit,'#') and
                          id(substring($wit,2))">
            <xsl:for-each select="id(substring($wit,2))">
              <xsl:value-of select="if (@n) then @n else @xml:id"/>
            </xsl:for-each>
        </xsl:when>
          <xsl:when test="starts-with($wit,'#')">
            <xsl:value-of select="substring($wit,2)"/>
        </xsl:when>
        <xsl:otherwise>
          <xsl:for-each select="doc($wit)/*">
            <xsl:value-of select="if (@n) then @n else @xml:id"/>
          </xsl:for-each>
        </xsl:otherwise>
        </xsl:choose>
      </xsl:for-each>
      <xsl:if test="position() &lt; last()">, </xsl:if>
    </xsl:for-each>
      </xsl:variable>
      <xsl:value-of select="$r"/>
  </xsl:function>

    <xsl:function name="tei:createSpecName" as="xs:string">
      <xsl:param name="context"/>
      <xsl:for-each select="$context">
        <xsl:value-of select="if (tei:altIdent) then
                              normalize-space(tei:altIdent) else @ident"/>
      </xsl:for-each>
    </xsl:function>

    <!-- work out a name prefix for ODD objects -->
  <xsl:function name="tei:createSpecPrefix" as="xs:string">
    <xsl:param name="context"/>
    <xsl:variable name="result">
      <xsl:for-each select="$context">
        <xsl:variable name="ns" select="ancestor-or-self::*[@ns][1]/@ns"/>
        <xsl:choose>
          <xsl:when test="@prefix">
            <xsl:value-of select="@prefix"/>
          </xsl:when>
          <xsl:when test="not($ns) or $ns='http://www.tei-c.org/ns/1.0' or self::tei:datatype"/>
          <xsl:when test="$ns='http://www.w3.org/XML/1998/namespace'">
            <xsl:text>xml:</xsl:text>
          </xsl:when>
          <xsl:otherwise>
            <xsl:value-of select="translate(tei:getPrefix($ns,.)[1],':','_')"/>
          </xsl:otherwise>
        </xsl:choose>
      </xsl:for-each>
    </xsl:variable>
    <xsl:value-of select="$result"/>
  </xsl:function>

  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc>Return a sequence of 0 or more namespace prefixes.</desc>
  </doc>
  <xsl:function name="tei:getPrefix" as="xs:string*">
    <xsl:param name="ns"/>
    <xsl:param name="here"/>
    <xsl:for-each select="in-scope-prefixes($here)">
      <xsl:choose>
        <xsl:when test=".=''"/>
        <xsl:when test="$ns=namespace-uri-for-prefix(.,$here)">
          <xsl:value-of select="concat(.,':')"/>
        </xsl:when>
      </xsl:choose>
    </xsl:for-each>
  </xsl:function>
  
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc>work out unique ID for generated Schematron</desc>
  </doc>
  <xsl:function name="tei:makePatternID" as="xs:string">
    <xsl:param name="context"/>
    <xsl:for-each select="$context">
      <xsl:variable name="num">
        <xsl:number level="any"/>
      </xsl:variable>
      <xsl:value-of select="(
        ../ancestor::*[@ident]/@ident/translate( .,':',''),
        'constraint',
        ../@ident,local-name(),
        $num
        )" separator="-"/>
      <!-- 
           Comment on the 1st XPath in the above sequence (the one
           that generates the pre-"constraint" part of the returned
           string, a sequence of hyphen-separated @ident attributes):
           We remove ':' from the value of each @ident to address
           issue #330, which occurs when the value of @ident is
           "xml:id" (or anything else with a colon). Note that the
           value we are generating has to be an XML NCName. The value
           of almost any @ident in the TEI is an XML Name, with two
           exceptions: prefixDef/@ident (which can contain '+, and can
           even start with '+', '-', or '.') and valItem/@ident (which
           can contain *anything*). However, neither <prefixDef> nor
           <valItem> have <constraintSpec>s, so we don't have to worry
           about those here. If we did, the clever XSLT 1.0 version of
           that XPath is
             ../ancestor::*[@ident]/@ident/translate( ., translate( .,'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789._-',''),'')
           That is probably blazingly fast, but (besides being hard to
           read and understand) has the disadvantage that it
           inappropriately nukes any NameChar that is not explicitly
           listed, like 'é'. Although not as clever and probably
           slower, it would be better to use just
             ../ancestor::*[@ident]/@ident/replace( .,'[^\c]|:','')
           if we ever have to worry about any character, not just colon.
           Note: I think "replace( .,'\C|:','')" should work, but it did
           not in my little test with Saxon-HE 9.8.0.11J.
                —Syd, 2018-09-25
      -->
    </xsl:for-each>
  </xsl:function>
  
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc>find nearest language code</desc>
  </doc>
 <xsl:function name="tei:findLanguage">
       <xsl:param name="context"/>
       <!-- note: $context should always be 1 node, so following -->
       <!-- for-each just sets context node and executes once for it -->
       <xsl:for-each select="$context">
         <xsl:value-of select="(ancestor-or-self::*[@xml:lang][1]/@xml:lang/string(),'en')[1]"/>
         <!-- 
           That XPath is a bit complex, so deserves some explanation.
           ( = start a (two item) sequence
           ancestor-or-self::tei:* = generate sequence of elements starting from the context
                                     node selecting each parent:: until the outermost element
           [@xml:lang] = filter the selected set to only those that have @xml:lang; note that
                         the sequence is still in closest to root order
           [1] = take only the first, i.e. closest, of those nodes
           /@xml:lang = take its @xml:lang attribute
           /string() = convert that attribute to a string
           , = separate the two items in our sequence; note that what's on the L will be
               either a single @xml:lang value or nothing
           'en' = second item in our two item sequence
           ) = end the (two item) sequence
           [1] = select the first item in the sequence: if the first item is nothing, it is
                 really a one item sequence, we get the 'en'; if the first item is a string
                 we get it.
         -->
       </xsl:for-each>
       <!-- Syd Bauman scripsit-->
     </xsl:function>

  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc>pare back a string to contain only alphanumerics and some punctuation</desc>
  </doc>
     <xsl:function name="tei:sanitize" as="xs:string">
       <xsl:param name="text"/>
       <xsl:variable name="alltext">
         <xsl:value-of select="($text)" separator=""/>
       </xsl:variable>
       <xsl:variable name="result"
           select="replace(normalize-space($alltext),'[^\w\[\]\\(\)._\s]+','')"/>
       <xsl:value-of select="if (string-length($result)&gt;127) then
         concat(substring($result,1,127),'...') else $result"/>
     </xsl:function>

  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc>map file suffix to mime type</desc>
  </doc>

     <xsl:function name="tei:generateMimeType" as="xs:string">
       <xsl:param name="filename"/>
       <xsl:param name="type"/>
       <xsl:variable name="filesuffix"
                     select="lower-case(tokenize($filename,'\.')[last()])"/>

        <xsl:choose>
          <xsl:when test="$type"><xsl:message>check <xsl:value-of select="($filename,$filesuffix,$type)"/></xsl:message><xsl:value-of select="$type"/></xsl:when>
          <xsl:when test="$filesuffix='bin'">application/vnd.openxmlformats-officedocument.oleObject</xsl:when>
          <xsl:when test="$filesuffix='emf'">image/x-emf</xsl:when>
          <xsl:when test="$filesuffix='gif'">image/gif</xsl:when>
          <xsl:when test="$filesuffix='m4v'">video/mpeg4</xsl:when>
          <xsl:when test="$filesuffix='mp4'">video/mpeg4</xsl:when>
          <xsl:when test="$filesuffix='mpeg'">video/mpeg4</xsl:when>
          <xsl:when test="$filesuffix='png'">image/png</xsl:when>
          <xsl:when test="$filesuffix='tif'">image/tiff</xsl:when>
          <xsl:when test="$filesuffix='tiff'">image/tiff</xsl:when>
          <xsl:when test="$filesuffix='wav'">audio/wav</xsl:when>
          <xsl:when test="$filesuffix='ogg'">audio/ogg</xsl:when>
          <xsl:when test="$filesuffix='mp3'">audio/mpeg</xsl:when>
          <xsl:when test="$filesuffix='wmf'">image/x-wmf</xsl:when>
          <xsl:otherwise>image/jpeg</xsl:otherwise>
        </xsl:choose>
     </xsl:function>


  <xsl:function name="tei:isMarginal" as="xs:boolean">
    <xsl:param name="place"/>
    <xsl:choose>
      <xsl:when test="tokenize($place,' ')=('margin', 
                      'margin/inline',
                      'marg1',
                      'marg2',
                      'marg3',
                      'marge',
                      'h',
                      'inter',
                      'right',
                      'left',
                      'divend',
                      'marginOuter',
                      'marginLeft',
                      'marginRight',
                      'margin-left',
                      'margin-right',
                      'margin_left',
                      'margin_right',
                      'margin-top',
                      'margin-bottom',
                      'top','opposite',
                      'overleaf',
                      'inspace')">true</xsl:when>
      <xsl:otherwise>false</xsl:otherwise>
    </xsl:choose>
  </xsl:function>

  <xsl:function name="tei:match" as="xs:boolean">
    <xsl:param name="att"/>
    <xsl:param name="value"/>
    <xsl:sequence select="if (tokenize($att,' ')=($value)) then true()
      else false()"/>
  </xsl:function>

  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc>Returns the current date.</desc>
  </doc>
  <xsl:function name="tei:whatsTheDate" as="node()+">
    <xsl:choose>
      <xsl:when test="$useFixedDate='true'">1970-01-01</xsl:when>
      <xsl:otherwise>
        <xsl:value-of select="
          format-dateTime(
            adjust-dateTime-to-timezone(
              current-dateTime(),
              'PT00H' cast as xs:dayTimeDuration
            ),
            '[Y]-[M02]-[D02]T[H02]:[m02]:[s02]Z'
          )"/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:function>
  
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc>Whether an element has any more (useful) text in its parent</desc>
  </doc>
  
<xsl:template match="processing-instruction()[name(.)='entity']" mode="#all">
    <xsl:value-of select="tei:escapeChars(concat('&amp;',normalize-space(.),';'),parent::*)"/>
</xsl:template>

</xsl:stylesheet>
