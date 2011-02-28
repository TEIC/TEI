<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
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
		xmlns:teidocx="http://www.tei-c.org/ns/teidocx/1.0"
                version="2.0"
                exclude-result-prefixes="cals ve o r m v wp w10 w wne mml tbx iso tei a xs pic fn">
    
    <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" scope="stylesheet" type="stylesheet">
      <desc>
         <p> TEI Utility stylesheet for making Word docx files
        from TEI XML (see docx-tei.xsl)</p>
         <p> This library is free software; you can redistribute it and/or
            modify it under the terms of the GNU Lesser General Public License as
            published by the Free Software Foundation; either version 2.1 of the
            License, or (at your option) any later version. This library is
            distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY;
            without even the implied warranty of MERCHANTABILITY or FITNESS FOR A
            PARTICULAR PURPOSE. See the GNU Lesser General Public License for more
            details. You should have received a copy of the GNU Lesser General Public
            License along with this library; if not, write to the Free Software
            Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA </p>
         <p>Author: See AUTHORS</p>
         <p>Id: $Id$</p>
         <p>Copyright: 2008, TEI Consortium</p>
      </desc></doc>

    
        <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>Converts a dimension into the 20th of a ps point.</desc></doc>
    <xsl:function name="teidocx:convert-dim-pt20" as="xs:integer">
        <xsl:param name="dim"/>
	     <xsl:value-of select="number(substring($dim,0,string-length($dim)-1)) * 20"/>
    </xsl:function>
    
    <xsl:function name="teidocx:convert-dim-pt" as="xs:integer">
        <xsl:param name="dim"/>
        <xsl:choose>
            <xsl:when test="ends-with($dim,'cm')">
                <xsl:value-of select="number(number(substring($dim,0,string-length($dim)-1))*28.3464567) cast as xs:integer"/>
            </xsl:when>
            <xsl:when test="ends-with($dim,'in')">
                <xsl:value-of select="number(number(substring($dim,0,string-length($dim)-1))*72) cast as xs:integer"/>
            </xsl:when>
            
            <xsl:when test="ends-with($dim,'mm')">
                <xsl:value-of select="number(number(substring($dim,0,string-length($dim)-1))*2.83464567) cast as xs:integer"/>
            </xsl:when>
            <xsl:when test="ends-with($dim,'pt')">
                <xsl:value-of select="number(substring($dim,0,string-length($dim)-1)) cast as xs:integer"/>
            </xsl:when>
            <xsl:when test="ends-with($dim,'px')">
                <xsl:value-of select="number(number(substring($dim,0,string-length($dim)-1))*0.75) cast as xs:integer"/>
            </xsl:when>
            
            <xsl:otherwise>
                -1
            </xsl:otherwise>
        </xsl:choose>
    </xsl:function>
    
        <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>Convert a dimension into english metric unit.</desc></doc>
    <xsl:function name="teidocx:convert-dim-emu" as="xs:integer">
        <xsl:param name="dim"/>
	<xsl:variable name="result">
	  <xsl:choose>
            <xsl:when test="ends-with($dim,'cm')">
	      <xsl:value-of select="number(number(substring($dim,0,string-length($dim)-1))*3600) cast as xs:integer"/>
            </xsl:when>
            <xsl:when test="ends-with($dim,'in')">
	      <xsl:value-of select="number(number(substring($dim,0,string-length($dim)-1))*9144) cast as xs:integer"/>
            </xsl:when>
            <xsl:when test="ends-with($dim,'mm')">
                <xsl:value-of select="number(number(substring($dim,0,string-length($dim)-1))*360) cast as xs:integer"/>
            </xsl:when>
            <xsl:when test="ends-with($dim,'pt')">
                <xsl:value-of select="number(number(number(substring($dim,0,string-length($dim)-1)) div 72) * 9144) cast as xs:integer"/>
            </xsl:when>
            <xsl:when test="ends-with($dim,'px')">
                <xsl:value-of select="number(number(substring($dim,0,string-length($dim)-1))*95.25) cast as xs:integer"/>
            </xsl:when>            
            <xsl:otherwise>
                -1
            </xsl:otherwise>
	  </xsl:choose>
	</xsl:variable>
	<xsl:value-of select="$result"/>
    </xsl:function>
    
        <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>Returns a listtype for a given stylename (return empty string to figure it out dynamically).</desc></doc>
    <xsl:function name="teidocx:get-listtype" as="xs:string">
        <xsl:param name="style"/>
        <xsl:text/>
    </xsl:function>
    
        <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>Returns the correct heading style (return empty string to figure it out dynamically).</desc></doc>
    <xsl:function name="teidocx:get-headingstyle" as="xs:string">
        <xsl:param name="element"/>
        <xsl:param name="level"/>

        <xsl:text/>
    </xsl:function>    
    
        <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>Defines whether or not a word paragraph is a first level heading.</desc></doc>
    <xsl:function name="teidocx:is-firstlevel-heading" as="xs:boolean">
        <xsl:param name="p"/>
        
        <xsl:choose>
            <xsl:when test="$p[w:pPr/w:pStyle/@w:val='heading 1']">true</xsl:when>
            <xsl:otherwise>false</xsl:otherwise>
        </xsl:choose>
    </xsl:function>
    
        <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>Defines whether or not a word paragraph is a  heading.</desc></doc>
    <xsl:function name="teidocx:is-heading" as="xs:boolean">
        <xsl:param name="p"/>
        
        <xsl:choose>
            <xsl:when test="$p[starts-with(w:pPr/w:pStyle/@w:val,'heading')]">true</xsl:when>
            <xsl:otherwise>false</xsl:otherwise>
        </xsl:choose>
    </xsl:function>

        <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>Defines whether or not a word paragraph is a list element.</desc></doc>
    <xsl:function name="teidocx:is-list" as="xs:boolean">
        <xsl:param name="p"/>        
        <xsl:choose>
            <xsl:when test="$p[contains(w:pPr/w:pStyle/@w:val,'List')]">true</xsl:when>
            <xsl:otherwise>false</xsl:otherwise>
        </xsl:choose>
    </xsl:function>

        <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>Defines whether or not a word paragraph is a table of contents.</desc></doc>
    <xsl:function name="teidocx:is-toc" as="xs:boolean">
        <xsl:param name="p"/>        
        <xsl:choose>
            <xsl:when test="$p[contains(w:pPr/w:pStyle/@w:val,'toc')]">true</xsl:when>
            <xsl:otherwise>false</xsl:otherwise>
        </xsl:choose>
    </xsl:function>

        <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>Defines whether or not a word paragraph is a figure element.</desc></doc>
    <xsl:function name="teidocx:is-figure" as="xs:boolean">
        <xsl:param name="p"/>        
        <xsl:choose>
            <xsl:when test="$p[contains(w:pPr/w:pStyle/@w:val,'Figure')]">true</xsl:when>
            <xsl:when test="$p[contains(w:pPr/w:pStyle/@w:val,'Caption')]">true</xsl:when>
            <xsl:otherwise>false</xsl:otherwise>
        </xsl:choose>
    </xsl:function>
    
        <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>Is given a header style and returns the style for the next level header.</desc></doc>
    <xsl:function name="teidocx:get-nextlevel-header" as="xs:string">
        <xsl:param name="current-header"/>
        <xsl:value-of select="translate($current-header,'12345678','23456789')"/>
    </xsl:function>
    
        <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>Define special rendering for attributes.</desc></doc>
    <xsl:function name="teidocx:render-bold" as="xs:boolean">
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
                <xsl:when test="self::tei:hi">true</xsl:when>
                <xsl:when test="self::tei:label[following-sibling::tei:item]">true</xsl:when>
                <xsl:when test="self::tei:term">true</xsl:when>
                <xsl:when test="self::tei:unclear">true</xsl:when>
                <xsl:otherwise>false</xsl:otherwise>
            </xsl:choose>
        </xsl:for-each>
    </xsl:function>
    
        <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>Whether to render something in italic code.</desc></doc>
    <xsl:function name="teidocx:render-italic" as="xs:boolean">
        <xsl:param name="element"/>
        <xsl:for-each select="$element">
            <xsl:choose>
                <xsl:when test="contains(@rend,'italics')">true</xsl:when>
                <xsl:when test="contains(@rend,'italic')">true</xsl:when>
                <xsl:when test="self::tei:emph">true</xsl:when>
		<xsl:when test="self::tbx:hi[@style='italics']">true</xsl:when>
                <xsl:when test="@rend='ital'">true</xsl:when>
                <xsl:when test="@rend='it'">true</xsl:when>
                <xsl:when test="@rend='att'">true</xsl:when>
                <xsl:when test="self::tei:att">true</xsl:when>
                <xsl:when test="self::tei:speaker">true</xsl:when>
                <xsl:when test="self::tei:gloss">true</xsl:when>
                <xsl:when test="self::tei:title">true</xsl:when>
                <xsl:when test="self::tei:name">true</xsl:when>
                <xsl:when test="self::tei:soCalled">true</xsl:when>
                <xsl:otherwise>false</xsl:otherwise>
            </xsl:choose>
        </xsl:for-each>
    </xsl:function>
    
        <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>Whether to render something in typewriter-like code.</desc></doc>
    <xsl:function name="teidocx:render-typewriter" as="xs:boolean">
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
      <desc>Is given an element and defines whether or not this element is to be rendered inline.</desc></doc>
    <xsl:function name="teidocx:is-inline" as="xs:boolean">
        <xsl:param name="element"/>
        <xsl:for-each select="$element">
            <xsl:choose>
		<xsl:when test="self::mml:math">true</xsl:when>
		<xsl:when test="self::tei:abbr">true</xsl:when>
		<xsl:when test="self::tei:affiliation">true</xsl:when>
		<xsl:when test="self::tei:altIdentifier">true</xsl:when>
		<xsl:when test="self::tei:add">true</xsl:when>
		<xsl:when test="self::tei:am">true</xsl:when>
		<xsl:when test="self::tei:att">true</xsl:when>
		<xsl:when test="self::tei:author">true</xsl:when>
		<xsl:when test="self::tei:bibl and not(parent::tei:div)">true</xsl:when>
		<xsl:when test="self::tei:biblScope">true</xsl:when>
		<xsl:when test="self::tei:br">true</xsl:when>
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
		<xsl:when test="self::tei:editionStmt">true</xsl:when>
		<xsl:when test="self::tei:emph">true</xsl:when>
		<xsl:when test="self::tei:ex">true</xsl:when>
		<xsl:when test="self::tei:expan">true</xsl:when>
		<xsl:when test="self::tei:figure[@place='inline']">true</xsl:when>
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
		<xsl:when test="self::tei:note[@place='bottom']">true</xsl:when>
		<xsl:when test="self::tei:note[@place='comment']">true</xsl:when>
		<xsl:when test="self::tei:note[@place='end']">true</xsl:when>
		<xsl:when test="self::tei:note[@place='foot']">true</xsl:when>
		<xsl:when test="self::tei:num">true</xsl:when>
		<xsl:when test="self::tei:orgName">true</xsl:when>
		<xsl:when test="self::tei:orig">true</xsl:when>
		<xsl:when test="self::tei:origDate">true</xsl:when>
		<xsl:when test="self::tei:origPlace">true</xsl:when>
		<xsl:when test="self::tei:origPlace">true</xsl:when>
		<xsl:when test="self::tei:pb">true</xsl:when>
		<xsl:when test="self::tei:persName">true</xsl:when>
		<xsl:when test="self::tei:placeName">true</xsl:when>
		<xsl:when test="self::tei:ptr">true</xsl:when>
		<xsl:when test="self::tei:publisher">true</xsl:when>
		<xsl:when test="self::tei:pubPlace">true</xsl:when>
		<xsl:when test="self::tei:q[tei:l]">false</xsl:when>
		<xsl:when test="self::tei:q[not(parent::tei:div)]">true</xsl:when>
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
		<xsl:when test="self::teidocx:dynamicContent">true</xsl:when>
		<xsl:when test="self::w:drawing">true</xsl:when>
                <xsl:when test="self::m:oMath">true</xsl:when>
                <xsl:otherwise>
                    <xsl:choose>
                        <xsl:when test="empty($element/..)">false</xsl:when>
                        <xsl:when test="teidocx:is-inline($element/..)">true</xsl:when>
                        <xsl:otherwise>false</xsl:otherwise>
                    </xsl:choose>
                </xsl:otherwise>
            </xsl:choose>
        </xsl:for-each>
    </xsl:function>

    <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>
		Returns the current date.</desc></doc>

	  <xsl:function name="teidocx:whatsTheDate">
        <xsl:value-of select="format-dateTime(current-dateTime(),'[Y]-[M02]-[D02]T[H02]:[m02]:[s02]Z')"/>
    </xsl:function>

   <!--
    <xsl:function name="teidocx:getColNum">
      <xsl:param name="col"/>
      <xsl:value-of
	  select="ancestor::cals:table/cals:tgroup/cals:colspec[@colname=$col]/@colnum"/>
    </xsl:function>
-->
</xsl:stylesheet>
