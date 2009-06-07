<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    xmlns:tei="http://www.tei-c.org/ns/1.0" version="2.0" xmlns:iso="http://www.iso.org/ns/1.0"
    xmlns:xs="http://www.w3.org/2001/XMLSchema"
    xmlns:ve="http://schemas.openxmlformats.org/markup-compatibility/2006"
    xmlns:o="urn:schemas-microsoft-com:office:office"
    xmlns:r="http://schemas.openxmlformats.org/officeDocument/2006/relationships"
    xmlns:m="http://schemas.openxmlformats.org/officeDocument/2006/math"
    xmlns:v="urn:schemas-microsoft-com:vml" xmlns:fn="http://www.w3.org/2005/02/xpath-functions"
    xmlns:wp="http://schemas.openxmlformats.org/drawingml/2006/wordprocessingDrawing"
    xmlns:a="http://schemas.openxmlformats.org/drawingml/2006/main"
    xmlns:w10="urn:schemas-microsoft-com:office:word"
    xmlns:w="http://schemas.openxmlformats.org/wordprocessingml/2006/main"
    xmlns:wne="http://schemas.microsoft.com/office/word/2006/wordml"
    xmlns:mml="http://www.w3.org/1998/Math/MathML"
    xmlns:tbx="http://www.lisa.org/TBX-Specification.33.0.html"
    xmlns:pic="http://schemas.openxmlformats.org/drawingml/2006/picture"
    xmlns:xd="http://www.pnp-software.com/XSLTdoc"
    xmlns:teidocx="http://www.tei-c.org/ns/teidocx/1.0"
    exclude-result-prefixes="ve o r m v wp w10 w wne mml tbx iso tei a xs pic fn">
    
    <!-- import variables -->
    <xsl:import href="variables.xsl"/>

    <xd:doc type="stylesheet">
        <xd:short> TEI Utility stylesheet for making Word docx files
        from TEI XML (see docx-tei.xsl)</xd:short>
        <xd:detail> This library is free software; you can redistribute it and/or
            modify it under the terms of the GNU Lesser General Public License as
            published by the Free Software Foundation; either version 2.1 of the
            License, or (at your option) any later version. This library is
            distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY;
            without even the implied warranty of MERCHANTABILITY or FITNESS FOR A
            PARTICULAR PURPOSE. See the GNU Lesser General Public License for more
            details. You should have received a copy of the GNU Lesser General Public
            License along with this library; if not, write to the Free Software
            Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA </xd:detail>
        <xd:author>See AUTHORS</xd:author>
        <xd:cvsId>$Id$</xd:cvsId>
        <xd:copyright>2008, TEI Consortium</xd:copyright>
    </xd:doc>
    
    <!-- Converts a dimension into the 20th of a ps point -->
    <xsl:function name="teidocx:convert-dim-pt20" as="xs:integer">
        <xsl:param name="dim"/>
        <xsl:value-of select="number(teidocx:convert-dim-pt($dim)*20)"/>
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
            
            <xsl:otherwise>
                -1
            </xsl:otherwise>
        </xsl:choose>
    </xsl:function>
    
    <!-- convert a dimension into english metric unit -->
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
                <xsl:value-of
                    select="number(number(number(substring($dim,0,string-length($dim)-1)) div 72) * 9144) cast as xs:integer"/>
            </xsl:when>
            
            <xsl:otherwise>
                -1
            </xsl:otherwise>
        </xsl:choose>
	</xsl:variable>
	<xsl:value-of select="$result"/>
    </xsl:function>
    
    <!-- returns a listtype for a given stylename (return empty string to figure it out dynamically) -->
    <xsl:function name="teidocx:get-listtype" as="xs:string">
        <xsl:param name="style"/>
        <xsl:text></xsl:text>
    </xsl:function>
    
    <!-- returns the correct heading style (return empty string to figure it out dynamically)-->
    <xsl:function name="teidocx:get-headingstyle" as="xs:string">
        <xsl:param name="element"/>
        <xsl:param name="level"/>

        <xsl:text></xsl:text>
    </xsl:function>    
    
    <!-- defines whether or not a word paragraph is a first level heading -->
    <xsl:function name="teidocx:is-firstlevel-heading" as="xs:boolean">
        <xsl:param name="p"/>
        
        <xsl:choose>
            <xsl:when test="$p[w:pPr/w:pStyle/@w:val='heading 1']">true</xsl:when>
            <xsl:otherwise>false</xsl:otherwise>
        </xsl:choose>
    </xsl:function>
    
    <!-- defines whether or not a word paragraph is a  heading -->
    <xsl:function name="teidocx:is-heading" as="xs:boolean">
        <xsl:param name="p"/>
        
        <xsl:choose>
            <xsl:when test="$p[starts-with(w:pPr/w:pStyle/@w:val,'heading')]">true</xsl:when>
            <xsl:otherwise>false</xsl:otherwise>
        </xsl:choose>
    </xsl:function>
    
    <!-- Is given a header style and returns the style for the next level header -->
    <xsl:function name="teidocx:get-nextlevel-header" as="xs:string">
        <xsl:param name="current-header"/>
        <xsl:value-of select="translate($current-header,'12345678','23456789')"/>
    </xsl:function>
    
    <!-- define special rendering for attributes -->
    <xsl:function name="teidocx:render-bold" as="xs:boolean">
        <xsl:param name="element"/>
        <xsl:for-each select="$element">
            <xsl:choose>
                <xsl:when test="parent::tei:hi[starts-with(@rend,'specList-')]">true</xsl:when>
                <xsl:when test="@rend='bold'">true</xsl:when>
                <xsl:when test="@rend='label'">true</xsl:when>
		<xsl:when test="ancestor-or-self::tei:cell[@role='label']">true</xsl:when>
		<xsl:when test="ancestor-or-self::tei:cell[@rend='wovenodd-col1']">true</xsl:when>
		<xsl:when test="self::tei:cell and parent::tei:row[@role='label']">true</xsl:when>
                <xsl:when test="self::tei:hi">true</xsl:when>
                <xsl:when test="self::tei:label[following-sibling::tei:item]">true</xsl:when>
                <xsl:when test="self::tei:term">true</xsl:when>
                <xsl:otherwise>false</xsl:otherwise>
            </xsl:choose>
        </xsl:for-each>
    </xsl:function>
    
    <xsl:function name="teidocx:render-italic" as="xs:boolean">
        <xsl:param name="element"/>
        <xsl:for-each select="$element">
            <xsl:choose>
                <xsl:when test="@rend='italics'">true</xsl:when>
                <xsl:when test="@rend='ital'">true</xsl:when>
                <xsl:when test="@rend='att'">true</xsl:when>
                <xsl:when test="self::tei:att">true</xsl:when>
                <xsl:when test="self::tei:gloss">true</xsl:when>
                <xsl:when test="self::tei:title">true</xsl:when>
                <xsl:when test="self::tei:name">true</xsl:when>
                <xsl:when test="self::tei:soCalled">true</xsl:when>
                <xsl:otherwise>false</xsl:otherwise>
            </xsl:choose>
        </xsl:for-each>
    </xsl:function>
    
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
    
    <!-- is given an element and defines whether or not this element is to be rendered inline. -->
    <xsl:function name="teidocx:is-inline" as="xs:boolean">
        <xsl:param name="element"/>
        <xsl:for-each select="$element">
            <xsl:choose>
                <xsl:when test="self::m:oMath or
                    
                    self::teidocx:dynamicContent or
                    
                    self::w:drawing or
                    self::w:object or
                    
                    self::iso:wordObject[w:object] or
                    
		    self::tei:am or	
		    self::tei:att or	
		    self::tei:choice or
		    self::tei:damage or
		    self::tei:ex or
		    self::tei:expan or
		    self::tei:gap or
		    self::tei:genName or
		    self::tei:geogName or
		    self::tei:orig or
		    self::tei:origPlace or
		    self::tei:roleName or
		    self::tei:supplied or
                    self::tei:abbr or
                    self::tei:affiliation or
                    self::tei:author or
                    self::tei:br or
                    self::tei:c or
                    self::tei:code or
                    self::tei:date or
                    self::tei:editionStmt or
                    self::tei:emph or
                    self::tei:foreign or
                    self::tei:forename or
                    self::tei:gi or
                    self::tei:gloss or
                    self::tei:graphic or
                    self::tei:hi[not(w:*)] or
                    self::tei:ident or
                    self::tei:idno or
                    self::tei:lb or
                    self::tei:locus or
                    self::tei:mentioned or
                    self::tei:name or
                    self::tei:note[@place='end' or @place='bottom' ] or
                    self::tei:note[@place='foot' or @place='bottom' ] or
                    self::tei:num or
                    self::tei:orgName or
                    self::tei:pb or
                    self::tei:persName or
                    self::tei:placeName or
                    self::tei:ptr or
                    self::tei:publisher or
                    self::tei:q or
                    self::tei:ref or
                    self::tei:seg or
                    self::tei:soCalled or
                    self::tei:surname or 
                    self::tei:term[not(ancestor-or-self::*/@type='termsAndDefinitions')] or
                    self::tei:title or
                    self::tei:val">
                    true
                </xsl:when>
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
    
</xsl:stylesheet>
