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
    xmlns:teidocx="http://www.tei-c.org/ns/teidocx/1.0"
    exclude-result-prefixes="ve o r m v wp w10 w wne mml tbx iso tei a xs pic fn">
    
    <!-- import variables -->
    <xsl:import href="variables.xsl"/>
    
    <!-- Converts a dimension into the 20th of a ps point -->
    <xsl:function name="teidocx:convert-dim-pt20" as="xs:integer">
        <xsl:param name="dim"/>
        <xsl:value-of select="teidocx:convert-dim-pt($dim)*20"/>
    </xsl:function>
    
    <xsl:function name="teidocx:convert-dim-pt" as="xs:integer">
        <xsl:param name="dim"/>
        <xsl:choose>
            <xsl:when test="ends-with($dim,'cm')">
                <xsl:value-of select="number(substring($dim,0,string-length($dim)-1))*28.3464567 cast as xs:integer"/>
            </xsl:when>
            <xsl:when test="ends-with($dim,'in')">
                <xsl:value-of select="number(substring($dim,0,string-length($dim)-1))*72 cast as xs:integer"/>
            </xsl:when>
            
            <xsl:when test="ends-with($dim,'mm')">
                <xsl:value-of select="number(substring($dim,0,string-length($dim)-1))*2.83464567 cast as xs:integer"/>
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
        <xsl:choose>
            <xsl:when test="ends-with($dim,'cm')">
                <xsl:value-of select="number(substring($dim,0,string-length($dim)-1))*360000 cast as xs:integer"/>
            </xsl:when>
            <xsl:when test="ends-with($dim,'in')">
                <xsl:value-of select="number(substring($dim,0,string-length($dim)-1))*91440 cast as xs:integer"/>
            </xsl:when>
            
            <xsl:when test="ends-with($dim,'mm')">
                <xsl:value-of select="number(substring($dim,0,string-length($dim)-1))*36000 cast as xs:integer"/>
            </xsl:when>
            <xsl:when test="ends-with($dim,'pt')">
                <xsl:value-of select="number(substring($dim,0,string-length($dim)-1))*91440*72 cast as xs:integer"/>
            </xsl:when>
            
            <xsl:otherwise>
                -1
            </xsl:otherwise>
        </xsl:choose>
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
                <xsl:when test="self::tei:hi[@rend='label']">true</xsl:when>
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
                <xsl:when test="self::tei:code">true</xsl:when>
                <xsl:when test="self::tei:name">true</xsl:when>
                <xsl:when test="self::tei:soCalled">true</xsl:when>
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
                    
                    self::tei:abbr or
                    self::tei:author or
                    self::tei:br or
                    self::tei:c or
                    self::tei:code or
                    self::tei:editionStmt or
                    self::tei:date or
                    self::tei:emph or
                    
                    self::tei:foreign or
                    self::tei:graphic or
                    self::tei:hi[not(w:*)] or
                    self::tei:idno or
                    self::tei:item[preceding-sibling::tei:*[1]/self::tei:label] or
                    self::tei:label[following-sibling::tei:*[1]/self::tei:item] or
                    self::tei:lb or
                    self::tei:name or
                    self::tei:note[@place='foot'] or
                    self::tei:note[@place='end'] or
                    
                    self::tei:num or
                    self::tei:mentioned or
                    self::tei:orgName or
                    self::tei:publisher or
                    self::tei:pb or
                    self::tei:ptr or
                    self::tei:q or
                    self::tei:ref or
                    self::tei:seg or
                    self::tei:soCalled or
                    self::tei:term[not(ancestor-or-self::*/@type='termsAndDefinitions')] or
                    self::tei:title">
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