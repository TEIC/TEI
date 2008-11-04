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

    <xsl:import href="variables.xsl"/>
    
    <!-- returns a listtype for a given stylename (return empty string to figure it out dynamically)-->
    <xsl:function name="teidocx:get-listtype" as="xs:string">
        <xsl:param name="style"/>
        <xsl:choose>
            <xsl:when test="$style=$Terms">
                <xsl:text>termlist</xsl:text>
            </xsl:when>
            <xsl:when test="$style=$TermNum">
                <xsl:text>termlist</xsl:text>
            </xsl:when>
            <xsl:when test="starts-with($style,$ListBullet)">
                <xsl:text>unordered</xsl:text>
            </xsl:when>
            <xsl:when test="starts-with($style,$ListContinue)">
                <xsl:text>unordered</xsl:text>
            </xsl:when>
            <xsl:when test="starts-with($style,$ListNumber)">
                <xsl:text>ordered</xsl:text>
            </xsl:when>
            <xsl:when test="$style=$List">
                <xsl:text>ordered</xsl:text>
            </xsl:when>
            <xsl:otherwise>
                <xsl:text></xsl:text>
            </xsl:otherwise>
        </xsl:choose>
    </xsl:function>
    
    
    <!-- define special rendering -->
    <xsl:function name="teidocx:render-bold" as="xs:boolean">
        <xsl:param name="element"/>
        <xsl:for-each select="$element">
            <xsl:choose>
                <xsl:when test="self::tei:term">
                    true
                </xsl:when>
                <xsl:otherwise>false</xsl:otherwise>
            </xsl:choose>
        </xsl:for-each>
    </xsl:function>
    
    <xsl:function name="teidocx:render-italic" as="xs:boolean">
        <xsl:param name="element"/>
        <xsl:for-each select="$element">
            <xsl:choose>
                <xsl:when test="self::tei:term">
                    true
                </xsl:when>
                <xsl:otherwise>false</xsl:otherwise>
            </xsl:choose>
        </xsl:for-each>
    </xsl:function>
    
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
                    self::tei:editionStmt or
                    self::tei:date or
                    self::tei:emph or
                    self::tei:foreign or
                    self::tei:graphic or
                    self::tei:hi[not(w:*)] or
                    self::tei:idno or
                    self::tei:lb or
                    self::tei:note[@place='foot'] or
                    self::tei:note[@place='end'] or
                    self::tei:num or
                    self::tei:mentioned or
                    self::tei:orgName or
                    self::tei:publisher or
                    self::tei:pb or
                    self::tei:q or
                    self::tei:ref or
                    self::tei:seg or
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