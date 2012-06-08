<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
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
                exclude-result-prefixes="ve o r m v wp w10 w wne mml tbx iso tei a xs pic fn">

    
    <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" scope="stylesheet" type="stylesheet">
      <desc>
         <p> Specialized TEI stylesheet for transforming tei into Word docx files for Vesta </p>
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
    
    <!-- returns a listtype for a given stylename (return empty string to figure it out dynamically)-->
    <xsl:function name="tei:get-listtype" as="xs:string">
        <xsl:param name="style"/>
        <xsl:choose>
            <xsl:when test="starts-with($style,'dl')">
                <xsl:text>gloss</xsl:text>
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
                <xsl:text/>
            </xsl:otherwise>
        </xsl:choose>
    </xsl:function>
    
    <xsl:function name="tei:render-bold" as="xs:boolean">
        <xsl:param name="element"/>
        <xsl:for-each select="$element">
            <xsl:choose>
		<xsl:when test="ancestor-or-self::tei:cell[@rend='wovenodd-col1']">true</xsl:when>
		<xsl:when test="ancestor-or-self::tei:cell[@role='label']">true</xsl:when>
		<xsl:when test="self::tei:cell and parent::tei:row[@role='label']">true</xsl:when>
                <xsl:when test="@rend='bold'">true</xsl:when>
                <xsl:when test="@rend='label'">true</xsl:when>
                <xsl:when test="@rend='odd_label'">true</xsl:when>
                <xsl:when test="contains(@rend,'bold')">true</xsl:when>
                <xsl:when test="parent::tei:hi[@rend='bold']">true</xsl:when>
                <xsl:when test="parent::tei:hi[starts-with(@rend,'specList-')]">true</xsl:when>
                <xsl:when test="self::tei:docAuthor">true</xsl:when>
                <xsl:when test="self::tei:hi[@rend='label']">true</xsl:when>
                <xsl:when test="self::tei:label[following-sibling::tei:item]">true</xsl:when>
                <xsl:when test="self::tei:term">true</xsl:when>
                <xsl:when test="starts-with(@rend,'specList-')">true</xsl:when>
                <xsl:when test="starts-with(parent::tei:hi/@rend,'specList-')">true</xsl:when>
                <xsl:otherwise>false</xsl:otherwise>
            </xsl:choose>
        </xsl:for-each>
    </xsl:function>
    

</xsl:stylesheet>