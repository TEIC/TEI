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
                exclude-result-prefixes="#all">
    
    <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" scope="stylesheet" type="stylesheet">
      <desc>
         <p> TEI Utility stylesheet for making Word docx files
        from TEI XML (see docx-tei.xsl)</p>
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
         <p>Id: $Id: functions.xsl 11232 2012-12-18 18:06:19Z rahtz $</p>
         <p>Copyright: 2013, TEI Consortium</p>
      </desc></doc>
      <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>Converts a dimension into the 20th of a point.</desc></doc>
      <xsl:function name="tei:convert-dim-pt20" as="xs:integer">
        <xsl:param name="dim"/>
	<xsl:value-of select="tei:convert-dim-pt($dim) * 20"/>
      </xsl:function>
    
    <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>Convert a dimension into point.</desc></doc>
      <xsl:function name="tei:convert-dim-pt" as="xs:integer">
        <xsl:param name="dim"/>
        <xsl:choose>
	  <xsl:when test="ends-with($dim,'%')">
	    <xsl:value-of select="number($pageWidth * number(substring($dim,0,string-length($dim)))) cast as xs:integer"/>
	  </xsl:when>
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
      <desc>Returns a listtype for a given stylename (return empty string to figure it out dynamically).</desc></doc>

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
    

        <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>Returns the correct heading style (return empty string to figure it out dynamically).</desc></doc>
    <xsl:function name="tei:get-headingstyle" as="xs:string">
        <xsl:param name="element"/>
        <xsl:param name="level"/>

        <xsl:text/>
    </xsl:function>    
    

</xsl:stylesheet>