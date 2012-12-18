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
      <desc>Convert a dimension into english metric unit.</desc></doc>
    <xsl:function name="tei:convert-dim-emu" as="xs:integer">
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
    
        <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>Defines whether or not a word paragraph is a first level heading.</desc></doc>
    <xsl:function name="tei:is-firstlevel-heading" as="xs:boolean">
        <xsl:param name="p"/>
        
        <xsl:choose>
            <xsl:when test="$p[w:pPr/w:pStyle/@w:val='heading 1']">true</xsl:when>
            <xsl:when test="$p[w:pPr/w:pStyle/@w:val='Heading 1']">true</xsl:when>
            <xsl:otherwise>false</xsl:otherwise>
        </xsl:choose>
    </xsl:function>
    
        <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>Defines whether or not a word paragraph is a  heading.</desc></doc>
    <xsl:function name="tei:is-heading" as="xs:boolean">
        <xsl:param name="p"/>
	<xsl:variable name="s" select="$p/w:pPr/w:pStyle/@w:val"/>
        <xsl:choose>
            <xsl:when test="matches($s,'[Hh]eading.+')">true</xsl:when>
            <xsl:when test="matches($s,'[Cc]aption')">true</xsl:when>
            <xsl:when test="matches($s,'Figure[ ]?title')">true</xsl:when>
            <xsl:otherwise>false</xsl:otherwise>
        </xsl:choose>
    </xsl:function>

        <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>Defines whether or not a word paragraph is a list element.</desc></doc>
    <xsl:function name="tei:is-list" as="xs:boolean">
        <xsl:param name="p"/>        
        <xsl:choose>
            <xsl:when test="$p[contains(w:pPr/w:pStyle/@w:val,'List')]">true</xsl:when>
            <xsl:when
		test="$p[w:pPr/w:pStyle/@w:val='dl']">true</xsl:when>
	    <xsl:when test="$p/w:pPr/w:numPr[not(w:ins)]">true</xsl:when>
            <xsl:otherwise>false</xsl:otherwise>
        </xsl:choose>
    </xsl:function>

        <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>Defines whether or not a word paragraph is a table of contents.</desc></doc>
    <xsl:function name="tei:is-toc" as="xs:boolean">
        <xsl:param name="p"/>        
        <xsl:choose>
            <xsl:when test="$p[contains(w:pPr/w:pStyle/@w:val,'toc')]">true</xsl:when>
            <xsl:otherwise>false</xsl:otherwise>
        </xsl:choose>
    </xsl:function>

        <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>Defines whether or not a word paragraph is a figure element.</desc></doc>
    <xsl:function name="tei:is-figure" as="xs:boolean">
        <xsl:param name="p"/>        
        <xsl:choose>
            <xsl:when test="$p[matches(w:pPr/w:pStyle/@w:val,'[Ff]igure')]">true</xsl:when>
            <xsl:when test="$p[matches(w:pPr/w:pStyle/@w:val,'[Cc]aption')]">true</xsl:when>
            <xsl:when test="$p[matches(w:pPr/w:pStyle/@w:val,'Figuretitle')]">true</xsl:when>
            <xsl:when test="$p[w:r/w:drawing and not(w:r/w:t)]">true</xsl:when>
            <xsl:otherwise>false</xsl:otherwise>
        </xsl:choose>
    </xsl:function>
    

        <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>Defines whether or not a word paragraph is a line of poetry.</desc></doc>
    <xsl:function name="tei:is-line" as="xs:boolean">
        <xsl:param name="p"/>        
        <xsl:choose>
            <xsl:when test="$p[w:pPr/w:pStyle/@w:val='tei_l']">true</xsl:when>
            <xsl:otherwise>false</xsl:otherwise>
        </xsl:choose>
    </xsl:function>

        <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>Defines whether or not a word paragraph is gloss list.</desc></doc>
    <xsl:function name="tei:is-glosslist" as="xs:boolean">
        <xsl:param name="p"/>        
        <xsl:choose>
            <xsl:when test="$p[w:pPr/w:pStyle/@w:val='dl']">true</xsl:when>
            <xsl:otherwise>false</xsl:otherwise>
        </xsl:choose>
    </xsl:function>
    
        <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>Is given a header style and returns the style for the next level header.</desc></doc>
    <xsl:function name="tei:get-nextlevel-header" as="xs:string">
        <xsl:param name="current-header"/>
        <xsl:value-of select="translate($current-header,'12345678','23456789')"/>
    </xsl:function>
    

  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc>Work out the size of included graphics</desc>
  </doc>
  <xsl:function name="tei:graphicSizes" as="element()">
    <xsl:param name="element"/>
    <xsl:param name="filename"/>
    <xsl:for-each select="$element">
      <xsl:variable name="origheight">
        <xsl:choose>
          <xsl:when test="@teidocx:height">
            <xsl:value-of select="@teidocx:height"/>
          </xsl:when>
          <xsl:when test="doc-available(concat($wordDirectory,'/image-size-info.xml'))">
            <xsl:for-each select="document(concat($wordDirectory,'/image-size-info.xml'))">
              <xsl:value-of select="(number(key('H',$filename)/height) div 72) * 9144"/>
            </xsl:for-each>
          </xsl:when>
          <xsl:otherwise>0</xsl:otherwise>
        </xsl:choose>
      </xsl:variable>
      <xsl:variable name="origwidth">
        <xsl:choose>
          <xsl:when test="@teidocx:width">
            <xsl:value-of select="@teidocx:width"/>
          </xsl:when>
          <xsl:when test="doc-available(concat($wordDirectory,'/image-size-info.xml'))">
            <xsl:for-each select="document(concat($wordDirectory,'/image-size-info.xml'))">
              <xsl:value-of select="(number(key('W',$filename)/width) div 72) * 9144"/>
            </xsl:for-each>
          </xsl:when>
          <xsl:otherwise>0</xsl:otherwise>
        </xsl:choose>
      </xsl:variable>
      <!--
                
                is there a number present?
                
                not(number(substring(@width,0,string-length(@width)-1))=NAN) and 
                not(number(substring(@height,0,string-length(@height)-1))=NAN)">
                
            -->
      <xsl:variable name="Width">
        <!-- remembering that pageWidth is already divided by 100 -->
        <xsl:choose>
          <xsl:when test="contains(@width,'%')">
            <xsl:value-of select="number($pageWidth * number(substring-before(@width,'%'))) cast as xs:integer"/>
          </xsl:when>
          <xsl:when test="@width">
            <xsl:value-of select="tei:convert-dim-emu(@width)"/>
          </xsl:when>
          <xsl:when test="@scale and $origwidth">
            <xsl:value-of select="($origwidth *  number(@scale)) cast as xs:integer"/>
          </xsl:when>
          <xsl:when test="@height and $origheight and $origwidth">
            <xsl:variable name="h">
              <xsl:choose>
                <xsl:when test="contains(@height,'%')">
                  <xsl:value-of select="number($pageHeight * (number(substring-before(@height,'%')))) cast as xs:integer"/>
                </xsl:when>
                <xsl:otherwise>
                  <xsl:value-of select="tei:convert-dim-emu(@height)"/>
                </xsl:otherwise>
              </xsl:choose>
            </xsl:variable>
            <xsl:value-of select="number(($h * $origwidth) div $origheight)    cast as xs:integer"/>
          </xsl:when>
          <xsl:when test="$origwidth">
            <xsl:value-of select="$origwidth"/>
          </xsl:when>
          <xsl:otherwise>
            <xsl:message terminate="yes">no way to work out image width for
                            <xsl:value-of select="$filename"/>
                        </xsl:message>
          </xsl:otherwise>
        </xsl:choose>
      </xsl:variable>
      <xsl:variable name="Height">
        <xsl:choose>
          <xsl:when test="contains(@height,'%')">
            <xsl:value-of select="number($pageHeight * (number(substring-before(@height,'%')))) cast as xs:integer"/>
          </xsl:when>
          <xsl:when test="@height">
            <xsl:value-of select="tei:convert-dim-emu(@height)"/>
          </xsl:when>
          <xsl:when test="@scale and $origheight">
            <xsl:value-of select="($origheight * number(@scale)) cast as xs:integer"/>
          </xsl:when>
          <xsl:when test="@width[contains(.,'%')]">
            <xsl:value-of select="number($pageHeight * (number(substring-before(@width,'%')))) cast as xs:integer"/>
          </xsl:when>
          <xsl:when test="@width[not(contains(.,'%'))] and $origheight and $origwidth">
            <xsl:value-of select="number(  (number($Width) *  number($origheight)) div number($origwidth)) cast as xs:integer"/>
          </xsl:when>
          <xsl:when test="$origheight">
            <xsl:value-of select="$origheight"/>
          </xsl:when>
          <xsl:otherwise>
            <xsl:message terminate="yes">no way to work out image height for
                            <xsl:value-of select="$filename"/>
                        </xsl:message>
          </xsl:otherwise>
        </xsl:choose>
      </xsl:variable>
      <sizes Height="{$Height}" Width="{$Width}" origheight="{$origheight}" origwidth="{$origwidth}"/>
    </xsl:for-each>
  </xsl:function>



</xsl:stylesheet>
