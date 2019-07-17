<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet xmlns:a="http://schemas.openxmlformats.org/drawingml/2006/main"
                xmlns:cals="http://www.oasis-open.org/specs/tm9901"
                xmlns:contypes="http://schemas.openxmlformats.org/package/2006/content-types"
                xmlns:cp="http://schemas.openxmlformats.org/package/2006/metadata/core-properties"
                xmlns:dc="http://purl.org/dc/elements/1.1/"
                xmlns:dcmitype="http://purl.org/dc/dcmitype/"
                xmlns:dcterms="http://purl.org/dc/terms/"
                xmlns:html="http://www.w3.org/1999/xhtml"
                xmlns:iso="http://www.iso.org/ns/1.0"
                xmlns:m="http://schemas.openxmlformats.org/officeDocument/2006/math"
                xmlns:mml="http://www.w3.org/1998/Math/MathML"
                xmlns:o="urn:schemas-microsoft-com:office:office"
                xmlns:pic="http://schemas.openxmlformats.org/drawingml/2006/picture"
                xmlns:r="http://schemas.openxmlformats.org/officeDocument/2006/relationships"
                xmlns:tbx="http://www.lisa.org/TBX-Specification.33.0.html"
                xmlns:tei="http://www.tei-c.org/ns/1.0"
                xmlns:teidocx="http://www.tei-c.org/ns/teidocx/1.0"
                xmlns:teix="http://www.tei-c.org/ns/Examples"
                xmlns:v="urn:schemas-microsoft-com:vml"
                xmlns:fn="http://www.w3.org/2005/02/xpath-functions"
                xmlns:ve="http://schemas.openxmlformats.org/markup-compatibility/2006"
                xmlns:w10="urn:schemas-microsoft-com:office:word"
                xmlns:w="http://schemas.openxmlformats.org/wordprocessingml/2006/main"
                xmlns:wne="http://schemas.microsoft.com/office/word/2006/wordml"
                xmlns:wp="http://schemas.openxmlformats.org/drawingml/2006/wordprocessingDrawing"
                
                xmlns:xs="http://www.w3.org/2001/XMLSchema"
                xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
                xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                version="2.0"
                exclude-result-prefixes="cp ve o r m v wp w10 w wne mml tbx iso      tei a xs pic fn xsi dc dcterms dcmitype     contypes teidocx teix html cals">
    
    
    <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" scope="stylesheet" type="stylesheet">
      <desc>
         <p> TEI stylesheet for making Word docx files from TEI XML </p>
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
    
    
     <xsl:template name="write-docxfile-numbering-definition">
	     <xsl:if test="$debug='true'">
	        <xsl:message>Writing out <xsl:value-of select="$numberDoc"/>
         </xsl:message>
	     </xsl:if>

        <xsl:result-document href="{$numberDoc}">
            <w:numbering>

                <!-- for headlines -->
                <w:abstractNum w:abstractNumId="1">
                    <w:multiLevelType w:val="multilevel"/>
                    <w:name w:val="heading"/>
                    <w:lvl w:ilvl="0">
                        <w:start w:val="1"/>
                        <w:numFmt w:val="decimal"/>
                        <w:pStyle w:val="Heading1"/>
                        <w:lvlText w:val="%1"/>
                        <w:lvlJc w:val="left"/>
                        <w:pPr>
                            <w:ind w:left="432" w:hanging="432"/>
                        </w:pPr>
                        <w:rPr>
                            <w:rFonts w:hint="default"/>
                        </w:rPr>
                    </w:lvl>
                    <w:lvl w:ilvl="1">
                        <w:start w:val="1"/>
                        <w:numFmt w:val="decimal"/>
                        <w:pStyle w:val="Heading2"/>
                        <w:lvlText w:val="%1.%2"/>
                        <w:lvlJc w:val="left"/>
                        <w:pPr>
                            <w:ind w:left="648" w:hanging="648"/>
                        </w:pPr>
                        <w:rPr>
                            <w:rFonts w:hint="default"/>
                        </w:rPr>
                    </w:lvl>
                    <w:lvl w:ilvl="2">
                        <w:start w:val="1"/>
                        <w:numFmt w:val="decimal"/>
                        <w:pStyle w:val="Heading3"/>
                        <w:lvlText w:val="%1.%2.%3"/>
                        <w:lvlJc w:val="left"/>
                        <w:pPr>
                            <w:ind w:left="864" w:hanging="864"/>
                        </w:pPr>
                        <w:rPr>
                            <w:rFonts w:hint="default"/>
                        </w:rPr>
                    </w:lvl>
                    <w:lvl w:ilvl="3">
                        <w:start w:val="1"/>
                        <w:numFmt w:val="decimal"/>
                        <w:pStyle w:val="Heading4"/>
                        <w:lvlText w:val="%1.%2.%3.%4"/>
                        <w:lvlJc w:val="left"/>
                        <w:pPr>
                            <w:ind w:left="1080" w:hanging="1080"/>
                        </w:pPr>
                        <w:rPr>
                            <w:rFonts w:hint="default"/>
                        </w:rPr>
                    </w:lvl>
                    <w:lvl w:ilvl="4">
                        <w:start w:val="1"/>
                        <w:numFmt w:val="decimal"/>
                        <w:pStyle w:val="Heading5"/>
                        <w:lvlText w:val="%1.%2.%3.%4.%5"/>
                        <w:lvlJc w:val="left"/>
                        <w:pPr>
                            <w:ind w:left="1296" w:hanging="1296"/>
                        </w:pPr>
                        <w:rPr>
                            <w:rFonts w:hint="default"/>
                        </w:rPr>
                    </w:lvl>
                    <w:lvl w:ilvl="5">
                        <w:start w:val="1"/>
                        <w:numFmt w:val="decimal"/>
                        <w:pStyle w:val="Heading6"/>
                        <w:lvlText w:val="%1.%2.%3.%4.%5.%6"/>
                        <w:lvlJc w:val="left"/>
                        <w:pPr>
                            <w:ind w:left="1512" w:hanging="1512"/>
                        </w:pPr>
                        <w:rPr>
                            <w:rFonts w:hint="default"/>
                        </w:rPr>
                    </w:lvl>
                    <w:lvl w:ilvl="6">
                        <w:start w:val="1"/>
                        <w:numFmt w:val="decimal"/>
                        <w:pStyle w:val="Heading7"/>
                        <w:lvlText w:val="%1.%2.%3.%4.%5.%6.%7"/>
                        <w:lvlJc w:val="left"/>
                        <w:pPr>
                            <w:ind w:left="1728" w:hanging="1728"/>
                        </w:pPr>
                        <w:rPr>
                            <w:rFonts w:hint="default"/>
                        </w:rPr>
                    </w:lvl>
                    <w:lvl w:ilvl="7">
                        <w:start w:val="1"/>
                        <w:numFmt w:val="decimal"/>
                        <w:pStyle w:val="Heading8"/>
                        <w:lvlText w:val="%1.%2.%3.%4.%5.%6.%7.%8"/>
                        <w:lvlJc w:val="left"/>
                        <w:pPr>
                            <w:ind w:left="1944" w:hanging="1944"/>
                        </w:pPr>
                        <w:rPr>
                            <w:rFonts w:hint="default"/>
                        </w:rPr>
                    </w:lvl>
                    <w:lvl w:ilvl="8">
                        <w:start w:val="1"/>
                        <w:numFmt w:val="decimal"/>
                        <w:pStyle w:val="Heading9"/>
                        <w:lvlText w:val="%1.%2.%3.%4.%5.%6.%7.%8.%9"/>
                        <w:lvlJc w:val="left"/>
                        <w:pPr>
                            <w:ind w:left="2160" w:hanging="2160"/>
                        </w:pPr>
                        <w:rPr>
                            <w:rFonts w:hint="default"/>
                        </w:rPr>
                    </w:lvl>
                </w:abstractNum>


                <!-- unordered lists -->
		<xsl:call-template name="defineUnorderedLists"/>

                <!-- ordered lists -->
		<xsl:call-template name="defineOrderedLists"/>


                <!-- for sections in Annex -->
                <w:abstractNum w:abstractNumId="4">
                    <w:multiLevelType w:val="multilevel"/>
                    <w:lvl w:ilvl="0">
                        <w:start w:val="1"/>
                        <w:numFmt w:val="upperLetter"/>
                        <w:pStyle w:val="ANNEX"/>
                        <w:suff w:val="nothing"/>
                        <w:lvlText w:val="Annex %1: "/>
                        <w:lvlJc w:val="left"/>
                        <w:pPr>
                            <w:ind w:left="0" w:firstLine="0"/>
                        </w:pPr>
                        <w:rPr>
                            <w:rFonts w:hint="default"/>
                        </w:rPr>
                    </w:lvl>
                    <w:lvl w:ilvl="1">
                        <w:start w:val="1"/>
                        <w:numFmt w:val="decimal"/>
                        <w:pStyle w:val="a2"/>
                        <w:lvlText w:val="%1.%2"/>
                        <w:lvlJc w:val="left"/>
                        <w:pPr>
                            <w:ind w:left="720" w:hanging="720"/>
                        </w:pPr>
                        <w:rPr>
                            <w:rFonts w:hint="default"/>
                        </w:rPr>
                    </w:lvl>
                    <w:lvl w:ilvl="2">
                        <w:start w:val="1"/>
                        <w:numFmt w:val="decimal"/>
                        <w:pStyle w:val="a3"/>
                        <w:lvlText w:val="%1.%2.%3"/>
                        <w:lvlJc w:val="left"/>
                        <w:pPr>
                            <w:ind w:left="936" w:hanging="936"/>
                        </w:pPr>
                        <w:rPr>
                            <w:rFonts w:hint="default"/>
                        </w:rPr>
                    </w:lvl>
                    <w:lvl w:ilvl="3">
                        <w:start w:val="1"/>
                        <w:numFmt w:val="decimal"/>
                        <w:pStyle w:val="a4"/>
                        <w:lvlText w:val="%1.%2.%3.%4"/>
                        <w:lvlJc w:val="left"/>
                        <w:pPr>
                            <w:ind w:left="1152" w:hanging="1152"/>
                        </w:pPr>
                        <w:rPr>
                            <w:rFonts w:hint="default"/>
                        </w:rPr>
                    </w:lvl>
                    <w:lvl w:ilvl="4">
                        <w:start w:val="1"/>
                        <w:numFmt w:val="decimal"/>
                        <w:pStyle w:val="a5"/>
                        <w:lvlText w:val="%1.%2.%3.%4.%5"/>
                        <w:lvlJc w:val="left"/>
                        <w:pPr>
                            <w:ind w:left="1368" w:hanging="1368"/>
                        </w:pPr>
                        <w:rPr>
                            <w:rFonts w:hint="default"/>
                        </w:rPr>
                    </w:lvl>
                    <w:lvl w:ilvl="5">
                        <w:start w:val="1"/>
                        <w:numFmt w:val="decimal"/>
                        <w:pStyle w:val="a6"/>
                        <w:lvlText w:val="%1.%2.%3.%4.%5.%6"/>
                        <w:lvlJc w:val="left"/>
                        <w:pPr>
                            <w:ind w:left="1584" w:hanging="1584"/>
                        </w:pPr>
                        <w:rPr>
                            <w:rFonts w:hint="default"/>
                        </w:rPr>
                    </w:lvl>
                    <w:lvl w:ilvl="6">
                        <w:start w:val="1"/>
                        <w:numFmt w:val="decimal"/>
                        <w:lvlText w:val="%1.%2.%3.%4.%5.%6.%7"/>
                        <w:lvlJc w:val="left"/>
                        <w:pPr>
                            <w:ind w:left="1800" w:hanging="1800"/>
                        </w:pPr>
                        <w:rPr>
                            <w:rFonts w:hint="default"/>
                        </w:rPr>
                    </w:lvl>
                    <w:lvl w:ilvl="7">
                        <w:start w:val="1"/>
                        <w:numFmt w:val="decimal"/>
                        <w:lvlText w:val="%1.%2.%3.%4.%5.%6.%7.%8"/>
                        <w:lvlJc w:val="left"/>
                        <w:pPr>
                            <w:ind w:left="2016" w:hanging="2016"/>
                        </w:pPr>
                        <w:rPr>
                            <w:rFonts w:hint="default"/>
                        </w:rPr>
                    </w:lvl>
                    <w:lvl w:ilvl="8">
                        <w:start w:val="1"/>
                        <w:numFmt w:val="decimal"/>
                        <w:lvlText w:val="%1.%2.%3.%4.%5.%6.%7.%8.%9"/>
                        <w:lvlJc w:val="left"/>
                        <w:pPr>
                            <w:ind w:left="2232" w:hanging="2232"/>
                        </w:pPr>
                        <w:rPr>
                            <w:rFonts w:hint="default"/>
                        </w:rPr>
                    </w:lvl>
                </w:abstractNum>

                <!-- for tables -->
                <w:abstractNum w:abstractNumId="5">
                    <w:multiLevelType w:val="hybridMultilevel"/>
                    <w:lvl w:ilvl="0">
                        <w:start w:val="1"/>
                        <w:numFmt w:val="decimal"/>
                        <w:pStyle w:val="Tabletitle"/>
                        <w:suff w:val="space"/>
                        <w:lvlText w:val="Table %1"/>
                        <w:lvlJc w:val="left"/>
                        <w:pPr>
                            <w:ind w:left="720" w:hanging="360"/>
                        </w:pPr>
                        <w:rPr>
                            <w:rFonts w:hint="default"/>
                        </w:rPr>
                    </w:lvl>
                    <w:lvl w:ilvl="1">
                        <w:start w:val="1"/>
                        <w:numFmt w:val="lowerLetter"/>
                        <w:lvlText w:val="%2."/>
                        <w:lvlJc w:val="left"/>
                        <w:pPr>
                            <w:ind w:left="1440" w:hanging="360"/>
                        </w:pPr>
                    </w:lvl>
                    <w:lvl w:ilvl="2">
                        <w:start w:val="1"/>
                        <w:numFmt w:val="lowerRoman"/>
                        <w:lvlText w:val="%3."/>
                        <w:lvlJc w:val="right"/>
                        <w:pPr>
                            <w:ind w:left="2160" w:hanging="180"/>
                        </w:pPr>
                    </w:lvl>
                    <w:lvl w:ilvl="3">
                        <w:start w:val="1"/>
                        <w:numFmt w:val="decimal"/>
                        <w:lvlText w:val="%4."/>
                        <w:lvlJc w:val="left"/>
                        <w:pPr>
                            <w:ind w:left="2880" w:hanging="360"/>
                        </w:pPr>
                    </w:lvl>
                    <w:lvl w:ilvl="4">
                        <w:start w:val="1"/>
                        <w:numFmt w:val="lowerLetter"/>
                        <w:lvlText w:val="%5."/>
                        <w:lvlJc w:val="left"/>
                        <w:pPr>
                            <w:ind w:left="3600" w:hanging="360"/>
                        </w:pPr>
                    </w:lvl>
                    <w:lvl w:ilvl="5">
                        <w:start w:val="1"/>
                        <w:numFmt w:val="lowerRoman"/>
                        <w:lvlText w:val="%6."/>
                        <w:lvlJc w:val="right"/>
                        <w:pPr>
                            <w:ind w:left="4320" w:hanging="180"/>
                        </w:pPr>
                    </w:lvl>
                    <w:lvl w:ilvl="6">
                        <w:start w:val="1"/>
                        <w:numFmt w:val="decimal"/>
                        <w:lvlText w:val="%7."/>
                        <w:lvlJc w:val="left"/>
                        <w:pPr>
                            <w:ind w:left="5040" w:hanging="360"/>
                        </w:pPr>
                    </w:lvl>
                    <w:lvl w:ilvl="7">
                        <w:start w:val="1"/>
                        <w:numFmt w:val="lowerLetter"/>
                        <w:lvlText w:val="%8."/>
                        <w:lvlJc w:val="left"/>
                        <w:pPr>
                            <w:ind w:left="5760" w:hanging="360"/>
                        </w:pPr>
                    </w:lvl>
                    <w:lvl w:ilvl="8">
                        <w:start w:val="1"/>
                        <w:numFmt w:val="lowerRoman"/>
                        <w:lvlText w:val="%9."/>
                        <w:lvlJc w:val="right"/>
                        <w:pPr>
                            <w:ind w:left="6480" w:hanging="180"/>
                        </w:pPr>
                    </w:lvl>
                </w:abstractNum>

                <!-- for bibliography -->
                <w:abstractNum w:abstractNumId="6">
                    <w:multiLevelType w:val="hybridMultilevel"/>
                    <w:lvl w:ilvl="0">
                        <w:start w:val="1"/>
                        <w:numFmt w:val="decimal"/>
                        <w:pStyle w:val="Bibliography"/>
                        <w:lvlText w:val="[%1]"/>
                        <w:lvlJc w:val="left"/>
                        <w:pPr>
                            <w:ind w:left="1080" w:hanging="1080"/>
                        </w:pPr>
                        <w:rPr>
                            <w:rFonts w:hint="default"/>
                        </w:rPr>
                    </w:lvl>
                    <w:lvl w:ilvl="1">
                        <w:start w:val="1"/>
                        <w:numFmt w:val="lowerLetter"/>
                        <w:lvlText w:val="%2."/>
                        <w:lvlJc w:val="left"/>
                        <w:pPr>
                            <w:ind w:left="1440" w:hanging="360"/>
                        </w:pPr>
                    </w:lvl>
                    <w:lvl w:ilvl="2">
                        <w:start w:val="1"/>
                        <w:numFmt w:val="lowerRoman"/>
                        <w:lvlText w:val="%3."/>
                        <w:lvlJc w:val="right"/>
                        <w:pPr>
                            <w:ind w:left="2160" w:hanging="180"/>
                        </w:pPr>
                    </w:lvl>
                    <w:lvl w:ilvl="3">
                        <w:start w:val="1"/>
                        <w:numFmt w:val="decimal"/>
                        <w:lvlText w:val="%4."/>
                        <w:lvlJc w:val="left"/>
                        <w:pPr>
                            <w:ind w:left="2880" w:hanging="360"/>
                        </w:pPr>
                    </w:lvl>
                    <w:lvl w:ilvl="4">
                        <w:start w:val="1"/>
                        <w:numFmt w:val="lowerLetter"/>
                        <w:lvlText w:val="%5."/>
                        <w:lvlJc w:val="left"/>
                        <w:pPr>
                            <w:ind w:left="3600" w:hanging="360"/>
                        </w:pPr>
                    </w:lvl>
                    <w:lvl w:ilvl="5">
                        <w:start w:val="1"/>
                        <w:numFmt w:val="lowerRoman"/>
                        <w:lvlText w:val="%6."/>
                        <w:lvlJc w:val="right"/>
                        <w:pPr>
                            <w:ind w:left="4320" w:hanging="180"/>
                        </w:pPr>
                    </w:lvl>
                    <w:lvl w:ilvl="6">
                        <w:start w:val="1"/>
                        <w:numFmt w:val="decimal"/>
                        <w:lvlText w:val="%7."/>
                        <w:lvlJc w:val="left"/>
                        <w:pPr>
                            <w:ind w:left="5040" w:hanging="360"/>
                        </w:pPr>
                    </w:lvl>
                    <w:lvl w:ilvl="7">
                        <w:start w:val="1"/>
                        <w:numFmt w:val="lowerLetter"/>
                        <w:lvlText w:val="%8."/>
                        <w:lvlJc w:val="left"/>
                        <w:pPr>
                            <w:ind w:left="5760" w:hanging="360"/>
                        </w:pPr>
                    </w:lvl>
                    <w:lvl w:ilvl="8">
                        <w:start w:val="1"/>
                        <w:numFmt w:val="lowerRoman"/>
                        <w:lvlText w:val="%9."/>
                        <w:lvlJc w:val="right"/>
                        <w:pPr>
                            <w:ind w:left="6480" w:hanging="180"/>
                        </w:pPr>
                    </w:lvl>
                </w:abstractNum>

                <!-- for figures -->
                <w:abstractNum w:abstractNumId="7">
                    <w:multiLevelType w:val="hybridMultilevel"/>
                    <w:lvl w:ilvl="0">
                        <w:start w:val="1"/>
                        <w:numFmt w:val="decimal"/>
                        <w:pStyle w:val="Figuretitle"/>
                        <w:suff w:val="space"/>
                        <w:lvlText w:val="Figure %1"/>
                        <w:lvlJc w:val="left"/>
                        <w:pPr>
                            <w:ind w:left="0" w:firstLine="0"/>
                        </w:pPr>
                        <w:rPr>
                            <w:rFonts w:hint="default"/>
                        </w:rPr>
                    </w:lvl>
                    <w:lvl w:ilvl="1">
                        <w:start w:val="1"/>
                        <w:numFmt w:val="lowerLetter"/>
                        <w:lvlText w:val="%2."/>
                        <w:lvlJc w:val="left"/>
                        <w:pPr>
                            <w:ind w:left="1440" w:hanging="360"/>
                        </w:pPr>
                    </w:lvl>
                    <w:lvl w:ilvl="2">
                        <w:start w:val="1"/>
                        <w:numFmt w:val="lowerRoman"/>
                        <w:lvlText w:val="%3."/>
                        <w:lvlJc w:val="right"/>
                        <w:pPr>
                            <w:ind w:left="2160" w:hanging="180"/>
                        </w:pPr>
                    </w:lvl>
                    <w:lvl w:ilvl="3">
                        <w:start w:val="1"/>
                        <w:numFmt w:val="decimal"/>
                        <w:lvlText w:val="%4."/>
                        <w:lvlJc w:val="left"/>
                        <w:pPr>
                            <w:ind w:left="2880" w:hanging="360"/>
                        </w:pPr>
                    </w:lvl>
                    <w:lvl w:ilvl="4">
                        <w:start w:val="1"/>
                        <w:numFmt w:val="lowerLetter"/>
                        <w:lvlText w:val="%5."/>
                        <w:lvlJc w:val="left"/>
                        <w:pPr>
                            <w:ind w:left="3600" w:hanging="360"/>
                        </w:pPr>
                    </w:lvl>
                    <w:lvl w:ilvl="5">
                        <w:start w:val="1"/>
                        <w:numFmt w:val="lowerRoman"/>
                        <w:lvlText w:val="%6."/>
                        <w:lvlJc w:val="right"/>
                        <w:pPr>
                            <w:ind w:left="4320" w:hanging="180"/>
                        </w:pPr>
                    </w:lvl>
                    <w:lvl w:ilvl="6">
                        <w:start w:val="1"/>
                        <w:numFmt w:val="decimal"/>
                        <w:lvlText w:val="%7."/>
                        <w:lvlJc w:val="left"/>
                        <w:pPr>
                            <w:ind w:left="5040" w:hanging="360"/>
                        </w:pPr>
                    </w:lvl>
                    <w:lvl w:ilvl="7">
                        <w:start w:val="1"/>
                        <w:numFmt w:val="lowerLetter"/>
                        <w:lvlText w:val="%8."/>
                        <w:lvlJc w:val="left"/>
                        <w:pPr>
                            <w:ind w:left="5760" w:hanging="360"/>
                        </w:pPr>
                    </w:lvl>
                    <w:lvl w:ilvl="8">
                        <w:start w:val="1"/>
                        <w:numFmt w:val="lowerRoman"/>
                        <w:lvlText w:val="%9."/>
                        <w:lvlJc w:val="right"/>
                        <w:pPr>
                            <w:ind w:left="6480" w:hanging="180"/>
                        </w:pPr>
                    </w:lvl>
                </w:abstractNum>

                <!-- for headlines -->
                <w:num w:numId="1">
                    <w:abstractNumId w:val="1"/>
                </w:num>

                <!-- unordered lists -->
                <w:num w:numId="2">
                    <w:abstractNumId w:val="2"/>
                </w:num>

                <!-- for annex sections -->
                <w:num w:numId="3">
                    <w:abstractNumId w:val="4"/>
                </w:num>

                <!-- for tables -->
                <w:num w:numId="5">
                    <w:abstractNumId w:val="5"/>
                </w:num>

                <!-- for bibliography -->
                <w:num w:numId="6">
                    <w:abstractNumId w:val="6"/>
                </w:num>

                <!-- for figures -->
                <w:num w:numId="7">
                    <w:abstractNumId w:val="7"/>
                </w:num>

                <!-- for the ordered lists style -->
                <w:num w:numId="8">
                    <w:abstractNumId w:val="3"/>
                    <w:lvlOverride w:ilvl="0">
                        <w:startOverride w:val="1"/>
                    </w:lvlOverride>
                    <w:lvlOverride w:ilvl="1">
                        <w:startOverride w:val="1"/>
                    </w:lvlOverride>
                    <w:lvlOverride w:ilvl="2">
                        <w:startOverride w:val="1"/>
                    </w:lvlOverride>
                    <w:lvlOverride w:ilvl="3">
                        <w:startOverride w:val="1"/>
                    </w:lvlOverride>
                    <w:lvlOverride w:ilvl="4">
                        <w:startOverride w:val="1"/>
                    </w:lvlOverride>
                    <w:lvlOverride w:ilvl="5">
                        <w:startOverride w:val="1"/>
                    </w:lvlOverride>
                    <w:lvlOverride w:ilvl="6">
                        <w:startOverride w:val="1"/>
                    </w:lvlOverride>
                    <w:lvlOverride w:ilvl="7">
                        <w:startOverride w:val="1"/>
                    </w:lvlOverride>
                    <w:lvlOverride w:ilvl="8">
                        <w:startOverride w:val="1"/>
                    </w:lvlOverride>
                </w:num>

                <!-- for the unordered lists style -->
                <w:num w:numId="9">
                    <w:abstractNumId w:val="2"/>
                </w:num>


                <!-- ordered lists -->
                <!-- 
                    We have to generate an instance for each list present in the
                    document.
                -->
                <xsl:for-each select="key('OL',1)">
                    <w:num>
                        <xsl:attribute name="w:numId">
                            <xsl:value-of select="position()+100"/>
                        </xsl:attribute>
                        <w:abstractNumId w:val="3"/>
                        <w:lvlOverride w:ilvl="0">
                            <w:startOverride w:val="1"/>
                        </w:lvlOverride>
                        <w:lvlOverride w:ilvl="1">
                            <w:startOverride w:val="1"/>
                        </w:lvlOverride>
                        <w:lvlOverride w:ilvl="2">
                            <w:startOverride w:val="1"/>
                        </w:lvlOverride>
                        <w:lvlOverride w:ilvl="3">
                            <w:startOverride w:val="1"/>
                        </w:lvlOverride>
                        <w:lvlOverride w:ilvl="4">
                            <w:startOverride w:val="1"/>
                        </w:lvlOverride>
                        <w:lvlOverride w:ilvl="5">
                            <w:startOverride w:val="1"/>
                        </w:lvlOverride>
                        <w:lvlOverride w:ilvl="6">
                            <w:startOverride w:val="1"/>
                        </w:lvlOverride>
                        <w:lvlOverride w:ilvl="7">
                            <w:startOverride w:val="1"/>
                        </w:lvlOverride>
                        <w:lvlOverride w:ilvl="8">
                            <w:startOverride w:val="1"/>
                        </w:lvlOverride>
                    </w:num>
                </xsl:for-each>


            </w:numbering>
        </xsl:result-document>
    </xsl:template>
    
    
    
    
    <xsl:template name="defineUnorderedLists">
        <w:abstractNum w:abstractNumId="2">
            <w:multiLevelType w:val="multilevel"/>
            <w:lvl w:ilvl="0">
                <w:start w:val="1"/>
                <w:numFmt w:val="bullet"/>
                <w:pStyle w:val="ListBullet"/>
                <w:lvlText w:val="{$bulletOne}"/>
                <w:lvlJc w:val="left"/>
                <w:pPr>
                    <w:ind w:left="360" w:hanging="0"/>
                </w:pPr>
                <w:rPr>
                    <w:rFonts w:ascii="Symbol" w:hAnsi="Symbol" w:hint="default"/>
                    <w:color w:val="auto"/>
                </w:rPr>
            </w:lvl>
            <w:lvl w:ilvl="1">
                <w:start w:val="1"/>
                <w:numFmt w:val="bullet"/>
                <w:pStyle w:val="ListBullet"/>
                <w:lvlText w:val="{$bulletTwo}"/>
                <w:lvlJc w:val="left"/>
                <w:pPr>
                    <w:ind w:left="720" w:hanging="360"/>
                </w:pPr>
                <w:rPr>
                    <w:rFonts w:ascii="Symbol" w:hAnsi="Symbol" w:hint="default"/>
                    <w:color w:val="auto"/>
                </w:rPr>
            </w:lvl>
            <w:lvl w:ilvl="2">
                <w:start w:val="1"/>
                <w:numFmt w:val="bullet"/>
                <w:pStyle w:val="ListBullet"/>
                <w:lvlText w:val="{$bulletThree}"/>
                <w:lvlJc w:val="left"/>
                <w:pPr>
                    <w:ind w:left="1080" w:hanging="360"/>
                </w:pPr>
                <w:rPr>
                    <w:rFonts w:ascii="Symbol" w:hAnsi="Symbol" w:hint="default"/>
                    <w:color w:val="auto"/>
                </w:rPr>
            </w:lvl>
            <w:lvl w:ilvl="3">
                <w:start w:val="1"/>
                <w:numFmt w:val="bullet"/>
                <w:pStyle w:val="ListBullet"/>
                <w:lvlText w:val="{$bulletFour}"/>
                <w:lvlJc w:val="left"/>
                <w:pPr>
                    <w:ind w:left="1440" w:hanging="360"/>
                </w:pPr>
                <w:rPr>
                    <w:rFonts w:ascii="Symbol" w:hAnsi="Symbol" w:hint="default"/>
                    <w:color w:val="auto"/>
                </w:rPr>
            </w:lvl>
            <w:lvl w:ilvl="4">
                <w:start w:val="1"/>
                <w:numFmt w:val="bullet"/>
                <w:pStyle w:val="ListBullet"/>
                <w:lvlText w:val="{$bulletFive}"/>
                <w:lvlJc w:val="left"/>
                <w:pPr>
                    <w:ind w:left="1800" w:hanging="360"/>
                </w:pPr>
                <w:rPr>
                    <w:rFonts w:ascii="Symbol" w:hAnsi="Symbol" w:hint="default"/>
                    <w:color w:val="auto"/>
                </w:rPr>
            </w:lvl>
            <w:lvl w:ilvl="5">
                <w:start w:val="1"/>
                <w:numFmt w:val="bullet"/>
                <w:pStyle w:val="ListBullet"/>
                <w:lvlText w:val="{$bulletSix}"/>
                <w:lvlJc w:val="left"/>
                <w:pPr>
                    <w:ind w:left="2160" w:hanging="360"/>
                </w:pPr>
                <w:rPr>
                    <w:rFonts w:ascii="Symbol" w:hAnsi="Symbol" w:hint="default"/>
                    <w:color w:val="auto"/>
                </w:rPr>
            </w:lvl>
            <w:lvl w:ilvl="6">
                <w:start w:val="1"/>
                <w:numFmt w:val="bullet"/>
                <w:pStyle w:val="ListBullet"/>
                <w:lvlText w:val="{$bulletSeven}"/>
                <w:lvlJc w:val="left"/>
                <w:pPr>
                    <w:ind w:left="2520" w:hanging="360"/>
                </w:pPr>
                <w:rPr>
                    <w:rFonts w:ascii="Symbol" w:hAnsi="Symbol" w:hint="default"/>
                    <w:color w:val="auto"/>
                </w:rPr>
            </w:lvl>
            <w:lvl w:ilvl="7">
                <w:start w:val="1"/>
                <w:numFmt w:val="bullet"/>
                <w:pStyle w:val="ListBullet"/>
                <w:lvlText w:val="{$bulletEight}"/>
                <w:lvlJc w:val="left"/>
                <w:pPr>
                    <w:ind w:left="2880" w:hanging="360"/>
                </w:pPr>
                <w:rPr>
                    <w:rFonts w:ascii="Symbol" w:hAnsi="Symbol" w:hint="default"/>
                    <w:color w:val="auto"/>
                </w:rPr>
            </w:lvl>
            
        </w:abstractNum>
    </xsl:template>
    
    <!-- ordered lists -->
    <xsl:template name="defineOrderedLists">
        <w:abstractNum w:abstractNumId="3">
            <w:multiLevelType w:val="multilevel"/>
            <w:lvl w:ilvl="0">
                <w:start w:val="1"/>
                <w:numFmt w:val="lowerLetter"/>
                <w:pStyle w:val="ListNumber"/>
                <w:lvlText w:val="%1)"/>
                <w:lvlJc w:val="left"/>
                <w:pPr>
                    <w:ind w:left="360" w:hanging="360"/>
                </w:pPr>
                <w:rPr>
                    <w:rFonts w:hint="default"/>
                </w:rPr>
            </w:lvl>
            <w:lvl w:ilvl="1">
                <w:start w:val="1"/>
                <w:numFmt w:val="decimal"/>
                <w:pStyle w:val="ListNumber2"/>
                <w:lvlText w:val="%2)"/>
                <w:lvlJc w:val="left"/>
                <w:pPr>
                    <w:ind w:left="720" w:hanging="360"/>
                </w:pPr>
                <w:rPr>
                    <w:rFonts w:hint="default"/>
                </w:rPr>
            </w:lvl>
            <w:lvl w:ilvl="2">
                <w:start w:val="1"/>
                <w:numFmt w:val="lowerRoman"/>
                <w:pStyle w:val="ListNumber3"/>
                <w:lvlText w:val="%3)"/>
                <w:lvlJc w:val="left"/>
                <w:pPr>
                    <w:ind w:left="1080" w:hanging="360"/>
                </w:pPr>
                <w:rPr>
                    <w:rFonts w:hint="default"/>
                </w:rPr>
            </w:lvl>
            <w:lvl w:ilvl="3">
                <w:start w:val="1"/>
                <w:numFmt w:val="upperRoman"/>
                <w:pStyle w:val="ListNumber4"/>
                <w:lvlText w:val="%4)"/>
                <w:lvlJc w:val="left"/>
                <w:pPr>
                    <w:ind w:left="1440" w:hanging="360"/>
                </w:pPr>
                <w:rPr>
                    <w:rFonts w:hint="default"/>
                </w:rPr>
            </w:lvl>
            <w:lvl w:ilvl="4">
                <w:start w:val="1"/>
                <w:numFmt w:val="lowerLetter"/>
                <w:lvlText w:val="(%5)"/>
                <w:lvlJc w:val="left"/>
                <w:pPr>
                    <w:ind w:left="1800" w:hanging="360"/>
                </w:pPr>
                <w:rPr>
                    <w:rFonts w:hint="default"/>
                </w:rPr>
            </w:lvl>
            <w:lvl w:ilvl="5">
                <w:start w:val="1"/>
                <w:numFmt w:val="lowerRoman"/>
                <w:lvlText w:val="(%6)"/>
                <w:lvlJc w:val="left"/>
                <w:pPr>
                    <w:ind w:left="2160" w:hanging="360"/>
                </w:pPr>
                <w:rPr>
                    <w:rFonts w:hint="default"/>
                </w:rPr>
            </w:lvl>
            <w:lvl w:ilvl="6">
                <w:start w:val="1"/>
                <w:numFmt w:val="decimal"/>
                <w:lvlText w:val="%7."/>
                <w:lvlJc w:val="left"/>
                <w:pPr>
                    <w:ind w:left="2520" w:hanging="360"/>
                </w:pPr>
                <w:rPr>
                    <w:rFonts w:hint="default"/>
                </w:rPr>
            </w:lvl>
            <w:lvl w:ilvl="7">
                <w:start w:val="1"/>
                <w:numFmt w:val="lowerLetter"/>
                <w:lvlText w:val="%8."/>
                <w:lvlJc w:val="left"/>
                <w:pPr>
                    <w:ind w:left="2880" w:hanging="360"/>
                </w:pPr>
                <w:rPr>
                    <w:rFonts w:hint="default"/>
                </w:rPr>
            </w:lvl>
            <w:lvl w:ilvl="8">
                <w:start w:val="1"/>
                <w:numFmt w:val="lowerRoman"/>
                <w:lvlText w:val="%9."/>
                <w:lvlJc w:val="left"/>
                <w:pPr>
                    <w:ind w:left="3240" w:hanging="360"/>
                </w:pPr>
                <w:rPr>
                    <w:rFonts w:hint="default"/>
                </w:rPr>
            </w:lvl>
        </w:abstractNum>
        
    </xsl:template>
    
</xsl:stylesheet>
