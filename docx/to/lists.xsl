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


    <xsl:key name="AN"
	 match="w:abstractNum"
	 use="w:lvl/w:pStyle/@w:val"/>
    <xsl:key name="NUMS"
	     match="w:num[not(w:lvlOverride)]"
	     use="w:abstractNumId/@w:val"/>
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

    <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>Handle value lists</desc>
   </doc>
    <xsl:template match="tei:label[following-sibling::tei:*[1]/self::tei:item]">
        <xsl:param name="nop"/>
        <w:p>
            <w:pPr>
                <w:pStyle w:val="dl"/>
                <w:ind w:left="567" w:hanging="567"/>
            </w:pPr>
            <xsl:apply-templates>
                <xsl:with-param name="nop">true</xsl:with-param>
            </xsl:apply-templates>
            <w:r>
	      <xsl:choose>
		<xsl:when test="$glossListSeparator='tab'">
		  <w:tab/>
		</xsl:when>
		<xsl:when test="$glossListSeparator='break'">
		  <w:br/>
		</xsl:when>
		<xsl:otherwise>
		  <xsl:message terminate="yes">the parameter glossListSeparator can only have values 'break' or 'tab'</xsl:message>
		</xsl:otherwise>
	      </xsl:choose>
            </w:r>
            <xsl:for-each select="following-sibling::tei:item[1]">
	      <xsl:apply-templates>
		<xsl:with-param name="nop">true</xsl:with-param>
	      </xsl:apply-templates>
            </xsl:for-each>
        </w:p>
    </xsl:template>

    <xsl:template match="tei:item//tei:list">
        <xsl:param name="nop"/>
        <xsl:apply-templates>
            <xsl:with-param name="nop">false</xsl:with-param>
        </xsl:apply-templates>
    </xsl:template>

    <xsl:template match="tei:item[preceding-sibling::tei:*[1]/self::tei:label]"/>


    <xsl:template match="comment()"/>
    
    <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>Handle list items</desc>
   </doc>
    <xsl:template match="tei:item">
        <xsl:param name="nop"/>
        <xsl:variable name="listStyle">
            <xsl:choose>
                <xsl:when test="tei:isUnorderedList(..)">
                    <xsl:call-template name="getStyleName">
                        <xsl:with-param name="in">
                            <xsl:text>List Continue</xsl:text>
                            <xsl:call-template name="listNumberDepth"/>
                        </xsl:with-param>
                    </xsl:call-template>
                </xsl:when>
                <xsl:when test="tei:isOrderedList(..)">
                    <xsl:call-template name="getStyleName">
                        <xsl:with-param name="in">
                            <xsl:text>List Number</xsl:text>
                            <xsl:call-template name="listNumberDepth"/>
                        </xsl:with-param>
                    </xsl:call-template>
                </xsl:when>
                <xsl:when test="../@type='termlist'"/>
                <xsl:otherwise>
                    <xsl:text>ListParagraph</xsl:text>
                </xsl:otherwise>
            </xsl:choose>
        </xsl:variable>
        <xsl:variable name="pPr">
            <w:pPr>
                <xsl:if test="string-length($listStyle) &gt; 1">
                    <w:pStyle w:val="{$listStyle}"/>
                </xsl:if>
                <xsl:choose>
                    <xsl:when test="tei:isUnorderedList(..)">
                        <w:numPr>
                            <w:ilvl>
                                <xsl:attribute name="w:val">
                                    <xsl:value-of select="count(ancestor::tei:list) - 1"/>
                                </xsl:attribute>
                            </w:ilvl>
			    <w:numId>
			      <xsl:attribute name="w:val">
				<xsl:choose>
				  <xsl:when
				      test="$isofreestanding='true'">
				    <xsl:text>2</xsl:text>
				  </xsl:when>
				  <xsl:otherwise>
				    <xsl:for-each select="document($numberDoc)">
				      <xsl:variable name="abstractNumId"
						    select="key('AN',$listStyle)/@w:abstractNumId"/>
				      <xsl:value-of select="key('NUMS',$abstractNumId)/@w:numId"/>
				    </xsl:for-each>
				  </xsl:otherwise>
				</xsl:choose>
			      </xsl:attribute>
			    </w:numId>
			</w:numPr>
                    </xsl:when>
                    <xsl:when test="tei:isOrderedList(..)">
                        <w:numPr>
                            <w:ilvl>
                                <xsl:attribute name="w:val">
                                    <xsl:value-of select="count(ancestor::tei:list) - 1"/>
                                </xsl:attribute>
                            </w:ilvl>
                            <w:numId>
			      <xsl:attribute name="w:val">
				<!-- @see template: numbering-definition ordered lists -->
				<xsl:variable name="CurrentList">
				  <xsl:value-of select="generate-id(..)"/>
				</xsl:variable>
				<xsl:for-each select="key('OL',1)">
				  <xsl:if test="$CurrentList=generate-id(.)">
				    <xsl:value-of select="position()+100"/>
				  </xsl:if>
				</xsl:for-each>
			      </xsl:attribute>
                            </w:numId>
                        </w:numPr>
                    </xsl:when>
                </xsl:choose>
            </w:pPr>
        </xsl:variable>
	<!--
	    <xsl:message>List item <xsl:value-of select="."/>, <xsl:value-of
	    select="$nop"/>, <xsl:value-of select="$listStyle"/></xsl:message>
	-->
        <xsl:call-template name="block-element">
            <xsl:with-param name="pPr" select="$pPr"/>
            <xsl:with-param name="nop" select="$nop"/>
        </xsl:call-template>
    </xsl:template>

    <xsl:template name="listNumberDepth">
        <xsl:choose>
            <xsl:when test="ancestor::tei:glossListEntry">
                <xsl:value-of select="count(ancestor::tei:list)                 + ancestor::tei:glossListEntry/@count"/>
            </xsl:when>
            <xsl:when test="parent::tei:list/ancestor::tei:list">
                <xsl:text> </xsl:text>
                <xsl:value-of select="count(ancestor::tei:list)"/>
            </xsl:when>
        </xsl:choose>
    </xsl:template>

</xsl:stylesheet>
