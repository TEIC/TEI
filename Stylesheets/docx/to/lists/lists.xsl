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

    <xsl:import href="../placeholders.xsl"/>

    <xsl:key name="AN"
	 match="w:abstractNum"
	 use="w:lvl/w:pStyle/@w:val"/>
    <xsl:key name="NUMS"
	     match="w:num[not(w:lvlOverride)]"
	     use="w:abstractNumId/@w:val"/>
    <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" scope="stylesheet" type="stylesheet">
      <desc>
         <p> TEI stylesheet for making Word docx files from TEI XML </p>
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
      </desc>
   </doc>

    <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>
        Handle value lists
    </desc>
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
                <w:tab/>
            </w:r>
            <xsl:for-each select="following-sibling::tei:item[1]">
                <xsl:apply-templates>
                    <xsl:with-param name="nop">true</xsl:with-param>
                </xsl:apply-templates>
            </xsl:for-each>
        </w:p>
    </xsl:template>

    <xsl:template match="tei:item/tei:list">
        <xsl:param name="nop"/>
        <xsl:apply-templates>
            <xsl:with-param name="nop">false</xsl:with-param>
        </xsl:apply-templates>
    </xsl:template>

    <xsl:template match="tei:item[preceding-sibling::tei:*[1]/self::tei:label]"/>


    <xsl:template match="comment()"/>
    
    <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc> 
        Handle list items
    </desc>
   </doc>
    <xsl:template match="tei:item">
        <xsl:param name="nop"/>


        <xsl:variable name="listStyle">
            <xsl:choose>
                <xsl:when test="../@type='unordered' or ../@type='simple' or not(../@type)">
                    <xsl:call-template name="getStyleName">
                        <xsl:with-param name="in">
                            <xsl:text>List Continue</xsl:text>
                            <xsl:call-template name="listNumberDepth"/>
                        </xsl:with-param>
                    </xsl:call-template>
                </xsl:when>
                <xsl:when test="../@type='ordered'">
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
                    <xsl:when test="../@type='unordered' or not(../@type)">
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
				    <xsl:for-each select="document(concat($word-directory,'/word/numbering.xml'))">
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
                    <xsl:when test="../@type='ordered'">
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
