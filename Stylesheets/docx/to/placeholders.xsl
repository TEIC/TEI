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
         <p>Template used to process block elements</p>
         <p>
            Template used to process block elements:
            
            Is implemented by the main stylesheet and should usually not be overridden.
        </p>
      </desc>
   </doc>
    <xsl:template name="block-element">
        <xsl:param name="style"/>
        <xsl:param name="select" select="."/>
        <xsl:param name="pPr"/>
        <xsl:param name="nop"/>
        <xsl:param name="bookmark-id"/>
        <xsl:param name="bookmark-name"/>
    </xsl:template>
    
    <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>
         <p>to a given style name, this template returns the correct style id
        looking it up in styles.xml</p>
         <p>
            
            The template is implemented by the main stylesheet.
        </p>
      </desc>
   </doc>
    <xsl:template name="getStyleName">
        <xsl:param name="in"/>
    </xsl:template>
    
    <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>
        A callback for any titlepages that belong to the front matter.
    </desc>
   </doc>
    <xsl:template name="titlepages">
    </xsl:template>
    
    <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>
        "Returns" the document's title (as plain string).
    </desc>
   </doc>
    <xsl:template name="generateTitle">
        Undefined Document
    </xsl:template>
    
    
    <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>
        
    </desc>
   </doc>
    <xsl:template name="document-title">
        <xsl:choose>
            <xsl:when test="/tei:TEI/tei:text/tei:front/tei:titlePage"> </xsl:when>
            <xsl:when
		test="/tei:TEI/tei:teiHeader/tei:fileDesc/tei:titleStmt/tei:title[@type='main']">
                <xsl:for-each select="/tei:TEI/tei:teiHeader/tei:fileDesc/tei:titleStmt/tei:title[@type='main']">
                    <xsl:call-template name="block-element">
                        <xsl:with-param name="style">Title</xsl:with-param>
                    </xsl:call-template>
                </xsl:for-each>
                <xsl:for-each select="/tei:TEI/tei:teiHeader/tei:fileDesc/tei:titleStmt/tei:title[@type='main']">
                    <xsl:call-template name="block-element">
                        <xsl:with-param name="style">Title</xsl:with-param>
                    </xsl:call-template>
                </xsl:for-each>
                <xsl:for-each select="/tei:TEI/tei:teiHeader/tei:fileDesc/tei:titleStmt/tei:title[@type='sub']">
                    <xsl:call-template name="block-element">
                        <xsl:with-param name="style">Subtitle</xsl:with-param>
                    </xsl:call-template>
                </xsl:for-each>
	    </xsl:when>
	    <xsl:otherwise>
                <xsl:for-each select="/tei:TEI/tei:teiHeader/tei:fileDesc/tei:titleStmt/tei:title">
                    <xsl:call-template name="block-element">
                        <xsl:with-param name="style">Title</xsl:with-param>
                    </xsl:call-template>
                </xsl:for-each>
	    </xsl:otherwise>  
        </xsl:choose>
    </xsl:template>
    
    
    <xsl:template name="created-by"/>
    <xsl:template name="headerParts"/>
</xsl:stylesheet>