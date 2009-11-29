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
                xmlns:its="http://www.w3.org/2005/11/its"
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
                exclude-result-prefixes="cp ve o r m v wp w10 w wne mml tbx iso its     tei a xs pic fn xsi dc dcterms dcmitype     contypes teidocx teix html cals">
    
    
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
         <p>Id: $Id: to.xsl 6832 2009-10-12 22:42:59Z rahtz $</p>
         <p>Copyright: 2008, TEI Consortium</p>
      </desc>
   </doc>
    
    
    <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>
        Dealing with sections
    </desc>
   </doc>
    <xsl:template match="tei:milestone">
        <xsl:param name="final-section">false</xsl:param>
        
        
        <!-- construct sectPr -->
        <xsl:variable name="sectPr">
            <w:sectPr>
                <xsl:for-each select="teidocx:footer">
		             <xsl:variable name="ref" select="@ref"/>
		             <xsl:if test="count(key('FOOTERS',$ref))&gt;0">
		                <xsl:variable name="footernum">
                        <xsl:for-each select="key('FOOTERS',$ref)">
			                     <xsl:number level="any"/>
			                  </xsl:for-each>
		                </xsl:variable>
		                <w:footerReference w:type="{@type}" r:id="{concat('rId',100+$footernum)}"/>
		             </xsl:if>
                </xsl:for-each>
                
                <xsl:for-each select="teidocx:header">
                    <xsl:variable name="ref" select="@ref"/>
		             <xsl:if test="count(key('HEADERS',$ref))&gt;0">
                    <xsl:variable name="headernum">
                        <xsl:for-each select="key('HEADERS',$ref)">
                            <xsl:number level="any"/>
                        </xsl:for-each>
                    </xsl:variable>
                    <w:headerReference w:type="{@type}" r:id="{concat('rId',100+$headernum)}"/>
		             </xsl:if>
                </xsl:for-each>
                
                <w:pgSz>
                    <xsl:choose>
                        <!-- landscape -->
                        <xsl:when test="teidocx:orientation/@type='landscape'">
                            <xsl:attribute name="w:orient">landscape</xsl:attribute>
                            <xsl:attribute name="w:w">15840</xsl:attribute>
                            <xsl:attribute name="w:h">12240</xsl:attribute>
                        </xsl:when>
                        <!-- portrait -->
                        <xsl:otherwise>
                            <xsl:attribute name="w:w">12240</xsl:attribute>
                            <xsl:attribute name="w:h">15840</xsl:attribute>
                        </xsl:otherwise>
                    </xsl:choose>
                </w:pgSz>
                <w:pgMar w:top="1440" w:right="1440" w:bottom="1440" w:left="1440" w:gutter="0"
                     w:footer="720"
                     w:header="720"/>
                <xsl:if test="teidocx:pageNumbering">
                    <w:pgNumType>
                        <xsl:if test="teidocx:pageNumbering/@start">
                            <xsl:attribute name="w:start" select="teidocx:pageNumbering/@start"/>
                        </xsl:if>
                        <xsl:if test="teidocx:pageNumbering/@type">
                            <xsl:attribute name="w:fmt" select="teidocx:pageNumbering/@type"/>
                        </xsl:if>
                    </w:pgNumType>
                </xsl:if>
                <xsl:if test="teidocx:header/@type='first' or teidocx:footer/@type='first'">
                    <w:titlePg/>
                </xsl:if>
                <w:docGrid w:linePitch="360"/>
            </w:sectPr>
        </xsl:variable>
        
        <!-- write out sectPr -->
        <xsl:choose>
            <xsl:when test="$final-section='false'">
                <w:p>
                    <w:pPr>
                        <xsl:copy-of select="$sectPr"/>
                    </w:pPr>
                </w:p>
            </xsl:when>
            <xsl:otherwise>
                <xsl:copy-of select="$sectPr"/>
            </xsl:otherwise>
        </xsl:choose>
    </xsl:template>
    
    
</xsl:stylesheet>