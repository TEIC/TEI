<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet 		
    version="2.0" 
    xmlns:a="http://schemas.openxmlformats.org/drawingml/2006/main"
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
    xmlns:v="urn:schemas-microsoft-com:vml" xmlns:fn="http://www.w3.org/2005/02/xpath-functions"
    xmlns:ve="http://schemas.openxmlformats.org/markup-compatibility/2006"
    xmlns:w10="urn:schemas-microsoft-com:office:word"
    xmlns:w="http://schemas.openxmlformats.org/wordprocessingml/2006/main"
    xmlns:wne="http://schemas.microsoft.com/office/word/2006/wordml"
    xmlns:wp="http://schemas.openxmlformats.org/drawingml/2006/wordprocessingDrawing"
    xmlns:xd="http://www.pnp-software.com/XSLTdoc"
    xmlns:xs="http://www.w3.org/2001/XMLSchema"
    xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    exclude-result-prefixes="cp ve o r m v wp w10 w wne mml tbx iso its
    tei a xs pic fn xsi dc dcterms dcmitype
    contypes teidocx teix html cals xd">
    
    <xsl:import href="../parameters.xsl"/>
    
    <xd:doc type="stylesheet">
        <xd:short> TEI stylesheet for making Word docx files from TEI XML </xd:short>
        <xd:detail> This library is free software; you can redistribute it and/or
            modify it under the terms of the GNU Lesser General Public License as
            published by the Free Software Foundation; either version 2.1 of the
            License, or (at your option) any later version. This library is
            distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY;
            without even the implied warranty of MERCHANTABILITY or FITNESS FOR A
            PARTICULAR PURPOSE. See the GNU Lesser General Public License for more
            details. You should have received a copy of the GNU Lesser General Public
            License along with this library; if not, write to the Free Software
            Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA </xd:detail>
        <xd:author>See AUTHORS</xd:author>
        <xd:cvsId>$Id: to.xsl 6832 2009-10-12 22:42:59Z rahtz $</xd:cvsId>
        <xd:copyright>2008, TEI Consortium</xd:copyright>
    </xd:doc>
    
    
    <xd:doc>
        <xd:short>Write _rels/.rels</xd:short>
    </xd:doc>
    <xsl:template name="write-docxfile-main-relationships">
        <xsl:result-document href="{concat($word-directory,'/_rels/.rels')}"
            standalone="yes">
            <Relationships xmlns="http://schemas.openxmlformats.org/package/2006/relationships">
                <Relationship Id="rId1"
                    Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/officeDocument"
                    Target="word/document.xml"/>
                <Relationship Id="rId2"
                    Type="http://schemas.openxmlformats.org/package/2006/relationships/metadata/core-properties"
                    Target="docProps/core.xml"/>
                <Relationship Id="rId3"
                    Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/extended-properties"
                    Target="docProps/app.xml"/>
                <Relationship Id="rId4"
                    Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/custom-properties" 
                    Target="docProps/custom.xml"/>
            </Relationships>
        </xsl:result-document>
    </xsl:template>
    
    
    <xd:doc>
        <xd:short>Write word/_rels/document.xml.rels</xd:short>
    </xd:doc>
    <xsl:template name="write-docxfile-relationships">
        <xsl:result-document href="{concat($word-directory,'/word/_rels/document.xml.rels')}"
            standalone="yes">
            <Relationships xmlns="http://schemas.openxmlformats.org/package/2006/relationships">
                
                <!-- Mandatory Relationships -->
                <Relationship Id="rId3"
                    Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/numbering"
                    Target="numbering.xml"/>
                <Relationship Id="rId4"
                    Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/styles"
                    Target="styles.xml"/>
                <Relationship Id="rId5"
                    Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/settings"
                    Target="settings.xml"/>
                <Relationship Id="rId7"
                    Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/footnotes"
                    Target="footnotes.xml"/>
                <Relationship Id="rId8"
                    Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/endnotes"
                    Target="endnotes.xml"/>
                <!-- odd stuff -->
                <Relationship Id="rId18"
                    Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/fontTable"
                    Target="fontTable.xml"/>
                
                <!-- Images -->
                
                <xsl:for-each select="key('BLIP',1)">
                    <xsl:choose>
                        <xsl:when test="@r:embed">
                            <Relationship Id="rId{position() + 200}"
                                Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/image"
                                Target="{@r:embed}"/>
                        </xsl:when>
                        <xsl:otherwise>
                            <Relationship Id="rId{position() + 200}"
                                Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/image"
                                Target="{@r:link}" TargetMode="External"/>
                        </xsl:otherwise>
                    </xsl:choose>
                </xsl:for-each>
                
                <!-- hyperlinks -->
                <!--
                    <xsl:for-each select="//tei:ptr">
                    <Relationship 
                    Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/hyperlink"
                    Target="{@target}" 
                    TargetMode="External">
                    <xsl:message>H <xsl:value-of select="@target"/></xsl:message>
                    <xsl:attribute name="Id">
                    <xsl:text>rId</xsl:text>
                    <xsl:variable name="n">
                    <xsl:number level="any"/>
                    </xsl:variable>
                    <xsl:value-of select="$n + 3000"/>
                    </xsl:attribute>
                    </Relationship>
                    </xsl:for-each>
                -->
                <xsl:for-each select="//tei:graphic[@url]">
                    <Relationship Id="rId{position() + 300}"
                        Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/image"
                        Target="{@url}"/>
                </xsl:for-each>
                
                <!-- Formulas -->
                <xsl:for-each select="//v:imagedata">
                    <Relationship Id="rId{position() + 1000}"
                        Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/image"
                        Target="{@r:id}"/>
                </xsl:for-each>
                
                <xsl:for-each select="//o:OLEObject">
                    <Relationship Id="rId{position() + 2000}"
                        Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/oleObject"
                        Target="{@r:id}"/>
                </xsl:for-each>
                
                
                
                <!-- our headers and footers -->
                <xsl:choose>
                    <xsl:when test="count(key('ALLHEADERS',1))=0 and count(key('ALLFOOTERS',1))=0">
                        <xsl:for-each select="document($defaultHeaderFooterFile)">
                            <xsl:call-template name="writeRelationshipsHeadersAndFooters"/>
                        </xsl:for-each>
                    </xsl:when>
                    <xsl:otherwise>
                        <xsl:call-template name="writeRelationshipsHeadersAndFooters"/>
                    </xsl:otherwise>
                </xsl:choose>
            </Relationships>
        </xsl:result-document>
        
    </xsl:template>
    
    <xsl:template name="writeRelationshipsHeadersAndFooters">
        <xsl:for-each select="key('ALLFOOTERS',1)">
            <xsl:variable name="num">
                <xsl:number/>
            </xsl:variable>
            <Relationship xmlns="http://schemas.openxmlformats.org/package/2006/relationships"
                Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/footer"
                Target="{concat('footer',position(),'.xml')}"
                Id="{concat('rId',100+$num)}"/>
        </xsl:for-each>
        
        <!-- count all footers -->
        <xsl:for-each select="key('ALLHEADERS',1)">
            <xsl:variable name="num">
                <xsl:number/>
            </xsl:variable>
            <Relationship xmlns="http://schemas.openxmlformats.org/package/2006/relationships"
                Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/header"
                Target="{concat('header',position(),'.xml')}"
                Id="{concat('rId',100+$num)}"
            />
        </xsl:for-each>
    </xsl:template>
    
</xsl:stylesheet>