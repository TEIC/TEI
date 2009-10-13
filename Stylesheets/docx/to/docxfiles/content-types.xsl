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
    
    <xsl:template name="write-docxfile-content-types">
        <xsl:result-document href="{concat($word-directory,'/%5BContent_Types%5D.xml')}">
            
            <Types xmlns="http://schemas.openxmlformats.org/package/2006/content-types">
                
                <Default Extension="jpeg" ContentType="image/jpeg"/>
                <Default Extension="jpg" ContentType="image/jpeg"/>
                <Default Extension="png" ContentType="image/png"/>
                <Default Extension="tiff" ContentType="image/tiff"/>
                <Default Extension="rels" ContentType="application/vnd.openxmlformats-package.relationships+xml"/>
                <Default Extension="xml" ContentType="application/xml"/>
                <Default Extension="bin" ContentType="application/vnd.openxmlformats-officedocument.oleObject"/>
                <Default Extension="wmf" ContentType="image/x-wmf"/>
                <Default Extension="emf" ContentType="image/x-emf"/>
                
                
                <!-- docprops -->
                <Override PartName="/docProps/core.xml"
                    ContentType="application/vnd.openxmlformats-package.core-properties+xml"/>
                <Override PartName="/docProps/app.xml"
                    ContentType="application/vnd.openxmlformats-officedocument.extended-properties+xml"/>
                <Override PartName="/docProps/custom.xml" 
                    ContentType="application/vnd.openxmlformats-officedocument.custom-properties+xml"/>
                
                <!-- word -->
                <Override PartName="/word/document.xml"
                    ContentType="application/vnd.openxmlformats-officedocument.wordprocessingml.document.main+xml"/>
                <Override PartName="/word/styles.xml"
                    ContentType="application/vnd.openxmlformats-officedocument.wordprocessingml.styles+xml"/>
                <Override PartName="/word/numbering.xml"
                    ContentType="application/vnd.openxmlformats-officedocument.wordprocessingml.numbering+xml"/>
                <Override PartName="/word/webSettings.xml"
                    ContentType="application/vnd.openxmlformats-officedocument.wordprocessingml.webSettings+xml"/>
                
                <Override PartName="/word/endnotes.xml"
                    ContentType="application/vnd.openxmlformats-officedocument.wordprocessingml.endnotes+xml"/>
                <Override PartName="/word/fontTable.xml"
                    ContentType="application/vnd.openxmlformats-officedocument.wordprocessingml.fontTable+xml"/>
                <Override PartName="/word/footnotes.xml"
                    ContentType="application/vnd.openxmlformats-officedocument.wordprocessingml.footnotes+xml"/>
                <Override PartName="/word/settings.xml"
                    ContentType="application/vnd.openxmlformats-officedocument.wordprocessingml.settings+xml"/>
                
                <!-- others -->
                <Override PartName="/word/theme/theme1.xml"
                    ContentType="application/vnd.openxmlformats-officedocument.theme+xml"/>
                
                <!-- headers -->
                <xsl:choose>
                    <xsl:when test="count(key('ALLHEADERS',1))=0">
                        <xsl:for-each select="document($defaultHeaderFooterFile)">
                            <xsl:call-template name="write-docxfile-content-types-header-references"/>
                        </xsl:for-each>
                    </xsl:when>
                    <xsl:otherwise>
                        <xsl:call-template name="write-docxfile-content-types-header-references"/>
                    </xsl:otherwise>
                </xsl:choose>
                
                
                <!-- footers -->
                <xsl:choose>
                    <xsl:when test="count(key('ALLFOOTERS',1))=0">
                        <xsl:for-each select="document($defaultHeaderFooterFile)">
                            <xsl:call-template name="write-docxfile-content-types-footer-references"/>
                        </xsl:for-each>
                    </xsl:when>
                    <xsl:otherwise>
                        <xsl:call-template name="write-docxfile-content-types-footer-references"/>
                    </xsl:otherwise>
                </xsl:choose>
            </Types>
            
        </xsl:result-document>
    </xsl:template>
    
    
    
    <xsl:template name="write-docxfile-content-types-footer-references">
        <xsl:for-each select="key('ALLFOOTERS',1)">
            <Override xmlns="http://schemas.openxmlformats.org/package/2006/content-types"
                ContentType="application/vnd.openxmlformats-officedocument.wordprocessingml.footer+xml">
                <xsl:attribute name="PartName" select="concat('/word/footer', position(), '.xml')"/>
            </Override>            
        </xsl:for-each>
    </xsl:template>
    
    <xsl:template name="write-docxfile-content-types-header-references">
        <xsl:for-each select="key('ALLHEADERS',1)">
            <Override xmlns="http://schemas.openxmlformats.org/package/2006/content-types"
                ContentType="application/vnd.openxmlformats-officedocument.wordprocessingml.header+xml">
                <xsl:attribute name="PartName" select="concat('/word/header', position(), '.xml')"/>
            </Override>
        </xsl:for-each>
    </xsl:template>
    
    
    
</xsl:stylesheet>