<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet version="2.0" xmlns:xs="http://www.w3.org/2001/XMLSchema"
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    xmlns:prop="http://schemas.openxmlformats.org/officeDocument/2006/custom-properties"
    xmlns:a="http://schemas.openxmlformats.org/drawingml/2006/main"
    xmlns:cp="http://schemas.openxmlformats.org/package/2006/metadata/core-properties"
    xmlns:dc="http://purl.org/dc/elements/1.1/" xmlns:dcterms="http://purl.org/dc/terms/"
    xmlns:dcmitype="http://purl.org/dc/dcmitype/" xmlns:iso="http://www.iso.org/ns/1.0"
    xmlns:m="http://schemas.openxmlformats.org/officeDocument/2006/math"
    xmlns:mml="http://www.w3.org/1998/Math/MathML"
    xmlns:mo="http://schemas.microsoft.com/office/mac/office/2008/main"
    xmlns:mv="urn:schemas-microsoft-com:mac:vml" xmlns:o="urn:schemas-microsoft-com:office:office"
    xmlns:pic="http://schemas.openxmlformats.org/drawingml/2006/picture"
    xmlns:r="http://schemas.openxmlformats.org/officeDocument/2006/relationships"
    xmlns:rel="http://schemas.openxmlformats.org/package/2006/relationships"
    xmlns:tbx="http://www.lisa.org/TBX-Specification.33.0.html"
    xmlns:tei="http://www.tei-c.org/ns/1.0" xmlns:teidocx="http://www.tei-c.org/ns/teidocx/1.0"
    xmlns:v="urn:schemas-microsoft-com:vml"
    xmlns:ve="http://schemas.openxmlformats.org/markup-compatibility/2006"
    xmlns:w10="urn:schemas-microsoft-com:office:word"
    xmlns:w="http://schemas.openxmlformats.org/wordprocessingml/2006/main"
    xmlns:wne="http://schemas.microsoft.com/office/word/2006/wordml"
    xmlns:wp="http://schemas.openxmlformats.org/drawingml/2006/wordprocessingDrawing"
    xmlns:xd="http://www.pnp-software.com/XSLTdoc" xmlns="http://www.tei-c.org/ns/1.0"
    exclude-result-prefixes="a cp dc dcterms dcmitype prop
    iso m mml mo mv o pic r rel
    tbx tei teidocx v xs ve w10 w wne wp xd">
    
    <xsl:import href="../parameters.xsl"/>
    
    <xd:doc type="stylesheet">
        <xd:short> TEI stylesheet for converting Word docx files to TEI </xd:short>
        <xd:detail> This library is free software; you can redistribute it and/or modify it under
            the terms of the GNU Lesser General Public License as published by the Free Software
            Foundation; either version 2.1 of the License, or (at your option) any later version.
            This library is distributed in the hope that it will be useful, but WITHOUT ANY
            WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A
            PARTICULAR PURPOSE. See the GNU Lesser General Public License for more details. You
            should have received a copy of the GNU Lesser General Public License along with this
            library; if not, write to the Free Software Foundation, Inc., 59 Temple Place, Suite
            330, Boston, MA 02111-1307 USA </xd:detail>
        <xd:author>See AUTHORS</xd:author>
        <xd:cvsId>$Id: from.xsl 6832 2009-10-12 22:42:59Z rahtz $</xd:cvsId>
        <xd:copyright>2008, TEI Consortium</xd:copyright>
    </xd:doc>
    
    <xd:doc>
        <xd:short></xd:short>
    </xd:doc>
    <xsl:template match="w:drawing">
        <xsl:choose>
            <xsl:when test="$convert-graphics='true'">
                <xsl:choose>
                    <xsl:when test="descendant::a:blip[1]/@r:embed">
                        <graphic>
                            <xsl:attribute name="width"
                                select="concat(number(descendant::wp:extent[1]/@cx) div 360000,'cm')"/>
                            <xsl:attribute name="height"
                                select="concat(number(descendant::wp:extent[1]/@cy) div 360000,'cm')"/>
                            <xsl:attribute name="url">
                                <xsl:variable name="rid" select="descendant::a:blip[1]/@r:embed"/>
                                <xsl:value-of
                                    select="document(concat($word-directory,'/word/_rels/document.xml.rels'))//rel:Relationship[@Id=$rid]/@Target"
                                />
                            </xsl:attribute>
                            
                            <!-- inline or block -->
                            <xsl:attribute name="rend">
                                <xsl:choose>
                                    <xsl:when test="wp:anchor">block</xsl:when>
                                    <xsl:otherwise>inline</xsl:otherwise>
                                </xsl:choose>
                            </xsl:attribute>
                            
                        </graphic>
                    </xsl:when>
                    <xsl:otherwise>
                        <graphic> Linked Graphic: <xsl:variable name="rid" select="@r:link"/>
                            <xsl:value-of
                                select="document(concat($word-directory,'/word/_rels/document.xml.rels'))//rel:Relationship[@Id=$rid]/@Target"
                            />
                        </graphic>
                    </xsl:otherwise>
                </xsl:choose>
            </xsl:when>
            <xsl:otherwise>
                <w:drawing>
                    <xsl:apply-templates mode="iden"/>
                </w:drawing>
            </xsl:otherwise>
        </xsl:choose>
    </xsl:template>
    
    <xd:doc>
        <xd:short>Guides the identity transformation of blips</xd:short>
    </xd:doc>
    <xsl:template match="a:blip" mode="iden">
        <a:blip>
            <xsl:choose>
                <xsl:when test="@r:embed">
                    <xsl:variable name="rid" select="@r:embed"/>
                    <xsl:attribute name="r:embed">
                        <xsl:value-of
                            select="document(concat($word-directory,'/word/_rels/document.xml.rels'))//rel:Relationship[@Id=$rid]/@Target"
                        />
                    </xsl:attribute>
                </xsl:when>
                <xsl:otherwise>
                    <xsl:variable name="rid" select="@r:link"/>
                    <xsl:attribute name="r:link">
                        <xsl:value-of
                            select="document(concat($word-directory,'/word/_rels/document.xml.rels'))//rel:Relationship[@Id=$rid]/@Target"
                        />
                    </xsl:attribute>
                </xsl:otherwise>
            </xsl:choose>
            
        </a:blip>
    </xsl:template>
    
</xsl:stylesheet>