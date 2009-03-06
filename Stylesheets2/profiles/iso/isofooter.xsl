<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    xmlns:tei="http://www.tei-c.org/ns/1.0" version="1.0"
    xmlns:ve="http://schemas.openxmlformats.org/markup-compatibility/2006"
    xmlns:o="urn:schemas-microsoft-com:office:office"
    xmlns:r="http://schemas.openxmlformats.org/officeDocument/2006/relationships"
    xmlns:m="http://schemas.openxmlformats.org/officeDocument/2006/math"
    xmlns:v="urn:schemas-microsoft-com:vml"
    xmlns:wp="http://schemas.openxmlformats.org/drawingml/2006/wordprocessingDrawing"
    xmlns:w10="urn:schemas-microsoft-com:office:word"
    xmlns:w="http://schemas.openxmlformats.org/wordprocessingml/2006/main"
    xmlns:wne="http://schemas.microsoft.com/office/word/2006/wordml">
    <xsl:output method="xml" version="1.0" encoding="UTF-8" indent="yes"/>
    <!-- create root element -->

    <xsl:template match="/">
        <xsl:apply-templates/>
    </xsl:template>

    <xsl:template match="w:ftr">
        <xsl:element name="ftr">
            <xsl:attribute name="copyright_notice">
                <xsl:apply-templates select="//w:p[w:pPr/w:pStyle/@w:val='CopyrightNotice']"/>
           </xsl:attribute>
        </xsl:element>
    </xsl:template>

 <xsl:template match="w:p">
     <xsl:apply-templates select="w:r"/>
 </xsl:template>
    
    <xsl:template match="w:r">
        <xsl:for-each select="w:t">
            <xsl:value-of select="."/>
        </xsl:for-each>
    </xsl:template>
    
</xsl:stylesheet>
