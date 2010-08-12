<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                xmlns:tei="http://www.tei-c.org/ns/1.0"
                xmlns:ve="http://schemas.openxmlformats.org/markup-compatibility/2006"
                xmlns:o="urn:schemas-microsoft-com:office:office"
                xmlns:r="http://schemas.openxmlformats.org/officeDocument/2006/relationships"
                xmlns:m="http://schemas.openxmlformats.org/officeDocument/2006/math"
                xmlns:v="urn:schemas-microsoft-com:vml"
                xmlns:wp="http://schemas.openxmlformats.org/drawingml/2006/wordprocessingDrawing"
                xmlns:w10="urn:schemas-microsoft-com:office:word"
                xmlns:w="http://schemas.openxmlformats.org/wordprocessingml/2006/main"
                xmlns:wne="http://schemas.microsoft.com/office/word/2006/wordml"
                version="1.0">
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" scope="stylesheet" type="stylesheet">
    <desc>
      <p> TEI stylesheet for simplifying TEI ODD markup </p>
      <p> This library is free software; you can redistribute it and/or modify it under the
      terms of the GNU Lesser General Public License as published by the Free Software Foundation;
      either version 2.1 of the License, or (at your option) any later version. This library is
      distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the
      implied warranty of MAINTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser
      General Public License for more details. You should have received a copy of the GNU Lesser
      General Public License along with this library; if not, write to the Free Software Foundation,
      Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA </p>
      <p>Author: See AUTHORS</p>
      <p>Id: $Id$</p>
      <p>Copyright: 2008, TEI Consortium</p>
    </desc>
  </doc>
    <xsl:output method="xml" version="1.0" encoding="UTF-8" indent="yes"/>
    <!-- create root element -->

    <xsl:template match="/">
        <xsl:apply-templates/>
    </xsl:template>

    <xsl:template match="w:hdr">
        <xsl:element name="hdr">
            <xsl:attribute name="document_type">
                <xsl:value-of select="//w:t[preceding-sibling::w:rPr/w:rStyle/@w:val='HeaderDocType']"/>
            </xsl:attribute>
            <xsl:attribute name="document_id_prefix">
                <xsl:value-of select="//w:t[preceding-sibling::w:rPr/w:rStyle/@w:val='HeaderDocIDPrefix']"/>
            </xsl:attribute>
            <xsl:attribute name="document_id">
                <xsl:value-of select="//w:t[preceding-sibling::w:rPr/w:rStyle/@w:val='HeaderDocID']"/>
            </xsl:attribute>
        </xsl:element>
    </xsl:template>

 
</xsl:stylesheet>