<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet xmlns:xs="http://www.w3.org/2001/XMLSchema" xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xmlns:prop="http://schemas.openxmlformats.org/officeDocument/2006/custom-properties" xmlns:a="http://schemas.openxmlformats.org/drawingml/2006/main" xmlns:cp="http://schemas.openxmlformats.org/package/2006/metadata/core-properties" xmlns:dc="http://purl.org/dc/elements/1.1/" xmlns:dcterms="http://purl.org/dc/terms/" xmlns:dcmitype="http://purl.org/dc/dcmitype/" xmlns:iso="http://www.iso.org/ns/1.0" xmlns:m="http://schemas.openxmlformats.org/officeDocument/2006/math" xmlns:mml="http://www.w3.org/1998/Math/MathML" xmlns:mo="http://schemas.microsoft.com/office/mac/office/2008/main" xmlns:mv="urn:schemas-microsoft-com:mac:vml" xmlns:o="urn:schemas-microsoft-com:office:office" xmlns:pic="http://schemas.openxmlformats.org/drawingml/2006/picture" xmlns:r="http://schemas.openxmlformats.org/officeDocument/2006/relationships" xmlns:rel="http://schemas.openxmlformats.org/package/2006/relationships" xmlns:tbx="http://www.lisa.org/TBX-Specification.33.0.html" xmlns:tei="http://www.tei-c.org/ns/1.0" xmlns:teidocx="http://www.tei-c.org/ns/teidocx/1.0" xmlns:v="urn:schemas-microsoft-com:vml" xmlns:ve="http://schemas.openxmlformats.org/markup-compatibility/2006" xmlns:w10="urn:schemas-microsoft-com:office:word" xmlns:w="http://schemas.openxmlformats.org/wordprocessingml/2006/main" xmlns:wne="http://schemas.microsoft.com/office/word/2006/wordml" xmlns:wp="http://schemas.openxmlformats.org/drawingml/2006/wordprocessingDrawing" xmlns="http://www.tei-c.org/ns/1.0" version="2.0" exclude-result-prefixes="a cp dc dcterms dcmitype prop     iso m mml mo mv o pic r rel     tbx tei teidocx v xs ve w10 w wne wp">
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" scope="stylesheet" type="stylesheet">
    <desc>
      <p> TEI stylesheet for converting Word docx files to TEI </p>
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
    <desc>Convert Word drawing objects</desc>
  </doc>
  <xsl:template match="w:drawing">
    <xsl:param name="n" tunnel="yes"/>
    <xsl:choose>
      <xsl:when test="$convertGraphics='true'">
        <xsl:variable name="mediaUrl"	  select="descendant::pic:cNvPr/@descr"/>
        <xsl:variable name="rid" select="descendant::a:blip[1]/@r:embed"/>
        <xsl:variable name="target" select="document(concat($wordDirectory,'/word/_rels/document.xml.rels'))//rel:Relationship[@Id=$rid]/@Target"/>
        <xsl:choose>
	  <xsl:when test="matches($target,'\.emf$') or   matches($target,'\.wmf$')">
            <xsl:sequence select="tei:docxError(concat('unable to handle  picture of Windows format ',$target))"/>
	  </xsl:when>
          <xsl:when test="descendant::a:blip[1]/@r:embed">
            <graphic>
              <xsl:variable name="c">
                <xsl:choose>
                  <xsl:when test="ancestor::w:tbl">
                    <xsl:number level="any" from="w:tbl"/>
                  </xsl:when>
                  <xsl:otherwise>
                    <xsl:number level="any"/>
                  </xsl:otherwise>
                </xsl:choose>
              </xsl:variable>
              <xsl:attribute name="n">
                <xsl:choose>
                  <xsl:when test="number($n)">
                    <xsl:value-of select="($n * 100) + $c"/>
                  </xsl:when>
                  <xsl:otherwise>
                    <xsl:text>100</xsl:text>
                    <xsl:number level="any"/>
                  </xsl:otherwise>
                </xsl:choose>
              </xsl:attribute>
              <xsl:attribute name="width" select="concat(number(descendant::wp:extent[1]/@cx) div 360000,'cm')"/>
              <xsl:attribute name="height" select="concat(number(descendant::wp:extent[1]/@cy) div 360000,'cm')"/>
              <xsl:choose>
                <xsl:when test="starts-with($mediaUrl,'movie::' )">
                  <xsl:attribute name="url">
                    <!-- The proper way to do it would be to extract the name from this
						      <xsl:value-of select="substring-after($mediaUrl, 'movie::')"/>
						-->
                    <xsl:value-of select="descendant::pic:cNvPr/@name"/>
                  </xsl:attribute>
                  <xsl:attribute name="mimeType">video/x-something</xsl:attribute>
                </xsl:when>
                <xsl:otherwise>
                  <xsl:attribute name="url" select="$target"/>
                </xsl:otherwise>
              </xsl:choose>
              <!-- inline or block -->
              <xsl:attribute name="rend">
                <xsl:choose>
                  <xsl:when test="wp:anchor">block</xsl:when>
                  <xsl:otherwise>inline</xsl:otherwise>
                </xsl:choose>
              </xsl:attribute>
              <xsl:if test=".//wp:docPr/@descr">
                <xsl:element name="desc">
                  <xsl:value-of select=".//wp:docPr/@descr"/>
                </xsl:element>
              </xsl:if>
            </graphic>
          </xsl:when>
            <xsl:when test="@r:link">
              <xsl:variable name="rid" select="@r:link"/>
	      <graphic
		  url="{document(concat($wordDirectory,'/word/_rels/document.xml.rels'))//rel:Relationship[@Id=$rid]/@Target}">
		<xsl:attribute name="rend">
                  <xsl:choose>
                    <xsl:when test="wp:anchor">block</xsl:when>
                    <xsl:otherwise>inline</xsl:otherwise>
                  </xsl:choose>
		</xsl:attribute>
		<xsl:if test=".//wp:docPr/@descr">
                  <xsl:element name="desc">
                    <xsl:value-of select=".//wp:docPr/@descr"/>
                  </xsl:element>
		</xsl:if>
	      </graphic>
            </xsl:when>
            <xsl:otherwise>
              <xsl:sequence select="tei:docxError('unable to handle picture here, no embed or link')"/>
            </xsl:otherwise>
        </xsl:choose>
      </xsl:when>
      <xsl:otherwise>
        <xsl:sequence select="tei:docxError('unable to handle image')"/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc>Guides the identity transformation of blips</desc>
  </doc>
  <xsl:template match="a:blip" mode="iden">
    <a:blip>
      <xsl:choose>
        <xsl:when test="@r:embed">
          <xsl:variable name="rid" select="@r:embed"/>
          <xsl:attribute name="r:embed">
            <xsl:value-of select="document(concat($wordDirectory,'/word/_rels/document.xml.rels'))//rel:Relationship[@Id=$rid]/@Target"/>
          </xsl:attribute>
        </xsl:when>
        <xsl:otherwise>
          <xsl:variable name="rid" select="@r:link"/>
          <xsl:attribute name="r:link">
            <xsl:value-of select="document(concat($wordDirectory,'/word/_rels/document.xml.rels'))//rel:Relationship[@Id=$rid]/@Target"/>
          </xsl:attribute>
        </xsl:otherwise>
      </xsl:choose>
    </a:blip>
  </xsl:template>
</xsl:stylesheet>
