<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xmlns="http://www.tei-c.org/ns/1.0" xmlns:tei="http://www.tei-c.org/ns/1.0" xmlns:teix="http://www.tei-c.org/ns/Examples" xmlns:iso="http://www.iso.org/ns/1.0" xmlns:xs="http://www.w3.org/2001/XMLSchema" xmlns:ve="http://schemas.openxmlformats.org/markup-compatibility/2006" xmlns:o="urn:schemas-microsoft-com:office:office" xmlns:r="http://schemas.openxmlformats.org/officeDocument/2006/relationships" xmlns:m="http://schemas.openxmlformats.org/officeDocument/2006/math" xmlns:v="urn:schemas-microsoft-com:vml" xmlns:fn="http://www.w3.org/2005/02/xpath-functions" xmlns:wp="http://schemas.openxmlformats.org/drawingml/2006/wordprocessingDrawing" xmlns:a="http://schemas.openxmlformats.org/drawingml/2006/main" xmlns:w10="urn:schemas-microsoft-com:office:word" xmlns:w="http://schemas.openxmlformats.org/wordprocessingml/2006/main" xmlns:wne="http://schemas.microsoft.com/office/word/2006/wordml" xmlns:mml="http://www.w3.org/1998/Math/MathML" xmlns:tbx="http://www.lisa.org/TBX-Specification.33.0.html" xmlns:pic="http://schemas.openxmlformats.org/drawingml/2006/picture" xmlns:teidocx="http://www.tei-c.org/ns/teidocx/1.0" xpath-default-namespace="http://www.tei-c.org/ns/1.0" version="2.0" exclude-result-prefixes="teix ve o r m v wp w10 w wne mml tbx iso a xs pic fn teidocx">
  <!-- import conversion style -->
  <xsl:import href="../../default/docx/to.xsl"/>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" scope="stylesheet" type="stylesheet">
    <desc>
      <p> TEI stylesheet for making Word docx files from TEI XML (see tei-docx.xsl) for Vesta </p>
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
  <xsl:param name="shadowGraphics">true</xsl:param>
  <xsl:param name="useNSPrefixes">false</xsl:param>
  <xsl:param name="pagebreakStyle">active</xsl:param>
  <xsl:param name="defaultHeaderFooterFile">../../profiles/oucscourses/docx/headersfooters.xml</xsl:param>
  <xsl:template match="teix:egXML|p[tei:match(@rend,'eg')]">
    <xsl:param name="simple">false</xsl:param>
    <xsl:param name="highlight"/>
    <xsl:call-template name="block-element">
      <xsl:with-param name="select">
        <p rend="Special" iso:style="font-family:DejaVu Sans Mono; font-size:18; text-align:left;">
          <xsl:call-template name="create-egXML-section"/>
        </p>
      </xsl:with-param>
    </xsl:call-template>
  </xsl:template>

  <xsl:template match="table[tei:match(@rend,'roomlabel')]">
    <xsl:variable name="data">
      <sessions>
        <xsl:for-each select="row">
          <xsl:variable name="r" select="."/>
          <xsl:for-each select="tokenize(normalize-space(replace(cell[1],'[, ]',' ')),' ')">
	    <xsl:if test="not(.='External')">
            <session>
              <weekday>
                <xsl:choose>
                  <xsl:when test="starts-with($r/cell[2],'Mon')">1</xsl:when>
                  <xsl:when test="starts-with($r/cell[2],'Tue')">2</xsl:when>
                  <xsl:when test="starts-with($r/cell[2],'Wed')">3</xsl:when>
                  <xsl:when test="starts-with($r/cell[2],'Thu')">4</xsl:when>
                  <xsl:when test="starts-with($r/cell[2],'Fri')">5</xsl:when>
                </xsl:choose>
              </weekday>
              <room>
                <xsl:value-of select="."/>
              </room>
              <xdate>
                <xsl:value-of select="$r/cell[2]"/>
              </xdate>
              <time><xsl:value-of select="$r/cell[3]"/> - <xsl:value-of select="$r/cell[4]"/></time>
              <title>
                <xsl:value-of select="$r/cell[5]"/>
              </title>
              <person>
                <xsl:value-of select="$r/cell[6]"/>
              </person>
            </session>
	    </xsl:if>
          </xsl:for-each>
        </xsl:for-each>
      </sessions>
    </xsl:variable>
    <xsl:for-each-group select="$data/sessions/session" group-by="concat(weekday,room)">
      <w:p>
        <w:pPr>
          <w:pStyle>
            <xsl:attribute name="w:val">DAY</xsl:attribute>
          </w:pStyle>
        </w:pPr>
        <w:r>
          <w:t>What's On <xsl:value-of select="xdate"/></w:t>
        </w:r>
      </w:p>
      <w:p>
        <w:pPr>
          <w:pStyle>
            <xsl:attribute name="w:val">ROOM</xsl:attribute>
          </w:pStyle>
        </w:pPr>
        <w:r>
          <w:t>Room: <xsl:value-of select="room"/></w:t>
        </w:r>
      </w:p>
      <xsl:for-each select="current-group()">
        <xsl:call-template name="roomtable"/>
      </xsl:for-each>
      <xsl:if test="position() != last()">
        <w:p>
          <w:r>
            <w:br w:type="page"/>
          </w:r>
        </w:p>
      </xsl:if>
    </xsl:for-each-group>
  </xsl:template>

  <xsl:template match="table[tei:match(@rend,'roomlabel2')]">
    <!-- room, date, start, end, title, person -->
    <xsl:variable name="data">
      <sessions>
        <xsl:for-each select="row">
          <xsl:variable name="r" select="."/>
          <xsl:for-each select="tokenize(normalize-space(replace(cell[1],' ',' ')),' ')">
	    <xsl:if test="not(.='External')">
            <session>
              <weekday>
                <xsl:choose>
                  <xsl:when test="starts-with($r/cell[2],'Mon')">1</xsl:when>
                  <xsl:when test="starts-with($r/cell[2],'Tue')">2</xsl:when>
                  <xsl:when test="starts-with($r/cell[2],'Wed')">3</xsl:when>
                  <xsl:when test="starts-with($r/cell[2],'Thu')">4</xsl:when>
                  <xsl:when test="starts-with($r/cell[2],'Fri')">5</xsl:when>
                </xsl:choose>
              </weekday>
              <room>
                <xsl:value-of select="."/>
              </room>
              <xdate>
                <xsl:value-of select="$r/cell[2]"/>
              </xdate>
              <time><xsl:value-of select="$r/cell[3]"/>:<xsl:value-of select="$r/cell[4]"/></time>
              <title>
                <xsl:value-of select="$r/cell[5]"/>
              </title>
              <person>
                <xsl:value-of select="$r/cell[6]"/>
              </person>
              <demons>
                <xsl:value-of select="$r/cell[7]"/>
              </demons>
            </session>
	    </xsl:if>
          </xsl:for-each>
        </xsl:for-each>
      </sessions>
    </xsl:variable>
    <xsl:for-each-group select="$data/sessions/session" group-by="concat(weekday,room)">
      <w:p>
        <w:pPr>
          <w:pStyle>
            <xsl:attribute name="w:val">ROOM</xsl:attribute>
          </w:pStyle>
        </w:pPr>
        <w:r>
          <w:t>Room: <xsl:value-of select="room"/> - <xsl:value-of select="xdate"/></w:t>
        </w:r>
      </w:p>
      <xsl:for-each select="current-group()">
        <xsl:call-template name="roomtable2"/>
	<w:p>
        <w:pPr>
          <w:pStyle>
            <xsl:attribute name="w:val">ROOM</xsl:attribute>
          </w:pStyle>
        </w:pPr>
        <w:r>
          <w:t>Key Signing In/Out</w:t>
        </w:r>
      </w:p>
      <w:tbl>
        <w:tblPr>
          <w:tblW w:w="4500" w:type="pct"/>
          <w:tblBorders>
            <w:top w:val="single" w:sz="6" w:space="0" w:color="auto"/>
            <w:left w:val="single" w:sz="6" w:space="0" w:color="auto"/>
            <w:bottom w:val="single" w:sz="6" w:space="0" w:color="auto"/>
            <w:right w:val="single" w:sz="6" w:space="0" w:color="auto"/>
          </w:tblBorders>
        </w:tblPr>
        <w:tblGrid>
          <w:gridCol w:w="2154"/>
          <w:gridCol w:w="6464"/>
        </w:tblGrid>
        <xsl:call-template name="twocells">
          <xsl:with-param name="cell1">Name</xsl:with-param>
          <xsl:with-param name="cell2">:</xsl:with-param>
        </xsl:call-template>
        <xsl:call-template name="twocells">
          <xsl:with-param name="cell1">Univ card</xsl:with-param>
          <xsl:with-param name="cell2">:</xsl:with-param>
        </xsl:call-template>
        <xsl:call-template name="twocells">
          <xsl:with-param name="cell1">Key Out</xsl:with-param>
          <xsl:with-param name="cell2">:</xsl:with-param>
        </xsl:call-template>
        <xsl:call-template name="twocells">
          <xsl:with-param name="cell1">Key In</xsl:with-param>
          <xsl:with-param name="cell2">:</xsl:with-param>
        </xsl:call-template>
      </w:tbl>
      </xsl:for-each>
      <w:p>
        <w:r>
          <xsl:choose>
            <xsl:when test="position() != last()">
              <w:br w:type="page"/>
            </xsl:when>
            <xsl:otherwise>
              <w:t> </w:t>
            </xsl:otherwise>
          </xsl:choose>
        </w:r>
      </w:p>
    </xsl:for-each-group>
  </xsl:template>
  <xsl:template match="details">
    <w:p>
      <w:r>
        <w:br w:type="page"/>
      </w:r>
      <w:r>
        <w:t> </w:t>
      </w:r>
    </w:p>
    <w:tbl>
      <w:tblPr>
        <w:tblW w:w="0" w:type="auto"/>
        <w:tblBorders>
          <w:top w:val="single" w:sz="6" w:space="0" w:color="auto"/>
          <w:left w:val="single" w:sz="6" w:space="0" w:color="auto"/>
          <w:bottom w:val="single" w:sz="6" w:space="0" w:color="auto"/>
          <w:right w:val="single" w:sz="6" w:space="0" w:color="auto"/>
        </w:tblBorders>
      </w:tblPr>
      <xsl:for-each select="p">
        <xsl:choose>
          <xsl:when test="@name='coursename'">
            <xsl:call-template name="twocells">
              <xsl:with-param name="cell1">Course name</xsl:with-param>
              <xsl:with-param name="cell2">
                <xsl:apply-templates/>
              </xsl:with-param>
            </xsl:call-template>
          </xsl:when>
          <xsl:when test="@name='date'">
            <xsl:call-template name="twocells">
              <xsl:with-param name="cell1">Date</xsl:with-param>
              <xsl:with-param name="cell2">
                <xsl:apply-templates/>
              </xsl:with-param>
            </xsl:call-template>
          </xsl:when>
          <xsl:when test="@name='time'">
            <xsl:call-template name="twocells">
              <xsl:with-param name="cell1">Time</xsl:with-param>
              <xsl:with-param name="cell2">
                <xsl:apply-templates/>
              </xsl:with-param>
            </xsl:call-template>
          </xsl:when>
          <xsl:when test="@name='room'">
            <xsl:call-template name="twocells">
              <xsl:with-param name="cell1">Lecture Room</xsl:with-param>
              <xsl:with-param name="cell2">
                <xsl:apply-templates/>
              </xsl:with-param>
            </xsl:call-template>
          </xsl:when>
          <xsl:when test="@name='cost'">
            <xsl:call-template name="twocells">
              <xsl:with-param name="cell1">Cost</xsl:with-param>
              <xsl:with-param name="cell2">
                <xsl:apply-templates/>
              </xsl:with-param>
            </xsl:call-template>
          </xsl:when>
        </xsl:choose>
      </xsl:for-each>
    </w:tbl>
  </xsl:template>
  <xsl:template name="roomtable">
    <w:p>
      <w:r>
        <w:t> </w:t>
      </w:r>
    </w:p>
    <w:tbl>
      <w:tblPr>
        <w:tblW w:w="0" w:type="auto"/>
        <w:tblBorders>
          <w:top w:val="single" w:sz="6" w:space="0" w:color="auto"/>
          <w:left w:val="single" w:sz="6" w:space="0" w:color="auto"/>
          <w:bottom w:val="single" w:sz="6" w:space="0" w:color="auto"/>
          <w:right w:val="single" w:sz="6" w:space="0" w:color="auto"/>
        </w:tblBorders>
      </w:tblPr>
      <xsl:call-template name="row">
        <xsl:with-param name="content" select="title"/>
      </xsl:call-template>
      <xsl:call-template name="row">
        <xsl:with-param name="content" select="time"/>
      </xsl:call-template>
      <xsl:call-template name="row">
        <xsl:with-param name="content" select="person"/>
      </xsl:call-template>
    </w:tbl>
  </xsl:template>
  <xsl:template name="roomtable2">
    <w:tbl>
      <w:tblPr>
        <w:tblW w:w="0" w:type="auto"/>
        <w:tblBorders>
          <w:top w:val="single" w:sz="6" w:space="0" w:color="auto"/>
          <w:left w:val="single" w:sz="6" w:space="0" w:color="auto"/>
          <w:bottom w:val="single" w:sz="6" w:space="0" w:color="auto"/>
          <w:right w:val="single" w:sz="6" w:space="0" w:color="auto"/>
        </w:tblBorders>
      </w:tblPr>
      <xsl:call-template name="row">
        <xsl:with-param name="content" select="title"/>
      </xsl:call-template>
      <xsl:call-template name="row">
        <xsl:with-param name="content" select="time"/>
      </xsl:call-template>
      <xsl:call-template name="row">
        <xsl:with-param name="content" select="person"/>
      </xsl:call-template>
      <xsl:call-template name="row">
        <xsl:with-param name="content" select="demons"/>
      </xsl:call-template>
    </w:tbl>
  </xsl:template>
  <xsl:template name="row">
    <xsl:param name="content"/>
    <w:tr>
      <w:tblPrEx>
        <w:tblLayout w:type="autofit"/>
      </w:tblPrEx>
      <w:tc>
        <w:p>
          <w:pPr>
            <w:pStyle>
              <xsl:attribute name="w:val">CourseTable</xsl:attribute>
            </w:pStyle>
          </w:pPr>
          <w:r>
            <w:t>
              <xsl:value-of select="$content"/>
            </w:t>
          </w:r>
        </w:p>
      </w:tc>
    </w:tr>
  </xsl:template>
  <xsl:template name="twocells">
    <xsl:param name="cell1"/>
    <xsl:param name="cell2"/>
    <w:tr>
      <w:tblPrEx>
        <w:tblLayout w:type="autofit"/>
      </w:tblPrEx>
      <w:tc>
        <w:p>
          <w:pPr>
            <w:pStyle>
              <xsl:attribute name="w:val">CourseTable</xsl:attribute>
            </w:pStyle>
          </w:pPr>
          <w:r>
            <w:rPr>
              <w:b/>
            </w:rPr>
            <w:t><xsl:value-of select="$cell1"/>:</w:t>
          </w:r>
        </w:p>
      </w:tc>
      <w:tc>
        <w:p>
          <w:r>
            <w:t>
              <xsl:value-of select="$cell2"/>
            </w:t>
          </w:r>
        </w:p>
      </w:tc>
    </w:tr>
  </xsl:template>
  <xsl:template name="document-title"/>
</xsl:stylesheet>
