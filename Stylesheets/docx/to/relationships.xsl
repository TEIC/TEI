<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet xmlns:a="http://schemas.openxmlformats.org/drawingml/2006/main" xmlns:cals="http://www.oasis-open.org/specs/tm9901" xmlns:contypes="http://schemas.openxmlformats.org/package/2006/content-types" xmlns:cp="http://schemas.openxmlformats.org/package/2006/metadata/core-properties" xmlns:dc="http://purl.org/dc/elements/1.1/" xmlns:dcmitype="http://purl.org/dc/dcmitype/" xmlns:dcterms="http://purl.org/dc/terms/" xmlns:html="http://www.w3.org/1999/xhtml" xmlns:iso="http://www.iso.org/ns/1.0" xmlns:m="http://schemas.openxmlformats.org/officeDocument/2006/math" xmlns:mml="http://www.w3.org/1998/Math/MathML" xmlns:o="urn:schemas-microsoft-com:office:office" xmlns:pic="http://schemas.openxmlformats.org/drawingml/2006/picture" xmlns:r="http://schemas.openxmlformats.org/officeDocument/2006/relationships" xmlns:tbx="http://www.lisa.org/TBX-Specification.33.0.html" xmlns:tei="http://www.tei-c.org/ns/1.0" xmlns:teidocx="http://www.tei-c.org/ns/teidocx/1.0" xmlns:teix="http://www.tei-c.org/ns/Examples" xmlns:v="urn:schemas-microsoft-com:vml" xmlns:fn="http://www.w3.org/2005/02/xpath-functions" xmlns:ve="http://schemas.openxmlformats.org/markup-compatibility/2006" xmlns:w10="urn:schemas-microsoft-com:office:word" xmlns:w="http://schemas.openxmlformats.org/wordprocessingml/2006/main" xmlns:wne="http://schemas.microsoft.com/office/word/2006/wordml" xmlns:wp="http://schemas.openxmlformats.org/drawingml/2006/wordprocessingDrawing" xmlns:xs="http://www.w3.org/2001/XMLSchema" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="2.0" exclude-result-prefixes="cp ve o r m v wp w10 w wne mml tbx iso      tei a xs pic fn xsi dc dcterms dcmitype     contypes teidocx teix html cals">
  <xsl:key name="TARGETS" use="1" match="tei:ref[@target]|tei:ptr[@target]"/>
  <xsl:key name="GRAPHICS" use="1" match="tei:graphic[@url]|tei:media[@url]"/>
  <xsl:key name="OLEOBJECTS" use="1" match="o:OLEObject"/>
  <xsl:key name="IMAGEDATA" use="1" match="v:imagedata"/>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" scope="stylesheet" type="stylesheet">
    <desc>
      <p> TEI stylesheet for making Word docx files from TEI XML </p>
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
    <desc>Write _rels/.rels</desc>
  </doc>
  <xsl:template name="write-docxfile-main-relationships">
    <xsl:if test="$debug='true'">
      <xsl:message>Writing out <xsl:value-of select="concat($wordDirectory,'/_rels/.rels')"/>
	</xsl:message>
    </xsl:if>
    <xsl:result-document href="{concat($wordDirectory,'/_rels/.rels')}" standalone="yes">
      <Relationships xmlns="http://schemas.openxmlformats.org/package/2006/relationships">
        <Relationship Id="rId1" Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/officeDocument" Target="word/document.xml"/>
        <Relationship Id="rId2" Type="http://schemas.openxmlformats.org/package/2006/relationships/metadata/core-properties" Target="docProps/core.xml"/>
        <Relationship Id="rId3" Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/extended-properties" Target="docProps/app.xml"/>
        <Relationship Id="rId4" Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/custom-properties" Target="docProps/custom.xml"/>
      </Relationships>
    </xsl:result-document>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc>Write  word/_rels/document.xml.rels</desc>
  </doc>
  <xsl:template name="write-docxfile-relationships">
    <xsl:variable name="R">
      <!-- Any raw blips left in? -->
      <xsl:for-each select="key('BLIP',1)">
        <xsl:choose>
          <xsl:when test="@r:embed">
            <r:Relationship Place="{tei:relPlace(.)}"  Id="rId{position() + 200}" Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/image" Target="{@r:embed}"/>
          </xsl:when>
          <xsl:otherwise>
            <r:Relationship Place="{tei:relPlace(.)}"  Id="rId{position() + 200}" Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/image" Target="{@r:link}" TargetMode="External"/>
          </xsl:otherwise>
        </xsl:choose>
      </xsl:for-each>
      <!-- images -->
      <xsl:for-each select="key('GRAPHICS',1)">
        <xsl:variable name="n">
          <xsl:number level="any"/>
        </xsl:variable>
        <r:Relationship Place="{tei:relPlace(.)}"  Id="rId{$n + 300}" Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/image" Target="media/resource{$n}.{tokenize(@url,'\.')[last()]}"/>
      </xsl:for-each>
      <!-- hyperlinks -->
      <xsl:for-each select="key('TARGETS',1)">
        <xsl:variable name="orig" select="."/>
	<xsl:variable name="P" select="tei:relPlace(.)"/>
        <xsl:variable name="n">
          <xsl:number count="tei:ptr|tei:ref" level="any"/>
        </xsl:variable>
        <xsl:for-each select="tokenize(@target, ' ')">
          <xsl:if test="not(starts-with(.,'#'))">
            <r:Relationship Place="{$P}" Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/hyperlink" Target="{tei:resolveURI($orig,.)}" TargetMode="External">
              <xsl:attribute name="Id">
                <xsl:text>rId</xsl:text>
                <xsl:value-of select="$n + 3000 + position()"/>
              </xsl:attribute>
	    </r:Relationship>
          </xsl:if>
        </xsl:for-each>
      </xsl:for-each>
      <!-- Formulas -->
      <xsl:for-each select="key('IMAGEDATA',1)">
        <r:Relationship Place="{tei:relPlace(.)}"  Id="rId{position() + 1000}" Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/image" Target="{@r:id}"/>
      </xsl:for-each>
      <xsl:for-each select="key('OLEOBJECTS',1)">
        <r:Relationship Place="{tei:relPlace(.)}"  Id="rId{position() + 2000}" Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/oleObject" Target="{@r:id}"/>
      </xsl:for-each>
    </xsl:variable>
    <xsl:if test="$debug='true'">
      <xsl:message>Writing out <xsl:value-of select="$relDoc"/>
	</xsl:message>
    </xsl:if>
    <xsl:result-document href="{$relDoc}" standalone="yes">
      <Relationships xmlns="http://schemas.openxmlformats.org/package/2006/relationships">
        <!-- Mandatory Relationships -->
        <Relationship Id="rId3" Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/numbering" Target="numbering.xml"/>
        <Relationship Id="rId4" Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/styles" Target="styles.xml"/>
        <Relationship Id="rId5" Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/settings" Target="settings.xml"/>
        <Relationship Id="rId7" Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/footnotes" Target="footnotes.xml"/>
        <Relationship Id="rId8" Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/endnotes" Target="endnotes.xml"/>
        <Relationship Id="rId9" Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/comments" Target="comments.xml"/>
        <!-- odd stuff -->
        <Relationship Id="rId18" Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/fontTable" Target="fontTable.xml"/>
          <xsl:for-each select="$R/r:Relationship[@Place='Document']">
	    <Relationship>
	      <xsl:copy-of select="@*[not(name(.)='Place')]"/>
	    </Relationship>
	  </xsl:for-each>

        <!-- our headers and footers -->
        <xsl:choose>
          <xsl:when test="count(key('ALLHEADERS',1))=0 and count(key('ALLFOOTERS',1))=0">
            <xsl:for-each select="doc($defaultHeaderFooterFile)">
              <xsl:call-template name="writeRelationshipsHeadersAndFooters"/>
            </xsl:for-each>
          </xsl:when>
          <xsl:otherwise>
            <xsl:call-template name="writeRelationshipsHeadersAndFooters"/>
          </xsl:otherwise>
        </xsl:choose>
      </Relationships>
    </xsl:result-document>
    <xsl:if test="$R/r:Relationship[@Place='End']">
      <!-- hyperlinks in endnotes -->
      <xsl:if test="$debug='true'">
        <xsl:message>Writing out <xsl:value-of select="$relDocEnd"/>
	  </xsl:message>
      </xsl:if>
      <xsl:result-document href="{$relDocEnd}" standalone="yes">
        <Relationships xmlns="http://schemas.openxmlformats.org/package/2006/relationships">
          <xsl:for-each select="$R/r:Relationship[@Place='End']">
	    <Relationship>
	      <xsl:copy-of select="@*[not(name(.)='Place')]"/>
	    </Relationship>
	  </xsl:for-each>
        </Relationships>
      </xsl:result-document>
    </xsl:if>
    <xsl:if test="$R/r:Relationship[@Place='Foot']">
      <!-- hyperlinks in footnotes -->
      <xsl:if test="$debug='true'">
        <xsl:message>Writing out <xsl:value-of
	select="$relDocFoot"/></xsl:message>
      </xsl:if>
      <xsl:result-document href="{$relDocFoot}" standalone="yes">
        <Relationships xmlns="http://schemas.openxmlformats.org/package/2006/relationships">
          <xsl:for-each select="$R/r:Relationship[@Place='Foot']">
	    <Relationship>
	      <xsl:copy-of select="@*[not(name(.)='Place')]"/>
	    </Relationship>
	  </xsl:for-each>
	</Relationships>
      </xsl:result-document>
      </xsl:if>
    </xsl:template>

  <xsl:template name="writeRelationshipsHeadersAndFooters">
    <xsl:for-each select="key('ALLFOOTERS',1)">
      <xsl:variable name="num">
        <xsl:number/>
      </xsl:variable>
      <Relationship xmlns="http://schemas.openxmlformats.org/package/2006/relationships" Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/footer" Target="{concat('footer',position(),'.xml')}" Id="{concat('rId',100+$num)}"/>
    </xsl:for-each>
    <!-- count all footers -->
    <xsl:for-each select="key('ALLHEADERS',1)">
      <xsl:variable name="num">
        <xsl:number/>
      </xsl:variable>
      <Relationship xmlns="http://schemas.openxmlformats.org/package/2006/relationships" Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/header" Target="{concat('header',position(),'.xml')}" Id="{concat('rId',100+$num)}"/>
    </xsl:for-each>
  </xsl:template>


  <xsl:function name="tei:relPlace" as="xs:string">
    <xsl:param name="element"/>
    <xsl:for-each select="$element">
      <xsl:choose>
	<xsl:when test="ancestor-or-self::tei:note[tei:isEndNote(.)]">End</xsl:when>
	<xsl:when test="ancestor-or-self::tei:note[tei:isFootNote(.)]">Foot</xsl:when>
	<xsl:otherwise>Document</xsl:otherwise>
      </xsl:choose>
    </xsl:for-each>
  </xsl:function>
</xsl:stylesheet>
