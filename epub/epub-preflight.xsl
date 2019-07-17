<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet xmlns:dc="http://purl.org/dc/elements/1.1/" xmlns:iso="http://www.iso.org/ns/1.0" xmlns="http://www.w3.org/1999/xhtml" xmlns:html="http://www.w3.org/1999/xhtml" xmlns:tei="http://www.tei-c.org/ns/1.0" xmlns:teix="http://www.tei-c.org/ns/Examples" xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xmlns:ncx="http://www.daisy.org/z3986/2005/ncx/" version="2.0" exclude-result-prefixes="iso tei teix dc html ncx">
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" scope="stylesheet" type="stylesheet">
    <desc>
      <p>
	TEI stylesheet for making ePub output, preflight pass to clean
	up text. 
      </p>
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

  <xsl:template match="@*|text()|comment()|processing-instruction()" mode="preflight">
    <xsl:copy-of select="."/>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc>[epub] Local mode to rewrite names of graphics inclusions;
      default is identifty transform</desc>
  </doc>
  <xsl:template match="*" mode="preflight">
    <xsl:copy>
      <xsl:apply-templates select="*|@*|processing-instruction()|comment()|text()" mode="preflight"/>
    </xsl:copy>
  </xsl:template>

  <xsl:template match="tei:graphic|tei:media" mode="preflight">
    <xsl:copy>
      <xsl:choose>
        <xsl:when test="$fixgraphicsurl='true'">
          <xsl:variable name="newName">
            <xsl:text>media/resource</xsl:text>
            <xsl:number level="any"/>
            <xsl:text>.</xsl:text>
            <xsl:value-of select="tokenize(@url,'\.')[last()]"/>
          </xsl:variable>
          <xsl:attribute name="url">
            <xsl:value-of select="$newName"/>
          </xsl:attribute>
          <xsl:copy-of select="@*[not(local-name()='url')]"/>
        </xsl:when>
        <xsl:otherwise>
          <xsl:copy-of select="@*"/>
        </xsl:otherwise>
      </xsl:choose>
    </xsl:copy>
  </xsl:template>
  <xsl:template match="tei:pb[@facs]" mode="preflight">
    <xsl:copy>
      <xsl:choose>
	<xsl:when test="starts-with(@facs,'tcp:')"/>
	<xsl:when test="starts-with(@facs,'unknown:')"/>
	<xsl:when test="tei:match(@rend,'none')">
          <xsl:copy-of select="@*"/>
	</xsl:when>
        <xsl:when test="$fixgraphicsurl='true'">
          <xsl:variable name="newName">
            <xsl:text>media/pageimage</xsl:text>
            <xsl:number level="any"/>
            <xsl:text>.</xsl:text>
            <xsl:value-of select="tokenize(@facs,'\.')[last()]"/>
          </xsl:variable>
          <xsl:attribute name="facs">
            <xsl:value-of select="$newName"/>
          </xsl:attribute>
          <xsl:copy-of select="@*[not(local-name()='facs')]"/>
        </xsl:when>
        <xsl:otherwise>
          <xsl:copy-of select="@*"/>
        </xsl:otherwise>
      </xsl:choose>
    </xsl:copy>
  </xsl:template>

  <xsl:template match="tei:hi[tei:pb]" mode="preflight">
    <xsl:variable name="atts" select="@*"/>
    <xsl:for-each-group select="node()" group-starting-with="tei:pb|tei:figure">
        <xsl:choose>
          <xsl:when test="self::tei:pb">
	      <xsl:apply-templates select="." mode="preflight"/>
	      <tei:hi>
		<xsl:copy-of select="$atts"/>
		<xsl:apply-templates select="current-group() except ."
				     mode="preflight"/>
	      </tei:hi>
          </xsl:when>
          <xsl:when test="self::tei:figure">
	    <xsl:apply-templates select="." mode="preflight"/>
	    <tei:hi>
	      <xsl:copy-of select="$atts"/>
	      <xsl:apply-templates select="current-group() except ."
				   mode="preflight"/>
	    </tei:hi>
          </xsl:when>
          <xsl:otherwise>
	    <tei:hi>
	      <xsl:copy-of select="$atts"/>
		<xsl:apply-templates select="current-group()"     mode="preflight"/>
	    </tei:hi>
          </xsl:otherwise>
        </xsl:choose>
      </xsl:for-each-group>

  </xsl:template>

</xsl:stylesheet>
