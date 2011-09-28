<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet xmlns:dc="http://purl.org/dc/elements/1.1/" xmlns:iso="http://www.iso.org/ns/1.0" xmlns="http://www.w3.org/1999/xhtml" xmlns:html="http://www.w3.org/1999/xhtml" xmlns:tei="http://www.tei-c.org/ns/1.0" xmlns:teix="http://www.tei-c.org/ns/Examples" xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xmlns:ncx="http://www.daisy.org/z3986/2005/ncx/" version="2.0" exclude-result-prefixes="iso tei teix dc html ncx">
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" scope="stylesheet" type="stylesheet">
    <desc>
      <p>
	TEI stylesheet for making ePub output, preflight pass to clean
	up text. 
      </p>
      <p>
	This library is free software; you can redistribute it and/or
	modify it under the terms of the GNU Lesser General Public
	License as published by the Free Software Foundation; either
	version 2.1 of the License, or (at your option) any later version.
	
	This library is distributed in the hope that it will be useful,
	but WITHOUT ANY WARRANTY; without even the implied warranty of
	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
	Lesser General Public License for more details.
	
	You should have received a copy of the GNU Lesser General Public
	License along with this library; if not, write to the Free Software
	Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
	
      </p>
      <p>Author: See AUTHORS</p>
      <p>Id: $Id: epub-common.xsl 9329 2011-09-20 09:47:43Z rahtz $</p>
      <p>Copyright: 2008, TEI Consortium</p>
    </desc>
  </doc>

  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc>[epub] Local mode to rewrite names of graphics inclusions;
      default is identity transform
      </desc>
  </doc>
  <xsl:template match="@*|text()|comment()|processing-instruction()" mode="preflight">
    <xsl:copy-of select="."/>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc>[epub] Local mode to rewrite names of graphics inclusions;
      default is identifty transform
      </desc>
  </doc>
  <xsl:template match="*" mode="preflight">
    <xsl:copy>
      <xsl:apply-templates select="*|@*|processing-instruction()|comment()|text()" mode="preflight"/>
    </xsl:copy>
  </xsl:template>

  <xsl:template match="tei:graphic" mode="preflight">
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
	<xsl:when test="@rend='none'">
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
    <xsl:for-each-group select="node()" group-starting-with="tei:pb">
        <xsl:choose>
          <xsl:when test="self::tei:pb">
	      <tei:pb>
		<xsl:copy-of select="@*"/>
	      </tei:pb>
	      <tei:hi>
		<xsl:copy-of select="$atts"/>
		<xsl:copy-of select="current-group() except ."/>
	      </tei:hi>
          </xsl:when>
          <xsl:otherwise>
	    <tei:hi>
	      <xsl:copy-of select="$atts"/>
	      <xsl:copy-of select="current-group()"/>
	    </tei:hi>
          </xsl:otherwise>
        </xsl:choose>
      </xsl:for-each-group>

  </xsl:template>

</xsl:stylesheet>
