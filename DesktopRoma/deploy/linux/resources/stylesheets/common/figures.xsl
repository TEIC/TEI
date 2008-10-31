<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet exclude-result-prefixes="xd tei edate"
  extension-element-prefixes="edate" version="1.0"
  xmlns:edate="http://exslt.org/dates-and-times"
  xmlns:tei="http://www.tei-c.org/ns/1.0"
  xmlns:xd="http://www.pnp-software.com/XSLTdoc"
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
  <xd:doc type="stylesheet">
    <xd:short> TEI stylesheet dealing with elements from the figures module. </xd:short>
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
    <xd:cvsId>$Id: figures.xsl 4801 2008-09-13 10:05:32Z rahtz $</xd:cvsId>
    <xd:copyright>2008, TEI Consortium</xd:copyright>
  </xd:doc>
  <xd:doc>
    <xd:short>Process elements tei:figure</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template match="tei:figure" mode="xref">
    <xsl:choose>
      <xsl:when test="$numberFigures='true'">
        <xsl:call-template name="i18n">
          <xsl:with-param name="word">figureWord</xsl:with-param>
        </xsl:call-template>
        <xsl:text> </xsl:text>
        <xsl:choose>
          <xsl:when test="ancestor::tei:front">
            <xsl:number count="tei:figure[tei:head]" from="tei:front"
              level="any"/>
          </xsl:when>
          <xsl:when test="ancestor::tei:back">
            <xsl:number count="tei:figure[tei:head]" from="tei:back" level="any"
            />
          </xsl:when>
          <xsl:when test="ancestor::tei:body">
            <xsl:number count="tei:figure[tei:head]" from="tei:body" level="any"
            />
          </xsl:when>
        </xsl:choose>
        <xsl:if test="tei:head">
          <xsl:text>, </xsl:text>
          <xsl:apply-templates mode="plain" select="tei:head"/>
        </xsl:if>
      </xsl:when>
      <xsl:otherwise>
        <xsl:text>this figure</xsl:text>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  <xd:doc>
    <xd:short>Process elements tei:table</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template match="tei:table" mode="xref">
    <xsl:choose>
      <xsl:when test="$numberTables='true'">
        <xsl:call-template name="i18n">
          <xsl:with-param name="word">tableWord</xsl:with-param>
        </xsl:call-template>
        <xsl:text> </xsl:text>
        <xsl:number level="any"/>
        <xsl:if test="tei:head">
          <xsl:text>. </xsl:text>
          <xsl:apply-templates mode="plain" select="tei:head"/>
        </xsl:if>
      </xsl:when>
      <xsl:otherwise>
        <xsl:text>this table</xsl:text>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  <xd:doc>
    <xd:short>[latex] Analyze attributes of graphics inclusion</xd:short>
    <xd:detail> </xd:detail>
    <xd:param name="mode">Type of output (fo, html, latex) being
    created</xd:param>
  </xd:doc>
  <xsl:template name="graphicsAttributes">
    <xsl:param name="mode">fo</xsl:param>
    <xsl:if test="@width">
      <xsl:choose>
        <xsl:when test="contains(@width,'%')">
          <xsl:choose>
            <xsl:when test="$mode='fo'">
              <xsl:attribute name="content-width">
                <xsl:value-of select="@width"/>
              </xsl:attribute>
            </xsl:when>
            <xsl:when test="$mode='latex'">
              <xsl:text>width=</xsl:text>
              <xsl:value-of select="substring-before(@width,'%') div 100"/>
              <xsl:text>\textwidth,</xsl:text>
            </xsl:when>
            <xsl:otherwise>
              <xsl:attribute name="width">
                <xsl:value-of select="@width"/>
              </xsl:attribute>
            </xsl:otherwise>
          </xsl:choose>
        </xsl:when>
        <xsl:otherwise>
          <xsl:variable name="w">
            <xsl:choose>
              <xsl:when test="contains(@width,'pt')">
                <xsl:value-of select="@width"/>
              </xsl:when>
              <xsl:when test="contains(@width,'px') and $mode='latex'">
                <xsl:value-of select="substring-before(@width,'px')"/>
                <xsl:text>pt</xsl:text>
              </xsl:when>
              <xsl:when test="contains(@width,'in')">
                <xsl:value-of select="@width"/>
              </xsl:when>
              <xsl:when test="contains(@width,'px')">
                <xsl:value-of select="@width"/>
              </xsl:when>
              <xsl:when test="contains(@width,'cm')">
                <xsl:value-of select="@width"/>
              </xsl:when>
              <xsl:otherwise>
                <xsl:value-of select="@width"/>
                <xsl:text>pt</xsl:text>
              </xsl:otherwise>
            </xsl:choose>
          </xsl:variable>
          <xsl:choose>
            <xsl:when test="$mode='fo'">
              <xsl:attribute name="content-width">
                <xsl:value-of select="$w"/>
              </xsl:attribute>
            </xsl:when>
            <xsl:when test="$mode='latex'">
              <xsl:text>width=</xsl:text>
              <xsl:value-of select="$w"/>
              <xsl:text>,</xsl:text>
            </xsl:when>
          </xsl:choose>
        </xsl:otherwise>
      </xsl:choose>
    </xsl:if>
    <xsl:if test="@height">
      <xsl:choose>
        <xsl:when test="contains(@height,'%')">
          <xsl:choose>
            <xsl:when test="$mode='fo'">
              <xsl:attribute name="content-height">
                <xsl:value-of select="@height"/>
              </xsl:attribute>
            </xsl:when>
            <xsl:when test="$mode='latex'">
              <xsl:text>height=</xsl:text>
              <xsl:value-of select="substring-before(@height,'%') div 100"/>
              <xsl:text>\textheight,</xsl:text>
            </xsl:when>
            <xsl:otherwise>
              <xsl:attribute name="height">
                <xsl:value-of select="@height"/>
              </xsl:attribute>
            </xsl:otherwise>
          </xsl:choose>
        </xsl:when>
        <xsl:otherwise>
          <xsl:variable name="h">
            <xsl:choose>
              <xsl:when test="contains(@height,'pt')">
                <xsl:value-of select="@height"/>
              </xsl:when>
              <xsl:when test="contains(@height,'px') and $mode='latex'">
                <xsl:value-of select="substring-before(@height,'px')"/>
                <xsl:text>pt</xsl:text>
              </xsl:when>
              <xsl:when test="contains(@height,'in')">
                <xsl:value-of select="@height"/>
              </xsl:when>
              <xsl:when test="contains(@height,'px')">
                <xsl:value-of select="@height"/>
              </xsl:when>
              <xsl:when test="contains(@height,'cm')">
                <xsl:value-of select="@height"/>
              </xsl:when>
              <xsl:otherwise>
                <xsl:value-of select="@height"/>
                <xsl:text>pt</xsl:text>
              </xsl:otherwise>
            </xsl:choose>
          </xsl:variable>
          <xsl:choose>
            <xsl:when test="$mode='fo'">
              <xsl:attribute name="content-height">
                <xsl:value-of select="$h"/>
              </xsl:attribute>
            </xsl:when>
            <xsl:when test="$mode='latex'">
              <xsl:text>height=</xsl:text>
              <xsl:value-of select="$h"/>
              <xsl:text>,</xsl:text>
            </xsl:when>
          </xsl:choose>
        </xsl:otherwise>
      </xsl:choose>
    </xsl:if>
    <xsl:variable name="s">
      <xsl:choose>
        <xsl:when test="@scale and contains(@scale,'%')">
          <xsl:value-of select="substring-before(@scale,'%') div 100"/>
        </xsl:when>
        <xsl:when test="@scale">
          <xsl:value-of select="@scale"/>
        </xsl:when>
        <xsl:when test="not(@width) and not(@height) and not($standardScale=1)">
          <xsl:value-of select="$standardScale"/>
        </xsl:when>
      </xsl:choose>
    </xsl:variable>
    <xsl:if test="not($s='')">
      <xsl:choose>
        <xsl:when test="$mode='fo'">
          <xsl:attribute name="scale">
            <xsl:value-of select="$s"/>
          </xsl:attribute>
        </xsl:when>
        <xsl:when test="$mode='latex'">
          <xsl:text>scale=</xsl:text>
          <xsl:value-of select="$s"/>
          <xsl:text>,</xsl:text>
        </xsl:when>
      </xsl:choose>
    </xsl:if>
  </xsl:template>
</xsl:stylesheet>
