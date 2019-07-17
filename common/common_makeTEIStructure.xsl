<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet xmlns:a="http://relaxng.org/ns/compatibility/annotations/1.0" xmlns:rng="http://relaxng.org/ns/structure/1.0" xmlns:xs="http://www.w3.org/2001/XMLSchema" xmlns:tei="http://www.tei-c.org/ns/1.0" xmlns="http://www.tei-c.org/ns/1.0" xmlns:teix="http://www.tei-c.org/ns/Examples" xmlns:xsl="http://www.w3.org/1999/XSL/Transform" exclude-result-prefixes="a rng tei teix xs" version="2.0">
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" scope="stylesheet" type="stylesheet">
    <desc>
      <p> TEI stylesheet dealing with converting other vocabularies to TEI </p>
      <p>This software is dual-licensed: 1. Distributed under a Creative Commons
				Attribution-ShareAlike 3.0 Unported License
				http://creativecommons.org/licenses/by-sa/3.0/ 2.
				http://www.opensource.org/licenses/BSD-2-Clause All rights reserved. Redistribution
				and use in source and binary forms, with or without modification, are permitted
				provided that the following conditions are met: * Redistributions of source code
				must retain the above copyright notice, this list of conditions and the following
				disclaimer. * Redistributions in binary form must reproduce the above copyright
				notice, this list of conditions and the following disclaimer in the documentation
				and/or other materials provided with the distribution. This software is provided by
				the copyright holders and contributors "as is" and any express or implied
				warranties, including, but not limited to, the implied warranties of merchantability
				and fitness for a particular purpose are disclaimed. In no event shall the copyright
				holder or contributors be liable for any direct, indirect, incidental, special,
				exemplary, or consequential damages (including, but not limited to, procurement of
				substitute goods or services; loss of use, data, or profits; or business
				interruption) however caused and on any theory of liability, whether in contract,
				strict liability, or tort (including negligence or otherwise) arising in any way out
				of the use of this software, even if advised of the possibility of such damage. </p>
      <p>Author: See AUTHORS</p>
      <p>Copyright: 2013, TEI Consortium</p>
    </desc>
  </doc>
  <xsl:template name="convertStructure">
    <text>
      <body>
        <xsl:variable name="Body">
          <HEAD xmlns="http://www.tei-c.org/ns/1.0"  level="1" magic="true">Start</HEAD>
          <xsl:call-template name="gatherText"/>
        </xsl:variable>
        <xsl:variable name="Body2">
          <xsl:for-each select="$Body">
	    <xsl:apply-templates select="*|processing-instruction()|comment()|text()" mode="pass1"/>
          </xsl:for-each>
        </xsl:variable>
        <xsl:for-each select="$Body2">
          <xsl:for-each-group select="*|comment()" group-starting-with="tei:HEAD[@level='1']">
            <xsl:choose>
              <xsl:when test="self::tei:HEAD[@level='1']">
		<xsl:call-template name="group-by-section"/>
              </xsl:when>
              <xsl:otherwise>
                <xsl:call-template name="inSectionCodeBlock"/>
              </xsl:otherwise>
            </xsl:choose>
          </xsl:for-each-group>
        </xsl:for-each>
      </body>
    </text>
  </xsl:template>
  <xsl:template name="group-by-section">
    <xsl:variable name="ThisHeader" select="number(@level)"/>
    <xsl:variable name="NextHeader" select="number(@level)+1"/>
    <xsl:choose>
      <xsl:when test="@magic">
        <xsl:for-each-group select="current-group() except ." group-starting-with="tei:HEAD[number(@level)=$NextHeader]">
          <xsl:choose>
            <xsl:when test="self::tei:HEAD">
              <xsl:call-template name="group-by-section"/>
            </xsl:when>
            <xsl:otherwise>
              <xsl:call-template name="inSectionCodeBlock"/>
            </xsl:otherwise>
          </xsl:choose>
        </xsl:for-each-group>
      </xsl:when>
      <xsl:otherwise>
        <div>
          <xsl:if test="@style">
            <xsl:attribute name="rend" select="@style"/>
          </xsl:if>
          <xsl:if test="not(@interpolated='true')">
            <head>
              <xsl:apply-templates mode="pass1"/>
            </head>
          </xsl:if>
          <xsl:for-each-group select="current-group() except ." group-starting-with="tei:HEAD[number(@level)=$NextHeader]">
            <xsl:choose>
              <xsl:when test="self::tei:HEAD">
                <xsl:call-template name="group-by-section"/>
              </xsl:when>
              <xsl:otherwise>
                <xsl:call-template name="inSectionCodeBlock"/>
              </xsl:otherwise>
            </xsl:choose>
          </xsl:for-each-group>
        </div>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  <xsl:template name="inSectionCodeBlock">
    <xsl:for-each-group select="current-group()" group-starting-with="tei:FCODE">
      <xsl:choose>
        <xsl:when test="position() mod 2 = 0">
          <!-- only even groups contain code blocks (that were fenced with ~~~) -->
          <eg>
            <xsl:for-each select="current-group()">
              <xsl:if test="not(self::tei:FCODE)">
                <xsl:copy>
                  <xsl:apply-templates/>
                </xsl:copy>
              </xsl:if>
            </xsl:for-each>
          </eg>
        </xsl:when>
        <xsl:otherwise>
          <!-- other groups should be processed as before -->
          <xsl:call-template name="inSection"/>
        </xsl:otherwise>
      </xsl:choose>
    </xsl:for-each-group>
  </xsl:template>
  <xsl:template name="inSection">
    <xsl:for-each-group select="current-group()" group-adjacent="if (self::tei:GLOSS) then 1      else if (self::tei:ITEM) then 2      else if (self::tei:NITEM) then 3      else if (self::tei:BQUOTE) then 4      else if (self::tei:BCODE) then 5      else if (self::tei:FCODE) then 6      else 7">

      <xsl:choose>
        <xsl:when test="current-grouping-key()=1">
          <list type="gloss">
            <xsl:for-each select="current-group()">
              <xsl:element name="{@n}">
		<xsl:apply-templates select="*|processing-instruction()|comment()|text()" mode="pass2"/>
              </xsl:element>
            </xsl:for-each>
          </list>
        </xsl:when>
        <xsl:when test="current-grouping-key()=2">
          <list type="unordered">
            <xsl:for-each select="current-group()">
              <xsl:element name="{@n}">
		<xsl:apply-templates select="*|processing-instruction()|comment()|text()" mode="pass2"/>
              </xsl:element>
            </xsl:for-each>
          </list>
        </xsl:when>
        <xsl:when test="current-grouping-key()=3">
          <list type="ordered">
            <xsl:for-each select="current-group()">
              <xsl:element name="{@n}">
		<xsl:apply-templates select="*|processing-instruction()|comment()|text()" mode="pass2"/>
              </xsl:element>
            </xsl:for-each>
          </list>
        </xsl:when>
        <xsl:when test="current-grouping-key()=4">
          <q>
            <xsl:for-each select="current-group()">
              <p>
		<xsl:apply-templates select="*|processing-instruction()|comment()|text()" mode="pass2"/>
              </p>
            </xsl:for-each>
          </q>
        </xsl:when>
        <xsl:when test="current-grouping-key()=5">
          <eg>
            <xsl:for-each select="current-group()">
              <l>
		<xsl:apply-templates select="*|processing-instruction()|comment()|text()" mode="pass2"/>
              </l>
            </xsl:for-each>
          </eg>
        </xsl:when>
        <xsl:when test="current-grouping-key()=6"/>
        <xsl:otherwise>
          <xsl:for-each select="current-group()">
            <xsl:apply-templates select="." mode="pass2"/>
          </xsl:for-each>
        </xsl:otherwise>
      </xsl:choose>
    </xsl:for-each-group>
  </xsl:template>
  <xsl:template match="tei:HEAD" mode="pass1">
    <xsl:if test="preceding-sibling::tei:HEAD">
      <xsl:variable name="prev" select="xs:integer(number(preceding-sibling::tei:HEAD[1]/@level))"/>
      <xsl:variable name="current" select="xs:integer(number(@level))"/>
      <xsl:if test="($current - $prev) &gt;1 ">
        <!--<xsl:message>Walk from <xsl:value-of select="$prev"/> to <xsl:value-of select="$current"/></xsl:message>-->
        <xsl:for-each select="$prev + 1   to $current - 1 ">
          <HEAD interpolated="true" level="{.}"/>
        </xsl:for-each>
      </xsl:if>
    </xsl:if>
    <xsl:copy>
      <xsl:apply-templates select="*|@*|processing-instruction()|comment()|text()" mode="pass1"/>
    </xsl:copy>
  </xsl:template>
  <xsl:template match="*" mode="pass1">
    <xsl:copy>
      <xsl:apply-templates select="*|@*|processing-instruction()|comment()|text()" mode="pass1"/>
    </xsl:copy>
  </xsl:template>
  <xsl:template match="@*|text()|comment()|processing-instruction()" mode="pass1">
    <xsl:copy-of select="."/>
  </xsl:template>
  <xsl:template match="@*|text()|comment()|processing-instruction()" mode="pass2">
    <xsl:copy-of select="."/>
  </xsl:template>
  <xsl:template match="tei:p[not(*) and normalize-space(.)='']" mode="pass2"/>
  <xsl:template match="*" mode="pass2">
    <xsl:copy>
      <xsl:apply-templates select="*|@*|processing-instruction()|comment()|text()" mode="pass2"/>
    </xsl:copy>
  </xsl:template>
  <xsl:template name="gatherText">
    <xsl:apply-templates/>
  </xsl:template>
</xsl:stylesheet>
