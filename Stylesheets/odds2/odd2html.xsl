<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet xmlns:s="http://www.ascc.net/xml/schematron" xmlns="http://www.w3.org/1999/xhtml" xmlns:xlink="http://www.w3.org/1999/xlink" xmlns:dbk="http://docbook.org/ns/docbook" xmlns:rng="http://relaxng.org/ns/structure/1.0" xmlns:tei="http://www.tei-c.org/ns/1.0" xmlns:teix="http://www.tei-c.org/ns/Examples" xmlns:xhtml="http://www.w3.org/1999/xhtml" xmlns:a="http://relaxng.org/ns/compatibility/annotations/1.0" xmlns:html="http://www.w3.org/1999/xhtml" xmlns:sch="http://purl.oclc.org/dsdl/schematron" xmlns:xs="http://www.w3.org/2001/XMLSchema" xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="2.0" exclude-result-prefixes="xlink dbk rng tei teix s xhtml sch a html xs xsl">
  <xsl:import href="teiodds.xsl"/>
  <xsl:import href="classatts.xsl"/>
  <xsl:import href="../xhtml2/tei.xsl"/>
  <xsl:import href="../xhtml2/oddprocessing.xsl"/>

  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" scope="stylesheet" type="stylesheet">
    <desc>
      <p> TEI stylesheet for making HTML from ODD </p>
      <p>This software is dual-licensed:

1. Distributed under a Creative Commons Attribution-ShareAlike 3.0
Unported License http://creativecommons.org/licenses/by-sa/3.0/ 

2. http://www.opensource.org/licenses/BSD-2-Clause
		
All rights reserved.

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
      <p>Id: $Id$</p>
      <p>Copyright: 2011, TEI Consortium</p>
    </desc>
  </doc>
  <xsl:output method="xml" encoding="utf-8"/>
  <xsl:param name="BITS">Bits</xsl:param>
  <xsl:param name="STDOUT">true</xsl:param>
  <xsl:param name="TAG"/>
  <xsl:param name="alignNavigationPanel">left</xsl:param>
  <xsl:param name="authorWord"/>
  <xsl:param name="autoToc">true</xsl:param>
  <xsl:param name="bottomNavigationPanel">true</xsl:param>
  <xsl:param
      name="cssFile">http://www.tei-c.org/release/xml/tei/stylesheet/tei.css</xsl:param>
  <xsl:param name="dateWord"/>
  <xsl:param name="displayMode">rnc</xsl:param>
  <xsl:param name="feedbackWords">Contact</xsl:param>
  <xsl:param name="footnoteBackLink">true</xsl:param>
  <xsl:param name="footnoteFile">false</xsl:param>
  <xsl:param name="homeURL">index.html</xsl:param>
  <xsl:param name="indent-width" select="3"/>
  <xsl:param name="line-width" select="80"/>
  <xsl:param name="numberBackHeadings">A.1</xsl:param>
  <xsl:param name="numberFrontHeadings"/>
  <xsl:param name="pageLayout">Simple</xsl:param>
  <xsl:param name="prenumberedHeadings">false</xsl:param>
  <xsl:param name="searchURL"/>
  <xsl:param name="searchWords"/>
  <xsl:param name="showNamespaceDecls">false</xsl:param>
  <xsl:param name="showTitleAuthor">1</xsl:param>
  <xsl:param name="splitBackmatter">yes</xsl:param>
  <xsl:param name="splitFrontmatter">yes</xsl:param>
  <xsl:param name="splitLevel">-1</xsl:param>
  <xsl:param name="subTocDepth">-1</xsl:param>
  <xsl:param name="tocDepth">3</xsl:param>
  <xsl:param name="tocElement">div</xsl:param>
  <xsl:param name="topNavigationPanel"/>
  <xsl:param name="useHeaderFrontMatter">true</xsl:param>
  <xsl:param name="verbose">false</xsl:param>
  <xsl:param name="doctypePublic">-//W3C//DTD XHTML 1.1//EN</xsl:param>
  <xsl:param name="doctypeSystem">http://www.w3.org/TR/xhtml11/DTD/xhtml11.dtd</xsl:param>

  <!-- these are ones to override -->
  <xsl:param name="feedbackURL">#</xsl:param>
  <xsl:param name="homeLabel">Home</xsl:param>
  <xsl:param name="homeWords">Home</xsl:param>
  <xsl:param name="institution"/>
  <xsl:param name="outputDir"/>
  <xsl:param name="parentURL">http://www.example.com/</xsl:param>
  <xsl:param name="parentWords"/>


  <xsl:template match="/">
    <xsl:variable name="resolvedClassatts">
      <xsl:apply-templates  mode="classatts"/>
    </xsl:variable>
    <xsl:for-each select="$resolvedClassatts">
      <xsl:call-template name="processTEI"/>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="copyrightStatement"/>

  <xsl:template match="processing-instruction()"/>

  <xsl:template name="headingNumberSuffix">
    <xsl:text> </xsl:text>
  </xsl:template>

  <xsl:template match="tei:divGen[@type='toc']">
    <xsl:call-template name="mainTOC"/>
  </xsl:template>

  <xsl:template name="oddTocEntry">
    <xsl:variable name="loc">
      <xsl:choose>
        <xsl:when test="number($splitLevel)=-1 or $STDOUT='true'">
          <xsl:text>#</xsl:text>
          <xsl:value-of select="@ident"/>
        </xsl:when>
        <xsl:otherwise>
          <xsl:text>ref-</xsl:text>
          <xsl:value-of select="@ident"/>
          <xsl:value-of select="$outputSuffix"/>
        </xsl:otherwise>
      </xsl:choose>
    </xsl:variable>
    <div class="oddTocEntry">
      <a href="{$loc}">
        <xsl:value-of select="@ident"/>
      </a>
    </div>
  </xsl:template>

  <xsl:template match="tei:divGen[@type='index']"/>

  <xsl:template name="logoPicture">
    <img src="jaco001d.gif" alt="" width="180"/>
  </xsl:template>

  <xsl:template name="hdr2">
    <xsl:comment>no nav </xsl:comment>
  </xsl:template>

  <xsl:template match="tei:hi[@rend='math']">
    <span class="math">
      <xsl:apply-templates/>
    </span>
  </xsl:template>

  <xsl:template name="makeHTMLHeading">
    <xsl:param name="text"/>
    <xsl:param name="class">title</xsl:param>
    <xsl:param name="level">1</xsl:param>
    <xsl:if test="not($text='')">
      <xsl:choose>
        <xsl:when test="$level='1'">
          <xsl:element name="h1">
            <xsl:attribute name="class">
              <xsl:value-of select="$class"/>
            </xsl:attribute>
            <a href="index.html">
              <xsl:value-of select="$text"/>
            </a>
          </xsl:element>
        </xsl:when>

        <xsl:otherwise>
          <xsl:element name="h{$level}">
            <xsl:attribute name="class">
              <xsl:value-of select="$class"/>
            </xsl:attribute>
            <xsl:value-of select="$text"/>
          </xsl:element>
        </xsl:otherwise>
      </xsl:choose>
    </xsl:if>
  </xsl:template>

  <xsl:template name="formatHeadingNumber">
    <xsl:param name="text"/>
    <xsl:param name="toc"/>
    <span class="headingNumber">
      <xsl:choose>
        <xsl:when test="$toc =''">
          <xsl:copy-of select="$text"/>
        </xsl:when>
        <xsl:when test="number(normalize-space($text))&lt;10">
          <xsl:text> </xsl:text>
          <xsl:copy-of select="$text"/>
        </xsl:when>
        <xsl:otherwise>
          <xsl:copy-of select="$text"/>
        </xsl:otherwise>
      </xsl:choose>
    </span>
  </xsl:template>

  <xsl:template name="navInterSep"> </xsl:template>

  <xsl:template name="generateSubTitle">
    <xsl:value-of select="tei:head"/>
  </xsl:template>

  <xsl:template name="printLink"/>

  <xsl:template match="tei:titlePage" mode="paging">
    <xsl:apply-templates select="."/>
  </xsl:template>

  <xsl:template name="generateEndLink">
      <xsl:param name="where"/>
      <xsl:choose>
	<xsl:when test="id($where)">
	  <xsl:apply-templates mode="generateLink" select="id($where)"/>
	</xsl:when>
	<xsl:otherwise>
	  <xsl:text>[ID </xsl:text>
	  <xsl:value-of select="$where"/>
	  <xsl:text> in TEI Guidelines]</xsl:text>
	</xsl:otherwise>
      </xsl:choose>
  </xsl:template>

</xsl:stylesheet>
