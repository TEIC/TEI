<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet xmlns="http://www.tei-c.org/ns/1.0"
		xmlns:xi="http://www.w3.org/2001/XInclude"
		xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
		xmlns:rng="http://relaxng.org/ns/structure/1.0"
		xmlns:xs="http://www.w3.org/2001/XMLSchema"
		xpath-default-namespace="http://www.tei-c.org/ns/1.0"
		exclude-result-prefixes="xi xs"
		version="2.0">
<!--
This software is dual-licensed:

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

     $Id$

identity transform
-->
  <xsl:output indent="no"/>

  <xsl:template match="/">
    <xsl:variable name="pass0">
      <xsl:apply-templates mode="xinclude"/>
    </xsl:variable>
    <xsl:apply-templates select="$pass0/*"/>
  </xsl:template>

  <xsl:template match="*" mode="xinclude">
    <xsl:copy>
      <xsl:apply-templates select="@*|*|processing-instruction()|comment()|text()" mode="xinclude"/>
    </xsl:copy>
  </xsl:template>

  <xsl:template match="comment()|@*|processing-instruction()|text()" mode="xinclude">
    <xsl:copy-of select="."/>
  </xsl:template>

  <xsl:template match="xi:include" mode="xinclude">
    <xsl:apply-templates
	select="doc(resolve-uri(@href,base-uri(/)))/*" mode="xinclude"/>
  </xsl:template>

  <!-- main pass -->
  <xsl:template match="comment()|@*|processing-instruction()|text()">
    <xsl:copy-of select="."/>
  </xsl:template>

  <xsl:template match="*">
    <xsl:copy>
      <xsl:apply-templates select="@*|*|processing-instruction()|comment()|text()"/>
    </xsl:copy>
  </xsl:template>

  <xsl:template match="processing-instruction('insert')">
    <xsl:variable name="info" select="doc(resolve-uri('../repodate.xml', static-base-uri()))"/>
    <xsl:choose>
      <xsl:when test=".='date'">
	<xsl:variable name="date">
	  <xsl:choose>
	    <xsl:when test="contains($info//*[local-name()='date'], 'T')">
	      <xsl:value-of select="substring-before($info//*[local-name()='date'],'T')"></xsl:value-of>
	    </xsl:when>
	    <xsl:otherwise>
	      <xsl:value-of select="substring-before($info//*[local-name()='date'],' ')"></xsl:value-of>
	    </xsl:otherwise>
	  </xsl:choose>
	</xsl:variable>
	<date when="{$date}">
	  <xsl:value-of select="format-date($date cast as xs:date, '[D1o] [MNn] [Y]', 'en', (), ())"/>
	</date>
      </xsl:when>
      <xsl:when test=".='year'">
        <xsl:variable name="date">
          <xsl:choose>
            <xsl:when test="contains($info//*[local-name()='date'], 'T')">
              <xsl:value-of select="substring-before($info//*[local-name()='date'],'T')"></xsl:value-of>
            </xsl:when>
            <xsl:otherwise>
              <xsl:value-of select="substring-before($info//*[local-name()='date'],' ')"></xsl:value-of>
            </xsl:otherwise>
          </xsl:choose>
        </xsl:variable>
	  <xsl:value-of select="format-date($date cast as xs:date, '[Y]', 'en', (), ())"/>
      </xsl:when>
      <xsl:when test=".='revision'">
	<xsl:variable name="r"
	    select="$info//*[local-name()='commit']/@revision"/>
        <xsl:choose>
          <xsl:when test="$info/*/@type = 'git'">
            <ref
              target="https://github.com/TEIC/TEI/commit/{$r}"><xsl:value-of select="$r"/></ref>
          </xsl:when>
          <xsl:otherwise>
            <ref
              target="http://sourceforge.net/p/tei/code/{$r}/tree/trunk/P5/"><xsl:value-of select="$r"/></ref>
          </xsl:otherwise>
        </xsl:choose>
      </xsl:when>
      <xsl:when test=".='version'">
	<xsl:variable name="v" select="normalize-space(unparsed-text(resolve-uri('../VERSION',static-base-uri())))"/>
	<ref   target="#ABTEI4">Version</ref>&#160;<ref target="../../readme-{replace($v,'[A-z]','')}.html"><xsl:value-of
	    select="$v"/></ref>
      </xsl:when>
      <xsl:when test=".='totalElements'"><xsl:value-of select="count(distinct-values(//elementSpec/@ident))"/></xsl:when>
      <xsl:when test=".='totalAttributes'"><xsl:value-of select="count(distinct-values(//attDef/@ident))"/></xsl:when>
      <xsl:when test=".='totalModelClasses'"><xsl:value-of select="count(distinct-values(//classSpec[@type='model']/@ident))"/></xsl:when>
      <xsl:when test=".='totalAttributeClasses'"><xsl:value-of select="count(distinct-values(//classSpec[@type='atts']/@ident))"/></xsl:when>
      <xsl:when test=".='totalDataSpec'"><xsl:value-of select="count(distinct-values(//dataSpec/@ident))"/></xsl:when>
      <xsl:when test=".='tab-content-models'"><xsl:call-template name="tab-content-models"/></xsl:when>
    </xsl:choose>
  </xsl:template>
  
<!-- Table of classes/content models for ST chapter -->
  <xsl:template name="tab-content-models">
    <table xml:id="tab-content-models" rend="display">
      
      <row role="label">
        <cell>Content model</cell>
        <cell>Number of elements using this</cell>
        <cell>Description</cell>
      </row>
      
      <xsl:variable name="rows">
        <xsl:for-each select="//macroSpec">
          <xsl:variable name="ident" select="@ident"/>
          
          <row>
            <cell><xsl:value-of select="@ident"/></cell>
            <cell><xsl:value-of select="count(distinct-values(//elementSpec[descendant::macroRef[@key=$ident]]/@ident))"/></cell>
            <cell><xsl:apply-templates select="desc[1]"/></cell>
          </row>
          
        </xsl:for-each>
      </xsl:variable>
      
      <xsl:for-each select="$rows/row">
        <xsl:sort select="cell[2] cast as xs:integer" order="descending"/>
        <xsl:if test="position() lt 8"><xsl:copy-of select="."/> 
        </xsl:if>
      </xsl:for-each>
    </table>
  </xsl:template>
</xsl:stylesheet>
