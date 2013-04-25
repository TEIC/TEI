<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet xmlns="http://www.tei-c.org/ns/1.0"
		xmlns:xi="http://www.w3.org/2001/XInclude"
		xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
		xmlns:rng="http://relaxng.org/ns/structure/1.0"
		xmlns:xs="http://www.w3.org/2001/XMLSchema"
		xpath-default-namespace="http://www.tei-c.org/ns/1.0"
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

  <xsl:template match="processing-instruction()[name()='tei']">
    <xsl:choose>
      <xsl:when test=".='version'">
	<xsl:value-of
	    select="normalize-space(unparsed-text(resolve-uri('../VERSION',base-uri(/))))"/>
      </xsl:when>
    </xsl:choose>
  </xsl:template>
  
  <xsl:template match="processing-instruction('insert')">
    <xsl:choose>
      <xsl:when test=".='totalElements'"><xsl:value-of select="count(distinct-values(//elementSpec/@ident))"/></xsl:when>
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
        <xsl:for-each select="//macroSpec[@type='pe']">
          <xsl:variable name="ident" select="@ident"/>
          
          <row>
            <cell><xsl:value-of select="@ident"/></cell>
            <cell><xsl:value-of select="count(distinct-values(//elementSpec[descendant::rng:ref[@name=$ident]]/@ident))"/></cell>
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
