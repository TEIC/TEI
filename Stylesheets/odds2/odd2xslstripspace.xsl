<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet 
    xmlns:gen="http://www.w3.org/1999/XSL/TransformAlias" 
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform" 
    xmlns:a="http://relaxng.org/ns/compatibility/annotations/1.0"
    xmlns:fo="http://www.w3.org/1999/XSL/Format" 
    xmlns:html="http://www.w3.org/1999/xhtml" 
    xmlns:i="http://www.iso.org/ns/1.0"
    xmlns:rng="http://relaxng.org/ns/structure/1.0"
    xmlns:s="http://www.ascc.net/xml/schematron" 
    xmlns:sch="http://purl.oclc.org/dsdl/schematron" 
    xmlns:tei="http://www.tei-c.org/ns/1.0"
    xmlns:teix="http://www.tei-c.org/ns/Examples" 
    xmlns:xi="http://www.w3.org/2001/XInclude"
    xmlns:xs="http://www.w3.org/2001/XMLSchema"
    exclude-result-prefixes="a fo html i rng s sch tei teix xi xs xsl" 
    version="2.0">
  <xsl:import href="teiodds.xsl"/>
  <xsl:import href="../common2/tagdocs.xsl"/>
  <xsl:import href="../common2/tei-param.xsl"/>
  <xsl:namespace-alias stylesheet-prefix="gen" result-prefix="xsl"/>
  <xsl:param name="cellName">cell</xsl:param>
  <xsl:param name="codeName">code</xsl:param>
  <xsl:param name="colspan"/>
  <xsl:param name="ddName"/>
  <xsl:param name="divName">div</xsl:param>
  <xsl:param name="dlName"/>
  <xsl:param name="dtName"/>
  <xsl:param name="hiName">hi</xsl:param>
  <xsl:param name="itemName"/>
  <xsl:param name="labelName">label</xsl:param>
  <xsl:param name="outputNS"/>
  <xsl:param name="rendName">rend</xsl:param>
  <xsl:param name="rowName"/>
  <xsl:param name="sectionName"/>
  <xsl:param name="segName">seg</xsl:param>
  <xsl:param name="spaceCharacter"/>
  <xsl:param name="tableName"/>
  <xsl:param name="ulName"/>
  <xsl:param name="urlName"/>
  <xsl:param name="xrefName"/>
  <xsl:param name="coded">false</xsl:param>
  <xsl:param name="showListRef">false</xsl:param>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" scope="stylesheet" type="stylesheet">
      <desc>
         <p> TEI stylesheet for making JSON from ODD </p>
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
   
   <xsl:template name="bitOut">
      <xsl:param name="grammar"/>
      <xsl:param name="element"/>
      <xsl:param name="content"/>
   </xsl:template>

   <xsl:template name="typewriter">
      <xsl:param name="text"/>
   </xsl:template>

   <xsl:template name="emphasize">
      <xsl:param name="class"/>
      <xsl:param name="content"/>
   </xsl:template>
   <xsl:template name="emptySlash">
     <xsl:param name="name"/>
   </xsl:template>
   <xsl:template name="generateEndLink">
      <xsl:param name="where"/>
   </xsl:template>
   <xsl:template name="identifyElement">
      <xsl:param name="id"/>
   </xsl:template>
   <xsl:template name="makeExternalLink">
      <xsl:param name="ptr" as="xs:boolean" select="false()"/>
      <xsl:param name="dest"/>
   </xsl:template>
   <xsl:template name="makeInternalLink">
      <xsl:param name="ptr" as="xs:boolean"  select="false()"/>
      <xsl:param name="target"/>
      <xsl:param name="dest"/>
      <xsl:param name="class"/>
      <xsl:param name="body"/>
   </xsl:template>
   <xsl:template name="makeSectionHead">
      <xsl:param name="name"/>
      <xsl:param name="id"/>
   </xsl:template>
   <xsl:template name="refdoc"/>
   <xsl:template name="showRNC">
      <xsl:param name="style"/>
      <xsl:param name="contents"/>
      <xsl:value-of select="$contents"/>
   </xsl:template>
   <xsl:template name="showSpace">
   </xsl:template>
   <xsl:template name="showSpaceBetweenItems"/>
   <xsl:template name="specHook">
     <xsl:param name="name"/>
   </xsl:template>
  <xsl:output encoding="utf-8" indent="yes" method="xml"/>
  <xsl:param name="TEIC">false</xsl:param>
  <xsl:param name="verbose"/>
  <xsl:param name="outputDir"/>
  <xsl:param name="appendixWords"/>
  <xsl:template name="makeAnchor">
      <xsl:param name="name"/>
  </xsl:template>
  <xsl:param name="splitLevel">-1</xsl:param>
  <xsl:variable name="oddmode">dtd</xsl:variable>
  <xsl:variable name="filesuffix"/>
   <!-- get list of output files -->
  <xsl:variable name="linkColor"/>

  <xsl:template match="/">
    <gen:strip-space>
      <xsl:attribute name="elements">
	<xsl:for-each select="//tei:elementSpec">
	  <xsl:sort select="@ident"/>
	  <xsl:variable name="id" select="@ident"/>
	  <xsl:choose>
	    <xsl:when test="tei:content//rng:ref[@name='macro.anyXML']"/>
	    <xsl:when test="tei:content/rng:empty"/>
	    <xsl:when test="tei:content/rng:text and count(tei:content/rng:*)=1"/>
	    <xsl:otherwise>
	      <xsl:variable name="Children">
		<xsl:for-each select="tei:content">
		  <xsl:call-template name="followRef"/>
		</xsl:for-each>
	      </xsl:variable>
	      <xsl:for-each select="$Children">
		<xsl:choose>
		  <xsl:when test="Element[@type='TEXT'] and
				  count(Element)=1"/>
		  <xsl:when test="count(Element)=0"/>
		  <xsl:when test="Element[@type='TEXT']"/>
		  <xsl:otherwise>
		    <xsl:value-of select="$id"/>
		    <xsl:text> </xsl:text>
		  </xsl:otherwise>
		</xsl:choose>
	      </xsl:for-each>
	    </xsl:otherwise>
	  </xsl:choose>
	</xsl:for-each>
      </xsl:attribute>
    </gen:strip-space>
  </xsl:template>

</xsl:stylesheet>
