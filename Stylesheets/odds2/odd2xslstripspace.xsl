<?xml version="1.0" encoding="utf-8"?>
<XSL:stylesheet 
    xmlns:XSL="http://www.w3.org/1999/XSL/Transform" 
    xmlns:xsl="http://www.w3.org/1999/XSL/TransformAlias" 
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
  <XSL:import href="teiodds.xsl"/>
  <XSL:import href="../common2/i18n.xsl"/>
  <XSL:import href="../common2/tagdocs.xsl"/>
  <XSL:import href="../common2/tei-param.xsl"/>
  <XSL:param name="cellName">cell</XSL:param>
  <XSL:param name="codeName">code</XSL:param>
  <XSL:param name="colspan"/>
  <XSL:param name="ddName"/>
  <XSL:param name="divName">div</XSL:param>
  <XSL:param name="dlName"/>
  <XSL:param name="dtName"/>
  <XSL:param name="hiName">hi</XSL:param>
  <XSL:param name="itemName"/>
  <XSL:param name="labelName">label</XSL:param>
  <XSL:param name="outputNS"/>
  <XSL:param name="rendName">rend</XSL:param>
  <XSL:param name="rowName"/>
  <XSL:param name="sectionName"/>
  <XSL:param name="segName">seg</XSL:param>
  <XSL:param name="spaceCharacter"/>
  <XSL:param name="tableName"/>
  <XSL:param name="ulName"/>
  <XSL:param name="urlName"/>
  <XSL:param name="xrefName"/>
  <XSL:param name="coded">false</XSL:param>
  <XSL:param name="showListRef">false</XSL:param>
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
   
   <XSL:template name="bitOut">
      <XSL:param name="grammar"/>
      <XSL:param name="element"/>
      <XSL:param name="content"/>
   </XSL:template>

   <XSL:template name="typewriter">
      <XSL:param name="text"/>
   </XSL:template>

   <XSL:template name="emphasize">
      <XSL:param name="class"/>
      <XSL:param name="content"/>
   </XSL:template>
   <XSL:template name="emptySlash">
     <XSL:param name="name"/>
   </XSL:template>
   <XSL:template name="generateEndLink">
      <XSL:param name="where"/>
   </XSL:template>
   <XSL:template name="identifyElement">
      <XSL:param name="id"/>
   </XSL:template>
   <XSL:template name="makeExternalLink">
      <XSL:param name="ptr" as="xs:boolean" select="false()"/>
      <XSL:param name="dest"/>
   </XSL:template>
   <XSL:template name="makeInternalLink">
      <XSL:param name="ptr" as="xs:boolean"  select="false()"/>
      <XSL:param name="target"/>
      <XSL:param name="dest"/>
      <XSL:param name="class"/>
      <XSL:param name="body"/>
   </XSL:template>
   <XSL:template name="makeSectionHead">
      <XSL:param name="name"/>
      <XSL:param name="id"/>
   </XSL:template>
   <XSL:template name="refdoc"/>
   <XSL:template name="showRNC">
      <XSL:param name="style"/>
      <XSL:param name="contents"/>
      <XSL:value-of select="$contents"/>
   </XSL:template>
   <XSL:template name="showSpace">
   </XSL:template>
   <XSL:template name="showSpaceBetweenItems"/>
   <XSL:template name="specHook">
     <XSL:param name="name"/>
   </XSL:template>
  <XSL:output encoding="utf-8" indent="yes" method="xml"/>
  <XSL:param name="TEIC">false</XSL:param>
  <XSL:param name="verbose"/>
  <XSL:param name="outputDir"/>
  <XSL:param name="appendixWords"/>
  <XSL:template name="makeAnchor">
      <XSL:param name="name"/>
  </XSL:template>
  <XSL:param name="splitLevel">-1</XSL:param>
  <XSL:variable name="oddmode">dtd</XSL:variable>
  <XSL:variable name="filesuffix"/>
   <!-- get list of output files -->
  <XSL:variable name="linkColor"/>

  <XSL:template match="/">
    <xsl:strip-space>
      <XSL:attribute name="elements">
	<XSL:for-each select="//tei:elementSpec">
	  <XSL:sort select="@ident"/>
	  <XSL:variable name="id" select="@ident"/>
	  <XSL:choose>
	    <XSL:when test="tei:content//rng:ref[@name='macro.anyXML']"/>
	    <XSL:when test="tei:content/rng:empty"/>
	    <XSL:when test="tei:content/rng:text and count(tei:content/rng:*)=1"/>
	    <XSL:otherwise>
	      <XSL:variable name="Children">
		<XSL:for-each select="tei:content">
		  <XSL:call-template name="followRef"/>
		</XSL:for-each>
	      </XSL:variable>
	      <XSL:for-each select="$Children">
		<XSL:choose>
		  <XSL:when test="Element[@type='TEXT'] and
				  count(Element)=1"/>
		  <XSL:when test="count(Element)=0"/>
		  <XSL:when test="Element[@type='TEXT']"/>
		  <XSL:otherwise>
		    <XSL:value-of select="$id"/>
		    <XSL:text> </XSL:text>
		  </XSL:otherwise>
		</XSL:choose>
	      </XSL:for-each>
	    </XSL:otherwise>
	  </XSL:choose>
	</XSL:for-each>
      </XSL:attribute>
    </xsl:strip-space>
  </XSL:template>

</XSL:stylesheet>
