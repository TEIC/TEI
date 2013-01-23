<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet xmlns:fo="http://www.w3.org/1999/XSL/Format"
                xmlns:s="http://www.ascc.net/xml/schematron"
                xmlns:fotex="http://www.tug.org/fotex"
                xmlns:tei="http://www.tei-c.org/ns/1.0"
                
                xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
                xmlns:sch="http://purl.oclc.org/dsdl/schematron"
                xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                exclude-result-prefixes="s tei fotex xsi sch fo"
                version="2.0">
  <xsl:import href="tei-param.xsl"/>
  <xsl:import href="core.xsl"/>
  <xsl:import href="textstructure.xsl"/>
  <xsl:import href="header.xsl"/>
  <xsl:import href="linking.xsl"/>
  <xsl:import href="msdescription.xsl"/>
  <xsl:import href="figures.xsl"/>
  <xsl:import href="textcrit.xsl"/>
  <xsl:import href="i18n.xsl"/>
  <xsl:import href="functions.xsl"/>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" scope="stylesheet" type="stylesheet">
      <desc>
         <p> TEI stylesheet definitions common for all of HTML, FO and LaTeX
      outputs </p>
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
 <xsl:strip-space elements="tei:additional tei:address tei:adminInfo
			    tei:altGrp tei:altIdentifier tei:analytic
			    tei:app tei:appInfo tei:application
			    tei:arc tei:argument tei:attDef
			    tei:attList tei:availability tei:back
			    tei:biblFull tei:biblStruct tei:bicond
			    tei:binding tei:bindingDesc tei:body
			    tei:broadcast tei:cRefPattern tei:calendar
			    tei:calendarDesc tei:castGroup
			    tei:castList tei:category tei:certainty
			    tei:char tei:charDecl tei:charProp
			    tei:choice tei:cit tei:classDecl
			    tei:classSpec tei:classes tei:climate
			    tei:cond tei:constraintSpec tei:correction
			    tei:custodialHist tei:decoDesc
			    tei:dimensions tei:div tei:div1 tei:div2
			    tei:div3 tei:div4 tei:div5 tei:div6
			    tei:div7 tei:divGen tei:docTitle tei:eLeaf
			    tei:eTree tei:editionStmt
			    tei:editorialDecl tei:elementSpec
			    tei:encodingDesc tei:entry tei:epigraph
			    tei:epilogue tei:equipment tei:event
			    tei:exemplum tei:fDecl tei:fLib
			    tei:facsimile tei:figure tei:fileDesc
			    tei:floatingText tei:forest tei:front
			    tei:fs tei:fsConstraints tei:fsDecl
			    tei:fsdDecl tei:fvLib tei:gap tei:glyph
			    tei:graph tei:graphic tei:group
			    tei:handDesc tei:handNotes tei:history
			    tei:hom tei:hyphenation tei:iNode tei:if
			    tei:imprint tei:incident tei:index
			    tei:interpGrp tei:interpretation tei:join
			    tei:joinGrp tei:keywords tei:kinesic
			    tei:langKnowledge tei:langUsage
			    tei:layoutDesc tei:leaf tei:lg tei:linkGrp
			    tei:list tei:listBibl tei:listChange
			    tei:listEvent tei:listForest tei:listNym
			    tei:listOrg tei:listPerson tei:listPlace
			    tei:listRef tei:listRelation
			    tei:listTranspose tei:listWit tei:location
			    tei:locusGrp tei:macroSpec tei:metDecl
			    tei:moduleRef tei:moduleSpec tei:monogr
			    tei:msContents tei:msDesc tei:msIdentifier
			    tei:msItem tei:msItemStruct tei:msPart
			    tei:namespace tei:node tei:normalization
			    tei:notatedMusic tei:notesStmt tei:nym
			    tei:objectDesc tei:org tei:particDesc
			    tei:performance tei:person tei:personGrp
			    tei:physDesc tei:place tei:population
			    tei:postscript tei:precision
			    tei:profileDesc tei:projectDesc
			    tei:prologue tei:publicationStmt
			    tei:quotation tei:rdgGrp tei:recordHist
			    tei:recording tei:recordingStmt
			    tei:refsDecl tei:relatedItem tei:relation
			    tei:relationGrp tei:remarks tei:respStmt
			    tei:respons tei:revisionDesc tei:root
			    tei:row tei:samplingDecl tei:schemaSpec
			    tei:scriptDesc tei:scriptStmt tei:seal
			    tei:sealDesc tei:segmentation
			    tei:seriesStmt tei:set tei:setting
			    tei:settingDesc tei:sourceDesc
			    tei:sourceDoc tei:sp tei:spGrp tei:space
			    tei:spanGrp tei:specGrp tei:specList
			    tei:state tei:stdVals tei:subst
			    tei:substJoin tei:superEntry
			    tei:supportDesc tei:surface tei:surfaceGrp
			    tei:table tei:tagsDecl tei:taxonomy
			    tei:teiCorpus tei:teiHeader tei:terrain
			    tei:text tei:textClass tei:textDesc
			    tei:timeline tei:titlePage tei:titleStmt
			    tei:trait tei:transpose tei:tree
			    tei:triangle tei:typeDesc tei:vAlt
			    tei:vColl tei:vDefault tei:vLabel
			    tei:vMerge tei:vNot tei:vRange tei:valItem
			    tei:valList tei:vocal"/>

  <xsl:key name="APP" match="tei:app" use="1"/>
  <xsl:key name="TAGREND" match="tei:tagUsage[@render]" use="@gi"/>


  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" type="string">
      <desc> Name of XSLT processor.</desc>
   </doc>
  <xsl:variable name="processor">
      <xsl:value-of select="system-property('xsl:vendor')"/>
  </xsl:variable>

  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>[common] turn names into quote characters<param name="quote">quote</param>
      </desc>
   </doc>
  <xsl:template name="getQuote">
      <xsl:param name="quote"/>
      <xsl:choose>
         <xsl:when test="$quote='laquo'">«</xsl:when>
         <xsl:when test="$quote='ldquo'">“</xsl:when>
         <xsl:when test="$quote='ldquor'">„</xsl:when>
         <xsl:when test="$quote='lsaquo'">‹</xsl:when>
         <xsl:when test="$quote='lsquo'">‘</xsl:when>
         <xsl:when test="$quote='lsquor'">‚</xsl:when>
         <xsl:when test="$quote='mdash'">—</xsl:when>
         <xsl:when test="$quote='raquo'">»</xsl:when>
         <xsl:when test="$quote='rdquo'">”</xsl:when>
         <xsl:when test="$quote='rdquor'">‟</xsl:when>
         <xsl:when test="$quote='rsaquo'">›</xsl:when>
         <xsl:when test="$quote='rsquo'">’</xsl:when>
         <xsl:when test="$quote='rsquor'">‛</xsl:when>
         <xsl:otherwise><xsl:value-of select="$quote"/></xsl:otherwise>
      </xsl:choose>
  </xsl:template>
  
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>[common] work out the date and time, unless we have been
      told not to</desc>
   </doc>
  <xsl:template name="whatsTheDate">
    <xsl:choose>
      	<xsl:when test="$useFixedDate='true'">1970-01-01</xsl:when>
	<xsl:otherwise>
	  <xsl:value-of
	      select="format-dateTime(current-dateTime(),'[Y]-[M02]-[D02]T[H02]:[m02]:[s02]Z')"/>
	</xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  

  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>[common] whether to put quotes around something. check the
      quotation element, @rend, @rendition etc</desc>
   </doc>
   <xsl:template name="makeQuote">
     <xsl:choose>
       <xsl:when
	   test="/*/tei:teiHeader//tei:editorialDecl/tei:quotation[@marks='all']">
	 <xsl:apply-templates/>
       </xsl:when>
       <xsl:when test="@rend='inline'">
	 <xsl:value-of select="$preQuote"/>
	 <xsl:apply-templates/>
	 <xsl:value-of select="$postQuote"/>
       </xsl:when>
       <xsl:when test="$outputTarget='latex'">
	 <xsl:value-of select="$preQuote"/>
	 <xsl:apply-templates/>
	 <xsl:value-of select="$postQuote"/>
       </xsl:when>
       <xsl:when test="@rend or @rendition or
		       key('TAGREND',local-name(.))">
	 <xsl:apply-templates/>
       </xsl:when>
       <xsl:otherwise>
	 <xsl:value-of select="$preQuote"/>
	 <xsl:apply-templates/>
	 <xsl:value-of select="$postQuote"/>
       </xsl:otherwise>
     </xsl:choose>
   </xsl:template>
   
  <xsl:template name="tei:makeText">
    <xsl:param name="letters"/>
    <xsl:value-of select="$letters"/>
  </xsl:template>

  <xsl:template name="processAsSection">
    <xsl:param name="heading"/>
    <xsl:param name="level"/>
    <xsl:param name="implicitBlock"/>
    <xsl:apply-templates/>
  </xsl:template>
  <xsl:template name="processWithLabel">
    <xsl:param name="before"/>
    <xsl:value-of select="$before"/>
    <xsl:text>: </xsl:text>
    <xsl:apply-templates/>
  </xsl:template>
  <xsl:template name="processLiteral">
    <xsl:param name="text"/>
    <xsl:value-of select="$text"/>
  </xsl:template>

</xsl:stylesheet>
