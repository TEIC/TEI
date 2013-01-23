<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet 
                xmlns:m="http://www.w3.org/1998/Math/MathML"
                xmlns:tei="http://www.tei-c.org/ns/1.0"
                xmlns:fotex="http://www.tug.org/fotex"
                xmlns="http://www.w3.org/1999/XSL/Format"
                xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                exclude-result-prefixes="tei fotex m"
                version="2.0">
  <xsl:import href="../common2/tei.xsl"/>
  <xsl:import href="tei-param.xsl"/>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" scope="stylesheet" type="stylesheet">
      <desc>
         <p>
    TEI stylesheet making XSL-FO output.
      </p>
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
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" type="string">
      <desc>
    Stylesheet constant for the input document.
  </desc>
   </doc>
  <xsl:variable name="top" select="/"/>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" type="string">
      <desc>
    Stylesheet constant for table specifications
  </desc>
   </doc>
  <xsl:variable name="tableSpecs">
      <xsl:choose>
         <xsl:when test="$readColSpecFile">
            <xsl:copy-of select="document($readColSpecFile,$top)/Info"/>
         </xsl:when>
         <xsl:otherwise>
            <Info/>
         </xsl:otherwise>
      </xsl:choose>
  </xsl:variable>
  <xsl:output indent="no" encoding="utf-8"/>
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

  <xsl:key name="DIVS"
            match="tei:div|tei:div1|tei:div2|tei:div3|tei:div4|tei:div5"
            use="'1'"/>
  <xsl:include href="tei-makecolspec.xsl"/>
  <xsl:include href="core.xsl"/>
  <xsl:include href="corpus.xsl"/>
  <xsl:include href="drama.xsl"/>
  <xsl:include href="figures.xsl"/>
  <xsl:include href="header.xsl"/>
  <xsl:include href="linking.xsl"/>
  <xsl:include href="namesdates.xsl"/>
  <xsl:include href="tagdocs.xsl"/>
  <xsl:include href="textcrit.xsl"/>
  <xsl:include href="textstructure.xsl"/>
  <xsl:include href="transcr.xsl"/>
  <xsl:include href="verse.xsl"/>

  <xsl:template name="processBlock">
    <xsl:param name="style"/>
    <xsl:apply-templates/>
  </xsl:template>


    <xsl:template name="processAsSection">
      <xsl:param name="level"/>
      <xsl:param name="heading"/>
      <xsl:param name="implicitBlock">false</xsl:param>
      <xsl:variable name="temp">
	<tei:head><xsl:value-of select="$heading"/></tei:head>
      </xsl:variable>
      <xsl:for-each select="$temp">
	<xsl:call-template name="NumberedHeading">
	  <xsl:with-param name="level" select="$level"/>
	</xsl:call-template>       
      </xsl:for-each>
      <block>
	<xsl:apply-templates/>
      </block>
    </xsl:template>
    
    <xsl:template name="processWithLabel">
      <xsl:param name="before"/>
      <i>
         <xsl:value-of select="$before"/>
      </i>
      <xsl:text>: </xsl:text>
      <xsl:value-of select="normalize-space(.)"/>
    </xsl:template>

  <xsl:template name="processInline">
    <xsl:param name="before"/>
    <xsl:param name="after"/>
    <xsl:param name="style"/>
    <xsl:value-of select="$before"/>
    <xsl:apply-templates/>
    <xsl:value-of select="$after"/>
  </xsl:template>

</xsl:stylesheet>