<xsl:stylesheet 
    version="2.0"
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    xmlns:tei="http://www.tei-c.org/ns/1.0"
    xpath-default-namespace="http://www.tei-c.org/ns/1.0">

  <xsl:import href="../common/common.xsl"/>

 <xsl:strip-space elements="additional address adminInfo
			    altGrp altIdentifier analytic
			    app appInfo application
			    arc argument attDef
			    attList availability back
			    biblFull biblStruct bicond
			    binding bindingDesc body
			    broadcast cRefPattern calendar
			    calendarDesc castGroup
			    castList category certainty
			    char charDecl charProp
			    choice cit classDecl
			    classSpec classes climate
			    cond constraintSpec correction
			    custodialHist decoDesc
			    dimensions div div1 div2
			    div3 div4 div5 div6
			    div7 divGen docTitle eLeaf
			    eTree editionStmt
			    editorialDecl elementSpec
			    encodingDesc entry epigraph
			    epilogue equipment event
			    exemplum fDecl fLib
			    facsimile figure fileDesc
			    floatingText forest front
			    fs fsConstraints fsDecl
			    fsdDecl fvLib gap glyph
			    graph graphic group
			    handDesc handNotes history
			    hom hyphenation iNode if
			    imprint incident index
			    interpGrp interpretation join
			    joinGrp keywords kinesic
			    langKnowledge langUsage
			    layoutDesc leaf lg linkGrp
			    list listBibl listChange
			    listEvent listForest listNym
			    listOrg listPerson listPlace
			    listRef listRelation
			    listTranspose listWit location
			    locusGrp macroSpec metDecl
			    moduleRef moduleSpec monogr
			    msContents msDesc msIdentifier
			    msItem msItemStruct msPart
			    namespace node normalization
			    notatedMusic notesStmt nym
			    objectDesc org particDesc
			    performance person personGrp
			    physDesc place population
			    postscript precision
			    profileDesc projectDesc
			    prologue publicationStmt
			    quotation rdgGrp recordHist
			    recording recordingStmt
			    refsDecl relatedItem relation
			    relationGrp remarks respStmt
			    respons revisionDesc root
			    row samplingDecl schemaSpec
			    scriptDesc scriptStmt seal
			    sealDesc segmentation
			    seriesStmt set setting
			    settingDesc sourceDesc
			    sourceDoc sp spGrp space
			    spanGrp specGrp specList
			    state stdVals subst
			    substJoin superEntry
			    supportDesc surface surfaceGrp
			    table tagsDecl taxonomy
			    teiCorpus teiHeader terrain
			    text textClass textDesc
			    timeline titlePage titleStmt
			    trait transpose tree
			    triangle typeDesc vAlt
			    vColl vDefault vLabel
			    vMerge vNot vRange valItem
			    valList vocal"/>

  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" scope="stylesheet" type="stylesheet">
      <desc>

         <p>This software is dual-licensed:

1. Distributed under a Creative Commons Attribution-ShareAlike 3.0
Unported License http://creativecommons.org/licenses/by-sa/3.0/ 

2. http://www.opensource.org/licenses/BSD-2-Clause
		


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
         
         <p>Copyright: 2013, TEI Consortium</p>
      </desc>
   </doc>

 <xsl:output method="text"/>

 <xsl:template match="teiHeader"/>
 <xsl:template match="figDesc"/>
 <xsl:template match="gap/desc"/>
 <xsl:template match="choice">
     <xsl:apply-templates select="*[1]"/>
 </xsl:template>
 <xsl:template match="speaker"/>
 <xsl:template match="facsimile"/>

<xsl:template name="appReading">
     <xsl:param name="lemma"/>
     <xsl:param name="lemmawitness"/>
     <xsl:param name="readings"/>
</xsl:template>

<xsl:template name="emphasize">
      <xsl:param name="class"/>
      <xsl:param name="content"/>
      <xsl:choose>
        <xsl:when test="$class='titlem'">
          <xsl:text>_</xsl:text>
            <xsl:copy-of select="$content"/>
            <xsl:text>_</xsl:text>
        </xsl:when>
        <xsl:when test="$class='titlej'">
          <xsl:text>_</xsl:text>
          <xsl:copy-of select="$content"/>
          <xsl:text>_</xsl:text>
        </xsl:when>
        <xsl:when test="$class='titlea'">
          <xsl:text>‘</xsl:text>
	  <xsl:copy-of select="$content"/>
          <xsl:text>’</xsl:text>
        </xsl:when>
        <xsl:otherwise>
          <xsl:copy-of select="$content"/>
        </xsl:otherwise>
      </xsl:choose>      
</xsl:template>

<xsl:template name="generateEndLink">
  <xsl:param name="where"/>
</xsl:template>

<xsl:template name="horizontalRule">
  <xsl:text>&#10;---&#10;</xsl:text>
</xsl:template>

<xsl:template name="makeBlock">
		<xsl:param name="style"/>
		<xsl:apply-templates/>
		<xsl:call-template name="newline"/>
</xsl:template>

<xsl:template name="makeInline">
      <xsl:param name="before"/>
      <xsl:param name="style"/>
      <xsl:param name="after"/>
</xsl:template>

<xsl:template name="makeSpan">
  <xsl:apply-templates/>
</xsl:template>

<xsl:template match="p">
  <xsl:call-template name="newline"/>
  <xsl:apply-templates/>
  <xsl:call-template name="newline"/>
</xsl:template>

  <xsl:template match="head">
      <xsl:choose>
         <xsl:when test="parent::castList"/>
         <xsl:when test="parent::figure"/>
         <xsl:when test="parent::list"/>
         <xsl:when test="parent::front or parent::body or
			 parent::back or parent::lg"> &#10;*<xsl:apply-templates/>&#10; </xsl:when>
         <xsl:when test="parent::table"/>
         <xsl:otherwise>
            <xsl:variable name="depth">
               <xsl:apply-templates mode="depth" select=".."/>
            </xsl:variable>
            <xsl:call-template name="newline"/>
	    <xsl:choose>
	      <xsl:when test="$depth=0">#</xsl:when>
	      <xsl:when test="$depth=1">##</xsl:when>
	      <xsl:when test="$depth=2">###</xsl:when>
	      <xsl:when test="$depth=3">####</xsl:when>
	      <xsl:when test="$depth=4">#####</xsl:when>
		  <xsl:otherwise>#</xsl:otherwise>
		</xsl:choose>
		<xsl:text> </xsl:text>
	    <xsl:apply-templates/>
	    <xsl:call-template name="newline"/>
	 </xsl:otherwise>
      </xsl:choose>
  </xsl:template>

  <xsl:template match="tei:item|tei:biblStruct">
    <xsl:call-template name="newline"/>
    <xsl:choose>
      <xsl:when test="tei:isOrderedList(..)">1. </xsl:when>
      <xsl:otherwise>* </xsl:otherwise>
    </xsl:choose>
    <xsl:apply-templates/>
    <xsl:call-template name="newline"/>
  </xsl:template>

    <xsl:template name="newline">
      <xsl:text>&#10;</xsl:text>
    </xsl:template>

	<xsl:template match="*"><xsl:apply-templates/></xsl:template>
	
</xsl:stylesheet>
