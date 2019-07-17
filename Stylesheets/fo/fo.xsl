<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet 
    xmlns:fo="http://www.w3.org/1999/XSL/Format"
    xmlns:xs="http://www.w3.org/2001/XMLSchema" 
    xmlns:m="http://www.w3.org/1998/Math/MathML"
    xmlns:tei="http://www.tei-c.org/ns/1.0"
    xmlns:fotex="http://www.tug.org/fotex"
    xmlns="http://www.w3.org/1999/XSL/Format"
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    exclude-result-prefixes="tei fotex m xs fo"
    version="2.0">
  <xsl:import href="../common/common.xsl"/>
  <xsl:import href="../common/verbatim.xsl"/>
  <xsl:import href="fo_param.xsl"/>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" scope="stylesheet" type="stylesheet">
      <desc>
         <p>
    TEI stylesheet making XSL-FO output.
      </p>
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
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" type="string">
      <desc>Stylesheet constant for the input document.</desc>
   </doc>
  <xsl:variable name="top" select="/"/>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" type="string">
      <desc>Stylesheet constant for table specifications</desc>
   </doc>
  <xsl:variable name="tableSpecs">
      <xsl:choose>
         <xsl:when test="not($readColSpecFile='')">
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
  
  <xsl:include href="fo_core.xsl"/>
  <xsl:include href="fo_corpus.xsl"/>
  <xsl:include href="fo_drama.xsl"/>
  <xsl:include href="fo_figures.xsl"/>
  <xsl:include href="fo_header.xsl"/>
  <xsl:include href="fo_linking.xsl"/>
  <xsl:include href="fo_namesdates.xsl"/>
  <xsl:include href="fo_tagdocs.xsl"/>
  <xsl:include href="fo_textcrit.xsl"/>
  <xsl:include href="fo_textstructure.xsl"/>
  <xsl:include href="fo_transcr.xsl"/>
  <xsl:include href="fo_verse.xsl"/>

  <xsl:template name="horizontalRule">
    <leader rule-thickness="1pt"/>
  </xsl:template>
  <xsl:template name="makeBlock">
    <xsl:param name="style"/>
      <block font-size="{tei:fontSize($style)}"
	     font-style="{tei:fontStyle($style)}"
	     font-weight="{tei:fontWeight($style)}">
	<xsl:if test="@xml:id">
	  <xsl:attribute name="id">
	    <xsl:value-of select="@xml:id"/>
	  </xsl:attribute>
	</xsl:if>
	<xsl:apply-templates/>
      </block>
  </xsl:template>
  
  <xsl:template name="makeSection">
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
    
    <xsl:template name="makeWithLabel">
      <xsl:param name="before"/>
      <inline font-style="italic">
         <xsl:value-of select="$before"/>
      </inline>
      <xsl:text>: </xsl:text>
      <xsl:value-of select="normalize-space(.)"/>
    </xsl:template>

  <xsl:template name="makeInline">
    <xsl:param name="before"/>
    <xsl:param name="after"/>
    <xsl:param name="style"/>
    <xsl:value-of select="$before"/>
      <inline font-style="{tei:fontStyle($style)}" font-weight="{tei:fontWeight($style)}">
	<xsl:apply-templates/>
      </inline>
    <xsl:value-of select="$after"/>
  </xsl:template>


  <xsl:function name="tei:fontSize" as="xs:string">
    <xsl:param name="style"/>
    <xsl:choose>
      <xsl:when test="$style='docAuthor'">14pt</xsl:when>
      <xsl:when test="$style='docTitle'">16pt</xsl:when>
      <xsl:when test="$style='titlePart'">16pt</xsl:when>
      <xsl:when test="$style='docDate'">14pt</xsl:when>
      <xsl:otherwise>12pt</xsl:otherwise>
    </xsl:choose>
  </xsl:function>

  <xsl:function name="tei:fontStyle" as="xs:string">
    <xsl:param name="style"/>
    <xsl:choose>
      <xsl:when test="$style=('bibl','docAuthor','titlem','italic','mentioned','term','foreign')">italic</xsl:when>
      <xsl:otherwise>normal</xsl:otherwise>
    </xsl:choose>
  </xsl:function>

  <xsl:function name="tei:fontWeight" as="xs:string">
    <xsl:param name="style"/>
    <xsl:choose>
      <xsl:when test="$style='docTitle'">bold</xsl:when>
      <xsl:when test="$style='titlePart'">bold</xsl:when>
      <xsl:when test="$style='bold'">bold</xsl:when>
      <xsl:otherwise>normal</xsl:otherwise>
    </xsl:choose>
  </xsl:function>

  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>[fo] work out column widths </desc>
   </doc>
  <xsl:template name="calculateTableSpecs">
      <xsl:variable name="tds">
       	<xsl:for-each select="tei:row">
       	  <xsl:variable name="row">
       	    <xsl:for-each select="tei:cell">
       	      <xsl:variable name="stuff">
       		<xsl:apply-templates/>
       	      </xsl:variable>
       	      <cell>
       		<xsl:value-of select="string-length($stuff)"/>
       	      </cell>
       	      <xsl:if test="@cols">
       		<xsl:variable name="c" select="xs:integer(@cols) - 1 "/>
       		<xsl:for-each select="1 to $c">
       		  <cell>0</cell>
       		</xsl:for-each>
       	      </xsl:if>
       	    </xsl:for-each>
       	  </xsl:variable>
       	  <xsl:for-each select="$row/fo:cell">
       	    <cell col="{position()}">
       	      <xsl:value-of select="."/>
       	    </cell>
       	  </xsl:for-each>
       	</xsl:for-each>
      </xsl:variable>
      <xsl:variable name="total">
	       <xsl:value-of select="sum($tds/fo:cell)"/>
      </xsl:variable>
   
<!-- Intervention by MDH working on FO output for FOP 2016-12-28: the algorithm
     below works OK except in cases where it creates a column too small to hold 
     a single word. In an effort to avoid this, we'll set a minimum width of 
     10% on any column, and when the algorithm generates a column smaller than
     this, we'll simply omit the columns.
     Note: this could be improved by a better approach which creates adjusted 
     but still proportional column widths, when someone has time to do it.
    -->
    <xsl:variable name="proposedCols">
      <xsl:for-each select="$tds/fo:cell">
           <xsl:sort select="@col" data-type="number"/>
           <xsl:variable name="c" select="@col"/>
           <xsl:if test="not(preceding-sibling::fo:cell[$c=@col])">
              <xsl:variable name="len">
                 <xsl:value-of select="sum(following-sibling::fo:cell[$c=@col]) + current()"/>
              </xsl:variable>
              <table-column column-number="{@col}" column-width="{$len div $total * 100}%">
                 <xsl:if test="$foEngine='passivetex'">
                    <xsl:attribute name="column-align" namespace="http://www.tug.org/fotex">L</xsl:attribute>
                 </xsl:if>
              </table-column>
           </xsl:if>
        </xsl:for-each>
    </xsl:variable>
    <xsl:choose>
      <xsl:when test="$proposedCols/fo:table-column/@column-width[xs:float(substring-before(., '%')) lt 10]">
        <!-- Drop the column width definitions altogether. -->
      </xsl:when>
      <xsl:otherwise><xsl:sequence select="$proposedCols"/></xsl:otherwise>
    </xsl:choose>
    
      <xsl:text>&#10;</xsl:text>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>[fo] </desc>
   </doc>
  <xsl:template name="deriveColSpecs">
      <xsl:choose>
	 <xsl:when test="tei:match(@rend,'wovenodd')">
	   <table-column column-number="1" column-width="25%"/>
	   <table-column column-number="2" column-width="75%"/>
	 </xsl:when>
	 <xsl:when test="tei:match(@rend,'attDef')">
	   <table-column column-number="1" column-width="20%"/>
	   <table-column column-number="2" column-width="80%"/>
	 </xsl:when>
	 <xsl:when test="tei:match(@rend,'attList')">
	   <table-column column-number="1" column-width="20%"/>
	   <table-column column-number="2" column-width="80%"/>
	 </xsl:when>
         <xsl:when test="not($readColSpecFile='')">
	   <xsl:variable name="no">
             <xsl:call-template name="generateTableID"/>
	   </xsl:variable>
           <xsl:choose>
               <xsl:when test="count($tableSpecs/Info/TableSpec[$no=@xml:id]) &gt; 0">
                  <xsl:for-each select="$tableSpecs/Info/TableSpec[$no=@xml:id]/table-column">
                     <xsl:copy-of select="."/>
                  </xsl:for-each>
               </xsl:when>
               <xsl:otherwise>
		 <!--<xsl:message>Build specs for Table <xsl:value-of select="$no"/></xsl:message>-->	       
               <xsl:call-template name="calculateTableSpecs"/>
               </xsl:otherwise>
            </xsl:choose>
         </xsl:when>
         <xsl:otherwise>
	   <!--<xsl:message>Build specs for Table <xsl:value-of select="$no"/></xsl:message>-->
           <xsl:call-template name="calculateTableSpecs"/>
         </xsl:otherwise>
      </xsl:choose>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>[fo] </desc>
   </doc>
  <xsl:template name="generateTableID">
      <xsl:choose>
         <xsl:when test="@xml:id">
            <xsl:value-of select="@xml:id"/>
         </xsl:when>
         <xsl:otherwise>
            <xsl:text>Table-</xsl:text>
            <xsl:number level="any"/>
         </xsl:otherwise>
      </xsl:choose>
  </xsl:template>
</xsl:stylesheet>
