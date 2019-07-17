<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet 
    xmlns:xs="http://www.w3.org/2001/XMLSchema"
    xpath-default-namespace="http://www.tei-c.org/ns/1.0"
    xmlns:tei="http://www.tei-c.org/ns/1.0"
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    version="2.0">
  <xsl:import href="../common/functions.xsl"/>

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

   <xsl:output encoding="utf-8" method="text"/>
   <xsl:param name="makeCSV">false</xsl:param>
   <xsl:param name="oneword">false</xsl:param>
   <xsl:variable name="q">"</xsl:variable>

   <xsl:template match="/">
     <xsl:variable name="pass0">
       <xsl:apply-templates select="*" mode="preflight"/>
     </xsl:variable>
     <xsl:apply-templates select="$pass0/*"/>
   </xsl:template>

   <xsl:template match="teiHeader"/>

   <xsl:template match="figDesc"/>

   <xsl:template match="gap/desc"/>

   <xsl:template match="choice">
     <xsl:apply-templates select="*[1]"/>
   </xsl:template>

   <xsl:template match="speaker"/>

   <xsl:template match="facsimile"/>

  <!-- handle text nodes and perform any necessary character
       conversion -->

<!-- also use this chance to break text into words and put them one
per line, if requested by "oneword" parameter -->

<!-- if makeCSV is requested, each word is in a line of a CSV file
     by itself followed by its position within the hierachy of the XML
     document
-->
  <xsl:function name="tei:escapeChars" as="xs:string">
    <xsl:param name="letters"/>
    <xsl:param name="context"/>
     <xsl:choose>
       <xsl:when test="$oneword='true'">
	 <xsl:variable name="foo">
	    <xsl:analyze-string
		select="normalize-space(translate($letters,'ſ','s'))"
		regex="(\w+)">
	      <xsl:matching-substring>
		<xsl:value-of select="regex-group(1)"/>
		<xsl:text>&#10;</xsl:text>
	      </xsl:matching-substring>
	    </xsl:analyze-string>
	 </xsl:variable>
	 <xsl:value-of select="$foo"/>
       </xsl:when>
       <xsl:when test="normalize-space($letters)=''">
	 <xsl:text/>
       </xsl:when>
       <xsl:when test="$makeCSV='true'">
	 <xsl:variable name="result">
	 <xsl:text>"</xsl:text>
	 <xsl:value-of select="replace(normalize-space($letters),'$q','$q$q')"/>
	 <xsl:text>","</xsl:text>
	 <xsl:for-each select="$context/ancestor::*">
	   <xsl:value-of select="name()"/>
	   <xsl:text>[</xsl:text>
	   <xsl:number/>
	   <xsl:text>]/</xsl:text>
	 </xsl:for-each>
	 <xsl:text>"&#10;</xsl:text>
	 </xsl:variable>
	 <xsl:value-of select="$result"/>
       </xsl:when>
       <xsl:otherwise>
	 <xsl:sequence select="concat(normalize-space(translate($letters,'ſ','s')),'&#10;')"/>
       </xsl:otherwise>
       </xsl:choose>
  </xsl:function>

   <xsl:template match="@*" mode="preflight">
     <xsl:copy-of select="."/>
   </xsl:template>

   <xsl:template match="text()" mode="preflight">
     <xsl:copy-of select="."/>
   </xsl:template>
   
   <xsl:template
       match="g|seg[@rend='decorInit']|hi[@rend='sup']|hi[@rend='sub']" mode="preflight">
     <xsl:apply-templates/>
   </xsl:template>

   <xsl:template match="lb[@rend='hidden']" mode="preflight"/>

   <xsl:template match="lb|pb|gb" mode="preflight">
     <xsl:text> </xsl:text>
   </xsl:template>

   <xsl:template match="*" mode="preflight">
     <xsl:copy>
       <xsl:apply-templates select="@*|*|text()" mode="preflight"/>
     </xsl:copy>
   </xsl:template>   

    <xsl:template name="space"/>
</xsl:stylesheet>
