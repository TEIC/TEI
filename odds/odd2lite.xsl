<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet xmlns="http://www.tei-c.org/ns/1.0"
                xmlns:a="http://relaxng.org/ns/compatibility/annotations/1.0"
		xmlns:dcr="http://www.isocat.org/ns/dcr"
                xmlns:fo="http://www.w3.org/1999/XSL/Format"
                xmlns:html="http://www.w3.org/1999/xhtml"
		xmlns:s="http://www.ascc.net/xml/schematron" 
                xmlns:rng="http://relaxng.org/ns/structure/1.0"
                xmlns:tei="http://www.tei-c.org/ns/1.0"
                xmlns:teix="http://www.tei-c.org/ns/Examples"
                xmlns:xs="http://www.w3.org/2001/XMLSchema"
                xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
                xmlns:sch="http://purl.oclc.org/dsdl/schematron"
                exclude-result-prefixes="fo a tei html s rng sch xsi
					 teix xs dcr"
                version="2.0">
  <xsl:import href="../common/verbatim.xsl"/>
  <xsl:import href="teiodds.xsl"/> 
  <xsl:import href="classatts.xsl"/>
  <xsl:import href="../common/common.xsl"/>
  <xsl:include href="../common/common_tagdocs.xsl"/>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" scope="stylesheet" type="stylesheet">
      <desc>
         <p> TEI stylesheet for making TEI Lite XML from ODD </p>
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
  <xsl:param name="summaryDoc">false</xsl:param>

  <xsl:param name="cellName">cell</xsl:param>
  <xsl:param name="codeName">code</xsl:param>
  <xsl:param name="colspan">cols</xsl:param>
  <xsl:param name="ddName">item</xsl:param>
  <xsl:param name="displayMode">rnc</xsl:param>
  <xsl:param name="divName">ab</xsl:param>
  <xsl:param name="dlName">list</xsl:param>
  <xsl:param name="dtName">label</xsl:param>
  <xsl:param name="hiName">hi</xsl:param>
  <xsl:param name="idPrefix">TEI.</xsl:param>
  <xsl:param name="itemName">item</xsl:param>
  <xsl:param name="labelName">label</xsl:param>
  <xsl:param name="oddmode">tei</xsl:param>
  <xsl:param name="outputNS">http://www.tei-c.org/ns/1.0</xsl:param>
  <xsl:param name="rendName">rend</xsl:param>
  <xsl:param name="rowName">row</xsl:param>
  <xsl:param name="sectionName">div</xsl:param>
  <xsl:param name="segName">seg</xsl:param>
  <xsl:param name="spaceCharacter">&#160;</xsl:param>
  <xsl:param name="splitLevel">-1</xsl:param>
  <xsl:param name="tableName">table</xsl:param>
  <xsl:param name="ulName">list</xsl:param>
  <xsl:param name="urlName">target</xsl:param>
  <xsl:param name="xrefName">ref</xsl:param>

  <xsl:variable name="TOP" select="/"/>
  <xsl:key name="ALTIDENTS" match="tei:elementSpec[tei:altIdent]" use="@ident"/>
  <xsl:key name="ALTIDENTS"
	   match="tei:elementSpec/tei:attList/tei:attDef[tei:altIdent]"
	   use="concat(ancestor::tei:elementSpec/@ident,@ident)"/>
  <xsl:key name="ALTIDENTS"
	   match="tei:classSpec/tei:attList/tei:attDef[tei:altIdent]" 
	   use="concat(ancestor::tei:classSpec/@ident,@ident)"/>


  <xsl:template match="/">
    <xsl:variable name="resolvedClassatts">
      <xsl:apply-templates  mode="classatts"/>
    </xsl:variable>
    <xsl:for-each select="$resolvedClassatts">
      <xsl:apply-templates/>
    </xsl:for-each>
  </xsl:template>


  <xsl:template name="identifyElement">
      <xsl:param name="id"/>
      <xsl:attribute name="xml:id">
         <xsl:value-of select="$id"/>
      </xsl:attribute>
  </xsl:template>
  <xsl:template name="verbatim-lineBreak">
      <xsl:param name="id"/>
      <xsl:text>&#10;</xsl:text>
  </xsl:template>
  <xsl:key match="tei:moduleSpec[@ident]" name="FILES" use="@ident"/>
  <xsl:variable name="top" select="/"/>

  <xsl:template match="@*|comment()|processing-instruction()">
      <xsl:copy-of select="."/>
  </xsl:template>

  <xsl:template match="comment()|processing-instruction()" mode="egXML">
    <xsl:copy-of select="."/>
  </xsl:template>

  <xsl:template match="@*[namespace-uri() = '']" mode="egXML">
    <xsl:attribute name="{tei:checkAltIdentAttribute(local-name(),local-name(parent::*))}">
      <xsl:value-of select="."/>
    </xsl:attribute>
  </xsl:template>

  <xsl:template match="@*" mode="egXML">
    <xsl:copy-of select="."/>
  </xsl:template>

  <xsl:template match="teix:*" mode="egXML"> 
    <xsl:element name="{tei:checkAltIdent(local-name())}" xmlns="http://www.tei-c.org/ns/Examples">
      <xsl:apply-templates
	  select="*|@*|processing-instruction()|comment()|text()" mode="egXML"/>
    </xsl:element>
  </xsl:template>

  <xsl:template match="*" mode="egXML"> 
    <xsl:copy>
      <xsl:apply-templates
	  select="*|@*|processing-instruction()|comment()|text()" mode="egXML"/>
    </xsl:copy>
  </xsl:template>

  <xsl:template match="teix:egXML">
      <xsl:param name="simple">false</xsl:param>
      <xsl:param name="highlight"/>
      <xsl:copy>
         <xsl:if test="not(@xml:lang)">
	           <xsl:copy-of select="parent::tei:*/@xml:lang"/>
         </xsl:if>
         <xsl:apply-templates
	     select="*|@*|processing-instruction()|comment()|text()"  mode="egXML"/>
      </xsl:copy>
  </xsl:template>


  <xsl:template match="*|tei:author|tei:title">
      <xsl:copy>
         <xsl:apply-templates select="*|@*|processing-instruction()|comment()|text()"/>
      </xsl:copy>
  </xsl:template>
  <xsl:template name="showSpace">
      <c xml:space="preserve"> </c>
  </xsl:template>

  <xsl:template name="showSpaceBetweenItems">
      <c xml:space="preserve"> </c>
  </xsl:template>

  <xsl:template name="makeInternalLink">
      <xsl:param name="ptr" as="xs:boolean"  select="false()"/>
      <xsl:param name="target"/>
      <xsl:param name="dest"/>
      <xsl:param name="class"/>
      <xsl:param name="body"/>
      <xsl:variable name="W">
         <xsl:choose>
            <xsl:when test="$target">
               <xsl:value-of select="$target"/>
            </xsl:when>
            <xsl:otherwise>
               <xsl:value-of select="$dest"/>
            </xsl:otherwise>
         </xsl:choose>
      </xsl:variable>
      <xsl:choose>
         <xsl:when test="not($body='')">
            <tei:ref target="#{$W}">
	      <xsl:if test="$class">
		<xsl:attribute name="rend" select="$class"/>
	      </xsl:if>
               <xsl:value-of select="$body"/>
            </tei:ref>
         </xsl:when>
         <xsl:when test="$ptr">
            <tei:ptr target="#{$W}"/>
         </xsl:when>
         <xsl:otherwise>
            <xsl:apply-templates/>
         </xsl:otherwise>
      </xsl:choose>
  </xsl:template>
  <xsl:template name="makeExternalLink">
      <xsl:param name="ptr" as="xs:boolean" select="false()"/>
      <xsl:param name="dest"/>
      <xsl:param name="title"/>
      <xsl:choose>
         <xsl:when test="$ptr">
            <tei:ptr target="{$dest}"/>
         </xsl:when>
         <xsl:otherwise>
            <tei:ref target="{$dest}">
	      <xsl:if test="$title">
		<xsl:attribute name="n" select="$title"/>
	      </xsl:if>
               <xsl:apply-templates/>
            </tei:ref>
         </xsl:otherwise>
      </xsl:choose>
  </xsl:template>
  <xsl:template name="generateEndLink">
      <xsl:param name="where"/>
      <xsl:value-of select="$where"/>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>[odds] Document an element, macro, or class</desc>
   </doc>
  <xsl:template name="refdoc">
      <xsl:if test="$verbose='true'">
         <xsl:message> refdoc for <xsl:value-of select="name(.)"/> - <xsl:value-of select="@ident"/>
         </xsl:message>
      </xsl:if>
      <xsl:choose>
         <xsl:when test="self::tei:classSpec and  count(key('CLASSMEMBERS',@ident))=0">
            <xsl:if test="$verbose='true'">
               <xsl:message> class <xsl:value-of select="@ident"/> omitted as it has no members
          </xsl:message>
            </xsl:if>
         </xsl:when>
         <xsl:otherwise>
            <xsl:apply-templates mode="weavebody" select="."/>
         </xsl:otherwise>
      </xsl:choose>
  </xsl:template>

  <xsl:template name="makeAnchor">
      <xsl:param name="name"/>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>[odds] <param name="text">text</param>
      </desc>
   </doc>
  <xsl:template name="typewriter">
      <xsl:param name="text"/>
      <code>
         <xsl:copy-of select="$text"/>
      </code>
  </xsl:template>

  <xsl:template name="makeSectionHead">
      <xsl:param name="name"/>
      <xsl:param name="id"/>
      <xsl:attribute name="type">
         <xsl:text>refdoc</xsl:text>
      </xsl:attribute>
      <xsl:attribute name="xml:id">
         <xsl:value-of select="$idPrefix"/>
         <xsl:value-of select="$id"/>
      </xsl:attribute>
      <head>
         <xsl:value-of select="$name"/>
      </head>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>[odds] <param name="grammar">grammar</param>
         <param name="content">content</param>
         <param name="element">element</param>
      </desc>
   </doc>
  <xsl:template name="schemaOut">
    <xsl:param name="grammar"/>
    <xsl:param name="content"/>
    <xsl:param name="element">pre</xsl:param>
    <eg rend="eg_rnc">
      <xsl:attribute name="xml:space">preserve</xsl:attribute>
      <xsl:call-template name="make-body-from-r-t-f">
        <xsl:with-param name="schema">
          <xsl:for-each select="$content/*">
            <xsl:call-template name="make-compact-schema"/>
          </xsl:for-each>
        </xsl:with-param>
      </xsl:call-template>
    </eg>
  </xsl:template>
  <xsl:template name="pureODDOut">
    <xsl:param name="grammar"/>
    <xsl:param name="content"/>
    <xsl:param name="element">pre</xsl:param>
    <eg xml:space="preserve">
<xsl:apply-templates mode="verbatim" select="$content/*/*"/>
    </eg>
  </xsl:template>
  <xsl:template name="PMOut">
    <xsl:param name="content"/>
    <xsl:param name="element">pre</xsl:param>
    <eg xml:space="preserve">
<xsl:apply-templates mode="verbatim" select="$content"/>
    </eg>
  </xsl:template>
  <xsl:template match="tei:remarks/tei:p | tei:remarks/tei:ab">
      <xsl:copy><xsl:apply-templates/></xsl:copy>
      <!--<xsl:if test="preceding-sibling::tei:p"><lb/></xsl:if>-->
  </xsl:template>
  <xsl:template match="tei:exemplum/tei:p">
      <xsl:apply-templates/>
  </xsl:template>
  <xsl:template name="linkTogether">
      <xsl:param name="name"/>
      <xsl:param name="reftext"/>
      <xsl:param name="class"/>
      <xsl:variable name="partialname">
         <xsl:value-of select="$idPrefix"/>
	 <xsl:value-of select="replace($name,'_(alternation|sequenceOptionalRepeatable|sequenceOptional|sequenceRepeatable|sequence)','')"/>
      </xsl:variable>
      <ref target="#{$partialname}">
	<xsl:if test="$class">
	  <xsl:attribute name="rend" select="$class"/>
	</xsl:if>
         <xsl:choose>
            <xsl:when test="$reftext=''">
               <xsl:value-of select="$name"/>
            </xsl:when>
            <xsl:otherwise>
               <xsl:value-of select="$reftext"/>
            </xsl:otherwise>
         </xsl:choose>
      </ref>
  </xsl:template>
  <xsl:template name="showRNC">
      <xsl:param name="style"/>
      <xsl:param name="contents"/>
      <xsl:value-of select="$contents"/>
  </xsl:template>
  <xsl:template name="emptySlash">
      <xsl:param name="name"/>
      <xsl:value-of select="$name"/>
      <xsl:text>/</xsl:text>
  </xsl:template>
  <xsl:template match="tei:gi">
    <xsl:variable name="name" select="."/>
      <xsl:choose>
         <xsl:when test="parent::tei:ref or parent::tei:head">
            <gi>
            <xsl:apply-templates/>
	    </gi>
         </xsl:when>
         <xsl:when test="(not(@scheme) or @scheme='TEI') and key('ELEMENTS',$name)">
            <xsl:for-each select="key('ELEMENTS',$name)[last()]">
	      <xsl:variable name="linkname" select="concat($idPrefix,tei:createSpecPrefix(.),$name)"/>
               <ref target="#{$linkname}">
                  <xsl:text>&lt;</xsl:text>
                  <xsl:choose>
                     <xsl:when test="tei:content/rng:empty">
                        <xsl:call-template name="emptySlash">
                           <xsl:with-param name="name">
                              <xsl:value-of select="(tei:altIdent|@ident)[last()]"/>
                           </xsl:with-param>
                        </xsl:call-template>
                     </xsl:when>
                     <xsl:otherwise>
                        <xsl:value-of select="(tei:altIdent|@ident)[last()]"/>
                     </xsl:otherwise>
                  </xsl:choose>
                  <xsl:text>&gt;</xsl:text>
               </ref>
            </xsl:for-each>
         </xsl:when>
         <xsl:otherwise>
            <gi>
	      <xsl:apply-templates/>
	    </gi>
         </xsl:otherwise>
      </xsl:choose>
  </xsl:template>
   <!-- debugging
<xsl:template name="SHOW">
 &lt;<xsl:value-of select="name()"/>
<xsl:for-each select="@*">
  <xsl:text> </xsl:text>
  <xsl:value-of select="name()"/>="<xsl:value-of select="."/>"
</xsl:for-each>
&gt;
   <xsl:for-each select="*">
     <xsl:call-template name="SHOW"/>
   </xsl:for-each>
 &lt;/<xsl:value-of select="name()"/>&gt;
</xsl:template>

-->
  <xsl:template name="emphasize">
      <xsl:param name="class"/>
      <xsl:param name="content"/>
      <hi rend="{$class}">
         <xsl:copy-of select="$content"/>
      </hi>
  </xsl:template>
  <xsl:template name="specHook">
      <xsl:param name="name"/>
      <index indexName="ODDS">
         <xsl:choose>
            <xsl:when test="local-name()='macroSpec'">
               <term>
                  <xsl:value-of select="$name"/>
                  <xsl:text> (macro)</xsl:text>
               </term>
            </xsl:when>
            <xsl:when test="local-name()='classSpec' and        @type='model'">
               <term>
                  <xsl:value-of select="$name"/>
                  <xsl:text> (model class)</xsl:text>
               </term>
            </xsl:when>
            <xsl:when test="local-name()='classSpec' and        @type='atts'">
               <term>
                  <xsl:value-of select="$name"/>
                  <xsl:text> (attribute class)</xsl:text>
               </term>
               <xsl:for-each select=".//tei:attDef">
                  <index indexName="ODDS">
                     <term sortKey="{@ident}">
                        <xsl:text>@</xsl:text>
                        <xsl:value-of select="@ident"/>
                     </term>
                  </index>
               </xsl:for-each>
            </xsl:when>
            <xsl:when test="local-name()='elementSpec'">
               <term sortKey="{$name}">
                  <xsl:text>&lt;</xsl:text>
                  <xsl:value-of select="$name"/>
                  <xsl:text>&gt;</xsl:text>
               </term>
               <xsl:for-each select=".//tei:attDef">
                  <index indexName="ODDS">
                     <term sortKey="{@ident}">
                        <xsl:text>@</xsl:text>
                        <xsl:value-of select="@ident"/>
                     </term>
                  </index>
               </xsl:for-each>
            </xsl:when>
         </xsl:choose>
      </index>
  </xsl:template>

  <xsl:template match="tei:schemaSpec">
    <xsl:if test="$verbose='true'">
      <xsl:message>Processing schemaSpec <xsl:value-of
        select="@ident"/>, summaryDoc=<xsl:value-of
          select="$summaryDoc"/>
      </xsl:message>
    </xsl:if>
    <xsl:choose>
      <xsl:when test="$summaryDoc='true'">
        <div>
          <head>Schema <xsl:value-of select="@ident"/>: Added components</head>
          <xsl:for-each select="tei:constraintSpec[@rend ='add']  
            | tei:classSpec[@rend ='add']  
            | tei:macroSpec[@rend ='add']  
            | tei:dataSpec[@rend ='add']
            | tei:elementSpec[@rend ='add']">
            <xsl:sort select="@ident"/>
            <xsl:apply-templates mode="weave" select="."/>
          </xsl:for-each>
        </div>
        <div>
          <head>Schema <xsl:value-of select="@ident"/>: changed components</head>
          <xsl:for-each select="tei:constraintSpec[@mode='change' or tei:match(@rend,'change')]  
            | tei:classSpec[@mode='change' or tei:match(@rend,'change')]  
            | tei:macroSpec[(@mode='change' or tei:match(@rend,'change'))]  
            | tei:dataSpec[(@mode='change' or tei:match(@rend,'change'))]
            | tei:elementSpec[(@mode='change' or tei:match(@rend,'change'))]">
            <xsl:sort select="@ident"/>
            <xsl:apply-templates mode="weave" select="."/>
          </xsl:for-each>
        </div>
        <div>
          <head>Schema <xsl:value-of select="@ident"/>:  unchanged components</head>
          <table>
            <xsl:for-each select="tei:constraintSpec[not(@mode or @rend)]  
              | tei:classSpec[not(@mode or @rend)]  
              | tei:macroSpec[not(@mode or  @rend)]  
              | tei:dataSpec[not(@mode or  @rend)]
              | tei:elementSpec[not(@mode or @rend)]">
              <xsl:sort select="@ident"/>
              <row>
                <cell>
                  <xsl:attribute name="xml:id">
                    <xsl:value-of select="concat($idPrefix,@ident)"/>
                  </xsl:attribute>
                  <hi>
                    <ref target="http://www.tei-c.org/release/doc/tei-p5-doc/{$documentationLanguage}/html/ref-{@ident}.html">
                      <xsl:value-of select="@ident"/>
                    </ref>
                  </hi>:
                  <xsl:sequence select="tei:makeDescription(., true(), true())"/>
                </cell>
              </row>
            </xsl:for-each>
          </table>
        </div>
      </xsl:when>
      <xsl:otherwise>
        <div>
          <head>Elements</head>
          <xsl:apply-templates mode="weave" select="tei:elementSpec">
            <xsl:sort select="@ident"/>
          </xsl:apply-templates>
        </div>
        <xsl:if test="tei:classSpec[@type='model']">
          <div>
            <head>Model classes</head>
            <xsl:apply-templates mode="weave" select="tei:classSpec[@type='model']">
              <xsl:sort select="@ident"/>
            </xsl:apply-templates>
          </div>
        </xsl:if>
        <xsl:if test="tei:classSpec[@type='atts']">
          <div>
            <head>Attribute classes</head>
            <xsl:apply-templates mode="weave" select="tei:classSpec[@type='atts']">
              <xsl:sort select="@ident"/>
            </xsl:apply-templates>
          </div>
        </xsl:if>
        <xsl:if test="tei:macroSpec">
          <div>
            <head>Macros</head>
            <xsl:apply-templates mode="weave" select="tei:macroSpec">
              <xsl:sort select="@ident"/>
            </xsl:apply-templates>
          </div>
        </xsl:if>
        <xsl:if test="tei:dataSpec">
          <div>
            <head>Datatypes</head>
            <xsl:apply-templates mode="weave" select="tei:dataSpec">
              <xsl:sort select="@ident"/>
            </xsl:apply-templates>
          </div>
        </xsl:if>
        <xsl:if test="tei:constraintSpec">
          <div>
            <head>Constraints</head>
            <table rend="wovenodd">
              <xsl:apply-templates mode="weave" select="tei:constraintSpec">
                <xsl:sort select="@ident"/>
              </xsl:apply-templates>
            </table>
          </div>
        </xsl:if>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>

  <xsl:template name="applyRendition"/>


  <xsl:template match="tei:gloss" mode="inLanguage">
      <seg>
        <!--  MDH 2018-04-29: working on ticket 138. Add flag if translation is out of date.   -->
        <xsl:variable name="transDate" select="@versionDate"/>
        <xsl:if test="preceding-sibling::tei:gloss[@xml:lang = 'en'][@versionDate gt $transDate]
          or following-sibling::tei:gloss[@xml:lang = 'en'][@versionDate gt $transDate]">
          <xsl:attribute name="rend" select="'outOfDateTranslation'"/>
        </xsl:if>
        
         <xsl:copy-of select="@xml:lang"/>
         <xsl:value-of select="."/>
      </seg>
  </xsl:template>

  <xsl:template match="tei:desc" mode="inLanguage">
    <seg>
      <!--  MDH 2018-04-29: working on ticket 138. Add flag if translation is out of date.   -->
      <xsl:variable name="transDate" select="@versionDate"/>
      <xsl:if test="preceding-sibling::tei:desc[@xml:lang = 'en'][@versionDate gt $transDate]
        or following-sibling::tei:desc[@xml:lang = 'en'][@versionDate gt $transDate]">
        <xsl:attribute name="rend" select="'outOfDateTranslation'"/>
      </xsl:if>
     
      <xsl:copy-of select="@xml:lang"/>
      <xsl:apply-templates/>
    </seg>
  </xsl:template>

  <xsl:template name="appReading">
     <xsl:param name="lemma"/>
     <xsl:param name="lemmawitness"/>
     <xsl:param name="readings"/>
     <xsl:value-of select="$lemma"/>
  </xsl:template>

  <xsl:template name="makeSpan">
    <xsl:apply-templates/>
  </xsl:template>

  <xsl:template match="tei:listRef" mode="weave"/>

  <xsl:template match="tei:ref" mode="weave" priority="9">
    <xsl:copy-of select="."/>
  </xsl:template>

  <xsl:template name="makeInline">
    <xsl:param name="before"/>
    <xsl:param name="after"/>
    <xsl:param name="style"/>
    <hi>
      <xsl:copy-of select="@xml:id"/>
      <xsl:if test="not($style='')">
	<xsl:attribute name="rend">
	  <xsl:value-of select="$style"/>
	</xsl:attribute>
      </xsl:if>
      <xsl:value-of select="$before"/>
      <xsl:apply-templates/>
      <xsl:value-of select="$after"/>
    </hi>
  </xsl:template>

  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc>how to make a horizontal rule</desc>
  </doc>
  <xsl:template name="horizontalRule">
    <milestone rend="hr"/>
  </xsl:template>

  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc>what to do with a block-level object</desc>
  </doc>
  <xsl:template name="makeBlock">
    <xsl:param name="style"/>
    <p>
      <xsl:if test="not($style='')">
	<xsl:attribute name="rend">
	  <xsl:value-of select="$style"/>
	</xsl:attribute>
      </xsl:if>
      <xsl:copy-of select="@xml:id"/>
      <xsl:apply-templates/>
    </p>
  </xsl:template>

  <xsl:template name="makeItem">
    <item>
      <xsl:copy-of select="@*"/>
      <xsl:apply-templates/>
    </item>
  </xsl:template>
  <xsl:template name="makeLabelItem">
    <item>
      <xsl:copy-of select="@*"/>
      <xsl:apply-templates/>
    </item>
  </xsl:template>

  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc>see whether an element has been renamed</desc>
  </doc>
  <xsl:function name="tei:checkAltIdent" as="xs:string">
    <xsl:param name="name"/>
    <xsl:for-each select="$TOP">
      <xsl:choose>
	<xsl:when test="key('ALTIDENTS',$name)">
	  <xsl:value-of select="key('ALTIDENTS',$name)/tei:altIdent"/>
	</xsl:when>
	<xsl:otherwise>
	  <xsl:value-of select="$name"/>
	</xsl:otherwise>
      </xsl:choose>
    </xsl:for-each>
  </xsl:function>

  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc>see whether an attribute has been renamed</desc>
  </doc>
  <xsl:function name="tei:checkAltIdentAttribute" as="xs:string">
    <xsl:param name="aname"/>
    <xsl:param name="ename"/>
    <xsl:variable name="name" select="concat($ename,$aname)"/>
    <xsl:for-each select="$TOP">
      <xsl:choose>
	<xsl:when test="key('ALTIDENTS',$name)">
	  <xsl:value-of select="key('ALTIDENTS',$name)/tei:altIdent"/>
	</xsl:when>
	<xsl:otherwise>
	  <xsl:value-of select="$aname"/>
	</xsl:otherwise>
      </xsl:choose>
    </xsl:for-each>
  </xsl:function>

</xsl:stylesheet>
