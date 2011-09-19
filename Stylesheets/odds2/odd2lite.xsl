<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet xmlns="http://www.tei-c.org/ns/1.0"
                xmlns:a="http://relaxng.org/ns/compatibility/annotations/1.0"
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
                exclude-result-prefixes="fo a tei html s rng sch xsi teix xs"
                version="2.0">
  <xsl:import href="../common2/verbatim.xsl"/>
  <xsl:import href="teiodds.xsl"/>
  <xsl:import href="../common2/tei.xsl"/>
  <xsl:include href="../common2/tagdocs.xsl"/>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" scope="stylesheet" type="stylesheet">
      <desc>
         <p> TEI stylesheet for making TEI Lite XML from ODD </p>
         <p> This library is free software; you can redistribute it and/or modify it under the
      terms of the GNU Lesser General Public License as published by the Free Software Foundation;
      either version 2.1 of the License, or (at your option) any later version. This library is
      distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the
      implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser
      General Public License for more details. You should have received a copy of the GNU Lesser
      General Public License along with this library; if not, write to the Free Software Foundation,
      Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA </p>
         <p>Author: See AUTHORS</p>
         <p>Id: $Id$</p>
         <p>Copyright: 2011, TEI Consortium</p>
      </desc>
   </doc>
  <xsl:param name="summaryDoc"  as="xs:boolean" select="false()"/>

  <xsl:param name="cellName">cell</xsl:param>
  <xsl:param name="xrefName">ref</xsl:param>
  <xsl:param name="urlName">target</xsl:param>
  <xsl:param name="ulName">list</xsl:param>
  <xsl:param name="dlName">list</xsl:param>
  <xsl:param name="codeName">code</xsl:param>
  <xsl:param name="colspan">cols</xsl:param>
  <xsl:param name="ddName">item</xsl:param>
  <xsl:param name="dtName">label</xsl:param>
  <xsl:param name="hiName">hi</xsl:param>
  <xsl:param name="itemName">item</xsl:param>
  <xsl:param name="labelName">label</xsl:param>
  <xsl:param name="rendName">rend</xsl:param>
  <xsl:param name="rowName">row</xsl:param>
  <xsl:param name="tableName">table</xsl:param>
  <xsl:param name="sectionName">div</xsl:param>
  <xsl:param name="divName">seg</xsl:param>
  <xsl:param name="segName">seg</xsl:param>
  <xsl:param name="outputNS">http://www.tei-c.org/ns/1.0</xsl:param>
  <xsl:param name="startAttribute"/>
  <xsl:param name="endAttribute"/>
  <xsl:param name="startAttributeValue"/>
  <xsl:param name="endAttributeValue"/>
  <xsl:param name="startComment"/>
  <xsl:param name="endComment"/>
  <xsl:param name="startElement"/>
  <xsl:param name="endElement"/>
  <xsl:param name="startElementName"/>
  <xsl:param name="endElementName"/>
  <xsl:param name="startNamespace"/>
  <xsl:param name="endNamespace"/>
  <xsl:param name="spaceCharacter"> </xsl:param>
  <xsl:param name="oddmode">tei</xsl:param>
  <xsl:param name="displayMode">rnc</xsl:param>
  <xsl:param name="splitLevel">-1</xsl:param>
  <xsl:param name="idPrefix">TEI.</xsl:param>
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

  <xsl:template match="teix:egXML">
      <xsl:param name="simple"  as="xs:boolean" select="false()"/>
      <xsl:param name="highlight"/>
      <xsl:copy>
         <xsl:if test="not(@xml:lang)">
	           <xsl:copy-of select="parent::tei:*/@xml:lang"/>
         </xsl:if>
         <xsl:apply-templates select="*|@*|processing-instruction()|comment()|text()"/>
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
      <xsl:param name="ptr"  as="xs:boolean"/>
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
      <xsl:param name="ptr"/>
      <xsl:param name="dest"/>
      <xsl:choose>
         <xsl:when test="$ptr">
            <tei:ptr target="{$dest}"/>
         </xsl:when>
         <xsl:otherwise>
            <tei:ref target="{$dest}">
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
      <xsl:if test="$verbose">
         <xsl:message> refdoc for <xsl:value-of select="name(.)"/> - <xsl:value-of select="@ident"/>
         </xsl:message>
      </xsl:if>
      <xsl:variable name="objectname">
         <xsl:choose>
            <xsl:when test="tei:altIdent">
               <xsl:value-of select="tei:altIdent"/>
            </xsl:when>
            <xsl:otherwise>
               <xsl:value-of select="@ident"/>
            </xsl:otherwise>
         </xsl:choose>
      </xsl:variable>
      <xsl:variable name="name">
         <xsl:value-of select="$objectname"/>
      </xsl:variable>
      <xsl:choose>
         <xsl:when test="$TEIC and @ident='att.global'">
            <xsl:apply-templates mode="weavebody" select="."/>
	 </xsl:when>
         <xsl:when test="self::tei:classSpec and  count(key('CLASSMEMBERS',@ident))=0">
            <xsl:if test="$verbose">
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
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>[odds] make a link<param name="class">class</param>
         <param name="id">id</param>
         <param name="name">name</param>
         <param name="text">text</param>
      </desc>
   </doc>
  <xsl:template name="makeLink">
      <xsl:param name="class"/>
      <xsl:param name="name"/>
      <xsl:param name="text"/>
      <ref>
         <xsl:attribute name="target">
            <xsl:choose>
               <xsl:when test="number($splitLevel)=-1">
                  <xsl:text>#</xsl:text>
                  <xsl:value-of select="$name"/>
               </xsl:when>
               <xsl:when test="$STDOUT">
                  <xsl:for-each select="key('IDENTS',$name)">
                     <xsl:call-template name="getSpecURL">
                        <xsl:with-param name="name">
                           <xsl:value-of select="$name"/>
                        </xsl:with-param>
                        <xsl:with-param name="type">
                           <xsl:value-of select="substring-before(local-name(),'Spec')"/>
                        </xsl:with-param>
                     </xsl:call-template>
                  </xsl:for-each>
               </xsl:when>
               <xsl:otherwise>
                  <xsl:text>ref-</xsl:text>
                  <xsl:value-of select="$name"/>
                  <xsl:value-of select="$outputSuffix"/>
               </xsl:otherwise>
            </xsl:choose>
         </xsl:attribute>
         <xsl:copy-of select="$text"/>
      </ref>
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
  <xsl:template name="bitOut">
      <xsl:param name="grammar"/>
      <xsl:param name="content"/>
      <xsl:param name="element">pre</xsl:param>
      <eg rend="eg_rnc">
         <xsl:call-template name="make-body-from-r-t-f">
            <xsl:with-param name="schema">
               <xsl:for-each select="$content/*">
                  <xsl:call-template name="make-compact-schema"/>
               </xsl:for-each>
            </xsl:with-param>
         </xsl:call-template>
      </eg>
  </xsl:template>
  <xsl:template match="tei:remarks/tei:p">
      <xsl:apply-templates/>
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
         <xsl:choose>
            <xsl:when test="contains($name,'_')">
               <xsl:value-of select="substring-before($name,'_')"/>
            </xsl:when>
            <xsl:otherwise>
               <xsl:value-of select="$name"/>
            </xsl:otherwise>
         </xsl:choose>
      </xsl:variable>
      <ref target="#{$partialname}">
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
      <xsl:choose>
         <xsl:when test="not(@scheme='') or parent::tei:ref or parent::tei:head">
            <xsl:text>&lt;</xsl:text>
            <xsl:apply-templates/>
            <xsl:text>&gt;</xsl:text>
         </xsl:when>
         <xsl:when test="key('ELEMENTS',.)">
            <xsl:for-each select="key('ELEMENTS',.)">
               <ref target="#{@ident}">
                  <xsl:text>&lt;</xsl:text>
                  <xsl:choose>
                     <xsl:when test="tei:content/rng:empty">
                        <xsl:call-template name="emptySlash">
                           <xsl:with-param name="name">
                              <xsl:value-of select="@ident"/>
                           </xsl:with-param>
                        </xsl:call-template>
                     </xsl:when>
                     <xsl:otherwise>
                        <xsl:value-of select="@ident"/>
                     </xsl:otherwise>
                  </xsl:choose>
                  <xsl:text>&gt;</xsl:text>
               </ref>
            </xsl:for-each>
         </xsl:when>
         <xsl:otherwise>
            <xsl:text>&lt;</xsl:text>
            <xsl:apply-templates/>
            <xsl:text>&gt;</xsl:text>
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
                     <term sortBy="{@ident}">
                        <xsl:text>@</xsl:text>
                        <xsl:value-of select="@ident"/>
                     </term>
                  </index>
               </xsl:for-each>
            </xsl:when>
            <xsl:when test="local-name()='elementSpec'">
               <term sortBy="{$name}">
                  <xsl:text>&lt;</xsl:text>
                  <xsl:value-of select="$name"/>
                  <xsl:text>&gt;</xsl:text>
               </term>
               <xsl:for-each select=".//tei:attDef">
                  <index indexName="ODDS">
                     <term sortBy="{@ident}">
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
      <xsl:if test="$verbose">
         <xsl:message>Processing schemaSpec <xsl:value-of select="@ident"/>, summaryDoc=<xsl:value-of select="$summaryDoc"/>
         </xsl:message>
      </xsl:if>
      <xsl:choose>
         <xsl:when test="$summaryDoc">
	   <div>
	     <head>Schema <xsl:value-of select="@ident"/>: changed components</head>
	     <xsl:for-each select="tei:classSpec[@mode or @rend='change']  
				   | tei:macroSpec[(@mode or @rend='change')]  
				   | tei:elementSpec[(@mode or @rend='change')]">
	       <xsl:sort select="@ident"/>
	       <xsl:apply-templates mode="weave" select="."/>
	     </xsl:for-each>
	   </div>
	   <div>
	   <head>Schema <xsl:value-of select="@ident"/>:  unchanged components</head>
	   <table>
	     <xsl:for-each select="tei:classSpec[not(@mode or @rend)]  
				   | tei:macroSpec[not(@mode or  @rend)]  
				   | tei:elementSpec[not(@mode or @rend)]">
	       <xsl:sort select="@ident"/>
	       <row>
		 <cell>
		   <xsl:attribute name="xml:id">
		     <xsl:value-of select="@ident"/>
		   </xsl:attribute>
		   <hi>
		     <ref target="http://www.tei-c.org/release/doc/tei-p5-doc/{$documentationLanguage}/html/ref-{@ident}.html">
		       <xsl:value-of select="@ident"/>
		     </ref>
		     </hi>:
		   <xsl:call-template name="makeDescription"/>
		 </cell>
	       </row>
	     </xsl:for-each>
	   </table>
	   </div>
	 </xsl:when>
         <xsl:otherwise>
	   <xsl:if test="tei:macroSpec">
	     <div>
	       <head>Macros</head>
	       <xsl:apply-templates mode="weave" select="tei:macroSpec">
		 <xsl:sort select="@ident"/>
	       </xsl:apply-templates>
	     </div>
	   </xsl:if>
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
	   <div>
	     <head>Elements</head>
	     <xsl:apply-templates mode="weave" select="tei:elementSpec">
	       <xsl:sort select="@ident"/>
	     </xsl:apply-templates>
	   </div>
	 </xsl:otherwise>
      </xsl:choose>
  </xsl:template>

  <xsl:template name="applyRendition"/>

  <xsl:template match="tei:gloss" mode="inLanguage">
      <seg>
         <xsl:copy-of select="@xml:lang"/>
         <xsl:value-of select="."/>
      </seg>
  </xsl:template>

  <xsl:template match="tei:desc" mode="inLanguage">
      <seg>
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

</xsl:stylesheet>
