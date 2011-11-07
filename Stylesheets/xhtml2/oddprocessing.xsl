<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet version="2.0"
  exclude-result-prefixes="#default s html a fo rng tei teix"
  xmlns:a="http://relaxng.org/ns/compatibility/annotations/1.0"
  xmlns:fo="http://www.w3.org/1999/XSL/Format"
  xmlns:html="http://www.w3.org/1999/xhtml"
  xmlns:rng="http://relaxng.org/ns/structure/1.0"
  xmlns:s="http://www.ascc.net/xml/schematron"
  xmlns:tei="http://www.tei-c.org/ns/1.0"
  xmlns:teix="http://www.tei-c.org/ns/Examples"
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
  xmlns="http://www.w3.org/1999/xhtml" >

  <xsl:param name="cssFile"/>
  <xsl:param name="cssSecondaryFile"/>
  <xsl:param name="summaryDoc">false</xsl:param>
  <xsl:include href="../common2/tagdocs.xsl"/>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" scope="stylesheet" type="stylesheet">
    <desc>
      <p> TEI stylesheet dealing with elements from the tagdocs module,
      making HTML output. </p>
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
  <xsl:param name="oddmode">html</xsl:param>
  <xsl:param name="xrefName">a</xsl:param>
  <xsl:param name="urlName">href</xsl:param>
  <xsl:param name="ulName">ul</xsl:param>
  <xsl:param name="dlName">dl</xsl:param>
  <xsl:param name="codeName">span</xsl:param>
  <xsl:param name="colspan">colspan</xsl:param>
  <xsl:param name="ddName">dd</xsl:param>
  <xsl:param name="dtName">dt</xsl:param>
  <xsl:param name="hiName">span</xsl:param>
  <xsl:param name="itemName">li</xsl:param>
  <xsl:param name="labelName">dt</xsl:param>
  <xsl:param name="rendName">class</xsl:param>
  <xsl:param name="rowName">tr</xsl:param>
  <xsl:param name="tableName">table</xsl:param>
  <xsl:param name="cellName">td</xsl:param>
  <xsl:param name="divName">div</xsl:param>
  <xsl:param name="sectionName">div</xsl:param>
  <xsl:param name="segName">span</xsl:param>
  <xsl:param name="outputNS">http://www.w3.org/1999/xhtml</xsl:param>
  <xsl:key match="tei:*" name="NameToID" use="@ident"/>
  <xsl:key name="MODEL-CLASS-MODULE" match="tei:classSpec[@type='model']" use="@module"/>
  <xsl:key name="ATT-CLASS-MODULE" match="tei:classSpec[@type='atts']" use="@module"/>
  <xsl:key name="ELEMENT-MODULE" match="tei:elementSpec" use="@module"/>
  <xsl:key name="MACRO-MODULE" match="tei:macroSpec" use="@module"/>
  <xsl:key name="ELEMENT-ALPHA" match="tei:elementSpec" use="substring(translate(@ident,'ABCDEFGHIJKLMNOPQRSTUVWXYZ','abcdefghijklmnopqrstuvwxyz'),1,1)"/>
  <xsl:key name="MODEL-CLASS-ALPHA" match="tei:classSpec[@type='model']" use="substring(translate(@ident,'ABCDEFGHIJKLMNOPQRSTUVWXYZ','abcdefghijklmnopqrstuvwxyz'),7,1)"/>
  <xsl:key name="ATT-CLASS-ALPHA" match="tei:classSpec[@type='atts']" use="substring(translate(@ident,'ABCDEFGHIJKLMNOPQRSTUVWXYZ','abcdefghijklmnopqrstuvwxyz'),5,1)"/>
  <xsl:key match="tei:moduleSpec[@ident]" name="FILES" use="@ident"/>
  <xsl:variable name="top" select="/"/>

  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc>[odds] Document an element, macro, or class</desc>
  </doc>
  <xsl:template name="refdoc">
    <xsl:if test="$verbose='true'">
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
      <xsl:when test="self::tei:classSpec and not(@ident='att.global') and         count(key('CLASSMEMBERS',@ident))=0">
        <xsl:if test="$verbose='true'">
          <xsl:message> class <xsl:value-of select="@ident"/> omitted as it has no members
      </xsl:message>
        </xsl:if>
      </xsl:when>
      <xsl:when test="number($splitLevel)=-1 or $STDOUT='true'">
        <xsl:apply-templates mode="weavebody" select="."/>
      </xsl:when>
      <xsl:otherwise>
        <xsl:variable name="BaseFile">
          <xsl:value-of select="$masterFile"/>
          <xsl:if test="ancestor::tei:teiCorpus">
            <xsl:text>-</xsl:text>
            <xsl:choose>
              <xsl:when test="@xml:id">
                <xsl:value-of select="@xml:id"/>
              </xsl:when>
              <xsl:otherwise>
                <xsl:number/>
              </xsl:otherwise>
            </xsl:choose>
          </xsl:if>
        </xsl:variable>
        <xsl:variable name="outName">
          <xsl:call-template name="outputChunkName">
            <xsl:with-param name="ident">
              <xsl:text>ref-</xsl:text>
              <xsl:value-of select="@ident"/>
            </xsl:with-param>
          </xsl:call-template>
        </xsl:variable>
        <xsl:if test="$verbose='true'">
          <xsl:message>Opening file <xsl:value-of select="$outName"/>
               </xsl:message>
        </xsl:if>
        <xsl:variable name="documentationLanguage">
          <xsl:call-template name="generateDoc"/>
        </xsl:variable>
        <xsl:variable name="langs">
          <xsl:value-of select="concat(normalize-space($documentationLanguage),' ')"/>
        </xsl:variable>
        <xsl:result-document doctype-public="{$doctypePublic}" doctype-system="{$doctypeSystem}" encoding="{$outputEncoding}" href="{$outName}" method="{$outputMethod}">
          <xsl:element name="html" namespace="{$outputNamespace}">
            <xsl:call-template name="addLangAtt"/>
            <xsl:comment>THIS IS A GENERATED FILE. DO NOT EDIT (7) </xsl:comment>
            <head>
              <title>
                <xsl:text>TEI </xsl:text>
                <xsl:value-of select="substring-before(local-name(),'Spec')"/>
                <xsl:text> </xsl:text>
                <xsl:value-of select="$name"/>
                <xsl:text> </xsl:text>
                <xsl:call-template name="makeGloss">
                  <xsl:with-param name="langs" select="$langs"/>
                </xsl:call-template>
              </title>
              <xsl:call-template name="metaHTML">
                <xsl:with-param name="title">
                  <xsl:value-of select="substring-before(local-name(),'Spec')"/>
                  <xsl:text> </xsl:text>
                  <xsl:value-of select="@ident"/>
                  <xsl:text> </xsl:text>
                  <xsl:call-template name="makeGloss">
                    <xsl:with-param name="langs" select="$langs"/>
                  </xsl:call-template>
                  <xsl:text> - </xsl:text>
                  <xsl:call-template name="generateTitle"/>
                </xsl:with-param>
              </xsl:call-template>
              <xsl:call-template name="includeCSS"/>
              <xsl:call-template name="generateLocalCSS"/>
              <xsl:call-template name="includeJavascript"/>
              <xsl:call-template name="javascriptHook"/>
            </head>
            <body id="TOP">
              <xsl:call-template name="bodyMicroData"/>
              <xsl:call-template name="guidelinesTop">
                <xsl:with-param name="name">
                  <xsl:value-of select="$name"/>
                </xsl:with-param>
              </xsl:call-template>
              <div class="main-content">
                <xsl:call-template name="startDivHook"/>
                <xsl:apply-templates mode="weavebody" select="."/>
              </div>
              <xsl:call-template name="stdfooter">
                <xsl:with-param name="file">
                  <xsl:text>ref-</xsl:text>
                  <xsl:value-of select="@ident"/>
                </xsl:with-param>
              </xsl:call-template>
              <xsl:call-template name="bodyEndHook"/>
            </body>
          </xsl:element>
        </xsl:result-document>
        <xsl:if test="$verbose='true'">
          <xsl:message>Closing file <xsl:value-of select="$outName"/>
               </xsl:message>
        </xsl:if>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc>[html] Provide a footer for each reference document</desc>
  </doc>
  <xsl:template name="refdocFooter">
    <xsl:call-template name="preAddressHook"/>
    <div style="margin: 20pt; font-weight: bold;">
      <a href="{$refDocFooterURL}">
        <xsl:value-of select="$refDocFooterText"/>
      </a>
    </div>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc>[html] <param name="text">text</param>
      </desc>
  </doc>
  <xsl:template name="typewriter">
    <xsl:param name="text"/>
    <tt>
      <xsl:copy-of select="$text"/>
    </tt>
  </xsl:template>
  <xsl:template name="showRNC">
    <xsl:param name="style"/>
    <xsl:param name="contents"/>
    <span class="{$style}">
      <xsl:choose>
        <xsl:when test="string-length($contents)&lt;50">
          <xsl:value-of select="$contents"/>
        </xsl:when>
        <xsl:otherwise>
          <xsl:call-template name="verbatim-reformatText">
            <xsl:with-param name="sofar">0</xsl:with-param>
            <xsl:with-param name="indent">
              <xsl:text> </xsl:text>
            </xsl:with-param>
            <xsl:with-param name="text">
              <xsl:value-of select="$contents"/>
            </xsl:with-param>
          </xsl:call-template>
        </xsl:otherwise>
      </xsl:choose>
    </span>
  </xsl:template>
  <xsl:template name="emptySlash">
    <xsl:param name="name"/>
    <span class="emptySlash">
      <xsl:value-of select="$name"/>
    </span>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc>Process elements teix:egXML</desc>
  </doc>
  <xsl:template match="teix:egXML">
    <xsl:param name="simple">false</xsl:param>
    <xsl:param name="highlight"/>
    <div>
      <xsl:attribute name="id">
        <xsl:apply-templates mode="ident" select="."/>
      </xsl:attribute>
      <xsl:attribute name="class">
	<xsl:text>pre</xsl:text>
	<xsl:if test="not(*)">
	  <xsl:text> cdata</xsl:text>
	</xsl:if>
	<xsl:choose>
	  <xsl:when test="@valid='feasible'">
	    <xsl:text> egXML_feasible</xsl:text>
	  </xsl:when>
	  <xsl:when test="@valid='false'">
	    <xsl:text> egXML_invalid</xsl:text>
	  </xsl:when>
	  <xsl:otherwise>
	    <xsl:text> egXML_valid</xsl:text>
	  </xsl:otherwise>
	</xsl:choose>
      </xsl:attribute>
      <xsl:choose>
        <xsl:when test="$simple='true'">
          <xsl:apply-templates mode="verbatim">
            <xsl:with-param name="highlight">
              <xsl:value-of select="$highlight"/>
            </xsl:with-param>
          </xsl:apply-templates>
        </xsl:when>
        <xsl:otherwise>
          <xsl:call-template name="egXMLStartHook"/>
          <xsl:apply-templates mode="verbatim">
            <xsl:with-param name="highlight">
              <xsl:value-of select="$highlight"/>
            </xsl:with-param>
          </xsl:apply-templates>
          <xsl:call-template name="egXMLEndHook"/>
        </xsl:otherwise>
      </xsl:choose>
    </div>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc>[html] <param name="grammar">grammar</param>
         <param name="content">content</param>
         <param name="element">element</param>
      </desc>
  </doc>
  <xsl:template name="bitOut">
    <xsl:param name="grammar"/>
    <xsl:param name="content"/>
    <xsl:param name="element">pre</xsl:param>
    <xsl:choose>
      <xsl:when test="$displayMode='both'">
        <div class="displayRelax">
          <button class="displayRelaxButton">
            <span class="RNG_Compact">
              <xsl:call-template name="i18n">
                <xsl:with-param name="word">Compact to XML format</xsl:with-param>
              </xsl:call-template>
            </span>
            <span class="RNG_XML">
              <xsl:call-template name="i18n">
                <xsl:with-param name="word">XML format to compact</xsl:with-param>
              </xsl:call-template>
            </span>
          </button>
          <pre class="RNG_XML">
            <xsl:apply-templates mode="verbatim" select="$content/*/*"/>
          </pre>
          <pre class="RNG_Compact">
            <xsl:call-template name="make-body-from-r-t-f">
              <xsl:with-param name="schema">
                <xsl:for-each select="$content/*">
                  <xsl:call-template name="make-compact-schema"/>
                </xsl:for-each>
              </xsl:with-param>
            </xsl:call-template>
          </pre>
        </div>
      </xsl:when>
      <xsl:when test="$displayMode='rng'">
        <xsl:element name="{$element}">
          <xsl:attribute name="class">eg</xsl:attribute>
          <xsl:apply-templates mode="verbatim" select="$content/*/*"/>
        </xsl:element>
      </xsl:when>
      <xsl:when test="$displayMode='rnc'">
        <xsl:element name="{$element}">
          <xsl:attribute name="class">eg</xsl:attribute>
          <xsl:call-template name="make-body-from-r-t-f">
            <xsl:with-param name="schema">
              <xsl:for-each select="$content/*">
                <xsl:call-template name="make-compact-schema"/>
              </xsl:for-each>
            </xsl:with-param>
          </xsl:call-template>
        </xsl:element>
      </xsl:when>
      <xsl:otherwise>
        <xsl:element name="{$element}">
          <xsl:attribute name="class">eg</xsl:attribute>
          <xsl:for-each select="$content/*">
            <xsl:apply-templates mode="literal"/>
          </xsl:for-each>
        </xsl:element>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  <xsl:template name="showSpace">
    <xsl:text> </xsl:text>
  </xsl:template>
  <xsl:template name="showSpaceBetweenItems">
    <xsl:text> </xsl:text>
  </xsl:template>
  <xsl:template match="tei:schemaSpec">
    <xsl:choose>
      <xsl:when test="tei:specGrpRef">
        <xsl:variable name="SPECS">
          <tei:schemaSpec>
            <xsl:copy-of select="@*"/>
            <xsl:apply-templates mode="expandSpecs"/>
          </tei:schemaSpec>
        </xsl:variable>
        <xsl:for-each select="$SPECS/tei:schemaSpec">
          <xsl:call-template name="schemaSpecWeave"/>
        </xsl:for-each>
      </xsl:when>
      <xsl:otherwise>
        <xsl:call-template name="schemaSpecWeave"/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  <xsl:template name="schemaSpecWeave">
    <xsl:if test="$verbose='true'">
      <xsl:message>Processing schemaSpec <xsl:value-of select="@ident"/>, summaryDoc=<xsl:value-of select="$summaryDoc"/>
         </xsl:message>
    </xsl:if>
    <xsl:choose>
      <xsl:when test="$summaryDoc='true'">
        <h2>Schema <xsl:value-of select="@ident"/>: changed components</h2>
        <xsl:for-each select="tei:classSpec[@mode or @rend='change']        | tei:macroSpec[(@mode or @rend='change')]        | tei:elementSpec[(@mode or @rend='change')]">
          <xsl:sort select="@ident"/>
          <xsl:apply-templates mode="weave" select="."/>
        </xsl:for-each>
        <h2>Schema <xsl:value-of select="@ident"/>:  unchanged  components</h2>
        <table>
          <xsl:for-each select="tei:classSpec[not(@mode or @rend)]          | tei:macroSpec[not(@mode or  @rend)]          | tei:elementSpec[not(@mode or @rend)]">
            <xsl:sort select="@ident"/>
            <tr>
              <td id="{@ident}"><a href="http://www.tei-c.org/release/doc/tei-p5-doc/{$documentationLanguage}/html/ref-{@ident}.html"><xsl:value-of select="@ident"/></a>:
		     <xsl:call-template name="makeDescription"/></td>
            </tr>
          </xsl:for-each>
        </table>
      </xsl:when>
      <xsl:otherwise>
        <h2>Schema <xsl:value-of select="@ident"/>: Elements</h2>
        <xsl:apply-templates mode="weave" select="tei:elementSpec">
          <xsl:sort select="@ident"/>
        </xsl:apply-templates>
        <xsl:if test="tei:classSpec[@type='model']">
          <h2>Schema <xsl:value-of select="@ident"/>: Model classes</h2>
          <xsl:apply-templates mode="weave" select="tei:classSpec[@type='model']">
            <xsl:sort select="@ident"/>
          </xsl:apply-templates>
        </xsl:if>
        <xsl:if test="tei:classSpec[@type='atts']">
          <h2>Schema <xsl:value-of select="@ident"/>: Attribute classes</h2>
          <xsl:apply-templates mode="weave" select="tei:classSpec[@type='atts']">
            <xsl:sort select="@ident"/>
          </xsl:apply-templates>
        </xsl:if>
        <xsl:if test="tei:macroSpec">
          <h2>Schema <xsl:value-of select="@ident"/>: Macros</h2>
          <xsl:apply-templates mode="weave" select="tei:macroSpec">
            <xsl:sort select="@ident"/>
          </xsl:apply-templates>
        </xsl:if>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc>[odds] make a link<param name="name">name</param>
         <param name="id">id</param>
      </desc>
  </doc>
  <xsl:template name="makeSectionHead">
    <xsl:param name="name"/>
    <xsl:param name="id"/>
    <h3>
      <xsl:attribute name="class">
      <xsl:text>oddSpec</xsl:text>
      <xsl:if test="@status">
	<xsl:text> status_</xsl:text>
	<xsl:value-of select="@status"/>
      </xsl:if>
      </xsl:attribute>
      <xsl:call-template name="makeAnchor">
        <xsl:with-param name="name">
          <xsl:value-of select="$id"/>
        </xsl:with-param>
      </xsl:call-template>
      <xsl:value-of select="$name"/>
      <xsl:if test="@ns">
	[<xsl:value-of select="@ns"/>]
      </xsl:if>
    </h3>
  </xsl:template>
  <xsl:template name="specHook">
    <xsl:param name="name"/>
  </xsl:template>
  <xsl:template match="tei:ident">
    <xsl:choose>
      <xsl:when test="@type='class' and key('CLASSES',.)">
        <xsl:call-template name="linkTogether">
          <xsl:with-param name="name">
            <xsl:value-of select="."/>
          </xsl:with-param>
          <xsl:with-param name="reftext">
            <xsl:value-of select="."/>
          </xsl:with-param>
        </xsl:call-template>
      </xsl:when>
      <xsl:when test="@type">
        <span class="ident-{@type}">
          <xsl:apply-templates/>
        </span>
      </xsl:when>
      <xsl:otherwise>
        <span class="ident">
          <xsl:apply-templates/>
        </span>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  <xsl:template match="tei:gi">
    <xsl:variable name="lookup" select="."/>
    <xsl:choose>
      <xsl:when test="parent::tei:ref or parent::tei:head or string-length(@scheme)&gt;0">
        <span class="gi">
          <xsl:text>&lt;</xsl:text>
          <xsl:apply-templates/>
          <xsl:text>&gt;</xsl:text>
        </span>
      </xsl:when>
      <xsl:when test="key('ELEMENTS',$lookup)">
        <xsl:for-each select="key('ELEMENTS',$lookup)[last()]">
          <xsl:call-template name="linkTogether">
            <xsl:with-param name="class">gi</xsl:with-param>
            <xsl:with-param name="name">
              <xsl:value-of select="@ident"/>
            </xsl:with-param>
            <xsl:with-param name="reftext">
              <xsl:choose>
                <xsl:when test="tei:content/rng:empty">
                  <span class="emptySlash">
                    <xsl:value-of select="(tei:altIdent|@ident)[last()]"/>
                  </span>
                </xsl:when>
                <xsl:otherwise>
                  <xsl:value-of select="(tei:altIdent|@ident)[last()]"/>
                </xsl:otherwise>
              </xsl:choose>
            </xsl:with-param>
          </xsl:call-template>
        </xsl:for-each>
      </xsl:when>
      <xsl:otherwise>
        <span class="gi">
          <xsl:text>&lt;</xsl:text>
          <xsl:apply-templates/>
          <xsl:text>&gt;</xsl:text>
        </span>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  <xsl:template name="processSchemaFragment">
    <xsl:param name="filename"/>
    <div class="schemaFragment">
      <xsl:if test="tei:elementSpec">
        <h2>
          <xsl:call-template name="i18n">
            <xsl:with-param name="word">Elements defined</xsl:with-param>
          </xsl:call-template>
        </h2>
        <xsl:apply-templates mode="weave" select="tei:elementSpec">
          <xsl:sort select="tei:altIdent|@ident"/>
        </xsl:apply-templates>
      </xsl:if>
      <xsl:if test="tei:classSpec">
        <h2>
          <xsl:call-template name="i18n">
            <xsl:with-param name="word">Classes defined</xsl:with-param>
          </xsl:call-template>
        </h2>
        <xsl:apply-templates mode="weave" select="tei:classSpec">
          <xsl:sort select="tei:altIdent|@ident"/>
        </xsl:apply-templates>
      </xsl:if>
      <xsl:if test="tei:macroSpec">
        <h2>
          <xsl:call-template name="i18n">
            <xsl:with-param name="word">Macros defined</xsl:with-param>
          </xsl:call-template>
        </h2>
        <xsl:apply-templates mode="weave" select="tei:macroSpec">
          <xsl:sort select="tei:altIdent|@ident"/>
        </xsl:apply-templates>
      </xsl:if>
      <xsl:apply-templates select="tei:specGrpRef"/>
    </div>
  </xsl:template>
  <xsl:template name="listSpecs">
    <xsl:for-each select="..//tei:schemaSpec">
      <hr/>
      <xsl:for-each select="tei:classSpec">
        <xsl:sort select="tei:altIdent"/>
        <xsl:sort select="@ident"/>
        <xsl:element name="{$tocElement}">
          <xsl:attribute name="class">toclist0</xsl:attribute>
          <a class="toclist" href="#{@ident}">
            <xsl:choose>
              <xsl:when test="tei:altIdent">
                <xsl:value-of select="tei:altIdent"/>
              </xsl:when>
              <xsl:otherwise>
                <xsl:value-of select="@ident"/>
              </xsl:otherwise>
            </xsl:choose>
          </a>
        </xsl:element>
      </xsl:for-each>
      <hr/>
      <xsl:for-each select="tei:elementSpec">
        <xsl:sort select="tei:altIdent"/>
        <xsl:sort select="@ident"/>
        <xsl:element name="{$tocElement}">
          <xsl:attribute name="class">toclist0</xsl:attribute>
          <a class="toclist" href="#{@ident}">
            <xsl:choose>
              <xsl:when test="tei:altIdent">
                <xsl:value-of select="tei:altIdent"/>
              </xsl:when>
              <xsl:otherwise>
                <xsl:value-of select="@ident"/>
              </xsl:otherwise>
            </xsl:choose>
          </a>
        </xsl:element>
      </xsl:for-each>
      <hr/>
      <xsl:for-each select="tei:macroSpec">
        <xsl:sort select="tei:altIdent"/>
        <xsl:sort select="@ident"/>
        <xsl:element name="{$tocElement}">
          <xsl:attribute name="class">toclist0</xsl:attribute>
          <a class="toclist" href="#{@ident}">
            <xsl:choose>
              <xsl:when test="tei:altIdent">
                <xsl:value-of select="tei:altIdent"/>
              </xsl:when>
              <xsl:otherwise>
                <xsl:value-of select="@ident"/>
              </xsl:otherwise>
            </xsl:choose>
          </a>
        </xsl:element>
      </xsl:for-each>
    </xsl:for-each>
  </xsl:template>
  <xsl:template match="tei:elementSpec[@mode='delete']">
    <dt>Element <xsl:value-of select="@ident"/>
      </dt>
    <dd>
      <b>DELETED</b>
    </dd>
  </xsl:template>
  <xsl:template name="verbatim-lineBreak">
    <xsl:param name="id"/>
    <xsl:text disable-output-escaping="yes">&lt;br/&gt;</xsl:text>
  </xsl:template>
  <xsl:template match="rng:ref/@name" mode="attributetext">
    <xsl:variable name="me">
      <xsl:choose>
        <xsl:when test="contains(.,'.attributes')">
          <xsl:value-of select="substring-before(.,'.attributes')"/>
        </xsl:when>
        <xsl:when test="contains(.,'.content')">
          <xsl:value-of select="substring-before(.,'.content')"/>
        </xsl:when>
        <xsl:when test="contains(.,'.attribute.')">
          <xsl:value-of select="substring-before(.,'.attribute.')"/>
        </xsl:when>
        <xsl:otherwise>
          <xsl:value-of select="."/>
        </xsl:otherwise>
      </xsl:choose>
    </xsl:variable>
    <xsl:variable name="n" select="."/>
    <xsl:choose>
      <xsl:when test="contains(.,'.localattributes')">
        <xsl:value-of select="$n"/>
      </xsl:when>
      <xsl:when test="contains(.,'.content')">
        <xsl:value-of select="$n"/>
      </xsl:when>
      <xsl:when test="ancestor::teix:egXML">
        <xsl:value-of select="$n"/>
      </xsl:when>
      <xsl:otherwise>
        <xsl:for-each select="$top">
          <xsl:call-template name="linkTogether">
            <xsl:with-param name="name">
              <xsl:value-of select="$me"/>
            </xsl:with-param>
            <xsl:with-param name="reftext">
              <xsl:value-of select="$n"/>
            </xsl:with-param>
          </xsl:call-template>
        </xsl:for-each>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  <xsl:template match="rng:*">
    <xsl:copy>
      <xsl:copy-of select="@*"/>
      <xsl:apply-templates select="rng:*|tei:*|text()|comment()"/>
    </xsl:copy>
  </xsl:template>
  <xsl:template match="rng:zeroOrMore">
    <xsl:choose>
      <xsl:when test="count(rng:*)=1 and rng:zeroOrMore">
        <xsl:apply-templates select="rng:*|tei:*|text()|comment()"/>
      </xsl:when>
      <xsl:otherwise>
        <rng:zeroOrMore>
          <xsl:copy-of select="@*"/>
          <xsl:apply-templates select="rng:*|tei:*|text()|comment()"/>
        </rng:zeroOrMore>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  <xsl:template match="tei:listRef" mode="weave"/>
  <xsl:template match="tei:elementSpec[@mode='delete']" mode="weave"/>
  <xsl:template match="a:documentation" mode="verbatim"/>
  <xsl:template match="tei:ptr[@type='cit']">
    <a class="citlink">
      <xsl:for-each select="id(substring(@target,2))">
        <xsl:attribute name="href">
          <xsl:apply-templates select="." mode="generateLink"/>
        </xsl:attribute>
        <xsl:apply-templates select="." mode="xref"/>
      </xsl:for-each>
    </a>
  </xsl:template>
  <xsl:template match="tei:divGen[@type='macrocat']" priority="100">
    <h3>Alphabetical list</h3>
    <xsl:for-each select="key('MACRODOCS',1)">
      <xsl:sort select="@module"/>
      <xsl:if test="generate-id(.)=generate-id(key('MACRO-MODULE',@module)[1])">
        <div id="macro-{@module}">
          <h3>
            <xsl:for-each select="key('MODULES',@module)">
              <xsl:text>[</xsl:text>
              <xsl:value-of select="@ident"/>
              <xsl:text>] </xsl:text>
              <xsl:call-template name="makeDescription"/>
            </xsl:for-each>
          </h3>
          <xsl:for-each select="key('MACRO-MODULE',@module)">
            <xsl:sort select="@ident"/>
            <xsl:call-template name="refDocLink"/>
          </xsl:for-each>
        </div>
      </xsl:if>
    </xsl:for-each>
    <xsl:apply-templates mode="weave" select="key('MACRODOCS',1)">
      <xsl:sort select="@ident"/>
    </xsl:apply-templates>
  </xsl:template>
  <xsl:template match="tei:divGen[@type='elementcat']"  priority="100">
    <div class="atozwrapper">
      <xsl:call-template name="atozHeader">
        <xsl:with-param name="Key">ELEMENT-ALPHA</xsl:with-param>
      </xsl:call-template>
      <xsl:for-each select="key('ELEMENTDOCS',1)">
        <xsl:sort select="translate(@ident,$uc,$lc)"/>
        <xsl:variable name="letter">
          <xsl:value-of select="substring(@ident,1,1)"/>
        </xsl:variable>
        <xsl:if test="generate-id(.)=generate-id(key('ELEMENT-ALPHA',$letter)[1])">
          <div id="element-{$letter}" class="atoz">
            <span class="listhead">
              <xsl:value-of select="$letter"/>
            </span>
            <ul class="atoz">
              <xsl:for-each select="key('ELEMENT-ALPHA',$letter)">
                <xsl:sort select="@ident"/>
                <li>
		  <xsl:call-template name="refDocLink"/>
                </li>
              </xsl:for-each>
            </ul>
          </div>
        </xsl:if>
      </xsl:for-each>
    </div>
    <div id="byMod">
      <xsl:for-each select="key('ELEMENTDOCS',1)">
        <xsl:sort select="@module"/>
        <xsl:if test="generate-id(.)=generate-id(key('ELEMENT-MODULE',@module)[1])">
          <div>
            <h3>
              <xsl:for-each select="key('MODULES',@module)">
                <xsl:text>[</xsl:text>
                <xsl:value-of select="@ident"/>
                <xsl:text>] </xsl:text>
                <xsl:call-template name="makeDescription"/>
              </xsl:for-each>
            </h3>
            <xsl:for-each select="key('ELEMENT-MODULE',@module)">
              <xsl:sort
		  select="@ident"/>
	      <xsl:call-template name="refDocLink"/>
	    </xsl:for-each>
          </div>
        </xsl:if>
      </xsl:for-each>
    </div>
    <xsl:for-each select="key('ELEMENTDOCS',1)">
      <xsl:apply-templates mode="weave" select="."/>
    </xsl:for-each>
  </xsl:template>


  <xsl:template name="refDocLink">
    <span
	class="refDocLink">
      <a>
	<xsl:attribute name="href">
	  <xsl:choose>
	    <xsl:when test="number($splitLevel)=-1 or $STDOUT='true'">
	      <xsl:text>#</xsl:text>
	      <xsl:value-of select="@ident"/>
	    </xsl:when>
	    <xsl:otherwise>
	      <xsl:text>ref-</xsl:text>
	      <xsl:value-of
		  select="@ident"/>
	      <xsl:value-of select="$outputSuffix"/>
	    </xsl:otherwise>
	  </xsl:choose>
	</xsl:attribute>
	<xsl:value-of select="@ident"/>
      </a>
      <xsl:text> </xsl:text>
    </span>
  </xsl:template>

  <xsl:template match="tei:divGen[@type='modelclasscat']"  priority="100">
    <div class="atozwrapper">
      <xsl:call-template name="atozHeader">
        <xsl:with-param name="Key">MODEL-CLASS-ALPHA</xsl:with-param>
      </xsl:call-template>
      <xsl:for-each select="key('MODELCLASSDOCS',1)">
        <xsl:sort select="translate(substring-after(@ident,'model.'),$uc,$lc)"/>
        <xsl:variable name="letter">
          <xsl:value-of select="substring(@ident,7,1)"/>
        </xsl:variable>
        <xsl:if test="generate-id(.)=generate-id(key('MODEL-CLASS-ALPHA',$letter)[1])">
          <div id="element-{$letter}" class="atoz">
            <span class="listhead">
              <xsl:value-of select="$letter"/>
            </span>
            <ul class="atoz">
              <xsl:for-each select="key('MODEL-CLASS-ALPHA',$letter)">
                <xsl:sort select="translate(substring-after(@ident,'model.'),$lc,$uc)"/>
                <li>
		  <xsl:call-template name="refDocLink"/>
                </li>
              </xsl:for-each>
            </ul>
          </div>
        </xsl:if>
      </xsl:for-each>
    </div>
    <div id="byMod">
      <xsl:for-each select="key('MODELCLASSDOCS',1)">
        <xsl:sort select="@module"/>
        <xsl:if test="generate-id(.)=generate-id(key('MODEL-CLASS-MODULE',@module)[1])">
          <div>
            <h3>
              <xsl:for-each select="key('MODULES',@module)">
                <xsl:text>[</xsl:text>
                <xsl:value-of select="@ident"/>
                <xsl:text>] </xsl:text>
                <xsl:call-template name="makeDescription"/>
              </xsl:for-each>
            </h3>
            <xsl:for-each select="key('MODEL-CLASS-MODULE',@module)">
              <xsl:sort
		  select="@ident"/>
	      <xsl:call-template name="refDocLink"/>
            </xsl:for-each>
          </div>
        </xsl:if>
      </xsl:for-each>
    </div>
    <xsl:for-each select="key('MODELCLASSDOCS',1)">
      <xsl:apply-templates select="." mode="weave"/>
    </xsl:for-each>
  </xsl:template>
  <xsl:template match="tei:divGen[@type='attclasscat']"  priority="100">
    <div class="atozwrapper">
      <xsl:call-template name="atozHeader">
        <xsl:with-param name="Key">ATT-CLASS-ALPHA</xsl:with-param>
      </xsl:call-template>
      <xsl:for-each select="key('ATTCLASSDOCS',1)">
        <xsl:sort select="translate(substring-after(@ident,'att.'),$uc,$lc)"/>
        <xsl:variable name="letter">
          <xsl:value-of select="substring(@ident,5,1)"/>
        </xsl:variable>
        <xsl:if test="generate-id(.)=generate-id(key('ATT-CLASS-ALPHA',$letter)[1])">
          <div id="element-{$letter}" class="atoz">
            <span class="listhead">
              <xsl:value-of select="$letter"/>
            </span>
            <ul class="atoz">
              <xsl:for-each select="key('ATT-CLASS-ALPHA',$letter)">
                <xsl:sort select="translate(substring-after(@ident,'att.'),$lc,$uc)"/>
                <li>
		  <xsl:call-template name="refDocLink"/>
		</li>
              </xsl:for-each>
            </ul>
          </div>
        </xsl:if>
      </xsl:for-each>
    </div>
    <div id="byMod">
      <xsl:for-each select="key('ATTCLASSDOCS',1)">
        <xsl:sort select="@module"/>
        <xsl:if test="generate-id(.)=generate-id(key('ATT-CLASS-MODULE',@module)[1])">
          <div>
            <h3>
              <xsl:for-each select="key('MODULES',@module)">
                <xsl:text>[</xsl:text>
                <xsl:value-of select="@ident"/>
                <xsl:text>] </xsl:text>
                <xsl:call-template name="makeDescription"/>
              </xsl:for-each>
            </h3>
            <xsl:for-each
		select="key('ATT-CLASS-MODULE',@module)">
	      <xsl:call-template name="refDocLink"/>
            </xsl:for-each>
          </div>
        </xsl:if>
      </xsl:for-each>
    </div>
    <xsl:apply-templates mode="weave" select="key('ATTCLASSDOCS',1)">
      <xsl:sort select="@ident"/>
    </xsl:apply-templates>
  </xsl:template>
  <xsl:template name="javascriptHook">
    <xsl:call-template name="jsForOdds"/>
  </xsl:template>
  <xsl:template name="jsForOdds">
    <script type="text/javascript">
      <xsl:comment>
        <xsl:text disable-output-escaping="yes">
var displayXML=0;
states=new Array()
states[0]="element-a"
states[1]="element-b"
states[2]="element-c"
states[3]="element-d"
states[4]="element-e"
states[5]="element-f"
states[6]="element-g"
states[7]="element-h"
states[8]="element-i"
states[9]="element-j"
states[10]="element-k"
states[11]="element-l"
states[12]="element-m"
states[13]="element-n"
states[14]="element-o"
states[15]="element-p"
states[16]="element-q"
states[17]="element-r"
states[18]="element-s"
states[19]="element-t"
states[20]="element-u"
states[21]="element-v"
states[22]="element-w"
states[23]="element-x"
states[24]="element-y"
states[25]="element-z"

function startUp() {

}

function hideallExcept(elm) {
for (var i = 0; i &lt; states.length; i++) {
 var layer;
 if (layer = document.getElementById(states[i]) ) {
  if (states[i] != elm) {
    layer.style.display = "none";
  }
  else {
   layer.style.display = "block";
      }
  }
 }
 var mod;
 if ( mod = document.getElementById('byMod') ) {
     mod.style.display = "none";
 }
}

function showall() {
 for (var i = 0; i &lt; states.length; i++) {
   var layer;
   if (layer = document.getElementById(states[i]) ) {
      layer.style.display = "block";
      }
  }
}

function showByMod() {
  hideallExcept('');
  var mod;
  if (mod = document.getElementById('byMod') ) {
     mod.style.display = "block";
     }
}

	</xsl:text>
      </xsl:comment>
    </script>
  </xsl:template>

  <xsl:template name="identifyElement">
    <xsl:param name="id"/>
    <xsl:attribute name="id">
      <xsl:value-of select="$id"/>
    </xsl:attribute>
  </xsl:template>

  <xsl:template name="guidelinesTop">
    <xsl:param name="name"/>
    <div id="hdr">
      <xsl:call-template name="stdheader">
        <xsl:with-param name="title">
          <xsl:value-of select="$name"/>
        </xsl:with-param>
      </xsl:call-template>
    </div>
  </xsl:template>

  <xsl:template name="atozHeader">
    <xsl:param name="Key"/>
    <div id="azindex">
      <span>
        <xsl:call-template name="i18n">
          <xsl:with-param name="word">Sorted alphabetically</xsl:with-param>
        </xsl:call-template>
      </span>
      <ul class="index">
        <xsl:if test="count(key($Key,'a'))&gt;0">
          <li>
            <a onclick="hideallExcept('element-a');" href="#">a</a>
          </li>
        </xsl:if>
        <xsl:if test="count(key($Key,'b'))&gt;0">
          <li>
            <a onclick="hideallExcept('element-b');" href="#">b</a>
          </li>
        </xsl:if>
        <xsl:if test="count(key($Key,'c'))&gt;0">
          <li>
            <a onclick="hideallExcept('element-c');" href="#">c</a>
          </li>
        </xsl:if>
        <xsl:if test="count(key($Key,'d'))&gt;0">
          <li>
            <a onclick="hideallExcept('element-d');" href="#">d</a>
          </li>
        </xsl:if>
        <xsl:if test="count(key($Key,'e'))&gt;0">
          <li>
            <a onclick="hideallExcept('element-e');" href="#">e</a>
          </li>
        </xsl:if>
        <xsl:if test="count(key($Key,'f'))&gt;0">
          <li>
            <a onclick="hideallExcept('element-f');" href="#">f</a>
          </li>
        </xsl:if>
        <xsl:if test="count(key($Key,'g'))&gt;0">
          <li>
            <a onclick="hideallExcept('element-g');" href="#">g</a>
          </li>
        </xsl:if>
        <xsl:if test="count(key($Key,'h'))&gt;0">
          <li>
            <a onclick="hideallExcept('element-h');" href="#">h</a>
          </li>
        </xsl:if>
        <xsl:if test="count(key($Key,'i'))&gt;0">
          <li>
            <a onclick="hideallExcept('element-i');" href="#">i</a>
          </li>
        </xsl:if>
        <xsl:if test="count(key($Key,'j'))&gt;0">
          <li>
            <a onclick="hideallExcept('element-j');" href="#">j</a>
          </li>
        </xsl:if>
        <xsl:if test="count(key($Key,'k'))&gt;0">
          <li>
            <a onclick="hideallExcept('element-k');" href="#">k</a>
          </li>
        </xsl:if>
        <xsl:if test="count(key($Key,'l'))&gt;0">
          <li>
            <a onclick="hideallExcept('element-l');" href="#">l</a>
          </li>
        </xsl:if>
        <xsl:if test="count(key($Key,'m'))&gt;0">
          <li>
            <a onclick="hideallExcept('element-m');" href="#">m</a>
          </li>
        </xsl:if>
        <xsl:if test="count(key($Key,'n'))&gt;0">
          <li>
            <a onclick="hideallExcept('element-n');" href="#">n</a>
          </li>
        </xsl:if>
        <xsl:if test="count(key($Key,'o'))&gt;0">
          <li>
            <a onclick="hideallExcept('element-o');" href="#">o</a>
          </li>
        </xsl:if>
        <xsl:if test="count(key($Key,'p'))&gt;0">
          <li>
            <a onclick="hideallExcept('element-p');" href="#">p</a>
          </li>
        </xsl:if>
        <xsl:if test="count(key($Key,'q'))&gt;0">
          <li>
            <a onclick="hideallExcept('element-q');" href="#">q</a>
          </li>
        </xsl:if>
        <xsl:if test="count(key($Key,'r'))&gt;0">
          <li>
            <a onclick="hideallExcept('element-r');" href="#">r</a>
          </li>
        </xsl:if>
        <xsl:if test="count(key($Key,'s'))&gt;0">
          <li>
            <a onclick="hideallExcept('element-s');" href="#">s</a>
          </li>
        </xsl:if>
        <xsl:if test="count(key($Key,'t'))&gt;0">
          <li>
            <a onclick="hideallExcept('element-t');" href="#">t</a>
          </li>
        </xsl:if>
        <xsl:if test="count(key($Key,'u'))&gt;0">
          <li>
            <a onclick="hideallExcept('element-u');" href="#">u</a>
          </li>
        </xsl:if>
        <xsl:if test="count(key($Key,'v'))&gt;0">
          <li>
            <a onclick="hideallExcept('element-v');" href="#">v</a>
          </li>
        </xsl:if>
        <xsl:if test="count(key($Key,'w'))&gt;0">
          <li>
            <a onclick="hideallExcept('element-w');" href="#">w</a>
          </li>
        </xsl:if>
        <xsl:if test="count(key($Key,'x'))&gt;0">
          <li>
            <a onclick="hideallExcept('element-x');" href="#">x</a>
          </li>
        </xsl:if>
        <xsl:if test="count(key($Key,'y'))&gt;0">
          <li>
            <a onclick="hideallExcept('element-y');" href="#">y</a>
          </li>
        </xsl:if>
        <xsl:if test="count(key($Key,'z'))&gt;0">
          <li>
            <a onclick="hideallExcept('element-z');" href="#">z</a>
          </li>
        </xsl:if>
        <li class="showall">
          <a onclick="showall();" href="#">
            <xsl:call-template name="i18n">
              <xsl:with-param name="word">Show all</xsl:with-param>
            </xsl:call-template>
          </a>
        </li>
        <li class="showall">
          <a onclick="showByMod();" href="#">
            <xsl:call-template name="i18n">
              <xsl:with-param name="word">Show by module</xsl:with-param>
            </xsl:call-template>
          </a>
        </li>
      </ul>
    </div>
  </xsl:template>
</xsl:stylesheet>
