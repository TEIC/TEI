<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet 
                xmlns:fotex="http://www.tug.org/fotex"
                xmlns:a="http://relaxng.org/ns/compatibility/annotations/1.0"
                xmlns="http://www.w3.org/1999/XSL/Format"
                xmlns:rng="http://relaxng.org/ns/structure/1.0"
                xmlns:tei="http://www.tei-c.org/ns/1.0"
                xmlns:teix="http://www.tei-c.org/ns/Examples"
                xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                exclude-result-prefixes="a fotex rng tei teix"
                version="2.0">
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" scope="stylesheet" type="stylesheet">
      <desc>
         <p>
    TEI stylesheet dealing  with elements from the core module, making XSL-FO output.
      </p>
         <p>
This software is dual-licensed:

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
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>Process elements  processing-instruction()[name()='xmltex']</desc>
   </doc>
  <xsl:template match="processing-instruction()[name()='xmltex']">
      <xsl:message>xmltex pi <xsl:value-of select="."/>
      </xsl:message>
      <xsl:copy-of select="."/>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc/>
   </doc>
  <xsl:template match="tei:ab">
      <block>
         <xsl:apply-templates/>
      </block>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>abbreviation</desc>
   </doc>
  <xsl:template match="tei:abbr">
      <xsl:apply-templates/>
  </xsl:template>

  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>line break</desc>
   </doc>
  <xsl:template match="tei:cell//tei:lb">
      <xsl:choose>
         <xsl:when test="$foEngine='passivetex'">&#8232;</xsl:when>
         <xsl:otherwise>
            <inline linefeed-treatment="preserve">
               <xsl:text>&#10;</xsl:text>
            </inline>
         </xsl:otherwise>
      </xsl:choose>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc/>
   </doc>
  <xsl:template match="tei:code">
      <inline font-family="{$typewriterFont}">
         <xsl:apply-templates/>
      </inline>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>deletions</desc>
   </doc>
  <xsl:template match="tei:del">
      <inline text-decoration="line-through">
         <xsl:apply-templates/>
      </inline>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc/>
   </doc>
  <xsl:template match="tei:eg">
      <block font-family="{$typewriterFont}" background-color="{$exampleBackgroundColor}"
             color="{$exampleColor}"
             white-space-treatment="preserve"
             linefeed-treatment="preserve"
             white-space-collapse="false"
             wrap-option="no-wrap"
             text-indent="0em"
             hyphenate="false"
             start-indent="{$exampleMargin}"
             text-align="start"
             font-size="{$exampleSize}"
             space-before.optimum="4pt"
             space-after.optimum="4pt">
         <xsl:if test="not($flowMarginLeft='')">
            <xsl:attribute name="padding-start">
               <xsl:value-of select="$exampleMargin"/>
            </xsl:attribute>
         </xsl:if>
         <xsl:if test="parent::tei:exemplum">
            <xsl:text>&#10;</xsl:text>
         </xsl:if>
         <xsl:value-of select="translate(.,' ','&#160;')"/>
      </block>
  </xsl:template>

  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc/>
   </doc>
  <xsl:template match="teix:egXML">
      <xsl:param name="simple">false</xsl:param>
      <xsl:param name="highlight"/>
      <block font-family="{$typewriterFont}" background-color="{$exampleBackgroundColor}"
             color="{$exampleColor}"
             white-space-treatment="preserve"
             linefeed-treatment="preserve"
             white-space-collapse="false"
             wrap-option="no-wrap"
             text-indent="0em"
             hyphenate="false"
             start-indent="{$exampleMargin}"
             text-align="start"
             font-size="{$exampleSize}"
             space-before.optimum="4pt"
             space-after.optimum="4pt">
         <xsl:if test="not($flowMarginLeft='')">
            <xsl:attribute name="padding-start">
               <xsl:value-of select="$exampleMargin"/>
            </xsl:attribute>
         </xsl:if>
         <xsl:apply-templates mode="verbatim">
	   <xsl:with-param name="highlight">
	     <xsl:value-of select="$highlight"/>
	   </xsl:with-param>
         </xsl:apply-templates>
      </block>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc/>
   </doc>
  <xsl:template match="tei:eg[tei:match(@rend,'kwic')]/tei:lb"/>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc/>
   </doc>
  <xsl:template match="tei:emph">
      <inline>
	<xsl:call-template name="rend"/>
        <xsl:apply-templates/>
      </inline>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc/>
   </doc>
  <xsl:template match="tei:epigraph">
      <block text-align="center" space-before.optimum="4pt" space-after.optimum="4pt"
             start-indent="{$exampleMargin}">
         <xsl:apply-templates/>
      </block>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc/>
   </doc>
  <xsl:template match="tei:epigraph/tei:lg">
      <block text-align="center" space-before.optimum="4pt" space-after.optimum="4pt">
         <xsl:apply-templates/>
      </block>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc/>
   </doc>
  <xsl:template match="tei:epigraph/tei:q">
      <block space-before.optimum="4pt" space-after.optimum="4pt"
             start-indent="{$exampleMargin}">
         <xsl:apply-templates/>
      </block>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc/>
   </doc>
  <xsl:template match="tei:gap">
      <inline border-style="solid">
         <xsl:text>[</xsl:text>
         <xsl:value-of select="@reason"/>
         <xsl:text>]</xsl:text>
      </inline>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc/>
   </doc>
  <xsl:template match="tei:gi">
      <inline hyphenate="false" color="{$giColor}" font-family="{$typewriterFont}">
         <xsl:text>&lt;</xsl:text>
         <xsl:apply-templates/>
         <xsl:text>&gt;</xsl:text>
      </inline>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc/>
   </doc>
  <xsl:template match="tei:att">
      <inline hyphenate="false" color="{$giColor}" font-family="{$typewriterFont}"
              font-style="italic">
         <xsl:text>@</xsl:text>
         <xsl:apply-templates/>
      </inline>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>gloss</desc>
   </doc>
  <xsl:template match="tei:gloss">
      <xsl:apply-templates/>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>hi</desc>
   </doc>
  <xsl:template match="tei:hi">
      <inline>
         <xsl:call-template name="rend"/>
         <xsl:apply-templates/>
      </inline>
  </xsl:template>
  <xsl:template match="tei:seg">
    <xsl:choose>
      <xsl:when test="@rend">
	<inline>
          <xsl:call-template name="rend"/>
         <xsl:apply-templates/>
      </inline>
      </xsl:when>
      <xsl:otherwise>
	<xsl:apply-templates/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc/>
   </doc>
  <xsl:template match="tei:ident">
      <inline color="{$identColor}" font-family="{$sansFont}">
         <xsl:apply-templates/>
      </inline>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc/>
   </doc>
  <xsl:template match="tei:index"/>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc/>
   </doc>
  <xsl:template match="tei:interp">
      <xsl:apply-templates/>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc/>
   </doc>
  <xsl:template match="tei:interpGrp">
      <xsl:apply-templates/>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc/>
   </doc>
  <xsl:template match="tei:item" mode="catalogue">
      <table-cell>
         <block>
            <xsl:choose>
               <xsl:when test="tei:label">
                  <inline font-weight="bold">
                     <xsl:apply-templates select="tei:label" mode="print"/>
                  </inline>
               </xsl:when>
               <xsl:otherwise>
                  <inline font-weight="bold">
                     <xsl:apply-templates mode="print" select="preceding-sibling::tei:*[1]"/>
                  </inline>
               </xsl:otherwise>
            </xsl:choose>
         </block>
      </table-cell>
      <table-cell>
         <block>
            <xsl:apply-templates/>
         </block>
      </table-cell>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc/>
   </doc>
  <xsl:template match="tei:item|tei:biblStruct|tei:listBibl/tei:bibl">
      <xsl:call-template name="makeItem"/>
  </xsl:template>
  <xsl:template match="tei:item" mode="xref">
      <xsl:variable name="listdepth" select="count(ancestor::tei:list)"/>
      <xsl:if test="parent::tei:list[@type='bibliography']">
         <xsl:text> [</xsl:text>
      </xsl:if>
      <xsl:variable name="listNFormat">
         <xsl:choose>
            <xsl:when test="$listdepth=1">
               <xsl:text>1</xsl:text>
            </xsl:when>
            <xsl:when test="$listdepth=2">
               <xsl:text>i</xsl:text>
            </xsl:when>
            <xsl:when test="$listdepth=3">
               <xsl:text>a</xsl:text>
            </xsl:when>
            <xsl:when test="$listdepth=4">
               <xsl:text>I</xsl:text>
            </xsl:when>
         </xsl:choose>
      </xsl:variable>
      <xsl:number format="{$listNFormat}"/>
      <xsl:if test="parent::tei:list[@type='bibliography']">
         <xsl:text>]</xsl:text>
      </xsl:if>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc/>
   </doc>
  <xsl:template match="tei:kw">
      <inline font-style="italic">
         <xsl:apply-templates/>
      </inline>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc/>
   </doc>
  <xsl:template match="tei:l">
      <block space-before.optimum="0pt" space-after.optimum="0pt">
         <xsl:choose>
            <xsl:when test="starts-with(@rend,'indent(')">
               <xsl:attribute name="text-indent">
                  <xsl:value-of select="concat(substring-before(substring-after(@rend,'('),')'),'em')"/>
               </xsl:attribute>
            </xsl:when>
            <xsl:when test="starts-with(@rend,'indent')">
               <xsl:attribute name="text-indent">1em</xsl:attribute>
            </xsl:when>
         </xsl:choose>
         <xsl:apply-templates/>
      </block>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc/>
   </doc>
  <xsl:template match="tei:label" mode="print">
      <xsl:apply-templates/>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>label element in mode 'print'</desc>
   </doc>
  <xsl:template match="tei:list/tei:label"/>
  <xsl:template name="lineBreakAsPara">
    <block/>
  </xsl:template>
  <xsl:template name="lineBreak">
    <xsl:choose>
      <xsl:when test="parent::tei:hi">
	<inline
	    white-space-treatment="preserve"
	    white-space-collapse="false">&#xA;</inline>
      </xsl:when>
      <xsl:when test="$foEngine='passivetex'">&#8232;</xsl:when>
      <xsl:otherwise>
	<block/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>list bibl</desc>
   </doc>
  <xsl:template match="tei:list|tei:listBibl">
      <xsl:if test="child::tei:head">
         <block font-style="italic" text-align="start" space-before.optimum="4pt">
            <xsl:for-each select="tei:head">
               <xsl:apply-templates/>
            </xsl:for-each>
         </block>
      </xsl:if>
      <xsl:choose>
	<xsl:when test="tei:msDesc">
               <xsl:apply-templates/>
	</xsl:when>
         <xsl:when test="@type='runin'">
            <block>
               <xsl:apply-templates mode="inline"/>
            </block>
         </xsl:when>
         <xsl:otherwise>
            <list-block>
               <xsl:call-template name="setListIndents"/>
               <xsl:apply-templates/>
            </list-block>
         </xsl:otherwise>
      </xsl:choose>
  </xsl:template>
    
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>cit element</desc>
   </doc>
  <xsl:template match="tei:cit">
    <xsl:choose>
      <xsl:when test="tei:match(@rend,'display') or tei:match(@rend,'block')">
	<block font-size="8pt">
	  <xsl:apply-templates/>
	</block>
      </xsl:when>
      <xsl:otherwise>
	<xsl:apply-templates/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>catalogue-style list</desc>
   </doc>
  <xsl:template match="tei:list[@type='catalogue']">
      <block space-before="{$spaceAroundTable}" space-after="{$spaceAroundTable}">
         <table>
<!--   MDH: FOP (at 2.1, which is current on 2016-12-30) does not support the 
       default @table-layout="auto".          -->
             <xsl:if test="$foEngine = 'fop'">
                 <xsl:attribute name="table-layout" select="'fixed'"/>
                 <xsl:attribute name="width" select="'100%'"/>
             </xsl:if>
            <table-column column-number="1" column-width="20%">
               <xsl:if test="$foEngine='passivetex'">
                  <xsl:attribute name="column-align" namespace="http://www.tug.org/fotex">p</xsl:attribute>
               </xsl:if>
            </table-column>
            <table-column column-number="2" column-width="80%">
               <xsl:if test="$foEngine='passivetex'">
                  <xsl:attribute name="column-align" namespace="http://www.tug.org/fotex">p</xsl:attribute>
               </xsl:if>
            </table-column>
            <table-body>
               <xsl:for-each select="tei:item">
                  <table-row>
                     <xsl:apply-templates select="." mode="catalogue"/>
                  </table-row>
               </xsl:for-each>
            </table-body>
         </table>
      </block>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>line group</desc>
   </doc>
  <xsl:template match="tei:lg">
      <block start-indent="{$exampleMargin}" text-align="start" space-before.optimum="4pt"
             space-after.optimum="4pt">
         <xsl:apply-templates/>
      </block>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>a name</desc>
   </doc>
  <xsl:template match="tei:name">
      <xsl:apply-templates/>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>end note processing</desc>
   </doc>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>end note</desc>
   </doc>
  <xsl:template name="endNote">    
    <basic-link>
      <xsl:attribute name="internal-destination">
	<xsl:value-of select="generate-id()"/>
      </xsl:attribute>
      <inline font-size="{$footnotenumSize}" vertical-align="super">
	<xsl:choose>
	  <xsl:when test="@n">
	    <xsl:value-of select="@n"/>
	  </xsl:when>
	  <xsl:otherwise>
	    <xsl:call-template name="calculateEndNoteNumber"/>
	  </xsl:otherwise>
	</xsl:choose>
      </inline>
    </basic-link>
  </xsl:template>

  <xsl:template name="plainNote">
    <xsl:element name="{if (tei:isInline(.)) then 'inline' else 'block'}">
      <xsl:attribute name="font-size" select="$footnoteSize"/>
      <xsl:attribute name="font-style">italic</xsl:attribute>
      <xsl:text> [</xsl:text>
      <xsl:choose>
	<xsl:when test="@n">
	  <xsl:value-of select="@n"/>
	</xsl:when>
	<xsl:otherwise>
	  <xsl:sequence select="tei:i18n('Note')"/>
	  <xsl:text>: </xsl:text>
	</xsl:otherwise>
      </xsl:choose>
      <xsl:apply-templates/>
      <xsl:text>] </xsl:text>
    </xsl:element>
  </xsl:template>

  <xsl:template name="displayNote">
    <block text-indent="0pt" end-indent="{$exampleMargin}" start-indent="{$exampleMargin}"
	   font-size="{$exampleSize}"
	   space-before.optimum="{$exampleBefore}"
	   space-after.optimum="{$exampleAfter}">
      <xsl:apply-templates/>
    </block>
  </xsl:template>

  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>Create a footnote</desc>
   </doc>
    
  <xsl:template name="footNote">
        <xsl:variable name="FootID">
          <xsl:choose>
            <xsl:when test="@n">
              <xsl:value-of select="@n"/>
            </xsl:when>
            <xsl:otherwise>
              <xsl:call-template name="calculateFootnoteNumber"/>
            </xsl:otherwise>
          </xsl:choose>
        </xsl:variable>
        <footnote>
          <inline>
            <xsl:if test="not(@target)">
              <xsl:attribute name="font-size">
                  <xsl:value-of select="$footnotenumSize"/>
              </xsl:attribute>
              <xsl:attribute name="vertical-align">super</xsl:attribute>
              <xsl:value-of select="$FootID"/>
            </xsl:if>
          </inline>
          <footnote-body>
            <block end-indent="0pt" start-indent="0pt" text-align="start" font-style="normal"
                   text-indent="{$parIndent}"
                   font-size="{$footnoteSize}">
              <xsl:attribute name="line-height">
                <xsl:choose>
                  <xsl:when test="$lineheightApplicationRules = ('footnote', 'all')">
                    <xsl:choose>
                      <xsl:when test="ancestor::tei:front">
                        <xsl:value-of select="$lineheightFrontpage"/>
                      </xsl:when>
                      <xsl:when test="ancestor::tei:body">
                        <xsl:value-of select="$lineheightBodypage"/>
                      </xsl:when>
                      <xsl:when test="ancestor::tei:back">
                        <xsl:value-of select="$lineheightBackpage"/>
                      </xsl:when>
                    </xsl:choose>
                  </xsl:when>
                  <xsl:otherwise>normal</xsl:otherwise>
                </xsl:choose>
              </xsl:attribute>
              <xsl:if test="@xml:id">
                  <xsl:attribute name="id">
                     <xsl:value-of select="@xml:id"/>
                  </xsl:attribute>
              </xsl:if>
              <xsl:if test="not(@target)">
                  <inline font-size="{$footnotenumSize}" vertical-align="super">
                     <xsl:value-of select="$FootID"/>
                  </inline>
                  <xsl:text> </xsl:text>
              </xsl:if>
              <xsl:apply-templates/>
            </block>
          </footnote-body>
        </footnote>
   </xsl:template>


  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>Process element tei:p</desc>
   </doc>

  <xsl:template match="tei:p">
      <block>
         <xsl:if test="not(parent::tei:note)">
            <xsl:attribute name="line-height">
               <xsl:choose>
                  <xsl:when test="ancestor::tei:front">
                     <xsl:value-of select="$lineheightFrontpage"/>
                  </xsl:when>
                  <xsl:when test="ancestor::tei:body">
                     <xsl:value-of select="$lineheightBodypage"/>
                  </xsl:when>
                  <xsl:when test="ancestor::tei:back">
                     <xsl:value-of select="$lineheightBackpage"/>
                  </xsl:when>
<!-- MDH 2017-04-02: It turns out that metadata in the header may 
     also be processed with this, so provide a default. -->
                   <xsl:otherwise>
                     <xsl:value-of select="'1'"/>
                   </xsl:otherwise>
               </xsl:choose>
            </xsl:attribute>
         </xsl:if>
         <xsl:if test="preceding-sibling::tei:p">
            <xsl:attribute name="text-indent">
               <xsl:value-of select="$parIndent"/>
            </xsl:attribute>
            <xsl:attribute name="space-before.optimum">
               <xsl:value-of select="$parSkip"/>
            </xsl:attribute>
            <xsl:attribute name="space-before.maximum">
               <xsl:value-of select="$parSkipmax"/>
            </xsl:attribute>
         </xsl:if>
         <xsl:if test="@xml:lang">
            <xsl:attribute name="country">
               <xsl:value-of select="substring-after(@xml:lang,'-')"/>
            </xsl:attribute>
            <xsl:attribute name="language">
               <xsl:value-of select="substring-before(@xml:lang,'-')"/>
            </xsl:attribute>
         </xsl:if>
         <xsl:apply-templates/>
      </block>
  </xsl:template>

  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc/>
   </doc>
  <xsl:template match="tei:pb">
      <xsl:variable name="e" select="if (tei:isInline(..)) then
	'inline' else 'block'"/>
      <xsl:choose>
         <xsl:when test="parent::tei:list"/>
         <xsl:when test="$pagebreakStyle='active'">
	   <xsl:element name="{$e}">
	     <xsl:attribute name="break-before">page</xsl:attribute>
	     <xsl:if test="@xml:id">
	       <xsl:attribute name="id">
		 <xsl:value-of select="@xml:id"/>
	       </xsl:attribute>
	     </xsl:if>
	   </xsl:element>
         </xsl:when>
         <xsl:when test="$pagebreakStyle='visible'">
	   <xsl:element name="{$e}">
	     <xsl:if test="@xml:id">
	       <xsl:attribute name="id">
		 <xsl:value-of select="@xml:id"/>
	       </xsl:attribute>
	     </xsl:if>
	     <xsl:text> [</xsl:text>
	     <xsl:value-of select="@unit"/>
	     <xsl:text> Page </xsl:text>
	     <xsl:value-of select="@n"/>
	     <xsl:text>] </xsl:text>
	   </xsl:element>
         </xsl:when>
      </xsl:choose>
  </xsl:template>

  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc>
      <p>Handle cb, gb,and milestone elements here by summarily ignoring
        them, because the template in common/common_core.xsl (which calls
        'makeInline') does not work for us: it spits out text that is not
        wrapped in an FO element, and thus causes an error. (See issue
        <a href="https://github.com/TEIC/Stylesheets/issues/334">334</a>.)
        For some ideas on what *might* be done here instead, you should
        probably take a look at the template for the tei:pb element as a
        starting point.</p>
      <p> — Syd on behalf of TEI Stylesheets group, 2018-11-19</p>
    </desc>
  </doc>
  <xsl:template match="tei:cb|tei:gb|tei:milestone">
    <xsl:comment> This stylesheet does not handle <xsl:value-of select="local-name(.)"/> yet. </xsl:comment>
  </xsl:template>
    
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>Quotations</desc>
   </doc>
  <xsl:template match="tei:quote">
    <xsl:choose>
      <xsl:when test="not(tei:isInline(.))">
      <block text-align="start" text-indent="0pt" end-indent="{$exampleMargin}"
             start-indent="{$exampleMargin}"
             font-size="{$quoteSize}"
             space-before.optimum="{$exampleBefore}"
             space-after.optimum="{$exampleAfter}">
        <xsl:attribute name="line-height">
          <xsl:choose>
            <xsl:when test="$lineheightApplicationRules = ('block-quote', 'all')and (not(parent::tei:note) or $lineheightApplicationRules = 'note')">
              <xsl:choose>
                <xsl:when test="ancestor::tei:front">
                  <xsl:value-of select="$lineheightFrontpage"/>
                </xsl:when>
                <xsl:when test="ancestor::tei:body">
                  <xsl:value-of select="$lineheightBodypage"/>
                </xsl:when>
                <xsl:when test="ancestor::tei:back">
                  <xsl:value-of select="$lineheightBackpage"/>
                </xsl:when>
              </xsl:choose>
            </xsl:when>
            <xsl:otherwise>normal</xsl:otherwise>
          </xsl:choose>
        </xsl:attribute>
	<xsl:if test="@xml:id">
	  <xsl:attribute name="id">
	    <xsl:value-of select="@xml:id"/>
	  </xsl:attribute>
	</xsl:if>
         <xsl:apply-templates/>
      </block>
      </xsl:when>
      <xsl:otherwise>
	<xsl:call-template name="makeQuote"/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>


  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>quoted text</desc>
   </doc>
  <xsl:template match="tei:q|tei:said">
      <xsl:choose>
         <xsl:when test="tei:text">
            <xsl:apply-templates/>
         </xsl:when>
         <xsl:when test="tei:match(@rend,'literal')">
            <block white-space-collapse="false" wrap-option="no-wrap" font-size="{$exampleSize}"
                   space-before.optimum="4pt"
                   space-after.optimum="4pt"
                   start-indent="{$exampleMargin}"
                   font-family="{$typewriterFont}">
               <xsl:apply-templates/>
            </block>
         </xsl:when>
         <xsl:when test="tei:match(@rend,'eg')">
            <block text-align="start" font-size="{$exampleSize}" space-before.optimum="4pt"
                   text-indent="0pt"
                   space-after.optimum="4pt"
                   start-indent="{$exampleMargin}"
                   font-family="{$typewriterFont}">
               <xsl:apply-templates/>
            </block>
         </xsl:when>
	 <xsl:when test="not(tei:isInline(.))">
	   <block text-align="start" text-indent="0pt" end-indent="{$exampleMargin}"
		  start-indent="{$exampleMargin}"
		  font-size="{$quoteSize}"
		  space-before.optimum="{$exampleBefore}"
		  space-after.optimum="{$exampleAfter}">
	     <xsl:apply-templates/>
	   </block>
         </xsl:when>
	 <xsl:otherwise>
	   <xsl:call-template name="makeQuote"/>
	 </xsl:otherwise>
      </xsl:choose>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>decorative initial letter</desc>
   </doc>
  <xsl:template match="tei:seg[tei:match(@rend,'decorInit')]">
    <inline background-color="yellow"              font-size="36pt">
      <xsl:apply-templates/>
    </inline>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>[fo] add an ID</desc>
   </doc>
  <xsl:template name="addID">
      <xsl:attribute name="id">
         <xsl:choose>
            <xsl:when test="@xml:id">
               <xsl:value-of select="@xml:id"/>
            </xsl:when>
            <xsl:otherwise>
               <xsl:value-of select="generate-id()"/>
            </xsl:otherwise>
         </xsl:choose>
      </xsl:attribute>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>[fo] process "rend" attribute<param name="value">value of "rend" attribute</param>
      </desc>
   </doc>
  <xsl:template name="applyRend">
      <xsl:param name="value"/>
      <xsl:choose>
         <xsl:when test="$value='gothic'">
            <xsl:attribute name="font-family">fantasy</xsl:attribute>
         </xsl:when>
         <xsl:when test="$value='calligraphic'">
            <xsl:attribute name="font-family">cursive</xsl:attribute>
         </xsl:when>
         <xsl:when test="$value='BO'">
            <xsl:attribute name="font-style">italic</xsl:attribute>
            <xsl:attribute name="text-decoration">underline</xsl:attribute>
         </xsl:when>
      </xsl:choose>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>[fo] </desc>
   </doc>
  <xsl:template name="calculateEndNoteNumber">
      <xsl:number level="any" format="i" count="tei:note[@place='end']"/>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>[fo] </desc>
   </doc>
  <xsl:template name="calculateFootnoteNumber">
      <xsl:number from="tei:text" level="any" count="tei:note[@place='foot']"/>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc>process rend attribute</desc>
   </doc>
  <xsl:template name="rend">
      <xsl:if test="tei:render-smallcaps(.)" >
        <xsl:attribute name="font-variant">small-caps</xsl:attribute>
      </xsl:if>
    <xsl:if test="tei:render-bold(.)" >
      <xsl:attribute name="font-weight">bold</xsl:attribute>
    </xsl:if>
    <xsl:if test="tei:render-italic(.)" >
      <xsl:attribute name="font-style">italic</xsl:attribute>
    </xsl:if>
    <xsl:if test="tei:render-underline(.)" >
      <xsl:attribute name="text-decoration">underline</xsl:attribute>
    </xsl:if>
    <xsl:if test="tei:render-subscript(.)" >
      <xsl:attribute name="vertical-align">sub</xsl:attribute>
    </xsl:if>
    <xsl:if test="tei:render-superscript(.)" >
      <xsl:attribute name="vertical-align">super</xsl:attribute>
    </xsl:if>
    <xsl:if test="tei:render-strike(.)">
      <xsl:attribute name="text-decoration">line-through</xsl:attribute>
    </xsl:if>
    <xsl:if test="tei:render-typewriter(.)">
      <xsl:attribute name="font-family">
        <xsl:value-of select="$typewriterFont"/>
      </xsl:attribute>
    </xsl:if>      
    <xsl:for-each select="tokenize(@rend, ' ')">
      <xsl:call-template name="applyRend">
        <xsl:with-param name="value" select="."/>
      </xsl:call-template>
    </xsl:for-each>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>[fo] Spacing setup for list blocks</desc>
   </doc>
  <xsl:template name="setListIndents">
      <xsl:attribute name="provisional-distance-between-starts">
         <xsl:choose>
            <xsl:when test="self::tei:listBibl[tei:biblStruct]">
               <xsl:value-of select="$betweenBiblStarts"/>
            </xsl:when>
            <xsl:when test="@type='gloss'">
               <xsl:value-of select="$betweenGlossStarts"/>
            </xsl:when>
            <xsl:otherwise>
               <xsl:value-of select="$betweenStarts"/>
            </xsl:otherwise>
         </xsl:choose>
      </xsl:attribute>
      <xsl:attribute name="provisional-label-separation">
         <xsl:value-of select="$labelSeparation"/>
      </xsl:attribute>
      <xsl:attribute name="text-indent">0em</xsl:attribute>
      <xsl:attribute name="margin-right">
         <xsl:value-of select="$listRightMargin"/>
      </xsl:attribute>
      <xsl:variable name="listdepth" select="count(ancestor::tei:list)"/>
      <xsl:choose>
         <xsl:when test="$listdepth=0">
            <xsl:attribute name="space-before">
               <xsl:value-of select="$listAbove-1"/>
            </xsl:attribute>
            <xsl:attribute name="space-after">
               <xsl:value-of select="$listBelow-1"/>
            </xsl:attribute>
         </xsl:when>
         <xsl:when test="$listdepth=1">
            <xsl:attribute name="space-before">
               <xsl:value-of select="$listAbove-2"/>
            </xsl:attribute>
            <xsl:attribute name="space-after">
               <xsl:value-of select="$listBelow-2"/>
            </xsl:attribute>
         </xsl:when>
         <xsl:when test="$listdepth=2">
            <xsl:attribute name="space-before">
               <xsl:value-of select="$listAbove-3"/>
            </xsl:attribute>
            <xsl:attribute name="space-after">
               <xsl:value-of select="$listBelow-3"/>
            </xsl:attribute>
         </xsl:when>
         <xsl:when test="$listdepth=3">
            <xsl:attribute name="space-before">
               <xsl:value-of select="$listAbove-4"/>
            </xsl:attribute>
            <xsl:attribute name="space-after">
               <xsl:value-of select="$listBelow-4"/>
            </xsl:attribute>
         </xsl:when>
      </xsl:choose>
  </xsl:template>

  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>Process element soCalled</desc>
   </doc>
  <xsl:template match="tei:soCalled">
      <xsl:value-of select="$preQuote"/>
      <xsl:apply-templates/>
      <xsl:value-of select="$postQuote"/>
  </xsl:template>

   <xsl:template name="emphasize">
      <xsl:param name="class"/>
      <xsl:param name="content"/>
      <xsl:choose>
         <xsl:when test="$class='titlem'">
            <inline>
	      <xsl:attribute name="font-style">italic</xsl:attribute>
	      <xsl:copy-of select="$content"/>
            </inline>
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

  <xsl:template name="applyRendition"/>

  <xsl:template name="makeSpan">
    <xsl:apply-templates/>
  </xsl:template>

</xsl:stylesheet>
