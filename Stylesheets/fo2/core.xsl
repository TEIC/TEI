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
      <desc/>
   </doc>
  <xsl:template match="tei:abbr">
      <xsl:apply-templates/>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc/>
   </doc>
  <xsl:template match="tei:add">
      <xsl:choose>
         <xsl:when test="@place='sup' or @place='above'">
            <inline vertical-align="super">
               <xsl:apply-templates/>
            </inline>
         </xsl:when>
         <xsl:when test="@place='sub' or @place='below'">
            <inline vertical-align="sub">
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
  <xsl:template match="tei:byline">
      <block text-align="center">
         <xsl:apply-templates/>
      </block>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc/>
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
      <desc/>
   </doc>
  <xsl:template match="tei:corr">
      <xsl:text>[</xsl:text>
      <xsl:apply-templates/>
      <xsl:text>]</xsl:text>
      <xsl:choose>
         <xsl:when test="@sic">
            <footnote>
               <footnote-citation>
                  <inline font-size="8pt" vertical-align="super">
                     <xsl:number format="a" level="any" count="tei:corr"/>
                  </inline>
               </footnote-citation>
               <list-block>
                  <xsl:attribute name="provisional-distance-between-starts">
                     <xsl:value-of select="$betweenStarts"/>
                  </xsl:attribute>
                  <xsl:attribute name="provisional-label-separation">
                     <xsl:value-of select="$labelSeparation"/>
                  </xsl:attribute>
                  <list-item>
                     <list-item-label end-indent="label-end()">
                        <block>
                           <inline font-size="{$footnoteSize}" vertical-align="super">
                              <xsl:number format="a" level="any" count="tei:corr"/>
                           </inline>
                        </block>
                     </list-item-label>
                     <list-item-body start-indent="body-start()">
                        <block font-size="{$footnoteSize}">
                           <xsl:value-of select="@sic"/>
                        </block>
                     </list-item-body>
                  </list-item>
               </list-block>
            </footnote>
         </xsl:when>
      </xsl:choose>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc/>
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
  <xsl:template match="tei:eg[@rend='kwic']/tei:lb"/>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc/>
   </doc>
  <xsl:template match="tei:emph">
      <inline font-style="italic">
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
  <xsl:template match="tei:foreign">
      <inline font-style="italic">
         <xsl:apply-templates/>
      </inline>
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
              font-weight="bold">
         <xsl:text>@</xsl:text>
         <xsl:apply-templates/>
      </inline>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc/>
   </doc>
  <xsl:template match="tei:gloss">
      <xsl:apply-templates/>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc/>
   </doc>
  <xsl:template match="tei:hi">
      <inline>
         <xsl:call-template name="rend">
            <xsl:with-param name="defaultvalue" select="string('bold')"/>
            <xsl:with-param name="defaultstyle" select="string('font-weight')"/>
            <xsl:with-param name="rend" select="@rend"/>
         </xsl:call-template>
         <xsl:apply-templates/>
      </inline>
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
  <xsl:template match="tei:item|tei:biblStruct">
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
      <desc/>
   </doc>
  <xsl:template match="tei:label"/>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc/>
   </doc>
  <xsl:template match="tei:lb">
      <xsl:choose>
	<xsl:when test="$activeLinebreaks='true'">
	  <xsl:choose>
	    <!-- this is a *visible* linebreak character 
		 PassiveTeX implements it as a real line break
	    -->
	    <xsl:when test="$foEngine='passivetex'">&#8232;</xsl:when>
	    <xsl:when test="parent::tei:list">
	      <list-item>
		<list-item-label>
		  <block/>
		</list-item-label>
		<list-item-body>
		  <block/>
		</list-item-body>
	      </list-item>
	    </xsl:when>
	    <xsl:when test="parent::tei:hi">
	      <inline
		  white-space-treatment="preserve"
		  white-space-collapse="false">&#xA;</inline>
	    </xsl:when>
	    <xsl:otherwise>
	      <block/>
	    </xsl:otherwise>
	  </xsl:choose>
	</xsl:when>
	<xsl:otherwise>
	  <inline font-size="8pt">
	    <xsl:text>❡</xsl:text>
	  </inline>
	</xsl:otherwise>
      </xsl:choose>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc/>
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
         <xsl:when test="@type='runin'">
            <block>
               <xsl:apply-templates mode="runin"/>
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
      <xsl:when test="@rend='display'">
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
      <desc>Bibl elements</desc>
   </doc>
  <xsl:template match="tei:bibl">
    <xsl:choose>
      <xsl:when test="parent::tei:listBibl">
	<xsl:call-template name="makeItem"/>
      </xsl:when>
      <xsl:otherwise>
	<block>
	  <xsl:apply-templates/>
	</block>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc/>
   </doc>
  <xsl:template match="tei:list[@type='catalogue']">
      <block space-before="{$spaceAroundTable}" space-after="{$spaceAroundTable}">
         <table>
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
      <desc/>
   </doc>
  <xsl:template match="tei:lg">
      <block start-indent="{$exampleMargin}" text-align="start" space-before.optimum="4pt"
             space-after.optimum="4pt">
         <xsl:apply-templates/>
      </block>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc/>
   </doc>
  <xsl:template match="tei:mentioned">
      <inline>
         <xsl:call-template name="rend">
            <xsl:with-param name="defaultvalue" select="string('italic')"/>
            <xsl:with-param name="defaultstyle" select="string('font-style')"/>
         </xsl:call-template>
         <xsl:apply-templates/>
      </inline>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc/>
   </doc>
  <xsl:template match="tei:milestone">
      <block>
         <xsl:text>******************</xsl:text>
         <xsl:value-of select="@unit"/>
         <xsl:text> </xsl:text>
         <xsl:value-of select="@n"/>
         <xsl:text>******************</xsl:text>
      </block>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc/>
   </doc>
  <xsl:template match="tei:name">
      <xsl:apply-templates/>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc/>
   </doc>
  <xsl:template match="tei:note" mode="endnote">
      <block id="{generate-id()}">
         <xsl:call-template name="calculateEndNoteNumber"/>
         <xsl:text>. </xsl:text>
         <xsl:apply-templates/>
      </block>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc/>
   </doc>
  <xsl:template match="tei:note">
      <xsl:choose>
         <xsl:when test="ancestor::tei:p or ancestor::tei:item or ancestor::tei:cit">
            <xsl:apply-templates select="." mode="real"/>
         </xsl:when>
         <xsl:otherwise>
            <block>
               <xsl:apply-templates select="." mode="real"/>
            </block>
         </xsl:otherwise>
      </xsl:choose>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc/>
   </doc>
  <xsl:template match="tei:note" mode="real">
      <xsl:choose>
         <xsl:when test="@place='end'">
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
         </xsl:when>
         <xsl:when test="@place='inline'">
            <inline>
               <xsl:text> (</xsl:text>
               <xsl:apply-templates/>
               <xsl:text>)</xsl:text>
            </inline>
         </xsl:when>
         <xsl:when test="@place='display'">
            <block text-indent="0pt" end-indent="{$exampleMargin}" start-indent="{$exampleMargin}"
                   font-size="{$exampleSize}"
                   space-before.optimum="{$exampleBefore}"
                   space-after.optimum="{$exampleAfter}">
               <xsl:apply-templates/>
            </block>
         </xsl:when>
         <xsl:when test="@place='divtop'">
            <block text-indent="0pt" end-indent="{$exampleMargin}" start-indent="{$exampleMargin}"
                   font-style="italic"
                   font-size="{$exampleSize}"
                   space-before.optimum="{$exampleBefore}"
                   space-after.optimum="{$exampleAfter}">
               <xsl:apply-templates/>
            </block>
         </xsl:when>
         <xsl:otherwise>
	           <xsl:choose>
	              <xsl:when test="parent::tei:item">
	                 <block>
	                    <xsl:call-template name="makeFootnote"/>
	                 </block>
	              </xsl:when>
	              <xsl:otherwise>
	                 <xsl:call-template name="makeFootnote"/>
	              </xsl:otherwise>
	           </xsl:choose>
         </xsl:otherwise>
      </xsl:choose>
  </xsl:template>

  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>Create a footnote</desc>
   </doc>
    
  <xsl:template name="makeFootnote">
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
               <xsl:value-of select="substring-before(@xml:lang,'-')"/>
            </xsl:attribute>
            <xsl:attribute name="language">
               <xsl:value-of select="substring-after(@xml:lang,'-')"/>
            </xsl:attribute>
         </xsl:if>
         <xsl:apply-templates/>
      </block>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc/>
   </doc>
  <xsl:template match="tei:pb">
      <xsl:variable name="e">
         <xsl:choose>
	   <xsl:when test="parent::tei:body or parent::tei:front or    parent::tei:back or parent::tei:div">
	     <xsl:text>block</xsl:text>
	   </xsl:when>
	   <xsl:otherwise>
	     <xsl:text>inline</xsl:text>
	   </xsl:otherwise>
         </xsl:choose>
      </xsl:variable>
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
	              <xsl:text>✁[</xsl:text>
	              <xsl:value-of select="@unit"/>
	              <xsl:text> Page </xsl:text>
	              <xsl:value-of select="@n"/>
	              <xsl:text>]✁</xsl:text>
	           </xsl:element>
         </xsl:when>
      </xsl:choose>
  </xsl:template>

  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc/>
   </doc>
  <xsl:template match="tei:quote">
      <block text-align="start" text-indent="0pt" end-indent="{$exampleMargin}"
             start-indent="{$exampleMargin}"
             font-size="{$quoteSize}"
             space-before.optimum="{$exampleBefore}"
             space-after.optimum="{$exampleAfter}">
         <xsl:apply-templates/>
      </block>
  </xsl:template>


  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc/>
   </doc>
  <xsl:template match="tei:q">
      <xsl:choose>
         <xsl:when test="tei:text">
            <xsl:apply-templates/>
         </xsl:when>
         <xsl:when test="@rend='display' or tei:p or tei:lg">
            <block text-align="start" text-indent="0pt" end-indent="{$exampleMargin}"
                   start-indent="{$exampleMargin}"
                   font-size="{$quoteSize}"
                   space-before.optimum="{$exampleBefore}"
                   space-after.optimum="{$exampleAfter}">
               <xsl:apply-templates/>
            </block>
         </xsl:when>
         <xsl:when test="@rend='eg'">
            <block text-align="start" font-size="{$exampleSize}" space-before.optimum="4pt"
                   text-indent="0pt"
                   space-after.optimum="4pt"
                   start-indent="{$exampleMargin}"
                   font-family="{$typewriterFont}">
               <xsl:apply-templates/>
            </block>
         </xsl:when>
         <xsl:when test="@rend = 'qwic'">
            <block space-before="{$spaceAroundTable}" space-after="{$spaceAroundTable}">
               <inline-container>
                  <table font-size="{$exampleSize}" font-family="{$typewriterFont}"
                         start-indent="{$exampleMargin}">
                     <table-column column-number="1" column-width="">
                        <xsl:if test="$foEngine='passivetex'">
                           <xsl:attribute name="column-align" namespace="http://www.tug.org/fotex">p</xsl:attribute>
                        </xsl:if>
                     </table-column>
                     <table-column column-number="2" column-width="">
                        <xsl:if test="$foEngine='passivetex'">
                           <xsl:attribute name="column-align" namespace="http://www.tug.org/fotex">l</xsl:attribute>
                        </xsl:if>
                     </table-column>
                     <table-body>
                        <xsl:for-each select="tei:q">
                           <xsl:for-each select="tei:term">
                              <table-row>
                                 <table-cell>
                                    <block>
                                       <xsl:apply-templates select="preceding-sibling::node()"/>
                                    </block>
                                 </table-cell>
                                 <table-cell>
                                    <block>
                                       <xsl:apply-templates/>
                                       <xsl:apply-templates select="following-sibling::node()"/>
                                    </block>
                                 </table-cell>
                              </table-row>
                           </xsl:for-each>
                        </xsl:for-each>
                     </table-body>
                  </table>
               </inline-container>
            </block>
         </xsl:when>
         <xsl:when test="starts-with(@rend,'kwic')">
            <block space-before="{$spaceAroundTable}" space-after="{$spaceAroundTable}">
               <inline-container>
                  <table font-size="{$exampleSize}" start-indent="{$exampleMargin}"
                         font-family="{$typewriterFont}">
                     <table-column column-number="1" column-width="">
                        <xsl:if test="$foEngine='passivetex'">
                           <xsl:attribute name="column-align" namespace="http://www.tug.org/fotex">r</xsl:attribute>
                        </xsl:if>
                     </table-column>
                     <table-column column-number="2" column-width="">
                        <xsl:if test="$foEngine='passivetex'">
                           <xsl:attribute name="column-align" namespace="http://www.tug.org/fotex">l</xsl:attribute>
                        </xsl:if>
                     </table-column>
                     <table-body>
                        <xsl:for-each select="tei:term">
                           <table-row>
                              <table-cell>
                                 <block>
                                    <xsl:value-of select="preceding-sibling::node()[1]"/>
                                 </block>
                              </table-cell>
                              <table-cell>
                                 <block>
                                    <xsl:apply-templates/>
                                    <xsl:value-of select="following-sibling::node()[1]"/>
                                 </block>
                              </table-cell>
                           </table-row>
                        </xsl:for-each>
                     </table-body>
                  </table>
               </inline-container>
            </block>
         </xsl:when>
         <xsl:when test="@rend='literal'">
            <block white-space-collapse="false" wrap-option="no-wrap" font-size="{$exampleSize}"
                   space-before.optimum="4pt"
                   space-after.optimum="4pt"
                   start-indent="{$exampleMargin}"
                   font-family="{$typewriterFont}">
               <xsl:apply-templates/>
            </block>
         </xsl:when>
         <xsl:otherwise>
            <xsl:text>“</xsl:text>
            <xsl:apply-templates/>
            <xsl:text>”</xsl:text>
         </xsl:otherwise>
      </xsl:choose>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc/>
   </doc>
  <xsl:template match="tei:reg">
    <xsl:choose>
      <xsl:when test="parent::tei:p">
	<inline font-family="{$sansFont}">
	  <xsl:apply-templates/>
	</inline>
      </xsl:when>
      <xsl:otherwise>
	<block font-family="{$sansFont}">
	  <xsl:apply-templates/>
	</block>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc/>
   </doc>
  <xsl:template match="tei:rs">
      <xsl:apply-templates/>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc/>
   </doc>
  <xsl:template match="tei:s">
      <xsl:apply-templates/>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc/>
   </doc>
  <xsl:template match="tei:salute">
      <block text-align="left">
         <xsl:apply-templates/>
      </block>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc/>
   </doc>
  <xsl:template match="tei:seg">
      <block font-family="{$typewriterFont}" background-color="yellow"
             white-space-collapse="false"
             wrap-option="no-wrap"
             text-indent="0em"
             start-indent="{$exampleMargin}"
             text-align="start"
             font-size="{$exampleSize}"
             padding-before="8pt"
             padding-after="8pt"
             space-before.optimum="4pt"
             space-after.optimum="4pt">
         <xsl:apply-templates/>
      </block>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc/>
   </doc>
  <xsl:template match="tei:sic">
      <xsl:apply-templates/>
      <xsl:text> (sic)</xsl:text>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc/>
   </doc>
  <xsl:template match="tei:signed">
      <block text-align="left">
         <xsl:apply-templates/>
      </block>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc/>
   </doc>
  <xsl:template match="tei:term">
      <inline font-style="italic">
         <xsl:apply-templates/>
      </inline>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc/>
   </doc>
  <xsl:template match="tei:unclear">
      <inline text-decoration="blink">
         <xsl:apply-templates/>
      </inline>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>[fo] </desc>
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
         <xsl:when test="$value='ital' or $value='italic' or $value='it' or $value='i' or $value='italics'">
            <xsl:attribute name="font-style">italic</xsl:attribute>
         </xsl:when>
         <xsl:when test="$value='sc'">
            <xsl:attribute name="font-variant">small-caps</xsl:attribute>
         </xsl:when>
         <xsl:when test="$value='code'">
            <xsl:attribute name="font-family">
               <xsl:value-of select="$typewriterFont"/>
            </xsl:attribute>
         </xsl:when>
         <xsl:when test="$value='bo' or $value='bold'">
            <xsl:attribute name="font-weight">bold</xsl:attribute>
         </xsl:when>
         <xsl:when test="$value='BO'">
            <xsl:attribute name="font-style">italic</xsl:attribute>
            <xsl:attribute name="text-decoration">underline</xsl:attribute>
         </xsl:when>
         <xsl:when test="$value='UL' or $value='ul'">
            <xsl:attribute name="text-decoration">underline</xsl:attribute>
         </xsl:when>
         <xsl:when test="$value='sub'">
            <xsl:attribute name="vertical-align">sub</xsl:attribute>
         </xsl:when>
         <xsl:when test="$value='small'">
            <xsl:attribute name="font-size">small</xsl:attribute>
         </xsl:when>
         <xsl:when test="$value='strike'">
            <xsl:attribute name="text-decoration">line-through</xsl:attribute>
         </xsl:when>
         <xsl:when test="$value='sup'">
            <xsl:attribute name="vertical-align">super</xsl:attribute>
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
      <xsl:number from="tei:text" level="any" count="tei:note"/>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>[fo] </desc>
   </doc>
  <xsl:template name="makeItem">
<!-- item behaviour depends on the type attribute of our parent:
simple, bullets, ordered, gloss, unordered, or bibliography
-->
    <xsl:variable name="listdepth" select="count(ancestor::tei:list)"/>
      <list-item>
         <xsl:if test="not(parent::tei:note[@place='foot' or @place='bottom' ])">
            <xsl:attribute name="space-before.optimum">
               <xsl:value-of select="$listItemsep"/>
            </xsl:attribute>
         </xsl:if>
         <list-item-label end-indent="label-end()">
            <xsl:if test="@xml:id">
               <xsl:attribute name="id">
                  <xsl:value-of select="@xml:id"/>
               </xsl:attribute>
            </xsl:if>
            <xsl:text>&#10;</xsl:text>
            <block>
               <xsl:choose>
                  <xsl:when test="@n">
                     <xsl:attribute name="text-align">end</xsl:attribute>
                     <xsl:value-of select="@n"/>
                  </xsl:when>
                  <xsl:when test="../@type='bibliography'">
                     <xsl:attribute name="text-align">end</xsl:attribute>
                     <xsl:apply-templates mode="xref" select="."/>
                  </xsl:when>
                  <xsl:when test="../@type='ordered' or self::tei:bibl">
                     <xsl:attribute name="text-align">end</xsl:attribute>
                     <xsl:apply-templates mode="xref" select="."/>
                     <xsl:text>.</xsl:text>
                  </xsl:when>
                  <xsl:when test="../@type='gloss'">
                     <xsl:attribute name="text-align">start</xsl:attribute>
                     <xsl:attribute name="font-weight">bold</xsl:attribute>
                     <xsl:choose>
		       <xsl:when test="tei:label">
			 <xsl:apply-templates mode="print" select="tei:label"/>
		       </xsl:when>
		       <xsl:otherwise>
			 <xsl:apply-templates mode="print" select="preceding-sibling::tei:*[1]"/>
		       </xsl:otherwise>
                     </xsl:choose>
                  </xsl:when>
                  <xsl:when test="../@type='numbered' or
				  self::tei:biblStruct or self::tei:bibl">
		    <!-- numbered support added rbl 26.3.2005 -->
		    <xsl:attribute name="text-align">end</xsl:attribute>
                     <xsl:number/>
                     <xsl:text>.</xsl:text>
                  </xsl:when>
                  <xsl:when test="../@type='ordered'">
		    <!-- numbered support added rbl 26.3.2005 -->
		    <xsl:attribute name="text-align">end</xsl:attribute>
		    <xsl:number/>
		    <xsl:text>.</xsl:text>
                  </xsl:when>
                  <xsl:otherwise>
		    <xsl:attribute name="text-align">end</xsl:attribute>
		    <xsl:choose>
		      <xsl:when test="$listdepth=0">
			<xsl:value-of select="$bulletOne"/>
		      </xsl:when>
		      <xsl:when test="$listdepth=1">
			<xsl:value-of select="$bulletOne"/>
		      </xsl:when>
		      <xsl:when test="$listdepth=2">
			<xsl:value-of select="$bulletTwo"/>
		      </xsl:when>
		      <xsl:when test="$listdepth=3">
			<xsl:value-of select="$bulletThree"/>
		      </xsl:when>
		      <xsl:when test="$listdepth=4">
			<xsl:value-of select="$bulletFour"/>
		      </xsl:when>
		    </xsl:choose>
                  </xsl:otherwise>
               </xsl:choose>
            </block>
         </list-item-label>
         <list-item-body start-indent="body-start()">
	   <xsl:choose>
	     <xsl:when test="* and tei:list">
	       <xsl:for-each select="*">
		 <xsl:choose>
		   <xsl:when test="self::tei:list">
		     <xsl:apply-templates select="."/>
		   </xsl:when>
		   <xsl:otherwise>
		     <block font-weight="normal">
		       <xsl:apply-templates/>
		     </block>
		   </xsl:otherwise>
		 </xsl:choose>
	       </xsl:for-each>
	     </xsl:when>
	     <xsl:otherwise>
	       <block font-weight="normal">
		 <xsl:apply-templates/>
	       </block>
	     </xsl:otherwise>
	   </xsl:choose>
         </list-item-body>
      </list-item>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>[fo] <param name="defaultvalue">defaultvalue</param>
         <param name="defaultstyle">defaultstyle</param>
         <param name="rend">rend</param>
      </desc>
   </doc>
  <xsl:template name="rend">
      <xsl:param name="defaultvalue"/>
      <xsl:param name="defaultstyle"/>
      <xsl:param name="rend"/>
      <xsl:choose>
         <xsl:when test="$rend=''">
            <xsl:attribute name="{$defaultstyle}">
               <xsl:value-of select="$defaultvalue"/>
            </xsl:attribute>
         </xsl:when>
         <xsl:when test="contains($rend,';')">
            <xsl:call-template name="applyRend">
               <xsl:with-param name="value" select="substring-before($rend,';')"/>
            </xsl:call-template>
            <xsl:call-template name="rend">
               <xsl:with-param name="rend" select="substring-after($rend,';')"/>
            </xsl:call-template>
         </xsl:when>
         <xsl:otherwise>
            <xsl:call-template name="applyRend">
               <xsl:with-param name="value" select="$rend"/>
            </xsl:call-template>
         </xsl:otherwise>
      </xsl:choose>
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
