<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet xmlns:a="http://schemas.openxmlformats.org/drawingml/2006/main" xmlns:teix="http://www.tei-c.org/ns/Examples" xmlns:cals="http://www.oasis-open.org/specs/tm9901" xmlns:iso="http://www.iso.org/ns/1.0" xmlns:its="http://www.w3.org/2005/11/its" xmlns:m="http://schemas.openxmlformats.org/officeDocument/2006/math" xmlns:mml="http://www.w3.org/1998/Math/MathML" xmlns:o="urn:schemas-pmicrosoft-com:office:office" xmlns:pic="http://schemas.openxmlformats.org/drawingml/2006/picture" xmlns:r="http://schemas.openxmlformats.org/officeDocument/2006/relationships" xmlns:tbx="http://www.lisa.org/TBX-Specification.33.0.html" xmlns:tei="http://www.tei-c.org/ns/1.0" xmlns:teidocx="http://www.tei-c.org/ns/teidocx/1.0" xmlns:v="urn:schemas-microsoft-com:vml" xmlns:fn="http://www.w3.org/2005/02/xpath-functions" xmlns:ve="http://schemas.openxmlformats.org/markup-compatibility/2006" xmlns:w10="urn:schemas-microsoft-com:office:word" xmlns:w="http://schemas.openxmlformats.org/wordprocessingml/2006/main" xmlns:wne="http://schemas.microsoft.com/office/word/2006/wordml" xmlns:wp="http://schemas.openxmlformats.org/drawingml/2006/wordprocessingDrawing" xmlns:xs="http://www.w3.org/2001/XMLSchema" xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xmlns:html="http://www.w3.org/1999/xhtml" version="2.0" exclude-result-prefixes="teidocx cals ve o r m v wp       w10 w html wne mml tbx iso       tei teix a xs pic fn its">
  <!-- import conversion style -->
  <xsl:import href="../../../docx/to/teitodocx.xsl"/>
  <xsl:import href="tbx.xsl"/>
  <xsl:import href="../isoutils.xsl"/>
  <!-- import functions -->
  <xsl:include href="iso-functions.xsl"/>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" scope="stylesheet" type="stylesheet">
    <desc>
      <p>ISO-specific overrides for TEI stylesheet to convert TEI XML to Word DOCX XML.</p>
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
  <xsl:param name="typewriterFont">Courier</xsl:param>
  <xsl:param name="pagebreakStyle">active</xsl:param>
  <xsl:param name="tableMethod">cals</xsl:param>
  <xsl:param name="tableWidthPercentage">80</xsl:param>
  <xsl:param name="template">ISO</xsl:param>
  <xsl:variable name="align">
    <xsl:choose>
      <xsl:when test="$template='ISO'">left</xsl:when>
      <xsl:otherwise>right</xsl:otherwise>
    </xsl:choose>
  </xsl:variable>
  <xsl:param name="word-directory">..</xsl:param>
  <xsl:param name="tei-directory">./</xsl:param>
  <xsl:param name="numberFormat">fr</xsl:param>
  <xsl:variable name="orig" select="/"/>
  <!-- Styles -->
  <xsl:template match="tei:list[@type='termlist' and ancestor-or-self::*/@type='termsAndDefinitions']/tei:item/tei:abbr" mode="get-style">ExtRef</xsl:template>
  <xsl:template match="tei:seg[tei:match(@rend,'FormulaReference')]">FormulaReference</xsl:template>
  <xsl:template match="tei:seg[@iso:provision]" mode="get-style">
    <xsl:value-of select="@iso:provision"/>
  </xsl:template>
  <xsl:template match="tei:hi[tei:match(@rend,'language')]" mode="get-style">
    <xsl:text>language</xsl:text>
  </xsl:template>
  <xsl:template match="tei:hi[tei:match(@rend,'FigureFootnoteXref')]" mode="get-style">
    <xsl:text>FigureFootnoteXref</xsl:text>
  </xsl:template>
  <xsl:template match="tei:hi[tei:match(@rend,'source')]" mode="get-style">
    <xsl:text>source</xsl:text>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc>Inline Templates:
	Here we can overwrite how inline elements are rendered</desc>
  </doc>
  <xsl:template match="tei:c[@iso:style and @n]">
    <w:r>
      <xsl:variable name="renderingProperties">
        <xsl:call-template name="applyRend"/>
      </xsl:variable>
      <xsl:if test="not(empty($renderingProperties))">
        <w:rPr>
          <xsl:copy-of select="$renderingProperties"/>
        </w:rPr>
      </xsl:if>
      <w:sym w:char="{@n}">
        <xsl:for-each select="tokenize(@iso:style,';')">
          <xsl:if test="starts-with(.,'font-family')">
            <xsl:attribute name="w:font">
              <xsl:value-of select="normalize-space(substring-after(.,':'))"/>
            </xsl:attribute>
          </xsl:if>
        </xsl:for-each>
      </w:sym>
    </w:r>
  </xsl:template>
  <xsl:template match="tei:editionStmt">
    <w:r>
      <w:t><xsl:value-of select="tei:edition"/> Edition</w:t>
    </w:r>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc>Handle Numbers 
      </desc>
  </doc>
  <xsl:template match="tei:num">
    <w:r>
      <w:rPr>
        <w:rStyle w:val="isonumber"/>
        <xsl:if test="tei:render-bold(.)">
          <w:b/>
        </xsl:if>
      </w:rPr>
      <w:t>
        <xsl:choose>
          <xsl:when test="$numberFormat='fr'">
            <xsl:value-of select="."/>
          </xsl:when>
          <xsl:otherwise>
            <xsl:value-of select="translate(.,', ','.,')"/>
          </xsl:otherwise>
        </xsl:choose>
      </w:t>
    </w:r>
  </xsl:template>
  <xsl:template match="tei:num" mode="iden">
    <xsl:apply-templates/>
  </xsl:template>
  <xsl:template match="tei:seg[not(@*) and normalize-space(.)='']">
    <w:r>
      <w:t>
        <xsl:attribute name="xml:space">preserve</xsl:attribute>
        <xsl:text> </xsl:text>
      </w:t>
    </w:r>
  </xsl:template>
  <!--
	Block Templates:
	Here we can overwrite how block elements are rendered
    -->
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc> Dates </desc>
  </doc>
  <xsl:template match="tei:date">
    <xsl:choose>
      <xsl:when test="ancestor::tei:biblStruct">
        <xsl:apply-imports/>
      </xsl:when>
      <xsl:otherwise>
        <w:r>
          <w:rPr>
            <w:rStyle w:val="date"/>
          </w:rPr>
          <w:t>
            <xsl:value-of select="."/>
          </w:t>
        </w:r>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc> Process formulae </desc>
  </doc>
  <xsl:template match="tei:formula">
    <xsl:choose>
      <xsl:when test="parent::cals:entry or parent::tei:title">
        <xsl:apply-templates/>
      </xsl:when>
      <xsl:otherwise>
        <w:p>
          <w:pPr>
            <w:pStyle w:val="Formula"/>
          </w:pPr>
          <xsl:call-template name="block-element">
            <xsl:with-param name="nop">true</xsl:with-param>
          </xsl:call-template>
          <xsl:if test="@n">
            <w:r>
              <w:tab/>
            </w:r>
            <w:r>
              <w:rPr>
                <w:rStyle w:val="FormulaReference"/>
              </w:rPr>
              <w:t xml:space="preserve">
                <xsl:value-of select="@n"/>
              </w:t>
            </w:r>
          </xsl:if>
        </w:p>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc> Process plain paragraph </desc>
  </doc>
  <xsl:template match="tei:p">
    <xsl:param name="style"/>
    <xsl:param name="pPr" as="node()*"/>
    <xsl:variable name="pstyle">
      <w:pPr>
        <xsl:choose>
          <xsl:when test="@rend">
            <w:pStyle>
              <xsl:attribute name="w:val">
                <xsl:call-template name="getStyleName">
                  <xsl:with-param name="in" select="@rend"/>
                </xsl:call-template>
              </xsl:attribute>
            </w:pStyle>
          </xsl:when>
          <xsl:when test="string-length($style) &gt; 0">
            <w:pStyle w:val="{$style}"/>
          </xsl:when>
        </xsl:choose>
        <xsl:if test="@iso:style">
          <xsl:call-template name="undoSpecialStyle">
            <xsl:with-param name="css">
              <xsl:value-of select="@iso:style"/>
            </xsl:with-param>
          </xsl:call-template>
        </xsl:if>
      </w:pPr>
    </xsl:variable>
    <xsl:call-template name="block-element">
      <xsl:with-param name="pPr" select="$pstyle"/>
    </xsl:call-template>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc>Turn iso:style attribute back into Word styles</desc>
  </doc>
  <xsl:template name="undoSpecialStyle">
    <xsl:param name="css"/>
    <xsl:for-each-group select="tokenize($css,';')" group-adjacent="if (matches(., 'margin-(left|right)')) then 1 else                    if (matches(., 'margin-(top|bottom)')) then 2 else 0">
      <xsl:choose>
        <xsl:when test="current-grouping-key()=1">
          <xsl:call-template name="getStyleMarginsH"/>
        </xsl:when>
        <xsl:when test="current-grouping-key()=2">
          <xsl:call-template name="getStyleMarginsV"/>
        </xsl:when>
        <xsl:otherwise>
          <xsl:for-each select="current-group()">
            <xsl:if test="contains(., 'text-align')">
              <xsl:variable name="val">
                <xsl:value-of select="normalize-space(substring-after(.,':'))"/>
              </xsl:variable>
              <w:jc w:val="{$val}"/>
            </xsl:if>
            <xsl:if test="contains(., 'direction')">
              <xsl:variable name="val">
                <xsl:value-of select="normalize-space(substring-after(.,':'))"/>
              </xsl:variable>
              <xsl:if test="matches($val,'rtl')">
                <w:rtl/>
              </xsl:if>
            </xsl:if>
          </xsl:for-each>
        </xsl:otherwise>
      </xsl:choose>
    </xsl:for-each-group>
    <w:rPr>
      <xsl:call-template name="getStyleFonts">
        <xsl:with-param name="css" select="$css"/>
      </xsl:call-template>
    </w:rPr>
  </xsl:template>
  <xsl:template name="getStyleMarginsH">
    <w:ind>
      <xsl:for-each select="current-group()">
        <xsl:variable name="style">
          <xsl:value-of select="normalize-space(substring-before(.,':'))"/>
        </xsl:variable>
        <xsl:variable name="val">
          <xsl:value-of select="normalize-space(substring-after(.,':'))"/>
        </xsl:variable>
        <xsl:if test="contains($style, 'margin-left')">
          <xsl:attribute name="w:left">
            <xsl:value-of select="$val"/>
          </xsl:attribute>
        </xsl:if>
        <xsl:if test="contains($style, 'margin-right')">
          <xsl:attribute name="w:right">
            <xsl:value-of select="$val"/>
          </xsl:attribute>
        </xsl:if>
      </xsl:for-each>
    </w:ind>
  </xsl:template>
  <xsl:template name="getStyleMarginsV">
    <w:spacing>
      <xsl:for-each select="current-group()">
        <xsl:variable name="val">
          <xsl:value-of select="normalize-space(substring-after(.,':'))"/>
        </xsl:variable>
        <xsl:choose>
          <xsl:when test="contains(., 'margin-top')">
            <xsl:attribute name="w:before">
              <xsl:value-of select="$val"/>
            </xsl:attribute>
          </xsl:when>
          <xsl:when test="contains(., 'margin-bottom')">
            <xsl:attribute name="w:after">
              <xsl:value-of select="$val"/>
            </xsl:attribute>
          </xsl:when>
        </xsl:choose>
      </xsl:for-each>
    </w:spacing>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc>Extract font information from iso:style</desc>
  </doc>
  <xsl:template name="getStyleFonts">
    <xsl:param name="css"/>
    <xsl:variable name="tokenizedCss" select="tokenize($css,';')"/>
    <xsl:for-each select="$tokenizedCss">
      <xsl:variable name="val">
        <xsl:value-of select="normalize-space(substring-after(.,':'))"/>
      </xsl:variable>
      <xsl:choose>
        <xsl:when test="contains(., 'font-family')">
          <w:rFonts w:ascii="{$val}" w:hAnsi="{$val}"/>
        </xsl:when>
        <xsl:when test="contains(., 'font-size')">
          <w:sz w:val="{$val}"/>
        </xsl:when>
      </xsl:choose>
    </xsl:for-each>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc> Process bibliography</desc>
  </doc>
  <xsl:template match="tei:listBibl">
    <xsl:choose>
      <xsl:when test="ancestor-or-self::*[@type='normativeReferences']">
        <xsl:for-each select="tei:bibl">
          <xsl:call-template name="block-element">
            <xsl:with-param name="pPr" as="node()*">
              <w:pPr>
                <w:pStyle>
                  <xsl:attribute name="w:val">
                    <xsl:call-template name="getStyleName">
                      <xsl:with-param name="in">
                        <xsl:text>RefNorm</xsl:text>
                      </xsl:with-param>
                    </xsl:call-template>
                  </xsl:attribute>
                </w:pStyle>
              </w:pPr>
            </xsl:with-param>
          </xsl:call-template>
        </xsl:for-each>
      </xsl:when>
      <xsl:otherwise>
        <xsl:apply-templates/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc> Process notes which have a place attribute</desc>
  </doc>
  <xsl:template match="tei:note[@place]">
    <xsl:choose>
      <xsl:when test="@place='comment'">
        <xsl:call-template name="commentNote"/>
      </xsl:when>
      <xsl:when test="tei:isEndNote(.)">
        <xsl:call-template name="endNote"/>
      </xsl:when>
      <xsl:when test="tei:isFootNote(.)">
        <xsl:call-template name="footNote"/>
      </xsl:when>
      <xsl:when test="ancestor::tei:cell or ancestor::cals:entry">
        <xsl:call-template name="create-inlinenote"/>
      </xsl:when>
      <xsl:when test="@place='inline' and not(parent::tei:div or    parent::tei:list)">
        <xsl:apply-templates/>
      </xsl:when>
      <xsl:otherwise>
        <xsl:call-template name="block-element">
          <xsl:with-param name="pPr" as="node()*">
            <w:pPr>
              <w:pStyle>
                <xsl:attribute name="w:val">
                  <xsl:choose>
                    <xsl:when test="@type">
                      <xsl:value-of select="@type"/>
                    </xsl:when>
                    <xsl:otherwise>Note</xsl:otherwise>
                  </xsl:choose>
                </xsl:attribute>
              </w:pStyle>
            </w:pPr>
          </xsl:with-param>
          <xsl:with-param name="nop">false</xsl:with-param>
        </xsl:call-template>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc> Process notes which have a rend attribute</desc>
  </doc>
  <xsl:template match="tei:note[@rend]">
    <xsl:call-template name="block-element">
      <xsl:with-param name="pPr" as="node()*">
        <w:pPr>
          <w:pStyle>
            <xsl:attribute name="w:val">
              <xsl:choose>
                <xsl:when test="tei:match(@rend,'Notenumbered')">Notenumbered</xsl:when>
                <xsl:when test="tei:match(@rend,'Noteparagraph')">Noteparagraph</xsl:when>
                <xsl:when test="tei:match(@rend,'Notelist')">Notelist</xsl:when>
                <xsl:when test="tei:match(@rend,'Tablenote')">Tablenote</xsl:when>
                <xsl:when test="tei:match(@rend,'Figurenote')">Figurenote</xsl:when>
                <xsl:when test="tei:match(@rend,'Figurefootnote')">Figurefootnote</xsl:when>
                <xsl:when test="tei:match(@rend,'Exampleparagraph')">Exampleparagraph</xsl:when>
                <xsl:when test="tei:match(@rend,'Examplenumbered')">Examplenumbered</xsl:when>
                <xsl:otherwise>Note</xsl:otherwise>
              </xsl:choose>
            </xsl:attribute>
          </w:pStyle>
        </w:pPr>
      </xsl:with-param>
      <xsl:with-param name="nop">false</xsl:with-param>
    </xsl:call-template>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc> Process notes which have a type attribute</desc>
  </doc>
  <xsl:template match="tei:note[@type]">
    <xsl:choose>
      <xsl:when test="@type='remark'">
        <xsl:call-template name="create-remark"/>
      </xsl:when>
      <xsl:when test="@type='emphasize'">
        <xsl:call-template name="create-emphasize"/>
      </xsl:when>
      <xsl:otherwise>
        <xsl:call-template name="block-element">
          <xsl:with-param name="pPr" as="node()*">
            <w:pPr>
              <w:pStyle>
                <xsl:attribute name="w:val">
                  <xsl:choose>
                    <xsl:when test="@type">
                      <xsl:value-of select="@type"/>
                    </xsl:when>
                    <xsl:otherwise>Note</xsl:otherwise>
                  </xsl:choose>
                </xsl:attribute>
              </w:pStyle>
            </w:pPr>
          </xsl:with-param>
          <xsl:with-param name="nop">false</xsl:with-param>
        </xsl:call-template>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  <xsl:template name="footNote">
    <xsl:variable name="pPr">
      <xsl:choose>
        <xsl:when test="(@place='tablefoot') and  (ancestor::tei:cell or ancestor::cals:entry)">
          <w:pPr>
            <w:pStyle w:val="Tablefootnote"/>
          </w:pPr>
          <w:r>
            <w:rPr>
              <w:rStyle w:val="TableFootnoteXref"/>
              <w:position w:val="6"/>
              <w:sz w:val="16"/>
            </w:rPr>
            <w:t>
              <xsl:value-of select="@n"/>
            </w:t>
          </w:r>
          <w:r>
            <w:t>
              <xsl:text> </xsl:text>
            </w:t>
          </w:r>
        </xsl:when>
        <xsl:when test="@place='foot'  or @place='bottom'"/>
        <xsl:when test="@type='Example' and @iso:class='numbered'">
          <w:pPr>
            <w:pStyle w:val="Examplenumbered"/>
          </w:pPr>
        </xsl:when>
        <xsl:when test="@type='Example' and @iso:class='list'">
          <w:pPr>
            <w:pStyle w:val="Examplelist"/>
          </w:pPr>
        </xsl:when>
        <xsl:when test="@type='Example'">
          <w:pPr>
            <w:pStyle w:val="Example"/>
          </w:pPr>
        </xsl:when>
        <xsl:when test="parent::tei:cell or parent::cals:entry">
          <w:pPr>
            <xsl:variable name="Tablenote">
              <xsl:call-template name="getStyleName">
                <xsl:with-param name="in">
                  <xsl:value-of select="$Note"/>
                </xsl:with-param>
              </xsl:call-template>
            </xsl:variable>
            <w:pStyle w:val="Tablenote"/>
          </w:pPr>
        </xsl:when>
        <xsl:otherwise>
          <w:pPr>
            <w:pStyle w:val="Footnote"/>
          </w:pPr>
        </xsl:otherwise>
      </xsl:choose>
    </xsl:variable>
    <xsl:choose>
      <xsl:when test="$pPr/*">
        <xsl:call-template name="block-element">
          <xsl:with-param name="pPr" select="$pPr"/>
          <xsl:with-param name="nop">false</xsl:with-param>
        </xsl:call-template>
      </xsl:when>
      <xsl:otherwise>
        <xsl:variable name="num">
          <!--NEN: remove restriction [not(ancestor::cals:entry)] to count also foornotes in tables-->
          <xsl:number count="tei:note[@place='foot' or @place='bottom']" level="any"/>
        </xsl:variable>
        <xsl:variable name="id" select="$num+1"/>
        <w:r>
          <w:rPr>
            <w:rStyle w:val="FootnoteReference"/>
          </w:rPr>
          <w:footnoteReference w:id="{$id}"/>
        </w:r>
        <xsl:if test="ancestor::tei:bibl">
          <w:r>
            <w:rPr>
              <w:rStyle w:val="FootnoteReference"/>
            </w:rPr>
            <w:t>)</w:t>
          </w:r>
        </xsl:if>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  <xsl:template match="tei:note[@place]/tei:p">
    <xsl:apply-templates/>
  </xsl:template>
  <xsl:template name="create-inlinenote">
    <xsl:variable name="pPr">
      <w:pPr>
        <w:pStyle w:val="Tablenote"/>
      </w:pPr>
    </xsl:variable>
    <xsl:call-template name="block-element">
      <xsl:with-param name="pPr" select="$pPr"/>
      <xsl:with-param name="nop">true</xsl:with-param>
    </xsl:call-template>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc>Paragraphs in the front matter 
      </desc>
  </doc>
  <xsl:template match="tei:front/tei:div/tei:p[@type='foreword']">
    <xsl:call-template name="block-element">
      <xsl:with-param name="pPr" as="node()*">
        <w:pPr>
          <w:pStyle>
            <xsl:attribute name="w:val">
              <xsl:value-of select="concat(upper-case(substring(parent::tei:div/@type,1,1)),substring(parent::tei:div/@type,2))"/>
            </xsl:attribute>
          </w:pStyle>
        </w:pPr>
      </xsl:with-param>
    </xsl:call-template>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc>Table of contents section</desc>
  </doc>
  <xsl:template match="tei:front/tei:div[@type='toc']">
    <xsl:call-template name="block-element">
      <xsl:with-param name="pPr" as="node()*">
        <w:pPr>
          <w:pStyle w:val="zzContents"/>
        </w:pPr>
      </xsl:with-param>
    </xsl:call-template>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc> Definition lists </desc>
  </doc>
  <xsl:template match="tei:list[@type='gloss']">
    <xsl:for-each select="tei:head">
      <xsl:call-template name="block-element">
        <xsl:with-param name="pPr" as="node()*">
          <w:pPr>
            <w:pStyle w:val="dl"/>
            <w:tabs>
              <w:tab w:val="left" w:pos="403"/>
            </w:tabs>
          </w:pPr>
        </xsl:with-param>
      </xsl:call-template>
    </xsl:for-each>
    <xsl:for-each select="tei:label">
      <w:p>
        <w:pPr>
          <w:pStyle w:val="dl"/>
          <w:tabs>
            <w:tab w:val="left" w:pos="403"/>
          </w:tabs>
        </w:pPr>
        <xsl:apply-templates>
          <xsl:with-param name="nop">true</xsl:with-param>
        </xsl:apply-templates>
        <w:r>
          <w:tab/>
        </w:r>
        <xsl:for-each select="following-sibling::tei:item[1]">
          <xsl:apply-templates>
            <xsl:with-param name="nop">true</xsl:with-param>
          </xsl:apply-templates>
        </xsl:for-each>
      </w:p>
    </xsl:for-each>
  </xsl:template>
  <!-- Terms and Definitions -->
  <xsl:template match="tei:list[@type='termlist' and ancestor-or-self::*/@type='termsAndDefinitions']/tei:item/tei:term">
    <w:p>
      <w:pPr>
        <w:pStyle>
          <xsl:attribute name="w:val">
            <xsl:call-template name="getStyleName">
              <xsl:with-param name="in">
                <xsl:text>TermNum</xsl:text>
              </xsl:with-param>
            </xsl:call-template>
          </xsl:attribute>
        </w:pStyle>
      </w:pPr>
      <w:r>
        <w:t>
          <xsl:value-of select="ancestor-or-self::*[@n][1]/@n"/>
        </w:t>
      </w:r>
    </w:p>
    <xsl:call-template name="block-element">
      <xsl:with-param name="style">termPreferred</xsl:with-param>
    </xsl:call-template>
  </xsl:template>
  <xsl:template match="tei:list[@type='termlist' and ancestor-or-self::*/@type='termsAndDefinitions']/tei:item/tei:gloss">
    <xsl:call-template name="block-element">
      <xsl:with-param name="style">
        <xsl:variable name="style">
          <xsl:call-template name="getStyleName">
            <xsl:with-param name="in">
              <xsl:text>GlossText</xsl:text>
            </xsl:with-param>
          </xsl:call-template>
        </xsl:variable>
        <xsl:choose>
          <xsl:when test="$style=''">
            <xsl:text>Definition</xsl:text>
          </xsl:when>
          <xsl:otherwise>
            <xsl:value-of select="$style"/>
          </xsl:otherwise>
        </xsl:choose>
      </xsl:with-param>
    </xsl:call-template>
  </xsl:template>
  <!-- titleStmt -->
  <xsl:template match="tei:titleStmt">
    <xsl:param name="language" as="xs:string">en</xsl:param>
    <xsl:param name="id_start" as="xs:integer">0</xsl:param>
    <xsl:for-each select="tei:title">
      <xsl:if test="@xml:lang=$language">
        <xsl:variable name="title_type"><xsl:value-of select="@type"/>Title</xsl:variable>
        <xsl:variable name="id">
          <xsl:value-of select="position()+$id_start"/>
        </xsl:variable>
        <w:sdt>
          <w:sdtPr>
            <w:alias w:val="{$title_type}"/>
            <w:tag w:val="{$title_type}"/>
            <w:id w:val="{$id}"/>
            <w:lock w:val="sdtLocked"/>
            <w:placeholder>
              <w:docPart w:val="0949DFCE5F58405499B2741B51A42BCD"/>
            </w:placeholder>
            <w:text/>
          </w:sdtPr>
          <w:sdtContent>
            <w:r>
              <w:t>
                <xsl:value-of select="normalize-space(.)"/>
              </w:t>
            </w:r>
          </w:sdtContent>
        </w:sdt>
        <xsl:choose>
          <xsl:when test="position()=1 or position()=4">
            <w:r>
              <w:t xml:space="preserve"> </w:t>
            </w:r>
            <w:r>
              <w:t xml:space="preserve">– </w:t>
            </w:r>
          </xsl:when>
          <xsl:when test="position()=2 or position()=5">
            <w:r>
              <w:t xml:space="preserve"> </w:t>
            </w:r>
            <w:r>
              <xsl:choose>
                <xsl:when test="$language='en'">
                  <w:t xml:space="preserve">– Part </w:t>
                </xsl:when>
                <xsl:otherwise>
                  <w:t xml:space="preserve">– Partie </w:t>
                </xsl:otherwise>
              </xsl:choose>
            </w:r>
            <w:sdt>
              <w:sdtPr>
                <w:alias w:val="partNumber"/>
                <w:tag w:val="partNumber"/>
                <xsl:choose>
                  <xsl:when test="$language='en'">
                    <w:id w:val="680634476"/>
                  </xsl:when>
                  <xsl:otherwise>
                    <w:id w:val="680634477"/>
                  </xsl:otherwise>
                </xsl:choose>
                <w:lock w:val="sdtLocked"/>
                <w:placeholder>
                  <w:docPart w:val="0949DFCE5F58405499B2741B51A42BCD"/>
                </w:placeholder>
                <w:text/>
              </w:sdtPr>
              <w:sdtContent>
                <w:r>
                  <w:t>
                    <xsl:value-of select="//tei:publicationStmt/tei:idno[@iso:meta='partNumber']"/>
                  </w:t>
                </w:r>
              </w:sdtContent>
            </w:sdt>
            <w:r>
              <w:t xml:space="preserve">: </w:t>
            </w:r>
          </xsl:when>
          <xsl:otherwise/>
        </xsl:choose>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc> Document title </desc>
  </doc>
  <xsl:template name="document-title">
    <w:p>
      <w:pPr>
        <w:pStyle w:val="zzSTDTitle"/>
      </w:pPr>
      <xsl:sequence select="tei:generateTitle(.)"/>
    </w:p>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc>Table of Contents</desc>
  </doc>
  <xsl:template name="generate-toc">
    <w:p>
      <w:pPr>
        <w:pStyle w:val="TOC9"/>
        <w:tabs>
          <w:tab w:val="right" w:leader="dot" w:pos="9350"/>
        </w:tabs>
      </w:pPr>
      <w:r>
        <w:fldChar w:fldCharType="begin"/>
      </w:r>
      <w:r>
        <w:rPr>
          <w:noProof/>
        </w:rPr>
        <xsl:choose>
          <xsl:when test="starts-with(.//processing-instruction('isotoc')[1], ' TOC ')">
            <w:instrText xml:space="preserve">
              <xsl:value-of select=".//processing-instruction('isotoc')[1]"/>
            </w:instrText>
          </xsl:when>
          <xsl:otherwise>
            <w:instrText xml:space="preserve"> TOC \h \z \t "Heading 1,1,Heading 2,2,Heading 3,3,zzForeword,9,Introduction,9,ANNEX,1,zzBiblio,9,zzIndex,9" </w:instrText>
          </xsl:otherwise>
        </xsl:choose>
      </w:r>
      <w:r>
        <w:fldChar w:fldCharType="separate"/>
      </w:r>
      <w:r>
        <w:t>To refresh the TOC press F9!</w:t>
      </w:r>
      <w:r>
        <w:fldChar w:fldCharType="end"/>
      </w:r>
    </w:p>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc>Who created this document </desc>
  </doc>
  <xsl:template name="created-by">
    <xsl:value-of select="key('ISOMETA','secretariat')"/>
  </xsl:template>
  <xsl:template match="tei:availability" mode="titlepage">
    <xsl:param name="style"/>
    <xsl:for-each select="tei:p">
      <xsl:call-template name="block-element">
        <xsl:with-param name="style" select="$style"/>
      </xsl:call-template>
    </xsl:for-each>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc> Create all the files of the docx archive; for ISO, don't write out most of the
            auxiliary files. </desc>
  </doc>
  <xsl:template name="write-docxfiles">

    <xsl:if test="$isofreestanding='true'">
      <!-- header and footers -->
      <xsl:call-template name="write-docxfile-header-files"/>
      <!-- footer files -->
      <xsl:call-template name="write-docxfile-footer-files"/>
      <!-- numbering file -->
      <xsl:call-template name="write-docxfile-numbering-definition"/>
      <!-- main relationships -->
      <xsl:call-template name="write-docxfile-main-relationships"/>
      <!-- write Content Types -->
      <xsl:call-template name="write-docxfile-content-types"/>
      <!--  settings -->
      <xsl:call-template name="write-docxfile-settings"/>
    </xsl:if>
    <!-- relationships -->
    <xsl:call-template name="write-docxfile-relationships"/>
    <!-- footnotes file -->
    <xsl:call-template name="write-docxfile-footnotes-file"/>
    <!-- endnotes file -->
    <xsl:call-template name="write-docxfile-endnotes-file"/>
    <!-- comments file -->
    <xsl:call-template name="write-docxfile-comments-file"/>
    <xsl:call-template name="write-docxfile-docprops-core"/>
    <xsl:call-template name="write-docxfile-docprops-app"/>
    <xsl:call-template name="write-docxfile-docprops-custom"/>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc> Writes the main document.xml file, that contains all "real" content. </desc>
  </doc>
  <xsl:template name="create-document-dot-xml">
    <w:document>
      <w:body>
        <xsl:choose>
          <xsl:when test="$isofreestanding='true'">
            <xsl:call-template name="write-document-dot-xml-frontmatter"/>
            <xsl:call-template name="write-document-dot-xml-maincontent"/>
            <xsl:call-template name="write-document-dot-xml-backmatter"/>
          </xsl:when>
          <xsl:otherwise>
            <!-- Front -->
            <front>
              <xsl:call-template name="write-document-dot-xml-frontmatter"/>
            </front>
            <!-- Main -->
            <main>
              <xsl:call-template name="write-document-dot-xml-maincontent"/>
            </main>
            <!-- Back -->
            <back>
              <xsl:call-template name="write-document-dot-xml-backmatter"/>
            </back>
          </xsl:otherwise>
        </xsl:choose>
      </w:body>
    </w:document>
  </xsl:template>
  <xsl:template name="write-docxfile-docprops-custom">
    <xsl:if test="$debug='true'">
      <xsl:message>Writing out <xsl:value-of select="concat($word-directory,'/docProps/newcustom.xml')"/>    </xsl:message>
    </xsl:if>
    <xsl:result-document href="{concat($word-directory,'/docProps/newcustom.xml')}" standalone="yes">
      <Properties xmlns="http://schemas.openxmlformats.org/officeDocument/2006/custom-properties" xmlns:vt="http://schemas.openxmlformats.org/officeDocument/2006/docPropsVTypes">
        <property pid="2" name="DocIdentSDO">
          <xsl:attribute name="fmtid">
            <xsl:text>{D5CDD505-2E9C-101B-9397-08002B2CF9AE}</xsl:text>
          </xsl:attribute>
          <vt:lpwstr>
            <xsl:value-of select="key('ISOMETA','organization')"/>
          </vt:lpwstr>
        </property>
        <property pid="3" name="DocIdentProjectId">
          <xsl:attribute name="fmtid">
            <xsl:text>{D5CDD505-2E9C-101B-9397-08002B2CF9AE}</xsl:text>
          </xsl:attribute>
          <vt:lpwstr>
            <xsl:value-of select="key('ISOMETA','projectId')"/>
          </vt:lpwstr>
        </property>
        <property pid="4" name="DocIdentLanguage">
          <xsl:attribute name="fmtid">
            <xsl:text>{D5CDD505-2E9C-101B-9397-08002B2CF9AE}</xsl:text>
          </xsl:attribute>
          <vt:lpwstr>
            <xsl:value-of select="/tei:TEI/@xml:lang"/>
          </vt:lpwstr>
        </property>
        <property pid="5" name="DocIdentStage">
          <xsl:attribute name="fmtid">
            <xsl:text>{D5CDD505-2E9C-101B-9397-08002B2CF9AE}</xsl:text>
          </xsl:attribute>
          <vt:lpwstr>
            <xsl:value-of select="key('ISOMETA','stage')"/>
          </vt:lpwstr>
        </property>
        <property pid="6" name="DocIdentDate">
          <xsl:attribute name="fmtid">
            <xsl:text>{D5CDD505-2E9C-101B-9397-08002B2CF9AE}</xsl:text>
          </xsl:attribute>
          <vt:lpwstr>
            <xsl:value-of select="key('ISOMETA','docdate')"/>
          </vt:lpwstr>
        </property>
        <xsl:for-each select="key('ALLMETA',1)">
          <xsl:if test="@iso:meta != 'projectId' and not(starts-with(@iso:meta, 'fw_'))">
            <property name="{@iso:meta}">
              <xsl:attribute name="pid">
                <xsl:value-of select="position()+6"/>
              </xsl:attribute>
              <xsl:attribute name="fmtid">
                <xsl:text>{D5CDD505-2E9C-101B-9397-08002B2CF9AE}</xsl:text>
              </xsl:attribute>
              <vt:lpwstr>
                <xsl:value-of select="."/>
              </vt:lpwstr>
            </property>
          </xsl:if>
        </xsl:for-each>
        <property pid="1000" name="TEI_toDOCX">
          <xsl:attribute name="fmtid">
            <xsl:text>{D5CDD505-2E9C-101B-9397-08002B2CF9AE}</xsl:text>
          </xsl:attribute>
          <vt:lpwstr>2.15.0</vt:lpwstr>
        </property>
        <property pid="1001" name="WordTemplateURI">
          <xsl:attribute name="fmtid">
            <xsl:text>{D5CDD505-2E9C-101B-9397-08002B2CF9AE}</xsl:text>
          </xsl:attribute>
          <vt:lpwstr>
            <xsl:value-of select="ancestor-or-self::tei:TEI/tei:teiHeader/tei:encodingDesc/tei:appInfo/tei:application[@ident='WordTemplate']/tei:ptr/@target"/>
          </vt:lpwstr>
        </property>
        <xsl:for-each select="ancestor-or-self::tei:TEI/tei:teiHeader/tei:encodingDesc/tei:appInfo/tei:application">
          <xsl:if test="not(@ident='TEI_toDOCX')">
            <property name="{@ident}">
              <xsl:attribute name="pid">
                <xsl:value-of select="position()+1001"/>
              </xsl:attribute>
              <xsl:attribute name="fmtid">
                <xsl:text>{D5CDD505-2E9C-101B-9397-08002B2CF9AE}</xsl:text>
              </xsl:attribute>
              <vt:lpwstr>
                <xsl:value-of select="@version"/>
              </vt:lpwstr>
            </property>
          </xsl:if>
        </xsl:for-each>
      </Properties>
    </xsl:result-document>
  </xsl:template>
  <xsl:template name="write-document-dot-xml-maincontent">
    <!-- document title -->
    <xsl:if test="$isofreestanding='true'">
      <xsl:call-template name="document-title"/>
    </xsl:if>
    <!-- Describes the main part of the document -->
    <xsl:apply-templates select="tei:text/tei:body"/>
  </xsl:template>
  <xsl:template match="tei:q[@type='sdt']">
    <w:sdt>
      <w:sdtPr>
        <w:rPr>
          <w:noProof/>
          <w:lang w:val="en-GB"/>
        </w:rPr>
        <w:alias w:val="{@iso:meta}"/>
        <w:tag w:val="{@iso:meta}"/>
        <xsl:variable name="stdId">
          <xsl:number level="any"/>
        </xsl:variable>
        <w:id w:val="158666506{$stdId}"/>
      </w:sdtPr>
      <w:sdtContent>
        <xsl:apply-templates/>
      </w:sdtContent>
    </w:sdt>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc> Process CALS tables </desc>
  </doc>
  <xsl:template match="cals:table">
    <xsl:choose>
      <xsl:when test="@teidocx:corresp and $tableMethod='word'">
        <xsl:call-template name="tableheading-from-cals"/>
        <xsl:if test="$debug='true'">
          <xsl:message>read table from <xsl:value-of select="@teidocx:corresp"/></xsl:message>
        </xsl:if>
        <xsl:for-each select="document(concat($tei-directory,@teidocx:corresp))">
          <xsl:apply-templates mode="copytable"/>
        </xsl:for-each>
      </xsl:when>
      <xsl:otherwise>
        <xsl:apply-imports/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  <xsl:template match="         w:bottom |          w:gridCol |          w:gridSpan |          w:insideH |          w:insideV |          w:jc |          w:left |          w:pPr |         w:p |         w:right |         w:spacing |           w:tbl |          w:tblBorders |          w:tblCellMar |          w:tblGrid |          w:tblLayout |          w:tblLook |          w:tblPr |          w:tblStyle |          w:tblW |          w:tc |          w:tcBorders |          w:tcPr |          w:tcW |          w:top |          w:tr |          w:trPr |          w:vAlign " mode="copytable">
    <xsl:copy>
      <xsl:copy-of select="@*"/>
      <xsl:apply-templates/>
    </xsl:copy>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc>Handling headless sections</desc>
  </doc>
  <xsl:template match="tei:div[@type='headless']">
    <xsl:apply-templates/>
  </xsl:template>
  <xsl:template match="tei:div[@type='headless']/tei:p[1]">
    <xsl:variable name="level">
      <xsl:value-of select="count(ancestor-or-self::tei:div)"/>
    </xsl:variable>
    <xsl:variable name="s">
      <xsl:choose>
        <xsl:when test="@rend">
          <xsl:value-of select="@rend"/>
        </xsl:when>
        <xsl:when test="ancestor::tei:back">
          <xsl:text>pA</xsl:text>
          <xsl:value-of select="$level"/>
        </xsl:when>
        <xsl:otherwise>
          <xsl:text>p</xsl:text>
          <xsl:value-of select="$level"/>
        </xsl:otherwise>
      </xsl:choose>
    </xsl:variable>
    <!--
      <xsl:if test="$debug='true'">
	<xsl:message>Fire <xsl:value-of select="$level"/> [<xsl:value-of select="$s"/>] for <xsl:value-of select="@n"/>: <xsl:value-of select="."/></xsl:message>
      </xsl:if>
      -->
    <xsl:call-template name="block-element">
      <xsl:with-param name="style" select="$s"/>
    </xsl:call-template>
  </xsl:template>
  <xsl:template match="tei:milestone[@unit='section']" mode="pass2"/>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc>Deleted parts removed </desc>
  </doc>
  <xsl:template match="tei:del"/>
  <xsl:template name="simpleRun">
    <xsl:param name="text"/>
    <xsl:param name="prefix"/>
    <xsl:param name="italic">false</xsl:param>
    <xsl:if test="not($prefix='')">
      <w:r>
        <w:t>
          <xsl:attribute name="xml:space">preserve</xsl:attribute>
          <xsl:value-of select="$prefix"/>
        </w:t>
      </w:r>
    </xsl:if>
    <w:r>
      <xsl:if test="$italic='true'">
        <w:rPr>
          <w:i/>
        </w:rPr>
      </xsl:if>
      <w:t>
        <xsl:attribute name="xml:space">preserve</xsl:attribute>
        <xsl:value-of select="$text"/>
      </w:t>
    </w:r>
  </xsl:template>
  <xsl:template match="tei:figure/tei:head">
    <w:p>
      <w:pPr>
        <w:pStyle>
          <xsl:choose>
            <xsl:when test="@type='subtitle'">
              <xsl:attribute name="w:val">Figuresubtitle</xsl:attribute>
            </xsl:when>
            <xsl:otherwise>
              <xsl:attribute name="w:val">Figuretitle</xsl:attribute>
            </xsl:otherwise>
          </xsl:choose>
        </w:pStyle>
      </w:pPr>
      <xsl:if test="not(normalize-space(.)='')">
        <w:r>
          <w:t xml:space="preserve">— </w:t>
        </w:r>
      </xsl:if>
      <xsl:apply-templates/>
    </w:p>
  </xsl:template>
  <xsl:template match="processing-instruction()[name()='isotoc']">
    <xsl:value-of select="."/>
  </xsl:template>
  <!--
    <xsl:template match="tei:p/tei:cit">
      <xsl:if test="@n">
	<w:r>
	  <w:t>
	    <xsl:text>(</xsl:text>
	    <xsl:value-of select="@n"/>
	    <xsl:text>) </xsl:text>
	  </w:t>
	</w:r>
      </xsl:if>
      <w:r>
	<w:rPr>
	  <w:rStyle w:val="Quote"/>
	</w:rPr>
	<xsl:apply-templates/>
      </w:r>
    </xsl:template>

    <xsl:template match="tei:div/tei:cit">
      <xsl:variable name="content">
	<xsl:copy>
	  <xsl:if test="@n">
	    <xsl:text>(</xsl:text>
	    <xsl:value-of select="@n"/>
	    <xsl:text>) </xsl:text>
	  </xsl:if>
	  <xsl:copy-of
	      select="*|processing-instruction()|comment()|text()"/>
	</xsl:copy>
      </xsl:variable>
      <xsl:for-each select="$content">
	<xsl:call-template name="block-element">
	  <xsl:with-param name="style">Quote</xsl:with-param>
        </xsl:call-template>
      </xsl:for-each>


    </xsl:template>
-->
  <xsl:template match="/tei:TEI">
    <xsl:variable name="phase1">
      <xsl:copy>
        <xsl:apply-templates select="*|@*|text()|processing-instruction()|comment()" mode="phase1"/>
      </xsl:copy>
    </xsl:variable>
    <xsl:for-each select="$phase1/tei:TEI">
      <xsl:call-template name="create-document-dot-xml"/>
    </xsl:for-each>
  </xsl:template>
  <xsl:template match="@*|text()|comment()|processing-instruction()" mode="phase1">
    <xsl:copy-of select="."/>
  </xsl:template>
  <xsl:template match="*" mode="phase1">
    <xsl:copy>
      <xsl:apply-templates select="*|@*|processing-instruction()|comment()|text()" mode="phase1"/>
    </xsl:copy>
  </xsl:template>
  <xsl:template match="tei:p[not(text()) and tei:cit]" mode="phase1">
    <xsl:apply-templates/>
  </xsl:template>
  <xsl:template match="tei:cit" mode="phase1">
    <xsl:if test="@n">
      <p xmlns="http://www.tei-c.org/ns/1.0" rend="Example paragraph">
        <xsl:value-of select="@n"/>
      </p>
    </xsl:if>
    <xsl:copy>
      <xsl:apply-templates select="*|processing-instruction()|comment()|text()" mode="phase1"/>
    </xsl:copy>
  </xsl:template>
  <xsl:template name="mathrRpHook">
    <xsl:if test="ancestor::tbx:term/following-sibling::tbx:termNote/@iso:style">
      <rStyle xmlns="http://schemas.openxmlformats.org/wordprocessingml/2006/main" w:val="{ancestor::tbx:term/following-sibling::tbx:termNote/@iso:style}"/>
    </xsl:if>
  </xsl:template>
  <xsl:template match="tei:table">
    <xsl:call-template name="table-header"/>
    <xsl:call-template name="table-body"/>
  </xsl:template>
  <xsl:template match="tei:p[tei:match(@rend,'Table units')]"/>
  <!-- 
        NEN: <note type="Remark">
        OPMERKING
        Place paragraph with style Note starting with text 'Opmerking' followed by [tab]
    -->
  <xsl:template name="create-remark">
    <w:pPr>
      <w:pStyle w:val="Note"/>
    </w:pPr>
    <xsl:variable name="n">
      <xsl:number level="any" count="tei:note[@type='remark']"/>
    </xsl:variable>
    <w:bookmarkStart w:id="{$n}" w:name="{@xml:id}"/>
    <w:r>
      <w:t>OPMERKING</w:t>
    </w:r>
    <w:bookmarkEnd w:id="{$n}"/>
    <w:r>
      <w:tab/>
      <w:t>
        <xsl:value-of select="."/>
      </w:t>
    </w:r>
  </xsl:template>
  <!-- 
        emphasize
    -->
  <xsl:template name="create-emphasize">
    <w:r>
      <w:rPr>
        <w:rStyle w:val="RemarkReference"/>
        <w:vanish/>
      </w:rPr>
      <w:t>[ </w:t>
    </w:r>
  </xsl:template>

    <xsl:template name="write-docxfile-docprops-app">
	     <xsl:if test="$debug='true'">
	        <xsl:message>Writing out <xsl:value-of select="concat($wordDirectory,'/docProps/newapp.xml')"/>
         </xsl:message>
	     </xsl:if>

        <xsl:result-document href="{concat($wordDirectory,'/docProps/newapp.xml')}" standalone="yes">
            <Properties xmlns="http://schemas.openxmlformats.org/officeDocument/2006/extended-properties"
                     xmlns:vt="http://schemas.openxmlformats.org/officeDocument/2006/docPropsVTypes">
                <Template>STD_3_0_0.dotx</Template>
                <Application>TEIISO tei-docx.xsl</Application>
                <DocSecurity>0</DocSecurity>
                <SharedDoc>true</SharedDoc>
                <AppVersion>1.0</AppVersion>
            </Properties>
        </xsl:result-document>
    </xsl:template>
    <xsl:template name="write-docxfile-settings">
	     <xsl:if test="$debug='true'">
	        <xsl:message>Writing out <xsl:value-of select="concat($wordDirectory,'/word/settings.xml')"/>
         </xsl:message>
	     </xsl:if>

        <xsl:result-document href="{concat($wordDirectory,'/word/settings.xml')}" standalone="yes">
            
            <w:settings xmlns:sl="http://schemas.openxmlformats.org/schemaLibrary/2006/main">
                <w:zoom w:percent="100"/>
                <w:stylePaneSortMethod w:val="0000"/>
                <w:defaultTabStop w:val="720"/>
                <w:evenAndOddHeaders/>
                <w:characterSpacingControl w:val="doNotCompress"/>
                <w:footnotePr>
                    <w:footnote w:id="-1"/>
                    <w:footnote w:id="0"/>
                </w:footnotePr>
                <w:endnotePr>
                    <w:endnote w:id="-1"/>
                    <w:endnote w:id="0"/>
                </w:endnotePr>
                <w:compat/>
                <m:mathPr>
                    <m:mathFont m:val="Cambria Math"/>
                    <m:brkBin m:val="before"/>
                    <m:brkBinSub m:val="--"/>
                    <m:smallFrac/>
                    <m:dispDef/>
                    <m:lMargin m:val="432"/>
                    <m:rMargin m:val="0"/>
                    <m:defJc m:val="left"/>
                    <m:wrapIndent m:val="1440"/>
                    <m:intLim m:val="subSup"/>
                    <m:naryLim m:val="undOvr"/>
                </m:mathPr>
                <w:attachedSchema w:val="ActionsPane3"/>
                <w:themeFontLang w:val="en-US"/>
                <w:clrSchemeMapping w:bg1="light1" w:t1="dark1" w:bg2="light2" w:t2="dark2" w:accent1="accent1"
                                w:accent2="accent2"
                                w:accent3="accent3"
                                w:accent4="accent4"
                                w:accent5="accent5"
                                w:accent6="accent6"
                                w:hyperlink="hyperlink"
                                w:followedHyperlink="followedHyperlink"/>
                <w:decimalSymbol w:val="."/>
                <w:listSeparator w:val=","/>
            </w:settings>
        </xsl:result-document>
    </xsl:template>

</xsl:stylesheet>
