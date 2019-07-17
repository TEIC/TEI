<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet 
                xmlns:fotex="http://www.tug.org/fotex"
                xmlns:m="http://www.w3.org/1998/Math/MathML"
                xmlns:a="http://relaxng.org/ns/compatibility/annotations/1.0"
                xmlns="http://www.w3.org/1999/XSL/Format"
                xmlns:rng="http://relaxng.org/ns/structure/1.0"
                xmlns:tei="http://www.tei-c.org/ns/1.0"
                xmlns:teix="http://www.tei-c.org/ns/Examples"
                xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                exclude-result-prefixes="m a fotex rng tei teix"
                version="2.0">
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" scope="stylesheet" type="stylesheet">
      <desc>
         <p>
    TEI stylesheet
    dealing  with elements from the
      figures module, making XSL-FO output.
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
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>Deal with elements in math mode (just copy them)</desc>
   </doc>
  <xsl:template match="m:*|@*|comment()|processing-instruction()|text()" mode="math">
      <xsl:copy>
         <xsl:apply-templates mode="math" select="*|@*|processing-instruction()|text()"/>
      </xsl:copy>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>Process math elements</desc>
   </doc>
  <xsl:template match="m:math">
      <m:math>
         <xsl:copy-of select="@*"/>
         <xsl:apply-templates mode="math"/>
      </m:math>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc/>
   </doc>
  <xsl:template match="tei:cell">
      <table-cell>
         <xsl:if test="@cols &gt; 1">
            <xsl:attribute name="number-columns-spanned">
               <xsl:value-of select="@cols"/>
            </xsl:attribute>
         </xsl:if>
         <xsl:if test="@rows &gt; 1">
            <xsl:attribute name="number-rows-spanned">
               <xsl:value-of select="@rows"/>
            </xsl:attribute>
         </xsl:if>
         <xsl:call-template name="cellProperties"/>
         <block>
            <xsl:choose>
               <xsl:when test="@role='label' or parent::tei:row[@role='label' or parent::tei:row[@role='header']]">
                  <xsl:attribute name="font-weight">bold</xsl:attribute>
               </xsl:when>
            </xsl:choose>
            <xsl:apply-templates/>
         </block>
      </table-cell>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc/>
   </doc>
  <xsl:template match="tei:figDesc"/>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc/>
   </doc>
  <xsl:template match="tei:figure">
      <xsl:choose>
         <xsl:when test="tei:match(@rend,'display') or tei:head or tei:p">
               <block text-align="center">
               <xsl:call-template name="addID"/>
		 <xsl:apply-templates/>
               </block>
               <block>
                  <xsl:call-template name="figureCaptionstyle"/>
                  <xsl:call-template name="calculateFigureNumber"/>
                  <xsl:text>. </xsl:text>
                  <xsl:apply-templates select="tei:head"/>
               </block>
         </xsl:when>
         <xsl:otherwise>
	   <block>
	     <xsl:apply-templates/>
	   </block>
            <xsl:choose>
               <xsl:when test="$captionInlineFigures='true'">
                  <block>
                     <xsl:call-template name="figureCaptionstyle"/>
                     <xsl:call-template name="calculateFigureNumber"/>
                     <xsl:text>. </xsl:text>
                     <xsl:apply-templates select="tei:head"/>
                  </block>
               </xsl:when>
               <xsl:otherwise>
                  <xsl:if test="tei:head">
                     <block text-align="center">
                        <xsl:apply-templates select="tei:head"/>
                     </block>
                  </xsl:if>
               </xsl:otherwise>
            </xsl:choose>
         </xsl:otherwise>
      </xsl:choose>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc/>
   </doc>
  <xsl:template match="tei:figure" mode="xref">
      <xsl:if test="$showFloatLabel">
         <xsl:sequence select="tei:i18n('figureWord')"/>
         <xsl:text> </xsl:text>
      </xsl:if>
      <xsl:call-template name="calculateFigureNumber"/>
      <xsl:if test="$showFloatHead='true' and tei:head">
         <xsl:text> (</xsl:text>
         <xsl:apply-templates select="tei:head"/>
         <xsl:text>)</xsl:text>
      </xsl:if>
      <xsl:if test="$xrefShowPage='true'">
    on page
    <page-number-citation>
            <xsl:attribute name="ref-id">
               <xsl:choose>
                  <xsl:when test="@xml:id">
                     <xsl:value-of select="@xml:id"/>
                  </xsl:when>
                  <xsl:otherwise>
                     <xsl:value-of select="generate-id()"/>
                  </xsl:otherwise>
               </xsl:choose>
            </xsl:attribute>
         </page-number-citation> 
      </xsl:if>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>formulae </desc>
   </doc>
  <xsl:template match="tei:formula">
    <xsl:element name="{if (m:*) then 'instream-foreign-object' else 'wrapper'}">
      <xsl:if test="@xml:id">
	<xsl:attribute name="id">
	  <xsl:value-of select="@xml:id"/>
	</xsl:attribute>
      </xsl:if>
      <xsl:apply-templates/>
    </xsl:element>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc/>
   </doc>
  <xsl:template match="tei:formula" mode="xref">
      <xsl:number/>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>MathML in display formulae</desc>
   </doc>
  <xsl:template match="tei:formula[tei:match(@rend,'display')]/m:math">
      <m:math display="block">
         <xsl:copy-of select="@*"/>
         <xsl:apply-templates mode="math"/>
      </m:math>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>subequations</desc>
   </doc>
  <xsl:template match="tei:formula[tei:match(@rend,'subeqn')]/m:math">
      <xsl:apply-templates mode="math"/>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>handling graphics</desc>
   </doc>
  <xsl:template match="tei:graphic|tei:media">
      <xsl:call-template name="makePic"/>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc/>
   </doc>
  <xsl:template match="tei:row[@role='header']">
      <xsl:text>&#10;</xsl:text>
      <table-header>
         <xsl:apply-templates select="tei:cell"/>
      </table-header>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc/>
   </doc>
  <xsl:template match="tei:table" mode="xref">
      <xsl:if test="$showFloatLabel">
         <xsl:sequence select="tei:i18n('tableWord')"/>
         <xsl:text> </xsl:text>
      </xsl:if>
      <xsl:if test="$showFloatHead='true' and tei:head">
         <xsl:text> (</xsl:text>
         <xsl:apply-templates select="tei:head"/>
         <xsl:text>)</xsl:text>
      </xsl:if>
      <xsl:call-template name="calculateTableNumber"/>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc/>
   </doc>
  <xsl:template match="tei:table">
      <xsl:choose>
         <xsl:when test="tei:match(@rend,'eqnarray') and $foEngine='passivetex'">
            <fotex:eqnarray>
               <xsl:apply-templates select=".//tei:formula"/>
            </fotex:eqnarray>
         </xsl:when>
         <xsl:when test=".//tei:formula[tei:match(@rend,'subeqn')] and $foEngine='passivetex'">
            <fotex:eqnarray>
               <xsl:apply-templates select=".//tei:formula"/>
            </fotex:eqnarray>
         </xsl:when>
         <xsl:when test="$inlineTables or tei:match(@rend,'inline')">
            <xsl:if test="tei:head">
               <block>
                  <xsl:call-template name="tableCaptionstyle"/>
                  <xsl:if test="$makeTableCaption='true'">
		    <xsl:call-template name="calculateTableNumber"/>
                     <xsl:text>. </xsl:text>
                  </xsl:if>
                  <xsl:apply-templates select="tei:head"/>
               </block>
            </xsl:if>
            <xsl:call-template name="blockTable"/>
         </xsl:when>
         <xsl:otherwise>
            <xsl:call-template name="floatTable"/>
         </xsl:otherwise>
      </xsl:choose>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc/>
   </doc>
  <xsl:template match="tei:table[tei:match(@rend,'eqnarray')]">
      <xsl:choose>
         <xsl:when test="$foEngine='passivetex'">
            <fotex:eqnarray>
               <xsl:for-each select="tei:row">
                  <xsl:apply-templates select=".//tei:formula"/>
                  <xsl:if test="following-sibling::tei:row">
<!--        <character character="&#x2028;"/>-->
              <xsl:processing-instruction name="xmltex">\\</xsl:processing-instruction>
                  </xsl:if>
               </xsl:for-each>
            </fotex:eqnarray>
         </xsl:when>
         <xsl:otherwise>
            <xsl:apply-templates/>
         </xsl:otherwise>
      </xsl:choose>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>[fo] </desc>
   </doc>
  <xsl:template name="blockTable">
      <table text-align="{$tableAlign}" font-size="{$tableSize}">
<!--   MDH: FOP (at 2.1, which is current on 2016-12-30) does not support the 
       default @table-layout="auto".          -->
         <xsl:if test="$foEngine = 'fop'">
            <xsl:attribute name="table-layout" select="'fixed'"/>
            <xsl:attribute name="width" select="'100%'"/>
         </xsl:if>
         <xsl:call-template name="addID"/>
         <xsl:call-template name="deriveColSpecs"/>
         <xsl:apply-templates select="tei:row[@role='header']"/>
         <table-body text-indent="0pt">
            <xsl:for-each select="tei:row[not(@role='header')]">
               <xsl:text>&#10;</xsl:text>
               <table-row>
                  <xsl:apply-templates select="tei:cell"/>
               </table-row>
            </xsl:for-each>
         </table-body>
      </table>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>[fo] table cell properties</desc>
   </doc>
  <xsl:template name="cellProperties">
      <xsl:if test="@role='hi' or @role='label' or   parent::tei:row/@role='label'  or parent::tei:row/@role='header'">
         <xsl:attribute name="background-color">
            <xsl:value-of select="$defaultCellLabelBackground"/>
         </xsl:attribute>
      </xsl:if>
      <xsl:choose>
         <xsl:when test="ancestor::tei:table[1][tei:match(@rend,'frame') or tei:match(@rend,'wovenodd')]">
            <xsl:if test="not(parent::tei:row/preceding-sibling::tei:row)">
               <xsl:attribute name="border-before-style">solid</xsl:attribute>
            </xsl:if>
            <xsl:attribute name="border-after-style">solid</xsl:attribute>
            <xsl:if test="not(following-sibling::tei:cell)">
               <xsl:attribute name="border-end-style">solid</xsl:attribute>
            </xsl:if>
            <xsl:attribute name="border-start-style">solid</xsl:attribute>
         </xsl:when>
         <xsl:otherwise>
  </xsl:otherwise>
      </xsl:choose>
      <xsl:if test="not(ancestor::tei:table[1]/tei:match(@rend,'tight'))">
         <xsl:attribute name="padding">
            <xsl:value-of select="$tableCellPadding"/>
         </xsl:attribute>
      </xsl:if>
      <xsl:choose>
         <xsl:when test="@align">
            <xsl:attribute name="text-align">
               <xsl:value-of select="@align"/>
            </xsl:attribute>
         </xsl:when>
         <xsl:otherwise>
            <xsl:variable name="thiscol">
               <xsl:value-of select="position()"/>
            </xsl:variable>
            <xsl:variable name="tid">
               <xsl:value-of select="ancestor::tei:table/@xml:id"/>
            </xsl:variable>
            <xsl:variable name="align">
               <xsl:value-of select="$tableSpecs/Info/TableSpec[@xml:id=$tid]/table-column[@column-number=$thiscol]/@fotex:column-align"/>
            </xsl:variable>
            <!--
    <xsl:message>    Cell: whats my position: <xsl:value-of select="$thiscol"/>, <xsl:value-of select="$align"/>, <xsl:value-of select="$tid"/>
</xsl:message>
-->
        <xsl:choose>
               <xsl:when test="$align='R'">
                  <xsl:attribute name="text-align">right</xsl:attribute>
               </xsl:when>
               <xsl:when test="$align='L'">
                  <xsl:attribute name="text-align">left</xsl:attribute>
               </xsl:when>
               <xsl:when test="$align='C'">
                  <xsl:attribute name="text-align">center</xsl:attribute>
               </xsl:when>
               <xsl:when test="not($align='')">
                  <xsl:attribute name="text-align">
                     <xsl:value-of select="$align"/>
                  </xsl:attribute>
               </xsl:when>
               <xsl:otherwise>
                  <xsl:attribute name="text-align">
                     <xsl:value-of select="$cellAlign"/>
                  </xsl:attribute>
               </xsl:otherwise>
            </xsl:choose>
         </xsl:otherwise>
      </xsl:choose>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>[fo] </desc>
   </doc>
  <xsl:template name="floatTable">
      <table-and-caption>
         <xsl:if test="rend='landscape'">
            <xsl:attribute name="reference-direction">-90</xsl:attribute>
         </xsl:if>
         <xsl:call-template name="addID"/>
         <table-caption>
            <block text-align="{$tableCaptionAlign}" space-after="{$spaceBelowCaption}">
               <xsl:sequence select="tei:i18n('tableWord')"/>
               <xsl:call-template name="calculateTableNumber"/>
               <xsl:text>. </xsl:text>
               <xsl:apply-templates select="tei:head"/>
            </block>
         </table-caption>
         <xsl:call-template name="blockTable"/>
      </table-and-caption>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>[fo] Insert reference to graphics file </desc>
   </doc>
  <xsl:template name="makePic">
      <xsl:variable name="File" select="tei:resolveURI(.,@url)"/>
      <external-graphic>
         <xsl:call-template name="addID"/>
         <xsl:attribute name="src">
            <xsl:text>url(</xsl:text>
            <xsl:if test="not(starts-with($File,'./'))">
               <xsl:value-of select="$graphicsPrefix"/>
            </xsl:if>
            <xsl:value-of select="$File"/>
            <xsl:if test="not(contains($File,'.'))">
               <xsl:value-of select="$graphicsSuffix"/>
            </xsl:if>
            <xsl:text>)</xsl:text>
         </xsl:attribute>
         <xsl:call-template name="graphicsAttributes">
            <xsl:with-param name="mode">fo</xsl:with-param>
         </xsl:call-template>
      </external-graphic>
  </xsl:template>

  <xsl:template match="tei:binaryObject">
    <xsl:variable name="enc" select="if (@encoding) then @encoding
      else 'base64'"/>
    <xsl:variable name="mimeType" select="if (@mimeType) then @mimeType else 'image/auto'"/>
    <external-graphic content-height="scale-down-to-fit" 
                      content-width="scale-down-to-fit"
                      scaling="uniform"
                      max-width="100%"
                      max-height="100%">
       <xsl:attribute name="src" select="concat('url(''data:', $mimeType, ';', $enc, ',', normalize-space(.), ''')')"/>
      <xsl:call-template name="graphicsAttributes">
	     <xsl:with-param name="mode">fo</xsl:with-param>
      </xsl:call-template>
    </external-graphic>  
  </xsl:template>

</xsl:stylesheet>
