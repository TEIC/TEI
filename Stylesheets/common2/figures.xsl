<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet xmlns:tei="http://www.tei-c.org/ns/1.0"
                
                xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                exclude-result-prefixes="tei"
                version="2.0">
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" scope="stylesheet" type="stylesheet">
      <desc>
         <p> TEI stylesheet dealing with elements from the figures module. </p>
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
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>Process element figure</desc>
   </doc>
  <xsl:template match="tei:figure" mode="xref">
      <xsl:choose>
	<xsl:when test="tei:head">
	  <xsl:call-template name="calculateFigureNumber"/>
	  <xsl:text>, </xsl:text>
	  <xsl:apply-templates mode="plain" select="tei:head"/>
	</xsl:when>
	<xsl:otherwise>
            <xsl:text>this figure</xsl:text>
         </xsl:otherwise>
      </xsl:choose>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>Process element table</desc>
   </doc>
  <xsl:template match="tei:table" mode="xref">
      <xsl:choose>
         <xsl:when test="$numberTables='true'">
	   <xsl:call-template name="calculateTableNumber"/>
            <xsl:if test="tei:head">
               <xsl:text>. </xsl:text>
               <xsl:apply-templates mode="plain" select="tei:head"/>
            </xsl:if>
         </xsl:when>
         <xsl:otherwise>
	   <xsl:choose>
            <xsl:when test="tei:head">
               <xsl:apply-templates mode="plain" select="tei:head"/>
	    </xsl:when>
	    <xsl:otherwise>
	      <xsl:text>table</xsl:text>
	    </xsl:otherwise>
	   </xsl:choose>
         </xsl:otherwise>
      </xsl:choose>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>[latex] Analyze attributes of graphics inclusion<param name="mode">Type of output (fo, html, latex) being
    created</param>
      </desc>
   </doc>
  <xsl:template name="graphicsAttributes">
      <xsl:param name="mode">fo</xsl:param>
      <xsl:if test="@width">
         <xsl:choose>
            <xsl:when test="ends-with(@width,'%')">
               <xsl:choose>
                  <xsl:when test="$mode='fo'">
                     <xsl:attribute name="content-width">
                        <xsl:value-of select="@width"/>
                     </xsl:attribute>
                  </xsl:when>
                  <xsl:when test="$mode='latex'">
                     <xsl:text>width=</xsl:text>
                     <xsl:value-of select="number(substring-before(@width,'%')) div 100"/>
                     <xsl:text>\textwidth,</xsl:text>
                  </xsl:when>
                  <xsl:otherwise>
                     <xsl:attribute name="width">
                        <xsl:value-of select="@width"/>
                     </xsl:attribute>
                  </xsl:otherwise>
               </xsl:choose>
            </xsl:when>
            <xsl:otherwise>
               <xsl:variable name="w">
                  <xsl:choose>
                     <xsl:when test="ends-with(@width,'pt')">
                        <xsl:value-of select="@width"/>
                     </xsl:when>
                     <xsl:when test="ends-with(@width,'px') and $mode='latex'">
                        <xsl:value-of select="substring-before(@width,'px')"/>
                        <xsl:text>pt</xsl:text>
                     </xsl:when>
                     <xsl:when test="ends-with(@width,'in')">
                        <xsl:value-of select="@width"/>
                     </xsl:when>
                     <xsl:when test="ends-with(@width,'px')">
                        <xsl:value-of select="@width"/>
                     </xsl:when>
                     <xsl:when test="ends-with(@width,'cm')">
                        <xsl:value-of select="@width"/>
                     </xsl:when>
                     <xsl:otherwise>
                        <xsl:value-of select="@width"/>
                        <xsl:text>pt</xsl:text>
                     </xsl:otherwise>
                  </xsl:choose>
               </xsl:variable>
               <xsl:choose>
                  <xsl:when test="$mode='fo'">
                     <xsl:attribute name="content-width">
                        <xsl:value-of select="$w"/>
                     </xsl:attribute>
                  </xsl:when>
                  <xsl:when test="$mode='latex'">
                     <xsl:text>width=</xsl:text>
                     <xsl:value-of select="$w"/>
                     <xsl:text>,</xsl:text>
                  </xsl:when>
               </xsl:choose>
            </xsl:otherwise>
         </xsl:choose>
      </xsl:if>
      <xsl:if test="@height">
         <xsl:choose>
            <xsl:when test="ends-with(@height,'%')">
               <xsl:choose>
                  <xsl:when test="$mode='fo'">
                     <xsl:attribute name="content-height">
                        <xsl:value-of select="@height"/>
                     </xsl:attribute>
                  </xsl:when>
                  <xsl:when test="$mode='latex'">
                     <xsl:text>height=</xsl:text>
                     <xsl:value-of select="number(substring-before(@height,'%')) div 100"/>
                     <xsl:text>\textheight,</xsl:text>
                  </xsl:when>
                  <xsl:otherwise>
                     <xsl:attribute name="height">
                        <xsl:value-of select="@height"/>
                     </xsl:attribute>
                  </xsl:otherwise>
               </xsl:choose>
            </xsl:when>
            <xsl:otherwise>
               <xsl:variable name="h">
                  <xsl:choose>
                     <xsl:when test="ends-with(@height,'pt')">
                        <xsl:value-of select="@height"/>
                     </xsl:when>
                     <xsl:when test="ends-with(@height,'px') and $mode='latex'">
                        <xsl:value-of select="substring-before(@height,'px')"/>
                        <xsl:text>pt</xsl:text>
                     </xsl:when>
                     <xsl:when test="ends-with(@height,'in')">
                        <xsl:value-of select="@height"/>
                     </xsl:when>
                     <xsl:when test="ends-with(@height,'px')">
                        <xsl:value-of select="@height"/>
                     </xsl:when>
                     <xsl:when test="ends-with(@height,'cm')">
                        <xsl:value-of select="@height"/>
                     </xsl:when>
                     <xsl:otherwise>
                        <xsl:value-of select="@height"/>
                        <xsl:text>pt</xsl:text>
                     </xsl:otherwise>
                  </xsl:choose>
               </xsl:variable>
               <xsl:choose>
                  <xsl:when test="$mode='fo'">
                     <xsl:attribute name="content-height">
                        <xsl:value-of select="$h"/>
                     </xsl:attribute>
                  </xsl:when>
                  <xsl:when test="$mode='latex'">
                     <xsl:text>height=</xsl:text>
                     <xsl:value-of select="$h"/>
                     <xsl:text>,</xsl:text>
                  </xsl:when>
               </xsl:choose>
            </xsl:otherwise>
         </xsl:choose>
      </xsl:if>
      <xsl:variable name="s">
         <xsl:choose>
            <xsl:when test="@scale and ends-with(@scale,'%')">
               <xsl:value-of select="number(substring-before(@scale,'%')) div 100"/>
            </xsl:when>
            <xsl:when test="@scale">
               <xsl:value-of select="@scale"/>
            </xsl:when>
            <xsl:when test="not(@width) and not(@height) and not($standardScale=1)">
               <xsl:value-of select="$standardScale"/>
            </xsl:when>
         </xsl:choose>
      </xsl:variable>
      <xsl:if test="not($s='')">
         <xsl:choose>
            <xsl:when test="$mode='fo'">
               <xsl:attribute name="scale">
                  <xsl:value-of select="$s"/>
               </xsl:attribute>
            </xsl:when>
            <xsl:when test="$mode='latex'">
               <xsl:text>scale=</xsl:text>
               <xsl:value-of select="$s"/>
               <xsl:text>,</xsl:text>
            </xsl:when>
         </xsl:choose>
      </xsl:if>
  </xsl:template>

  <xsl:template name="calculateFigureNumber">
    <xsl:choose>
      <xsl:when test="ancestor::tei:front and  $numberFrontFigures='true'">
	<xsl:call-template name="i18n">
	  <xsl:with-param name="word">figureWord</xsl:with-param>
	</xsl:call-template>
	<xsl:text> </xsl:text>
	<xsl:number count="tei:figure[tei:head]" from="tei:front" level="any"/>
      </xsl:when>
      <xsl:when test="ancestor::tei:back and $numberBackFigures='true'">
	<xsl:call-template name="i18n">
	  <xsl:with-param name="word">figureWord</xsl:with-param>
	</xsl:call-template>
	<xsl:text> </xsl:text>
	<xsl:number count="tei:figure[tei:head]" from="tei:back" level="any"/>
      </xsl:when>
      <xsl:when test="ancestor::tei:body and $numberFigures='true'">
	<xsl:call-template name="i18n">
	  <xsl:with-param name="word">figureWord</xsl:with-param>
	</xsl:call-template>
	<xsl:text> </xsl:text>
	<xsl:number count="tei:figure[tei:head]" from="tei:body" level="any"/>
      </xsl:when>
    </xsl:choose>    
  </xsl:template>


  <xsl:template name="calculateTableNumber">
    <xsl:call-template name="i18n">
      <xsl:with-param name="word">tableWord</xsl:with-param>
    </xsl:call-template>
    <xsl:text> </xsl:text>
    <xsl:number level="any"/>
  </xsl:template>

</xsl:stylesheet>