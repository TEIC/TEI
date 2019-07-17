<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet xmlns:a="http://relaxng.org/ns/compatibility/annotations/1.0"
                xmlns:fo="http://www.w3.org/1999/XSL/Format"
                xmlns:rng="http://relaxng.org/ns/structure/1.0"
                xmlns:t="http://www.thaiopensource.com/ns/annotations"
                xmlns:tei="http://www.tei-c.org/ns/1.0"
                xmlns="http://www.tei-c.org/ns/1.0"
                
                xmlns:xs="http://www.w3.org/2001/XMLSchema"
                xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                exclude-result-prefixes="a t tei fo rng xs"
                version="2.0">
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" scope="stylesheet" type="stylesheet">
      <desc>
         <p> TEI stylesheet dealing with elements from the core module. </p>
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

  <xsl:template match="tei:table[tei:match(@rend,'valList')         or tei:match(@rend,'attDef')        or tei:match(@rend,'attList')         or tei:match(@rend,'specDesc')]">
      <list type="termlist">
         <xsl:apply-templates/>
      </list>
  </xsl:template>
  <xsl:template match="tei:table[tei:match(@rend,'attList')      or tei:match(@rend,'valList')      or tei:match(@rend,'attDef')      or tei:match(@rend,'specDesc')]/tei:row">
      <item>
         <xsl:apply-templates/>
      </item>
  </xsl:template>
  <xsl:template match="tei:table[tei:match(@rend,'attList')       or tei:match(@rend,'specDesc')       or tei:match(@rend,'valList')       or tei:match(@rend,'attDef')]/tei:row/tei:cell[1]">
      <xsl:choose>
         <xsl:when test="parent::tei:row/parent::tei:table[tei:match(@rend,'attList')]">
            <hi rend="bold">@<xsl:apply-templates/>
            </hi>
         </xsl:when>
         <xsl:when test="ancestor::tei:table[tei:match(@rend,'valList')]">
            <hi rend="bold">
               <xsl:apply-templates/>
            </hi>
         </xsl:when>
         <xsl:when test="ancestor::tei:table[tei:match(@rend,'specDesc')]">
            <hi rend="bold">@<xsl:apply-templates/>
            </hi>
         </xsl:when>
         <xsl:otherwise>
            <hi rend="bold">
               <xsl:apply-templates/>
            </hi>
         </xsl:otherwise>
      </xsl:choose>
  </xsl:template>
  <xsl:template match="tei:table[tei:match(@rend,'attList')        or tei:match(@rend,'valList')        or tei:match(@rend,'specDesc')        or tei:match(@rend,'attDef')]/tei:row/tei:cell[2]">
      <c rend="tab">
         <xsl:text>	</xsl:text>
      </c>
      <xsl:apply-templates/>
  </xsl:template>
  <xsl:template match="tei:index"/>
  <xsl:template match="processing-instruction()[name()='tex' and .='\ ']">
      <c xml:space="preserve"> </c>
  </xsl:template>
   <!-- identity transform -->
  <xsl:output method="xml" indent="yes"/>
  <xsl:template match="@*|text()|comment()|processing-instruction()">
      <xsl:copy-of select="."/>
  </xsl:template>
  <xsl:template match="*">
      <xsl:copy>
         <xsl:apply-templates select="*|@*|processing-instruction()|comment()|text()"/>
      </xsl:copy>
  </xsl:template>
</xsl:stylesheet>
