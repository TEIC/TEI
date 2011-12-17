<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xmlns:cals="http://www.oasis-open.org/specs/tm9901" xmlns:tei="http://www.tei-c.org/ns/1.0" xmlns:iso="http://www.iso.org/ns/1.0" xmlns:xs="http://www.w3.org/2001/XMLSchema" xmlns:ve="http://schemas.openxmlformats.org/markup-compatibility/2006" xmlns:o="urn:schemas-microsoft-com:office:office" xmlns:r="http://schemas.openxmlformats.org/officeDocument/2006/relationships" xmlns:m="http://schemas.openxmlformats.org/officeDocument/2006/math" xmlns:v="urn:schemas-microsoft-com:vml" xmlns:fn="http://www.w3.org/2005/02/xpath-functions" xmlns:wp="http://schemas.openxmlformats.org/drawingml/2006/wordprocessingDrawing" xmlns:a="http://schemas.openxmlformats.org/drawingml/2006/main" xmlns:w10="urn:schemas-microsoft-com:office:word" xmlns:w="http://schemas.openxmlformats.org/wordprocessingml/2006/main" xmlns:wne="http://schemas.microsoft.com/office/word/2006/wordml" xmlns:mml="http://www.w3.org/1998/Math/MathML" xmlns:tbx="http://www.lisa.org/TBX-Specification.33.0.html" xmlns:pic="http://schemas.openxmlformats.org/drawingml/2006/picture" xmlns:teidocx="http://www.tei-c.org/ns/teidocx/1.0" version="2.0" exclude-result-prefixes="cals ve o r m v wp w10 w wne mml tbx iso tei a xs pic fn">
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" scope="stylesheet" type="stylesheet">
    <desc>
      <p> TEI Utility stylesheet defining templates for use in
      processing ODD</p>

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
      <p>Id: $Id: functions.xsl 9464 2011-10-06 08:27:01Z rahtz $</p>
      <p>Copyright: 2008, TEI Consortium</p>
    </desc>
  </doc>
  <xsl:template name="makeDescription">
    <xsl:param name="includeValList">false</xsl:param>
    <xsl:param name="coded">true</xsl:param>
    <xsl:param name="showListRef">true</xsl:param>
    <xsl:variable name="documentationLanguage">
      <xsl:call-template name="generateDoc"/>
    </xsl:variable>
    <xsl:variable name="langs">
      <xsl:value-of select="concat(normalize-space($documentationLanguage),' ')"/>
    </xsl:variable>
    <xsl:variable name="firstLang">
      <xsl:value-of select="substring-before($langs,' ')"/>
    </xsl:variable>
    <!-- first the gloss -->
    <xsl:call-template name="makeGloss">
      <xsl:with-param name="langs" select="$langs"/>
    </xsl:call-template>
    <!-- now the description -->
    <xsl:choose>
      <xsl:when test="not(tei:desc)"> </xsl:when>
      <xsl:when test="count(tei:desc)=1">
        <xsl:for-each select="tei:desc">
          <xsl:apply-templates select="." mode="inLanguage"/>
        </xsl:for-each>
      </xsl:when>
      <xsl:when test="tei:desc[@xml:lang=$firstLang]">
        <xsl:for-each select="tei:desc[@xml:lang=$firstLang]">
          <xsl:apply-templates select="." mode="inLanguage"/>
        </xsl:for-each>
      </xsl:when>
      <xsl:otherwise>
        <xsl:variable name="D">
          <xsl:for-each select="tei:desc">
            <xsl:variable name="currentLang">
              <xsl:call-template name="findLanguage"/>
            </xsl:variable>
            <xsl:if test="contains($langs,concat($currentLang,' '))">
              <xsl:apply-templates select="." mode="inLanguage"/>
            </xsl:if>
          </xsl:for-each>
        </xsl:variable>
        <xsl:choose>
          <xsl:when test="$D='' and tei:desc[not(@xml:lang)]">
            <xsl:for-each select="tei:desc[not(@xml:lang)]">
              <xsl:apply-templates select="." mode="inLanguage"/>
            </xsl:for-each>
          </xsl:when>
          <xsl:when test="$coded='false'">
            <xsl:value-of select="$D"/>
          </xsl:when>
          <xsl:otherwise>
            <xsl:copy-of select="$D"/>
          </xsl:otherwise>
        </xsl:choose>
      </xsl:otherwise>
    </xsl:choose>
    <xsl:choose>
      <xsl:when test="$includeValList='false'"/>
      <xsl:when test="tei:valList[@type='open']">
        <xsl:text>&#10;</xsl:text>
        <xsl:call-template name="i18n">
          <xsl:with-param name="word">
            <xsl:text>Sample values include</xsl:text>
          </xsl:with-param>
        </xsl:call-template>
        <xsl:text>: </xsl:text>
        <xsl:for-each select="tei:valList/tei:valItem">
          <xsl:number/>
          <xsl:text>] </xsl:text>
          <xsl:choose>
            <xsl:when test="tei:altIdent=@ident">
              <xsl:value-of select="@ident"/>
            </xsl:when>
            <xsl:when test="tei:altIdent">
              <xsl:value-of select="normalize-space(tei:altIdent)"/>
            </xsl:when>
            <xsl:otherwise>
              <xsl:value-of select="@ident"/>
            </xsl:otherwise>
          </xsl:choose>
          <xsl:variable name="documentationLanguage">
            <xsl:call-template name="generateDoc"/>
          </xsl:variable>
          <xsl:variable name="langs">
            <xsl:value-of select="concat(normalize-space($documentationLanguage),' ')"/>
          </xsl:variable>
          <xsl:variable name="firstLang">
            <xsl:value-of select="substring-before($langs,' ')"/>
          </xsl:variable>
          <xsl:call-template name="makeGloss">
            <xsl:with-param name="langs" select="$langs"/>
          </xsl:call-template>
          <xsl:if test="following-sibling::tei:valItem">
            <xsl:text>; </xsl:text>
          </xsl:if>
        </xsl:for-each>
      </xsl:when>
      <xsl:when test="tei:valList[@type='semi']">
        <xsl:text>&#10;</xsl:text>
        <xsl:call-template name="i18n">
          <xsl:with-param name="word">
            <xsl:text>Suggested values include</xsl:text>
          </xsl:with-param>
        </xsl:call-template>
        <xsl:text>: </xsl:text>
        <xsl:for-each select="tei:valList/tei:valItem">
          <xsl:number/>
          <xsl:text>] </xsl:text>
          <xsl:choose>
            <xsl:when test="tei:altIdent=@ident">
              <xsl:value-of select="@ident"/>
            </xsl:when>
            <xsl:when test="tei:altIdent">
              <xsl:value-of select="normalize-space(tei:altIdent)"/>
            </xsl:when>
            <xsl:otherwise>
              <xsl:value-of select="@ident"/>
            </xsl:otherwise>
          </xsl:choose>
          <xsl:variable name="documentationLanguage">
            <xsl:call-template name="generateDoc"/>
          </xsl:variable>
          <xsl:variable name="langs">
            <xsl:value-of select="concat(normalize-space($documentationLanguage),' ')"/>
          </xsl:variable>
          <xsl:variable name="firstLang">
            <xsl:value-of select="substring-before($langs,' ')"/>
          </xsl:variable>
          <xsl:call-template name="makeGloss">
            <xsl:with-param name="langs" select="$langs"/>
          </xsl:call-template>
          <xsl:if test="following-sibling::tei:valItem">
            <xsl:text>; </xsl:text>
          </xsl:if>
        </xsl:for-each>
      </xsl:when>
    </xsl:choose>
    <xsl:if test="tei:listRef and $showListRef='true'">
	<xsl:text> [</xsl:text>
	<xsl:for-each select="tei:listRef/tei:*">
	  <xsl:apply-templates select="." mode="weave"/>
	  <xsl:if test="following-sibling::tei:*"> 
	    <xsl:text> </xsl:text>
	  </xsl:if>
	</xsl:for-each>
	<xsl:text>]</xsl:text>
    </xsl:if>
  </xsl:template>

  <xsl:template name="makeGloss">
    <xsl:param name="langs"/>
    <xsl:variable name="firstLang">
      <xsl:value-of select="substring-before($langs,' ')"/>
    </xsl:variable>
    <xsl:choose>
      <xsl:when test="not(tei:gloss)"/>
      <xsl:when test="string-length(tei:gloss[1])=0"/>
      <xsl:when test="count(tei:gloss)=1 and not(tei:gloss[@xml:lang])">
        <xsl:text> (</xsl:text>
        <xsl:apply-templates select="tei:gloss" mode="inLanguage"/>
        <xsl:text>) </xsl:text>
      </xsl:when>
      <xsl:when test="tei:gloss[@xml:lang=$firstLang]">
        <xsl:if test="not(tei:gloss[@xml:lang=$firstLang]='')">
          <xsl:text> (</xsl:text>
          <xsl:apply-templates select="tei:gloss[@xml:lang=$firstLang]" mode="inLanguage"/>
          <xsl:text>) </xsl:text>
        </xsl:if>
      </xsl:when>
      <xsl:otherwise>
        <xsl:variable name="G">
          <xsl:for-each select="tei:gloss">
            <xsl:variable name="currentLang">
              <xsl:call-template name="findLanguage"/>
            </xsl:variable>
            <xsl:if test="contains($langs,concat($currentLang,' '))">
              <xsl:text>(</xsl:text>
              <xsl:apply-templates select="." mode="inLanguage"/>
              <xsl:text>) </xsl:text>
            </xsl:if>
          </xsl:for-each>
        </xsl:variable>
        <xsl:choose>
          <xsl:when test="$G='' and tei:gloss[not(@xml:lang)]">
            <xsl:text> (</xsl:text>
            <xsl:apply-templates select="tei:gloss[not(@xml:lang)]" mode="inLanguage"/>
            <xsl:text>) </xsl:text>
          </xsl:when>
          <xsl:otherwise>
            <xsl:copy-of select="$G"/>
          </xsl:otherwise>
        </xsl:choose>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>

  <xsl:template name="findLanguage">
    <xsl:choose>
      <xsl:when test="@xml:lang">
        <xsl:value-of select="@xml:lang"/>
      </xsl:when>
      <xsl:when test="ancestor::tei:*[@xml:lang]">
        <xsl:value-of select="(ancestor::tei:*[@xml:lang])[1]/@xml:lang"/>
      </xsl:when>
      <xsl:otherwise>
        <xsl:text>en</xsl:text>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>

</xsl:stylesheet>