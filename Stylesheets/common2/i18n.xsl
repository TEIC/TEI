<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet 
    xmlns:a="http://relaxng.org/ns/compatibility/annotations/1.0"
    xmlns:fo="http://www.w3.org/1999/XSL/Format" 
    xmlns:html="http://www.w3.org/1999/xhtml" 
    xmlns:i="http://www.iso.org/ns/1.0"
    xmlns:rng="http://relaxng.org/ns/structure/1.0"
    xmlns:s="http://www.ascc.net/xml/schematron" 
    xmlns:sch="http://purl.oclc.org/dsdl/schematron" 
    xmlns:tei="http://www.tei-c.org/ns/1.0"
    xmlns:teix="http://www.tei-c.org/ns/Examples" 
    xmlns:xi="http://www.w3.org/2001/XInclude"
    xmlns:xs="http://www.w3.org/2001/XMLSchema"
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    exclude-result-prefixes="a fo html i rng s sch tei teix xi xs xsl" 
    version="2.0">
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" scope="stylesheet" type="stylesheet">
      <desc>
         <p> TEI stylesheet definitions common for all of HTML, FO and LaTeX
      outputs </p>
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
  <xsl:key match="entry" name="KEYS" use="key"/>
  <xsl:param name="documentationLanguage">en</xsl:param>

  <xsl:variable name="i18n"
		select="document('../i18n.xml',document(''))"/>

  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>[common] give language-specific version of a word or phrase<param name="word">the word(s) to translate</param>
      </desc>
   </doc>
  <xsl:template name="i18n">
      <xsl:param name="word"/>
      <xsl:variable name="Word">
         <xsl:value-of select="normalize-space($word)"/>
      </xsl:variable>
      <xsl:variable name="local">
         <xsl:call-template name="myi18n">
	           <xsl:with-param name="word">
	              <xsl:value-of select="$word"/>
	           </xsl:with-param>
         </xsl:call-template>
      </xsl:variable>
      <xsl:choose>
         <xsl:when test="string-length($local)&gt;0">
            <xsl:value-of select="$local"/>
         </xsl:when>
         <xsl:otherwise>
            <xsl:for-each select="$i18n">
	      <xsl:choose>
		<xsl:when test="key('KEYS',$Word)/text[@xml:lang=$documentationLanguage]">
		  <xsl:value-of select="key('KEYS',$Word)/text[@xml:lang=$documentationLanguage]"/>
		</xsl:when>
		<xsl:when test="key('KEYS',$Word)/text[@lang3=$documentationLanguage]">
		  <xsl:value-of select="key('KEYS',$Word)/text[lang3=$documentationLanguage]"/>
		</xsl:when>
		<xsl:otherwise>
		  <!--
		      <xsl:if test="$verbose='true'">
		      <xsl:message>NO TRANSLATION for <xsl:value-of 
		      select="$word"/> in <xsl:value-of select="$documentationLanguage"/></xsl:message>
		      </xsl:if>
		  -->
		  <xsl:value-of select="key('KEYS',$Word)/text[@xml:lang='en']"/>
		</xsl:otherwise>
	      </xsl:choose>
            </xsl:for-each>
         </xsl:otherwise>
      </xsl:choose>
  </xsl:template>

  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>[localisation] dummy template for overriding in a local system<param name="word">the word(s) to translate</param>
      </desc>
   </doc>
  <xsl:template name="myi18n">
	     <xsl:param name="word"/>
  </xsl:template>

  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" class="localisation" type="string">
      <desc> The language to use when
    generating text (use ISO 2-letter codes)</desc>
   </doc>
  <xsl:param name="lang">en</xsl:param>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" class="localisation">
      <desc>[common] Prefix text before an auto-generated table of contents </desc>
   </doc>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" class="localisation" type="string">
      <desc> The language the text is in
    (use ISO 2-letter codes)</desc>
   </doc>
  <xsl:param name="doclang">en</xsl:param>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" class="localisation">
      <desc>[common] Prefix text before an auto-generated table of contents </desc>
   </doc>
  <xsl:template name="contentsHeading">
      <xsl:call-template name="i18n">
         <xsl:with-param name="word">contentsHeading</xsl:with-param>
      </xsl:call-template>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" class="localisation">
      <desc>[common] Title for "Contents"; by default uses language-specific
      lookup table.</desc>
   </doc>
  <xsl:template name="contentsWord">
      <xsl:call-template name="i18n">
         <xsl:with-param name="word">contentsWord</xsl:with-param>
      </xsl:call-template>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" class="localisation">
      <desc>[common] Title for "Feedback"</desc>
   </doc>
  <xsl:template name="feedbackWords">
      <xsl:call-template name="i18n">
         <xsl:with-param name="word">feedbackWords</xsl:with-param>
      </xsl:call-template>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" class="localisation">
      <desc>[common] Title for "Search"; by default uses language-specific
      lookup table. </desc>
   </doc>
  <xsl:template name="searchWords">
      <xsl:call-template name="i18n">
         <xsl:with-param name="word">searchWords</xsl:with-param>
      </xsl:call-template>
  </xsl:template>
</xsl:stylesheet>