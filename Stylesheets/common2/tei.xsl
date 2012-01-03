<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet xmlns:fo="http://www.w3.org/1999/XSL/Format"
                xmlns:s="http://www.ascc.net/xml/schematron"
                xmlns:fotex="http://www.tug.org/fotex"
                xmlns:tei="http://www.tei-c.org/ns/1.0"
                
                xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
                xmlns:sch="http://purl.oclc.org/dsdl/schematron"
                xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                exclude-result-prefixes="s tei fotex xsi sch fo"
                version="2.0">
  <xsl:import href="tei-param.xsl"/>
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
  <xsl:include href="core.xsl"/>
  <xsl:include href="textstructure.xsl"/>
  <xsl:include href="header.xsl"/>
  <xsl:include href="linking.xsl"/>
  <xsl:include href="figures.xsl"/>
  <xsl:include href="textcrit.xsl"/>
  <xsl:include href="i18n.xsl"/>
  <xsl:include href="functions.xsl"/>

  <xsl:key name="APP" match="tei:app" use="1"/>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" type="string">
      <desc> Name of XSLT processor.</desc>
   </doc>
  <xsl:variable name="processor">
      <xsl:value-of select="system-property('xsl:vendor')"/>
  </xsl:variable>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>[common] turn names into quote characters<param name="quote">quote</param>
      </desc>
   </doc>
  <xsl:template name="getQuote">
      <xsl:param name="quote"/>
      <xsl:choose>
         <xsl:when test="$quote='laquo'">«</xsl:when>
         <xsl:when test="$quote='ldquo'">“</xsl:when>
         <xsl:when test="$quote='ldquor'">„</xsl:when>
         <xsl:when test="$quote='lsaquo'">‹</xsl:when>
         <xsl:when test="$quote='lsquo'">‘</xsl:when>
         <xsl:when test="$quote='lsquor'">‚</xsl:when>
         <xsl:when test="$quote='mdash'">—</xsl:when>
         <xsl:when test="$quote='raquo'">»</xsl:when>
         <xsl:when test="$quote='rdquo'">”</xsl:when>
         <xsl:when test="$quote='rdquor'">‟</xsl:when>
         <xsl:when test="$quote='rsaquo'">›</xsl:when>
         <xsl:when test="$quote='rsquo'">’</xsl:when>
         <xsl:when test="$quote='rsquor'">‛</xsl:when>
         <xsl:otherwise>?</xsl:otherwise>
      </xsl:choose>
  </xsl:template>
  
  <xsl:template name="whatsTheDate">
    <xsl:choose>
      	<xsl:when test="$useFixedDate='true'">1970-01-01</xsl:when>
	<xsl:otherwise>
	  <xsl:value-of
	      select="format-dateTime(current-dateTime(),'[Y]-[M02]-[D02]T[H02]:[m02]:[s02]Z')"/>
	</xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  

   <xsl:template name="makeQuote">
      <xsl:variable name="pre">
         <xsl:choose>
            <xsl:when test="contains(@rend,'PRE')">
	              <xsl:choose>
	                 <xsl:when test="contains(@rend,'POST')">
	                    <xsl:call-template name="getQuote">
	                       <xsl:with-param name="quote"
                                        select="normalize-space(substring-before(substring-after(@rend,'PRE'),'POST'))"/>
	                    </xsl:call-template>
	                 </xsl:when>
	                 <xsl:otherwise>
	                    <xsl:call-template name="getQuote">
	                       <xsl:with-param name="quote" select="normalize-space(substring-after(@rend,'PRE'))"/>
	                    </xsl:call-template>
	                 </xsl:otherwise>
	              </xsl:choose>
            </xsl:when>
            <xsl:otherwise>
	              <xsl:value-of select="$preQuote"/>
            </xsl:otherwise>
         </xsl:choose>
      </xsl:variable>
      <xsl:variable name="post">
         <xsl:choose>
            <xsl:when test="contains(@rend,'POST')">
	              <xsl:call-template name="getQuote">
	                 <xsl:with-param name="quote" select="normalize-space(substring-after(@rend,'POST'))"/>
	              </xsl:call-template>
            </xsl:when>
            <xsl:otherwise>
	              <xsl:value-of select="$postQuote"/>
            </xsl:otherwise>
         </xsl:choose>
      </xsl:variable>
      <xsl:value-of select="$pre"/>
      <xsl:apply-templates/>
      <xsl:value-of select="$post"/>
   </xsl:template>

  <xsl:template name="tei:makeText">
    <xsl:param name="letters"/>
    <xsl:value-of select="$letters"/>
  </xsl:template>
</xsl:stylesheet>
