<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet xmlns="http://www.w3.org/1999/xhtml"                  
                xmlns:a="http://relaxng.org/ns/compatibility/annotations/1.0"
		xmlns:teidocx="http://www.tei-c.org/ns/teidocx/1.0"
                xmlns:fo="http://www.w3.org/1999/XSL/Format"
                xmlns:rng="http://relaxng.org/ns/structure/1.0"
                xmlns:tei="http://www.tei-c.org/ns/1.0"
                xmlns:teix="http://www.tei-c.org/ns/Examples"
                xmlns:html="http://www.w3.org/1999/xhtml"
                xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                exclude-result-prefixes="html a fo rng tei teix teidocx"
                version="2.0">
  <xsl:import href="../common2/tei.xsl"/>
  <xsl:import href="tei-param.xsl"/>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" scope="stylesheet" type="stylesheet">
      <desc>
         <p>
    TEI stylesheet for making HTML output.
      </p>
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
  <xsl:include href="corpus.xsl"/>
  <xsl:include href="dictionaries.xsl"/>
  <xsl:include href="drama.xsl"/>
  <xsl:include href="figures.xsl"/>
  <xsl:include href="header.xsl"/>
  <xsl:include href="linking.xsl"/>
  <xsl:include href="namesdates.xsl"/>
  <xsl:include href="tagdocs.xsl"/>
  <xsl:include href="textstructure.xsl"/>
  <xsl:include href="textcrit.xsl"/>
  <xsl:include href="transcr.xsl"/>
  <xsl:include href="verse.xsl"/>
  <xsl:include href="../common2/verbatim.xsl"/>


  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" type="string">
      <desc>
Stylesheet constant setting the name of the main output file.
</desc>
   </doc>
  <xsl:variable name="top" select="/"/>
  <xsl:variable name="masterFile">
      <xsl:choose>
         <xsl:when test="not($outputName ='')">
            <xsl:choose>
               <xsl:when test="$STDOUT='true'">
                  <xsl:value-of select="$outputName"/>
               </xsl:when>
               <xsl:when test="contains($outputName,'.xml')">
                  <xsl:value-of select="substring-before($outputName,'.xml')"/>
               </xsl:when>
               <xsl:otherwise>
                  <xsl:value-of select="$outputName"/>
               </xsl:otherwise>
            </xsl:choose>
         </xsl:when>
         <xsl:when test="contains($REQUEST,'.ID=')">
            <xsl:call-template name="get-basename">
               <xsl:with-param name="file">
                  <xsl:value-of select="substring-before($REQUEST,'.ID=')"/>
               </xsl:with-param>
            </xsl:call-template>
         </xsl:when>
         <xsl:when test="not($REQUEST='')">
            <xsl:call-template name="get-basename">
               <xsl:with-param name="file">
                  <xsl:value-of select="$REQUEST"/>
               </xsl:with-param>
            </xsl:call-template>
         </xsl:when>
         <xsl:when test="$STDOUT='true'">
            <xsl:text>index.xml</xsl:text>
         </xsl:when>
         <xsl:otherwise>
            <xsl:text>index</xsl:text>
         </xsl:otherwise>
      </xsl:choose>
  </xsl:variable>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>[html] How to work out the filename component of a path<param name="file">filename</param>
      </desc>
   </doc>
  <xsl:template name="get-basename">
      <xsl:param name="file"/>
      <xsl:choose>
         <xsl:when test="contains($file,'/')">
            <xsl:call-template name="get-basename">
               <xsl:with-param name="file">
                  <xsl:value-of select="substring-after($file,'/')"/>
               </xsl:with-param>
            </xsl:call-template>
         </xsl:when>
         <xsl:otherwise>
            <xsl:choose>
               <xsl:when test="$STDOUT='true'">
                  <xsl:value-of select="$file"/>
               </xsl:when>
               <xsl:when test="contains($file,'.xml')">
                  <xsl:value-of select="substring-before($file,'.xml')"/>
               </xsl:when>
               <xsl:otherwise>
                  <xsl:value-of select="$file"/>
               </xsl:otherwise>
            </xsl:choose>
         </xsl:otherwise>
      </xsl:choose>
  </xsl:template>

  <xsl:template name="bodyMicroData"/>
</xsl:stylesheet>
