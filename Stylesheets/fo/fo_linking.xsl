<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet 
                xmlns:a="http://relaxng.org/ns/compatibility/annotations/1.0"
                xmlns="http://www.w3.org/1999/XSL/Format"
		xmlns:xs="http://www.w3.org/2001/XMLSchema"                
                xmlns:rng="http://relaxng.org/ns/structure/1.0"
                xmlns:tei="http://www.tei-c.org/ns/1.0"
                xmlns:teix="http://www.tei-c.org/ns/Examples"
                xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                exclude-result-prefixes="a rng tei teix xs"
                version="2.0">
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" scope="stylesheet" type="stylesheet">
      <desc>
         <p>
    TEI stylesheet
    dealing  with elements from the
      linking module, making XSL-FO output.
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
      <desc>[fo] <param name="where">target of link</param>
      </desc>
   </doc>
  <xsl:template name="generateEndLink">
      <xsl:param name="where"/>
      <xsl:value-of select="$where"/>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>[fo] <param name="ptr">whether the destination URL is also the body
    of the link</param>
         <param name="dest">destination URL</param>
      </desc>
   </doc>
  <xsl:template name="makeExternalLink">
      <xsl:param name="ptr" as="xs:boolean" select="false()"/>
      <xsl:param name="dest"/>
      <xsl:param name="title"/>
      <basic-link external-destination="url({$dest})">
         <xsl:choose>
            <xsl:when test="$ptr">
               <xsl:call-template name="showXrefURL">
                  <xsl:with-param name="dest">
                     <xsl:value-of select="$dest"/>
                  </xsl:with-param>
               </xsl:call-template>
            </xsl:when>
            <xsl:otherwise>
               <xsl:apply-templates/>
            </xsl:otherwise>
         </xsl:choose>
      </basic-link>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>[fo] <param name="ptr">ptr</param>
         <param name="target">target</param>
         <param name="dest">destination</param>
         <param name="body">body</param>
      </desc>
   </doc>
  <xsl:template name="makeInternalLink">
      <xsl:param name="ptr" as="xs:boolean" select="false()"/>
      <xsl:param name="class"/>
      <xsl:param name="target"/>
      <xsl:param name="dest"/>
      <xsl:param name="body"/>
<!--  The target of the link.  -->
      <xsl:variable name="W">
         <xsl:choose>
            <xsl:when test="$target">
               <xsl:value-of select="$target"/>
            </xsl:when>
            <xsl:when test="$dest = '#'">
               <xsl:value-of select="''"/>
            </xsl:when>
            <xsl:when test="contains($dest,'#')">
               <xsl:value-of select="substring($dest,2)"/>
            </xsl:when>
            <xsl:otherwise>
               <xsl:value-of select="$dest"/>
            </xsl:otherwise>
         </xsl:choose>
      </xsl:variable>
<!-- The clickable body content of the link. -->
      <xsl:variable name="content">
         <xsl:choose>
            <xsl:when test="not($body='')">
               <xsl:value-of select="$body"/>
            </xsl:when>
            <xsl:when test="$ptr">
               <xsl:apply-templates mode="xref" select="id($W)">
                  <xsl:with-param name="minimal" select="$minimalCrossRef"/>
               </xsl:apply-templates>
            </xsl:when>
            <xsl:otherwise>
               <xsl:apply-templates/>
            </xsl:otherwise>
         </xsl:choose>
      </xsl:variable>
<!--   We want to make sure that there is both target and content
       before making a link. Otherwise something has gone wrong, and
       we should just add a comment instead. 
       We also want to check that the target actually exists, otherwise
       there's no point in creating the link.
     -->
      <xsl:choose>
         <xsl:when test="string-length(normalize-space($W)) gt 0 and not(id($W))">
            <xsl:sequence select="$content"/>
         </xsl:when>
         <xsl:when test="string-length(normalize-space($W)) gt 0 and string-length(normalize-space($content)) gt 0">
            <basic-link internal-destination="{$W}">
               <xsl:call-template name="linkStyle"/>
               <xsl:sequence select="$content"/>
            </basic-link>
         </xsl:when>
         <xsl:otherwise>
            <xsl:comment>An internal link should have been created here,
                         but either there was no target, or no content to 
                         constitute the link.
                         Target: <xsl:value-of select="$W"/>
                         Content: <xsl:value-of select="$content"/></xsl:comment>
         </xsl:otherwise>
      </xsl:choose>
      
  </xsl:template>
</xsl:stylesheet>
