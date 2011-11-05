<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet xmlns="http://www.w3.org/1999/xhtml"
                
                xmlns:m="http://www.w3.org/1998/Math/MathML"
                xmlns:tei="http://www.tei-c.org/ns/1.0"
                xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                exclude-result-prefixes="tei m"
                version="2.0">
    <!-- import base conversion style -->

    <xsl:import href="../../../html/tei.xsl"/>
    <xsl:import href="../../../common/msdescription.xsl"/>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" scope="stylesheet" type="stylesheet">
      <desc>

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
         <p>Copyright: 2008, TEI Consortium</p>
      </desc>
   </doc>

  

    <xsl:output indent="no"/>
    <!--  -->
    <xsl:template name="msSection">
      <xsl:param name="level"/>
      <xsl:param name="heading"/>
      <xsl:param name="implicitBlock">false</xsl:param>
      <xsl:element name="h{$level}">
	        <xsl:value-of select="$heading"/>
      </xsl:element>
      <xsl:choose>
	        <xsl:when test="$implicitBlock='true'">
	           <p>
	              <xsl:apply-templates/>
	           </p>
	        </xsl:when>
	        <xsl:when test="*">
	           <xsl:apply-templates/>
	        </xsl:when>
	        <xsl:otherwise>
	           <p>
	              <xsl:apply-templates/>
	           </p>
	        </xsl:otherwise>
      </xsl:choose>
    </xsl:template>
    
    <xsl:template name="msLabelled">
      <xsl:param name="before"/>
      <i>
         <xsl:value-of select="$before"/>
      </i>
      <xsl:text>: </xsl:text>
      <xsl:value-of select="normalize-space(.)"/>
    </xsl:template>

    <xsl:template name="msInline">
      <xsl:param name="before"/>
      <xsl:param name="after"/>
      <xsl:param name="style"/>
      <span class="{local-name()}">
	        <xsl:value-of select="$before"/>
	        <xsl:choose>
	           <xsl:when test="$style='italic'">
	              <i>
	                 <xsl:value-of select="normalize-space(.)"/>
	              </i>
	           </xsl:when>
	           <xsl:when test="$style='bold'">
	              <b>
	                 <xsl:value-of select="normalize-space(.)"/>
	              </b>
	           </xsl:when>
	           <xsl:otherwise>
	              <xsl:value-of select="normalize-space(.)"/>
	           </xsl:otherwise>
	        </xsl:choose>
	        <xsl:value-of select="$after"/>
      </span>
    </xsl:template>

    <xsl:template name="msBlock">
      <xsl:param name="style"/>
      <div class="{$style}">
	        <xsl:apply-templates/>
      </div>
    </xsl:template>

    <xsl:template match="tei:body">
      <xsl:apply-templates/>
    </xsl:template>
    
    <xsl:template match="tei:teiHeader">
      <xsl:call-template name="pageLayoutSimple"/>
    </xsl:template>

    <xsl:template name="bodyHook">
      <xsl:apply-templates select="//tei:teiHeader/tei:fileDesc/tei:sourceDesc/tei:msDesc"/>
    </xsl:template>
    <xsl:param name="cssSecondaryFile">http://tei.oucs.ox.ac.uk/ENRICH/msdescription.css</xsl:param>
    <!-- <xsl:param name="cssFile">tei.css</xsl:param>-->


    <xsl:template match="tei:choice">
      <xsl:apply-templates/>
    </xsl:template>
</xsl:stylesheet>