<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet xmlns="http://www.w3.org/1999/xhtml"
                xmlns:tbx="http://www.lisa.org/TBX-Specification.33.0.html"
		xmlns:iso="http://www.iso.org/ns/1.0"
		xmlns:cals="http://www.oasis-open.org/specs/tm9901"
                xmlns:html="http://www.w3.org/1999/xhtml"
                xmlns:teix="http://www.tei-c.org/ns/Examples"
                xmlns:s="http://www.ascc.net/xml/schematron"
                xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                xmlns:tei="http://www.tei-c.org/ns/1.0"
                xmlns:t="http://www.thaiopensource.com/ns/annotations"
                xmlns:a="http://relaxng.org/ns/compatibility/annotations/1.0"
                xmlns:rng="http://relaxng.org/ns/structure/1.0"
                exclude-result-prefixes="tei html t a rng s iso tbx cals teix"
                version="2.0">
    <xsl:import href="../../../epub/tei-to-epub.xsl"/>

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
         <p>Id: $Id: to.xsl 9097 2011-07-12 10:28:32Z rahtz $</p>
         <p>Copyright: 2008, TEI Consortium</p>
      </desc>
   </doc>

    <xsl:param name="publisher">Oxford Text Archive, Oxford University</xsl:param>
    <xsl:param name="numberHeadings">false</xsl:param>
    <xsl:param name="numberHeadingsDepth">-1</xsl:param>
    <xsl:param name="numberBackHeadings"></xsl:param>
    <xsl:param name="numberFrontHeadings"></xsl:param>
    <xsl:param name="numberFigures">false</xsl:param>
    <xsl:param name="numberTables">false</xsl:param>
    <xsl:param name="autoToc">true</xsl:param>
    <xsl:param name="cssFile">../profiles/oepack/epub/oepack.css</xsl:param>
    <xsl:param name="javascriptFiles">
      ../profiles/oepack/epub/jquery-1.6.1.min.js,
      ../profiles/oepack/epub/popup.js,
      ../profiles/oepack/epub/jquery-css-transform.js,
      ../profiles/oepack/epub/rotate3Di.js
    </xsl:param>
    <xsl:param name="subject">Oxford Text Archive</xsl:param>
    <xsl:param name="pagebreakStyle">none</xsl:param>

    <xsl:template match="tei:title[@type='main']/text()">
      <xsl:value-of select="replace(.,' \[Electronic resource\]','')"/>
    </xsl:template>

    <!--
      <div class="pagebreak">
	<xsl:text>✁[</xsl:text>
	<xsl:text> Page </xsl:text>
	<xsl:value-of select="@n"/>
	<xsl:text>]✁</xsl:text>
      </div>
      -->
	
    <xsl:template match="tei:w[@type and @lemma]">
      <span class="wordtype{@type}">
	<xsl:apply-templates/>
      </span>
    </xsl:template>

  <xsl:template match="tei:sp">
      <xsl:choose>
	<xsl:when test="tei:ab and tei:speaker and ancestor::tei:text/@rend='firstfolio'">
	  <div class="spProseFirstFolio">
	    <xsl:for-each select="tei:speaker">
	      <span>
		<xsl:call-template name="rendToClass">
		  <xsl:with-param name="default">speaker</xsl:with-param>
		</xsl:call-template>
		<xsl:apply-templates/>
	      </span>
	    </xsl:for-each>
	    <xsl:text> </xsl:text>
	    <xsl:for-each select="tei:ab">
	      <xsl:choose>
		<xsl:when test="@type='song'">
		  <div class="firstfoliosong">
		    <xsl:apply-templates/>
		  </div>
		  </xsl:when>
		  <xsl:otherwise>
		    <xsl:apply-templates/>		    
		  </xsl:otherwise>
	      </xsl:choose>
	    </xsl:for-each>
	  </div>
	</xsl:when>
	<xsl:when test="$filePerPage='true'">
	  <xsl:apply-templates/>
	</xsl:when>
	<xsl:when test="tei:ab and tei:speaker">
	  <div class="spProse">
	    <xsl:for-each select="tei:speaker">
	      <span class="speaker">
		<xsl:apply-templates/>
	      </span>
	    </xsl:for-each>
	    <xsl:text> </xsl:text>
	    <xsl:for-each select="tei:ab">
	      <xsl:apply-templates/>
	    </xsl:for-each>
	  </div>
	</xsl:when>
	<xsl:otherwise>
	  <div class="sp">
	    <xsl:apply-templates/>
	  </div>
	</xsl:otherwise>
      </xsl:choose>
  </xsl:template>

  <xsl:template match="tei:speaker">
    <xsl:choose>
      <xsl:when test="$filePerPage='true'">
	<div class="speaker">
	  <xsl:apply-templates/>
	</div>
      </xsl:when>
      <xsl:otherwise>
	<span class="speaker">
	  <xsl:apply-templates/>
	</span>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>

  <xsl:template match="tei:stage">
    <xsl:variable name="e">
      <xsl:choose>
      <xsl:when test="parent::tei:head">span</xsl:when>
      <xsl:when test="parent::tei:l">span</xsl:when>
      <xsl:when test="parent::tei:p">span</xsl:when>
      <xsl:when test="parent::tei:ab">span</xsl:when>
      <xsl:otherwise>div</xsl:otherwise>
    </xsl:choose>
    </xsl:variable>
    <xsl:element name="{$e}">
      <xsl:call-template name="rendToClass">
	<xsl:with-param name="default">
	<xsl:choose>
	  <xsl:when
	      test="ancestor::tei:text/@rend='firstfolio'">stage</xsl:when>
	  <xsl:otherwise>stage it</xsl:otherwise>
	</xsl:choose>
	</xsl:with-param>
      </xsl:call-template>
      <xsl:apply-templates/>
    </xsl:element>
  </xsl:template>

  <xsl:template match="tei:cell/tei:lb"/>

  <xsl:template match="tei:body/tei:lb"/>

  <xsl:template match="tei:div/tei:lb"/>

   <xsl:template match="tei:titlePart" mode="simple">
      <xsl:if test="preceding-sibling::tei:titlePart">
         <br/>
      </xsl:if>
      <xsl:value-of select="."/>
   </xsl:template>

   <xsl:template name="bodyHook">
     <div id="popup"></div>   
     <div>
       <a href="#no-gloss" id="no-gloss">Show gloss</a>
       <a href="#version-switch" id="version-switch">Translation</a>
     </div>
     <div id="holder"></div>
   </xsl:template>

  <xsl:template match="tei:note[@type='gloss']"/>
  <xsl:template match="tei:note[@type='editorial']"/>

   <xsl:template match="tei:l">
     <div class="l">
       <xsl:apply-templates/>
       <xsl:for-each
	   select="tei:seg[@type='annotated']/tei:note[@type='gloss']">
	 <span class="gloss note">
	   <a href="#{generate-id(..)}">
	     <xsl:apply-templates/>
	   </a>
	 </span>
       </xsl:for-each>
     </div>
   </xsl:template>

   <xsl:template match="tei:lg">
     <div class="lg">
       <xsl:apply-templates/>
	 <xsl:for-each
	     select="tei:l/tei:seg[@type='annotated']/tei:note[@type='editorial']">
	   <span class="editorial note">
	     <a href="#{generate-id(..)}">
	       <xsl:apply-templates/>
	     </a>
	   </span>
	 </xsl:for-each>
       </div>
   </xsl:template>

   <xsl:template match="tei:seg[@type='annotated']">
     <span id="{generate-id()}">
       <xsl:apply-templates/>
     </span>
   </xsl:template>

</xsl:stylesheet>
