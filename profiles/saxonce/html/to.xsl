<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet xmlns:ixsl="http://saxonica.com/ns/interactiveXSLT" 
		xmlns:prop="http://saxonica.com/ns/html-property" 
		xmlns:html="http://www.w3.org/1999/xhtml"
		xmlns:style="http://saxonica.com/ns/html-style-property" 
		xmlns:xs="http://www.w3.org/2001/XMLSchema" 
		extension-element-prefixes="ixsl"
		xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
		xpath-default-namespace="http://www.tei-c.org/ns/1.0" 
		xmlns:tei="http://www.tei-c.org/ns/1.0" 
		xmlns:rng="http://relaxng.org/ns/structure/1.0"
		exclude-result-prefixes="#all"
		version="2.0">

    <!-- import base conversion style -->

    <xsl:import href="../../../html5/html5.xsl"/>

    <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" scope="stylesheet" type="stylesheet">
      <desc>

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
         <p>Id: $Id: to.xsl 12078 2013-05-05 12:51:58Z rahtz $</p>
         <p>Copyright: 2013, TEI Consortium</p>
      </desc>
   </doc>
<xsl:param name="currentID"/>

<xsl:template match="/">
  <xsl:result-document href="#title" method="ixsl:replace-content">
    <xsl:sequence select="tei:generateTitle(/*)"/>
  </xsl:result-document>
  
  <xsl:result-document href="#header" method="ixsl:replace-content">
    <p><xsl:sequence select="tei:generateTitle(/*)"/></p>
  </xsl:result-document>

  <xsl:result-document href="#main" method="ixsl:replace-content">
    <xsl:for-each select="*">
      <xsl:call-template name="mainTOC"/>
    </xsl:for-each>
  </xsl:result-document>

  <xsl:result-document href="#footer" method="ixsl:replace-content">
    <xsl:call-template name="stdfooter"/>
  </xsl:result-document>
  
</xsl:template>

<xsl:template name="processTEI">
  <xsl:result-document encoding="{$outputEncoding}"
		       href="#head" method="ixsl:replace-content">
	   <xsl:variable name="pagetitle">
	     <xsl:choose>
	       <xsl:when test="$currentID=''">
		 <xsl:sequence select="tei:generateTitle(.)"/>
	       </xsl:when>
	       <xsl:otherwise>
		 <xsl:choose>
		   <xsl:when test="$currentID='current'">
		     <xsl:apply-templates mode="xref" select="."/>
		   </xsl:when>
		   <xsl:when test="count(id($currentID))&gt;0">
		     <xsl:for-each select="id($currentID)">
		       <xsl:apply-templates mode="xref" select="."/>
		     </xsl:for-each>
		   </xsl:when>
		   <xsl:otherwise>
		     <xsl:apply-templates mode="xpath" select="descendant::text">
		       <xsl:with-param name="xpath" select="$currentID"/>
		       <xsl:with-param name="action" select="'header'"/>
		     </xsl:apply-templates>
		   </xsl:otherwise>
		 </xsl:choose>
		 <xsl:text> - </xsl:text>
		 <xsl:sequence select="tei:generateTitle(.)"/>
	       </xsl:otherwise>
	     </xsl:choose>
	   </xsl:variable>
	   <title>
	     <xsl:value-of select="$htmlTitlePrefix"/>
	     <xsl:text> </xsl:text>
	     <xsl:value-of select="$pagetitle"/>
	   </title>
	   <link href="/favicon.ico" rel="icon" type="image/x-icon"/>
	   <link href="/favicon.ico" rel="shortcut icon" type="image/x-icon"/>
	   <xsl:call-template name="headHook"/>
	   <xsl:call-template name="metaHTML">
	     <xsl:with-param name="title" select="$pagetitle"/>
	   </xsl:call-template>
	   <xsl:call-template name="includeCSS"/>
	   <xsl:call-template name="cssHook"/>
	   <xsl:call-template name="includeJavascript"/>
	   <xsl:call-template name="javascriptHook"/>
  </xsl:result-document>

  <xsl:result-document encoding="{$outputEncoding}"
		       href="#main" method="ixsl:replace-content">
    <div id="top">
      <xsl:sequence select="tei:generateTitle(.)"/>
    </div>
    <div id="main">
      
    </div>
  </xsl:result-document>  
</xsl:template>




  <xsl:template match="*" mode="ident">
    <xsl:variable name="BaseFile">
      <xsl:value-of select="$masterFile"/>
      <xsl:call-template name="addCorpusID"/>
    </xsl:variable>
    
    <xsl:choose>
      <xsl:when test="@xml:id and $useIDs='true'">
	<xsl:value-of select="@xml:id"/>
      </xsl:when>
      <xsl:otherwise>
	<xsl:variable name="xpath">
	  <xsl:for-each select="ancestor-or-self::tei:*">
	    <xsl:value-of select="local-name()"/>
	    <xsl:text>.</xsl:text>
	    <xsl:number/>
	    <xsl:if test="not(position() = last())">
	      <xsl:text>_</xsl:text>
	    </xsl:if>
	  </xsl:for-each>
	</xsl:variable>
	<xsl:value-of
	    select="substring-after(substring-after($xpath,'_text.'),'_')"/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>

  <xsl:template name="pageLayoutComplex">
      <xsl:param name="currentID"/>
      <xsl:result-document encoding="{$outputEncoding}"
			   href="#main" method="ixsl:replace-content">
	<xsl:call-template name="bodyMicroData"/>
	<xsl:call-template name="bodyJavascriptHook"/>
	<xsl:call-template name="bodyHook"/>
	<xsl:call-template name="mainPage">
	  <xsl:with-param name="currentID">
	    <xsl:value-of select="$currentID"/>
	  </xsl:with-param>
	</xsl:call-template>
      </xsl:result-document>
  </xsl:template>



  <xsl:template match="html:a[starts-with(@href,'#')]" mode="ixsl:onclick">
    <xsl:variable name="target" select="substring(@href,2)"/>
    <xsl:result-document  href="#main"
			  encoding="{$outputEncoding}"
			  method="ixsl:replace-content">
      <xsl:for-each select="ixsl:source()">
	<xsl:choose>
	  <xsl:when test="$target=''">
	    <xsl:call-template name="mainTOC"/>
	  </xsl:when>
	  <xsl:when test="count(id($target))&gt;0">
	    <xsl:for-each select="id($target)">
	      <xsl:apply-templates select="ancestor-or-self::div[last()]"/>
	    </xsl:for-each>
	  </xsl:when>
	  <xsl:otherwise>
	    <xsl:apply-templates mode="xpath" select="/TEI/text">
	      <xsl:with-param name="xpath" select="$target"/>
	    </xsl:apply-templates>
	  </xsl:otherwise>
	</xsl:choose>
      </xsl:for-each>
    </xsl:result-document>

  </xsl:template>

</xsl:stylesheet>
