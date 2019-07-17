<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet 
    xmlns="http://www.w3.org/1999/xhtml"
    xmlns:xs="http://www.w3.org/2001/XMLSchema"
    xmlns:tei="http://www.tei-c.org/ns/1.0"
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    exclude-result-prefixes="#all"
    version="2.0">
    <!-- import base conversion style -->

    <xsl:import href="../../../html/html.xsl"/>

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
         
         <p>Copyright: 2013, TEI Consortium</p>
      </desc>
   </doc>

   <xsl:output method="xhtml" omit-xml-declaration="yes"
	       encoding="utf-8"/>

    <xsl:param name="treestyle">d3VerticalTree</xsl:param>
    <xsl:param name="publisher">University of Oxford Text Archive</xsl:param>
    <xsl:param name="numberHeadings">false</xsl:param>
    <xsl:param name="numberHeadingsDepth">-1</xsl:param>
    <xsl:param name="numberBackHeadings"></xsl:param>
    <xsl:param name="numberFrontHeadings"></xsl:param>
    <xsl:param name="numberFigures">false</xsl:param>
    <xsl:param name="numberTables">false</xsl:param>
    <xsl:param name="autoToc">true</xsl:param>
    <xsl:param name="footnoteBackLink">true</xsl:param>
    <xsl:param name="subject">University of Oxford Text Archive</xsl:param>
    <xsl:param name="pagebreakStyle">visible</xsl:param>

    <xsl:param name="splitLevel">-1</xsl:param>
    <xsl:param name="googlestylesheet">oxford</xsl:param>
    <xsl:param name="cssPrintFile">/ota-print.css</xsl:param>
    <xsl:param name="cssFile">/otatext.css</xsl:param>
    <xsl:param name="sort">author</xsl:param>
    <xsl:param name="htmlTitlePrefix">[OTA] </xsl:param>

  <xsl:template name="additionalMenu">
    <xsl:element name="{if ($outputTarget='html5') then 'nav' else 'div'}">
      
      <ul class="OTAnav">
	<li class="navLabel"><span class="bold">O</span>xford <span class="bold"
	>T</span>ext <span class="bold">A</span>rchive: </li>
	<li class="navLink">
	<a href="/">Home</a> | </li>
	<li class="navLink">
        <a href="/about/">About</a> | </li>
      <li class="navLink">
        <a href="/about/news.xml">News</a> | </li>
      <li class="navLink">
        <a href="/catalogue/index-id.html">Catalogue</a> | </li>
      <li class="navLink">
        <a href="/about/contact.xml">Contact</a> | </li>
      <li class="navLink">
        <a href="/about/faq.xml">Help and FAQ</a> | </li>
      <li class="navLink">
        <a href="/about/search.xml">Search OTA</a>
      </li>
    </ul>
    </xsl:element>
  </xsl:template>

  <xsl:template name="mainPage">
    <xsl:param name="currentID"/>
    <div class="show-all" id="main">
      <!-- header -->

      <div id="hdr">
        <xsl:call-template name="hdr"/>
      </div>

      <div id="main-menu">
	<xsl:call-template name="additionalMenu"/>
      </div>


      <div id="onecol" class="main-content">
        <h1>
          <xsl:sequence select="tei:generateTitle(.)"/>
        </h1>

	<xsl:choose>
	  <xsl:when test="local-name(.)='div'">
	    <h1>
	      <xsl:apply-templates mode="section" select="tei:head"/>
	    </h1>
	    <xsl:apply-templates/>
	  </xsl:when>
	  <xsl:otherwise>
	    <xsl:call-template name="mainFrame">
	      <xsl:with-param name="currentID" select="$currentID"/>
	      <xsl:with-param name="minimal">true</xsl:with-param>
	    </xsl:call-template>
	  </xsl:otherwise>
	</xsl:choose>
      </div>


      <div class="clear" id="em"/>
    </div>

    <div>
      <xsl:call-template name="stdfooter"/>
    </div>

  </xsl:template>



  <xsl:template match="tei:body/tei:lb"/>

  <xsl:template match="tei:div/tei:lb"/>

   <xsl:template match="tei:titlePart" mode="simple">
      <xsl:if test="preceding-sibling::tei:titlePart">
         <br/>
      </xsl:if>
      <xsl:value-of select="."/>
   </xsl:template>

   <xsl:template name="cssHook">
     <style type="text/css">
   div.contamination {
   font-style:italic;
   }
   div.derivation-syntactic {
   font-style:italic;
   }
   div.extant {
   font-weight:bold;
   }
   div.hypothetical {
   font-style:italic;
   }
   div.lost {
   color: red;
   }
   div.main {
   font-weight: bold;
   }
   pre,div.pre,div.pre_eg,pre.eg,div.eg {
   clear:both;
   margin-top: 1em;
   margin-bottom:1em;
   border-top-width: 4px;
   border-bottom-width: 4px;
   border-left-width: 2px;
   border-right-width: 2px;
   border-style: solid;
   padding-top: 10px;
   padding-right: 10px;
   padding-bottom: 10px;
   padding-left: 10px;
   color: #000000;
   line-height: 1.1em;
   font-family: monospace;
   font-size: 10pt;
   white-space: pre;
   }
   .leaf {
    background-color: lightgrey;
   }
   .node { 
   font-weight: normal;
   font-size: 8pt;
   }
   .treediagram {
   }
   
     </style>
   </xsl:template>

  <xsl:function name="tei:escapeChars" as="xs:string">
    <xsl:param name="letters"/>
    <xsl:param name="context"/>
    <xsl:value-of select="translate($letters,'ſ','s')"/>
  </xsl:function>

</xsl:stylesheet>
