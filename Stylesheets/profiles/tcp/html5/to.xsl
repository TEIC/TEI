<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet 
    xmlns:xs="http://www.w3.org/2001/XMLSchema"
    xmlns="http://www.w3.org/1999/xhtml"
    xmlns:tei="http://www.tei-c.org/ns/1.0"
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    exclude-result-prefixes="tei"
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
         
         <p>Copyright: 2013, TEI Consortium</p>
      </desc>
   </doc>

   <xsl:output method="xhtml" omit-xml-declaration="yes"
	       encoding="utf-8"/>

   <xsl:param name="pageLayout">Complex</xsl:param>
    <xsl:param name="autoToc">true</xsl:param>
    <xsl:param name="cssFile">../tcp.css</xsl:param>
    <xsl:param name="footnoteBackLink">true</xsl:param>
    <xsl:param name="homeURL">http://www.ota.ox.ac.uk/tcp/</xsl:param>
    <xsl:param name="htmlTitlePrefix">[TCP] </xsl:param>
    <xsl:param name="institution">Text Creation Partnership EEBO, ECCO and Evans texts</xsl:param>
    <xsl:param name="numberBackHeadings"></xsl:param>
    <xsl:param name="numberFigures">false</xsl:param>
    <xsl:param name="numberFrontHeadings"></xsl:param>
    <xsl:param name="numberHeadings">false</xsl:param>
    <xsl:param name="numberHeadingsDepth">-1</xsl:param>
    <xsl:param name="numberTables">false</xsl:param>
    <xsl:param name="pagebreakStyle">visible</xsl:param>
    <xsl:param name="parentURL">http://www.textcreationpartnership.org/tcp-eebo/</xsl:param>
    <xsl:param name="parentWords">Text Creation Partnership</xsl:param>
    <xsl:param name="publisher">University of Oxford TCP</xsl:param>
    <xsl:param name="showTitleAuthor">true</xsl:param>
    <xsl:param name="sort">author</xsl:param>
    <xsl:param name="splitLevel">-1</xsl:param>
    <xsl:param name="subject">University of Oxford TCP</xsl:param>
    <xsl:param name="treestyle">d3VerticalTree</xsl:param>
    <xsl:param name="generationComment">false</xsl:param>
    <xsl:param name="separator"> | </xsl:param>

  <xsl:template name="mainPage">
    <xsl:param name="currentID"/>
    <div class="show-all" id="main">
      <!-- header -->

      <div id="hdr" class="stdheader">
        <xsl:call-template name="hdr"/>
	<xsl:call-template name="makeHTMLHeading">
	  <xsl:with-param name="class">author</xsl:with-param>
	  <xsl:with-param name="text">
	    <xsl:call-template name="generateAuthorList"/>
	    <xsl:sequence select="tei:generateDate(.)"/>
	    <xsl:sequence select="tei:generateEdition(.)"/>
	  </xsl:with-param>
	</xsl:call-template>
      </div>

      <div id="main-menu">
	<p><i>University of     <span class="bold">O</span>xford 
      <span class="bold">T</span>ext 
      <span class="bold">A</span>rchive</i></p>
      <ul class="OTAnav">
        <li class="navLink"><a    href="http://www.ota.ox.ac.uk/">Home</a> | 	</li>
	<li class="navLink"><a href="http://www.ota.ox.ac.uk/tcp">OTA TCP Catalogue</a>	| </li>
	<li class="navlink"><a href="http://www.textcreationpartnership.org/tcp-eebo/">Text
	Creation Partnership</a></li>
      </ul><hr/>
	<xsl:call-template name="mainTOC"/>
	<hr/>
	<p class="small">This material was created by the Text Creation Partnership in partnership with ProQuest's Early English Books Online, Gale Cengage's Eighteenth Century Collections Online, and Readex's Evans Early American Imprints.</p>
      </div>


      <div id="onecol" class="main-content">
	<xsl:call-template name="mainFrame">
	  <xsl:with-param name="currentID" select="$currentID"/>
	  <xsl:with-param name="minimal">true</xsl:with-param>
	</xsl:call-template>

	<div class="stdfooter">
	  <xsl:apply-templates select="/*/tei:teiHeader/tei:fileDesc/tei:publicationStmt/tei:availability/*"/>
	</div>
      </div>


      <div class="clear" id="em"/>
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


  <xsl:function name="tei:escapeChars" as="xs:string">
    <xsl:param name="letters"/>
    <xsl:param name="context"/>
    <xsl:value-of select="translate($letters,'ſ','s')"/>
  </xsl:function>

</xsl:stylesheet>
