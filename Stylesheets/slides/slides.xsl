<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet xmlns="http://www.w3.org/1999/xhtml" 
		xmlns:atom="http://www.w3.org/2005/Atom"
                xmlns:rng="http://relaxng.org/ns/structure/1.0"
                xmlns:teix="http://www.tei-c.org/ns/Examples"                
                xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                xmlns:tei="http://www.tei-c.org/ns/1.0"
                xmlns:xlink="http://www.w3.org/1999/xlink"
                xmlns:m="http://www.w3.org/1998/Math/MathML"
                xmlns:xhtml="http://www.w3.org/1999/xhtml"
                exclude-result-prefixes="tei xlink xhtml m"
                version="2.0">


  <xsl:strip-space elements="teix:* rng:* xsl:* xhtml:* atom:* m:*"/>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" scope="stylesheet" type="stylesheet">
      <desc>
         <p>
      TEI stylesheet for making HTML presentations from TEI documents
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
         <p>Id: $Id: teihtml-slides.xsl 12255 2013-06-16 11:30:39Z rahtz $</p>
         <p>Copyright: 2013, TEI Consortium</p>
      </desc>
   </doc>
  <xsl:param name="cssFile">http://www.tei-c.org/stylesheet/teislides.css</xsl:param>
  <xsl:param name="logoFile">logo.png</xsl:param>
  <xsl:param name="logoWidth">60</xsl:param>
  <xsl:param name="logoHeight">60</xsl:param>
  <xsl:param name="makingSlides">true</xsl:param>
  <xsl:param name="numberHeadings"/>
  <xsl:param name="splitLevel">0</xsl:param>
  <xsl:param name="STDOUT">false</xsl:param>
  <xsl:param name="subTocDepth">-1</xsl:param>
  <xsl:param name="topNavigationPanel"/>
  <xsl:param name="bottomNavigationPanel">true</xsl:param>
  <xsl:param name="linkPanel"/>
  <xsl:template name="copyrightStatement"/>

  <xsl:template match="tei:div" mode="number">
      <xsl:number level="any"/>
  </xsl:template>


  <xsl:template name="locateParentDiv">
    <xsl:choose>
      <xsl:when test="ancestor-or-self::tei:div">
	<xsl:apply-templates mode="genid" select="ancestor::tei:div[last()]"/>
      </xsl:when>
      <xsl:when test="ancestor-or-self::tei:div1">
	<xsl:apply-templates mode="genid" select="ancestor::tei:div1"/>
      </xsl:when>
    </xsl:choose>
  </xsl:template>

  <xsl:template match="tei:*" mode="genid">
    <xsl:value-of select="$masterFile"/>
    <xsl:apply-templates select="ancestor-or-self::tei:div1|ancestor-or-self::tei:div[1]" mode="number"/>
  </xsl:template>

  <xsl:template match="tei:docAuthor">
      <div class="docAuthor">
         <xsl:apply-templates/>
      </div>
  </xsl:template>

  <xsl:template match="tei:docDate">
      <div class="docDate">
         <xsl:apply-templates/>
      </div>
  </xsl:template>

  <xsl:template match="/tei:TEI">
      <xsl:param name="slidenum">
         <xsl:value-of select="$masterFile"/>0</xsl:param>

      <xsl:variable name="outName">
	<xsl:call-template name="outputChunkName">
	  <xsl:with-param name="ident">
	    <xsl:value-of select="$slidenum"/>
	  </xsl:with-param>
         </xsl:call-template>
      </xsl:variable>

      <xsl:if test="$verbose='true'">
         <xsl:message>Opening file <xsl:value-of select="$outName"/>
         </xsl:message>
      </xsl:if>
      <xsl:result-document doctype-public="{$doctypePublic}" doctype-system="{$doctypeSystem}"
                           encoding="{$outputEncoding}"
                           href="{$outName}"
                           method="{$outputMethod}">
         <xsl:call-template name="mainslide"/>
      </xsl:result-document>
      <xsl:if test="$verbose='true'">
         <xsl:message>Closing file <xsl:value-of select="$outName"/>
         </xsl:message>
      </xsl:if>

      <xsl:for-each select="tei:text/tei:body">
         <xsl:apply-templates select="tei:div|tei:div1"/>
      </xsl:for-each>
  </xsl:template>

  <xsl:template name="xrefpanel">
      <xsl:param name="homepage"/>
      <xsl:param name="mode"/>
      <xsl:variable name="first">
         <xsl:value-of select="$masterFile"/>0</xsl:variable>
      <xsl:variable name="prev">
         <xsl:choose>
            <xsl:when test="preceding-sibling::tei:div">
               <xsl:apply-templates select="preceding-sibling::tei:div[1]" mode="genid"/>
            </xsl:when>
            <xsl:when test="preceding::tei:div">
               <xsl:apply-templates select="preceding::tei:div[1]" mode="genid"/>
            </xsl:when>
            <xsl:when test="preceding::tei:div1">
               <xsl:apply-templates select="preceding::tei:div1[1]" mode="genid"/>
            </xsl:when>
         </xsl:choose>
      </xsl:variable>
      <xsl:if test="not($prev='')">
         <a class="xreflink" accesskey="p" href="{concat($prev,$outputSuffix)}">
            <span class="button">«</span>
         </a>
      </xsl:if>
      <xsl:text>  </xsl:text>
      <a class="xreflink" accesskey="f" href="{concat($first,$outputSuffix)}">
         <span class="button">^</span>
      </a>
      <xsl:variable name="next">
         <xsl:choose>
            <xsl:when test="tei:div">
               <xsl:apply-templates select="tei:div[1]" mode="genid"/>
	           </xsl:when>
            <xsl:when test="following-sibling::tei:div">
               <xsl:apply-templates select="following-sibling::tei:div[1]" mode="genid"/>
            </xsl:when>
            <xsl:when test="following::tei:div">
               <xsl:apply-templates select="following::tei:div[1]" mode="genid"/>
            </xsl:when>
            <xsl:when test="following::tei:div1">
               <xsl:apply-templates select="following::tei:div1[1]" mode="genid"/>
            </xsl:when>
         </xsl:choose>
      </xsl:variable>
      <xsl:if test="not($next='')">
         <a class="xreflink" accesskey="n" href="{concat($next,$outputSuffix)}">
            <span class="button">»</span>
         </a>
      </xsl:if>
      <xsl:text> </xsl:text>
      <xsl:apply-templates select="." mode="number"/>
  </xsl:template>

  <xsl:template name="mainslide">
      <html>
         <xsl:call-template name="addLangAtt"/>
         <head>
            <title>
               <xsl:sequence select="tei:generateTitle(.)"/>
            </title>
            <xsl:call-template name="includeCSS"/>
            <xsl:call-template name="cssHook"/>
            <xsl:call-template name="javascriptHook"/>
         </head>
         <body>
            <div class="slidetitle" style="font-size: 36pt;">
               <xsl:sequence select="tei:generateTitle(.)"/>
            </div>
            <div class="slidemain">
               <xsl:apply-templates select="tei:text/tei:front//tei:docAuthor"/>
               <xsl:apply-templates select="tei:text/tei:front//tei:docDate"/>
               <ul class="slidetoc">
                  <xsl:for-each select="tei:text/tei:body">
                     <xsl:for-each select="tei:div|tei:div1">
                        <xsl:variable name="n">
                           <xsl:apply-templates select="." mode="genid"/>
                        </xsl:variable>
                        <li class="slidetoc">
                           <a href="{$n}.xhtml">
                              <xsl:value-of select="tei:head"/>
                           </a>
                        </li>
                     </xsl:for-each>
                  </xsl:for-each>
               </ul>
            </div>
            <div class="slidebottom">
	      <div class="slidebottom-image">
		<img id="logo" src="{$logoFile}" width="{$logoWidth}" height="{$logoHeight}"
		     alt="logo"/>
	      </div>
	      <div class="slidebottom-text">
		<xsl:variable name="next">
		  <xsl:value-of select="$masterFile"/>
		  <xsl:text>1</xsl:text>
		</xsl:variable>
		<a accesskey="n" href="{concat($next,$outputSuffix)}">Start</a>
	      </div>
            </div>
         </body>
      </html>
  </xsl:template>

   <xsl:template name="includeJavascript">
      <xsl:variable name="prev">
         <xsl:choose>	
            <xsl:when test="preceding-sibling::tei:div">
	              <xsl:apply-templates select="preceding-sibling::tei:div[1]" mode="genid"/>
            </xsl:when>
            <xsl:when test="preceding::tei:div1">
	              <xsl:apply-templates select="preceding::tei:div1[1]" mode="genid"/>
            </xsl:when>
            <xsl:when test="preceding::tei:div">
	              <xsl:apply-templates select="preceding::tei:div[1]" mode="genid"/>
            </xsl:when>
            <xsl:otherwise>
	              <xsl:value-of select="$masterFile"/>
	              <xsl:text>0</xsl:text>
            </xsl:otherwise>
         </xsl:choose>
      </xsl:variable>
      <xsl:variable name="next">
         <xsl:choose>
            <xsl:when test="tei:div">
               <xsl:apply-templates select="tei:div[1]" mode="genid"/>
	           </xsl:when>

            <xsl:when test="following-sibling::tei:div">
	              <xsl:apply-templates select="following-sibling::tei:div[1]" mode="genid"/>
            </xsl:when>
            <xsl:when test="following::tei:div1">
	              <xsl:apply-templates select="following::tei:div1[1]" mode="genid"/>
            </xsl:when>
            <xsl:when test="following::tei:div">
	              <xsl:apply-templates select="following::tei:div[1]" mode="genid"/>
            </xsl:when>
            <xsl:otherwise>
	              <xsl:value-of select="$masterFile"/>
	              <xsl:text>0</xsl:text>
            </xsl:otherwise>
         </xsl:choose>
      </xsl:variable>
      <xsl:call-template name="writeJavascript">
         <xsl:with-param name="content">
            <xsl:text>
    var isOp = navigator.userAgent.indexOf('Opera') &gt; -1 ? 1 : 0;
    function keys(key) {
    if (!key) {
    key = event;
    key.which = key.keyCode;
    }
    switch (key.which) {
    case 10: // return
    case 32: // spacebar
    case 34: // page down
    case 39: // rightkey
    // case 40: 
    // downkey
    document.location = "</xsl:text>
            <xsl:value-of select="$next"/>
            <xsl:text>.xhtml";
    break;
    case 33: // page up
    case 37: // leftkey
    //case 38: 
    // upkey
    document.location = "</xsl:text>
            <xsl:value-of select="$prev"/>
            <xsl:text>.xhtml";
    break;
	}
	}
	function startup() {      
	if (!isOp) {		
	document.onkeyup = keys;
	document.onclick = clicker;
	}
	}
	
	function clicker(e) {
	var target;
	if (window.event) {
	target = window.event.srcElement;
	e = window.event;
	} else target = e.target;
	if (target.href != null ) return true;
	if (!e.which || e.which == 1) 
	document.location = "</xsl:text>
	           <xsl:value-of select="$next"/>
	           <xsl:text>.xhtml";
	}
	
	window.onload = startup;
  </xsl:text>
         </xsl:with-param>
      </xsl:call-template>
   </xsl:template>

  <xsl:template match="tei:body/tei:div[tei:div]">
      <xsl:variable name="slidenum">
         <xsl:value-of select="$masterFile"/>
         <xsl:number/>
      </xsl:variable>
      <xsl:variable name="outName">
         <xsl:call-template name="outputChunkName">
	           <xsl:with-param name="ident">
	              <xsl:value-of select="$slidenum"/>
	           </xsl:with-param>
         </xsl:call-template>
      </xsl:variable>

      <xsl:if test="$verbose='true'">
         <xsl:message>Opening file <xsl:value-of select="$outName"/>
         </xsl:message>
      </xsl:if>
      <xsl:result-document doctype-public="{$doctypePublic}" doctype-system="{$doctypeSystem}"
                           encoding="{$outputEncoding}"
                           href="{$outName}"
                           method="{$outputMethod}">
        <html>
            <xsl:call-template name="addLangAtt"/>
            <head>
               <title>
                  <xsl:value-of select="tei:head"/>
               </title>
               <xsl:call-template name="includeCSS"/>
               <xsl:call-template name="cssHook"/>
               <xsl:call-template name="includeJavascript"/>
               <xsl:call-template name="javascriptHook"/>
            </head>
            <body>
               <h1>
                  <xsl:value-of select="tei:head"/>
               </h1>
            </body>
        </html>
      </xsl:result-document>
      <xsl:if test="$verbose='true'">
         <xsl:message>Closing file <xsl:value-of select="$outName"/>
         </xsl:message>
      </xsl:if>

      <xsl:apply-templates select="tei:div"/>
  </xsl:template>


  <xsl:template match="tei:body/tei:div|tei:div">
      <xsl:choose>
         <xsl:when test="$splitLevel&gt;-1">
	           <xsl:variable name="slidenum">
	              <xsl:apply-templates select="." mode="genid"/>
	           </xsl:variable>
	
	           <xsl:variable name="outName">
	              <xsl:call-template name="outputChunkName">
	                 <xsl:with-param name="ident">
	                    <xsl:value-of select="$slidenum"/>
	                 </xsl:with-param>
	              </xsl:call-template>
	           </xsl:variable>
	
	           <xsl:if test="$verbose='true'">
	              <xsl:message>Opening file <xsl:value-of select="$outName"/>
               </xsl:message>
	           </xsl:if>
	           <xsl:result-document doctype-public="{$doctypePublic}" doctype-system="{$doctypeSystem}"
                                 encoding="{$outputEncoding}"
                                 href="{$outName}"
                                 method="{$outputMethod}">
	              <xsl:call-template name="slideout"/>
	           </xsl:result-document>
	           <xsl:if test="$verbose='true'">
	              <xsl:message>Closing file <xsl:value-of select="$outName"/>
               </xsl:message>
	           </xsl:if>
         </xsl:when>
         <xsl:otherwise>
	           <xsl:call-template name="slidebody"/>
         </xsl:otherwise>
      </xsl:choose>
  </xsl:template>


  <xsl:template name="slideout">
      <html>
         <xsl:call-template name="addLangAtt"/>
         <head>
            <title>
               <xsl:value-of select="tei:head"/>
            </title>
            <xsl:call-template name="includeCSS"/>
            <xsl:call-template name="cssHook"/>
            <xsl:call-template name="includeJavascript"/>
            <xsl:call-template name="javascriptHook"/>
         </head>
         <body>
            <xsl:call-template name="slidebody"/>
         </body>
      </html>
  </xsl:template>

  <xsl:template name="slidebody">
    <div class="slidetop">
      <div class="slidetitle">
	<xsl:call-template name="header">
	  <xsl:with-param name="display">full</xsl:with-param>
	</xsl:call-template>
      </div>
      <div class="xref">
	<xsl:call-template name="xrefpanel"/>
      </div>
    </div>
    <div class="slidemain">
      <xsl:apply-templates/>
    </div>
    <div class="slidebottom">
      <xsl:call-template name="slideBottom"/>
    </div>
  </xsl:template>


  <xsl:template name="slideBottom">
    <div class="slidebottom-image">
      <img id="logo" src="{$logoFile}" width="{$logoWidth}" height="${logoHeight}"
	   alt="logo"/>
    </div>
    <div class="slidebottom-text">
      <xsl:sequence select="tei:generateTitle(.)"/>
    </div>
  </xsl:template>

  <xsl:template match="tei:row">
	     <xsl:variable name="c">
	        <xsl:number/>
	     </xsl:variable>
	     <xsl:variable name="class">
	        <xsl:choose>
	           <xsl:when test="@role">
		             <xsl:value-of select="@role"/>
	           </xsl:when>
	           <xsl:when test="$c mod 2=0">
	              <xsl:text>Even</xsl:text>
	           </xsl:when>
	           <xsl:otherwise>
	              <xsl:text>Odd</xsl:text>
	           </xsl:otherwise>
	        </xsl:choose>
	     </xsl:variable>
	     <tr class="{$class}">
	        <xsl:apply-templates/>
	     </tr>
  </xsl:template>

  <xsl:template match="teix:egXML">
      <xsl:param name="simple">false</xsl:param>
      <xsl:param name="highlight"/>
    
      <div class="pre">
	        <xsl:apply-templates mode="verbatim">
	           <xsl:with-param name="highlight">
	              <xsl:value-of select="$highlight"/>
	           </xsl:with-param>
	        </xsl:apply-templates>
      </div>
  </xsl:template>

  <xsl:template match="xhtml:*">
      <xsl:copy-of select="."/>
  </xsl:template>

  <xsl:template match="tei:att">
      <span class="att">
         <xsl:apply-templates/>
      </span>
  </xsl:template>

</xsl:stylesheet>