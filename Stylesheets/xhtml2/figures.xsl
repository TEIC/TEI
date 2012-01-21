<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet xmlns="http://www.w3.org/1999/xhtml" 
                xmlns:a="http://relaxng.org/ns/compatibility/annotations/1.0"
                xmlns:fo="http://www.w3.org/1999/XSL/Format"
                xmlns:svg="http://www.w3.org/2000/svg"
                xmlns:html="http://www.w3.org/1999/xhtml"
                xmlns:teidocx="http://www.tei-c.org/ns/teidocx/1.0"
                xmlns:m="http://www.w3.org/1998/Math/MathML"
                xmlns:rng="http://relaxng.org/ns/structure/1.0"
                xmlns:tei="http://www.tei-c.org/ns/1.0"
                xmlns:teix="http://www.tei-c.org/ns/"                
                xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                exclude-result-prefixes="svg a fo html rng tei teidocx teix m"
                version="2.0">
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" scope="stylesheet" type="stylesheet">
      <desc>
         <p> TEI stylesheet dealing with elements from the figures module,
      making HTML output. </p>
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
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>Process elements m:*|@*|comment()|processing-instruction()|text()</desc>
   </doc>
  <xsl:template match="m:*|@*|comment()|processing-instruction()|text()" mode="math">
      <xsl:copy>
         <xsl:apply-templates mode="math" select="*|@*|processing-instruction()|text()"/>
      </xsl:copy>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>Process elements m:math</desc>
   </doc>
  <xsl:template match="m:math">
      <m:math>
         <xsl:copy-of select="@*"/>
         <xsl:apply-templates mode="math"/>
      </m:math>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>Process element cell</desc>
   </doc>
  <xsl:template match="tei:cell">
    <xsl:variable name="cellname">
      <xsl:choose>
	<xsl:when test="parent::tei:row[@rend='thead']">th</xsl:when>
	<xsl:otherwise>td</xsl:otherwise>
      </xsl:choose>
    </xsl:variable>
    <xsl:element name="{$cellname}">
      <xsl:for-each select="@*">
	<xsl:choose>
	  <xsl:when test="name(.) = 'width' or name(.) =
			  'border' or name(.) = 'cellspacing'
			  or name(.) = 'cellpadding'">
	    <xsl:copy-of select="."/>
	  </xsl:when>
	  <xsl:when test="name(.)='rend' and starts-with(.,'width:')">
	    <xsl:attribute name="width">
	      <xsl:value-of select="substring-after(.,'width:')"/>
	    </xsl:attribute>
	  </xsl:when>
	  <xsl:when test="name(.)='rend' and starts-with(.,'class:')">
	    <xsl:attribute name="class">
	      <xsl:value-of select="substring-after(.,'class:')"/>
	    </xsl:attribute>
	  </xsl:when>
	  <xsl:when test="name(.)='rend' and starts-with(.,'style=')">
	    <xsl:attribute name="style">
	      <xsl:value-of select="substring-after(.,'style=')"/>
	    </xsl:attribute>
	  </xsl:when>
	  <xsl:when test="name(.)='rend'">
	    <xsl:attribute name="class">
	      <xsl:value-of select="."/>
	    </xsl:attribute>
	  </xsl:when>
	  <xsl:when test="name(.)='cols'">
	    <xsl:attribute name="colspan">
	      <xsl:value-of select="."/>
	    </xsl:attribute>
	  </xsl:when>
	  <xsl:when test="name(.)='rows'">
	    <xsl:attribute name="rowspan">
	      <xsl:value-of select="."/>
	    </xsl:attribute>
	  </xsl:when>
	  <xsl:when test="name(.)='align'">
	    <xsl:attribute name="align">
	      <xsl:value-of select="."/>
	    </xsl:attribute>
	  </xsl:when>
	</xsl:choose>
      </xsl:for-each>
      <xsl:choose>
	<xsl:when test="@teidocx:align">
	  <xsl:attribute name="align">
	    <xsl:value-of select="@teidocx:align"/>
	  </xsl:attribute>
	</xsl:when>
	<xsl:when test="@align"/>
	<xsl:when test="not($cellAlign='left')">
	  <xsl:attribute name="align">
	    <xsl:value-of select="$cellAlign"/>
	  </xsl:attribute>
	</xsl:when>
      </xsl:choose>
      <xsl:if test="@role and not (@rend)">
	<xsl:attribute name="class">
	  <xsl:value-of select="@role"/>
	</xsl:attribute>
      </xsl:if>
      <xsl:if test="@xml:id">	   
	<xsl:call-template name="makeAnchor"/>
      </xsl:if>
      <xsl:apply-templates/>
    </xsl:element>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>Process element figDesc</desc>
   </doc>
  <xsl:template match="tei:figDesc"/>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>Process element figure</desc>
   </doc>
  <xsl:template match="tei:figure">
    <xsl:variable name="container">
      <xsl:choose>
	<xsl:when test="$outputTarget='html5'">figure</xsl:when>
	<xsl:otherwise>div</xsl:otherwise>
      </xsl:choose>
    </xsl:variable>
    <xsl:choose>
      <xsl:when test="parent::tei:head or @rend='inline' or @place='inline'">
	<xsl:apply-templates/>
      </xsl:when>
      <xsl:when test="parent::tei:ref">
	<xsl:apply-templates/>
      </xsl:when>
      <xsl:otherwise>
	<xsl:element name="{$container}">
	  <xsl:choose>
	    <xsl:when test="@rend">
	      <xsl:attribute name="class">
		<xsl:text>figure </xsl:text>
		<xsl:value-of select="@rend"/>
	      </xsl:attribute>
	    </xsl:when>
	    <xsl:when test="@rendition">
	      <xsl:call-template name="applyRendition"/>
	    </xsl:when>
	    <xsl:otherwise>
	      <xsl:attribute name="class">
		<xsl:text>figure</xsl:text>
	      </xsl:attribute>
	    </xsl:otherwise>
	  </xsl:choose>
	  <xsl:if test="@xml:id">
	    <xsl:attribute name="id">
	      <xsl:value-of select="@xml:id"/>
	    </xsl:attribute>
	  </xsl:if>
	  <xsl:call-template name="figureHook"/>
	  <xsl:apply-templates/>
	</xsl:element>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc>Process element figure/tei:head</desc>
  </doc>
  <xsl:template match="tei:figure/tei:head">
    <xsl:variable name="captionlabel">
      <xsl:for-each select="..">
	<xsl:call-template name="calculateFigureNumber"/>
      </xsl:for-each>
    </xsl:variable>
    <xsl:choose>
      <xsl:when test="$outputTarget='html5'">
	<figcaption>
	  <xsl:if test="@rend">
	    <xsl:attribute name="class" select="@rend"/>
	  </xsl:if>
	  <xsl:call-template name="rendering"/>
	  <xsl:text>. </xsl:text>
	  <xsl:copy-of select="$captionlabel"/>
	  <xsl:apply-templates/>
	</figcaption>
      </xsl:when>
      <xsl:otherwise>
	<span class="caption {@rend}">
	  <xsl:copy-of select="$captionlabel"/>
	  <xsl:text>. </xsl:text>
	  <xsl:apply-templates/>
	</span>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>

  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc>Process element formula</desc>
  </doc>
  <xsl:template match="tei:formula" mode="xref">
    <xsl:number/>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>Process element graphic</desc>
   </doc>
  <xsl:template match="tei:graphic">
      <xsl:call-template name="showGraphic"/>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>Process element row</desc>
   </doc>
  <xsl:template match="tei:row">
      <tr>
         <xsl:call-template name="rendToClass">
	           <xsl:with-param name="default"/>
         </xsl:call-template>
         <xsl:if test="@role">
            <xsl:attribute name="class">
               <xsl:value-of select="@role"/>
            </xsl:attribute>
         </xsl:if>
         <xsl:apply-templates/>
      </tr>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>Process element table</desc>
   </doc>
  <xsl:template match="tei:table">
      <div>
         <xsl:attribute name="class">
	   <xsl:text>table</xsl:text>
	   <xsl:if test="@align">
	     <xsl:text> </xsl:text>
	     <xsl:value-of select="@align"/>
	   </xsl:if>
	 </xsl:attribute>
	 <xsl:if test="@xml:id">	   
	   <xsl:call-template name="makeAnchor"/>
	 </xsl:if>
	 <table>
	   <xsl:call-template name="rendToClass">
	     <xsl:with-param name="id">false</xsl:with-param>
	   </xsl:call-template>
	   <xsl:if test="@rend='frame' or @rend='rules'">
	     <xsl:attribute name="rules">all</xsl:attribute>
	     <xsl:attribute name="border">1</xsl:attribute>
	   </xsl:if>
	   <xsl:for-each select="@*">
	     <xsl:if test="name(.)='summary' or name(.) = 'width' or name(.) = 'border' or name(.) = 'frame' or name(.) = 'rules' or name(.) = 'cellspacing' or name(.) = 'cellpadding'">
	       <xsl:copy-of select="."/>
	     </xsl:if>
	   </xsl:for-each>
	   <xsl:if test="tei:head">
	     <caption>
	       <xsl:apply-templates select="tei:head"/>
	     </caption>
	   </xsl:if>
	   <xsl:choose>
	     <xsl:when test="tei:row[@rend='thead']">
	       <thead>
		 <xsl:apply-templates
		     select="tei:row[@rend='thead']"/>
	       </thead>
	       <tbody>
		 <xsl:apply-templates select="tei:row[not(@rend='thead')]"/>
	       </tbody>
	     </xsl:when>
	     <xsl:otherwise>
	       <xsl:apply-templates select="tei:row"/>
	     </xsl:otherwise>
	   </xsl:choose>
	 </table>
	 <xsl:apply-templates select="tei:note"/>
      </div>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>Process element table[@rend='simple']</desc>
   </doc>
  <xsl:template match="tei:table[@rend='simple']">
      <table>
         <xsl:choose>
	           <xsl:when test="@rend">
	              <xsl:attribute name="class">
	                 <xsl:value-of select="@rend"/>
               </xsl:attribute>
	           </xsl:when>
	           <xsl:when test="@rendition">
	              <xsl:call-template name="applyRendition"/>
	           </xsl:when>
         </xsl:choose>
         <xsl:for-each select="@*">
            <xsl:if test="name(.)='summary'    or name(.) = 'width'    or name(.) = 'border'    or name(.) = 'frame'    or name(.) = 'rules'    or name(.) = 'cellspacing'    or name(.) = 'cellpadding'">
               <xsl:copy-of select="."/>
            </xsl:if>
         </xsl:for-each>
	 <xsl:if test="@xml:id">
	   <xsl:call-template name="makeAnchor"/>
	 </xsl:if>
         <xsl:apply-templates/>
      </table>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>[html] <param name="name">name</param>
         <param name="value">value</param>
      </desc>
   </doc>
  <xsl:template name="setDimension">
      <xsl:param name="name"/>
      <xsl:param name="value"/>

      <xsl:variable name="calcvalue">
         <xsl:choose>
            <xsl:when test="contains($value,'in')">
               <xsl:value-of select="round($dpi * number(substring-before($value,'in')))"/>
            </xsl:when>
            <xsl:when test="contains($value,'pt')">
               <xsl:value-of select="round($dpi * (number(substring-before($value,'pt')) div 72))"/>
            </xsl:when>
            <xsl:when test="contains($value,'cm')">
               <xsl:value-of select="round($dpi * (number(substring-before($value,'cm')) div 2.54 ))"/>
            </xsl:when>
            <xsl:when test="contains($value,'px')">
               <xsl:value-of select="substring-before($value,'px')"/>
            </xsl:when>
            <xsl:otherwise>
               <xsl:value-of select="$value"/>
            </xsl:otherwise>
         </xsl:choose>
      </xsl:variable>
      <xsl:attribute name="{$name}">
         <xsl:value-of select="$calcvalue"/>
      </xsl:attribute>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>[html] </desc>
   </doc>
  <xsl:template name="showGraphic">
      <xsl:variable name="File">
         <xsl:choose>
            <xsl:when test="@url">
               <xsl:value-of select="@url"/>
               <xsl:if test="not(contains(@url,'.'))">
                  <xsl:value-of select="$graphicsSuffix"/>
               </xsl:if>
            </xsl:when>
            <xsl:otherwise>
               <xsl:message terminate="yes">Cannot work out how to do a graphic
          </xsl:message>
            </xsl:otherwise>
         </xsl:choose>
      </xsl:variable>
      <xsl:variable name="Alt">
         <xsl:choose>
            <xsl:when test="tei:desc">
               <xsl:for-each select="tei:desc">
		 <xsl:apply-templates mode="plain"/>
	       </xsl:for-each>
            </xsl:when>
            <xsl:when test="tei:figDesc">
               <xsl:for-each select="tei:figDesc">
	                 <xsl:apply-templates mode="plain"/>
	              </xsl:for-each>
            </xsl:when>
            <xsl:when test="tei:head">
               <xsl:value-of select="tei:head/text()"/>
            </xsl:when>
            <xsl:when test="parent::tei:figure/tei:figDesc">
               <xsl:for-each select="parent::tei:figure/tei:figDesc">
	                 <xsl:apply-templates mode="plain"/>
	              </xsl:for-each>
            </xsl:when>
            <xsl:when test="parent::tei:figure/tei:head">
               <xsl:value-of select="parent::tei:figure/tei:head/text()"/>
            </xsl:when>
         </xsl:choose>
      </xsl:variable>
      <xsl:choose>
	<xsl:when test="$showFigures='true'">
	  <xsl:choose>
	    <xsl:when test="@type='thumbnail'"/>
	    <xsl:when test="starts-with(@mimeType, 'video')">
	      <video src="{$graphicsPrefix}{$File}"
		     controls="controls">
		<xsl:if test="../tei:graphic[@type='thumbnail']">
		  <xsl:attribute name="poster">
		    <xsl:value-of select="../tei:graphic[@type='thumbnail']/@url"/>
		  </xsl:attribute>
		</xsl:if>
	      </video>
	    </xsl:when>
	    <xsl:otherwise>
	      <img src="{$graphicsPrefix}{$File}">
		<xsl:attribute name="alt">
		  <xsl:value-of select="$Alt"/>
		</xsl:attribute>
		<xsl:call-template name="imgHook"/>
		<xsl:if test="@xml:id">
		  <xsl:attribute name="id">
		    <xsl:value-of select="@xml:id"/>
		  </xsl:attribute>
		</xsl:if>
		<xsl:call-template name="rendToClass"/>
		<xsl:if test="@width">
		  <xsl:call-template name="setDimension">
		    <xsl:with-param name="value">
		      <xsl:value-of select="@width"/>
		    </xsl:with-param>
		    <xsl:with-param name="name">width</xsl:with-param>
		  </xsl:call-template>
		</xsl:if>
		<xsl:if test="@height">
		  <xsl:call-template name="setDimension">
		    <xsl:with-param name="value">
		      <xsl:value-of select="@height"/>
		    </xsl:with-param>
		    <xsl:with-param name="name">height</xsl:with-param>
		  </xsl:call-template>
		</xsl:if>
	      </img>
	    </xsl:otherwise>
	  </xsl:choose>	  
         </xsl:when>
         <xsl:otherwise>
            <div class="altfigure">
	      <xsl:call-template name="i18n">
		<xsl:with-param name="word">figureWord</xsl:with-param>
	      </xsl:call-template>
	      <xsl:text> </xsl:text>
	      <xsl:for-each select="self::tei:figure|parent::tei:figure">
		<xsl:number count="tei:figure[tei:head]" level="any"/>
	      </xsl:for-each>
	      <xsl:text> </xsl:text>
	      <xsl:value-of select="$File"/>
	      <xsl:text> [</xsl:text>
	      <xsl:value-of select="$Alt"/>
	      <xsl:text>] </xsl:text>
	    </div>
         </xsl:otherwise>
      </xsl:choose>
  </xsl:template>
  <xsl:template match="tei:binaryObject">
      <img>
         <xsl:attribute name="src">
            <xsl:text>data:</xsl:text>
            <xsl:value-of select="@mimetype"/>
            <xsl:text>;base64,</xsl:text>
            <xsl:copy-of select="text()"/>
         </xsl:attribute>
         <xsl:if test="@width">
            <xsl:call-template name="setDimension">
               <xsl:with-param name="value">
                  <xsl:value-of select="@width"/>
               </xsl:with-param>
               <xsl:with-param name="name">width</xsl:with-param>
            </xsl:call-template>
         </xsl:if>
         <xsl:if test="@height">
            <xsl:call-template name="setDimension">
               <xsl:with-param name="value">
                  <xsl:value-of select="@height"/>
               </xsl:with-param>
               <xsl:with-param name="name">height</xsl:with-param>
            </xsl:call-template>
         </xsl:if>
      </img>
      <!-- also alt -->
    <!-- this is what we'll need for IE:
<style type="text/css">
   img {behavior: expression(fixBase64(this));}
  </style>
  <script type="text/javascript">
  	// a regular expression to test for Base64 data
	var BASE64_DATA = /^data:.*;base64/i;
	// path to the PHP module that will decode the encoded data
	var base64Path = "/my/base64.php";
	function fixBase64(img) {
		// stop the CSS expression from being endlessly evaluated
		img.runtimeStyle.behavior = "none";
		// check the image src
		if (BASE64_DATA.test(img.src)) {
		// pass the data to the PHP routine
		img.src = base64Path + "?" + img.src.slice(5);
		}
	};
  </script>
  Dean Edwards http://dean.edwards.name/weblog/2005/06/base64-sexy/
<?php
$data = split(";", $_SERVER["REDIRECT_QUERY_STRING"]);
$type = $data[0];
$data = split(",", $data[1]);
header("Content-type: ".$type);
echo base64_decode($data[1]);
?>
-->
  </xsl:template>

  <xsl:template match="svg:*">
      <xsl:copy-of select="."/>
  </xsl:template>

</xsl:stylesheet>
