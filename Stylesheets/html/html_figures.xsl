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
      <desc>Processing MathML to avoid prefixes (upsets browsers)</desc>
   </doc>
   <xsl:template   match="m:*"   mode="math">
     <xsl:element name="{local-name()}"
		  namespace="http://www.w3.org/1998/Math/MathML">
       <xsl:apply-templates select="node() | @*" mode="math"/>
     </xsl:element>
   </xsl:template>
   
   <xsl:template match="@*|comment()|processing-instruction()|text()" mode="math">
     <xsl:copy>
       <xsl:apply-templates mode="math" select="*|@*|processing-instruction()|text()"/>
      </xsl:copy>
  </xsl:template>

  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>Process elements m:math</desc>
   </doc>
  <xsl:template match="m:math">
    <xsl:apply-templates mode="math" select="."/>
  </xsl:template>

  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>Process element cell</desc>
   </doc>
  <xsl:template match="tei:cell">
    <xsl:variable name="cellname">
      <xsl:choose>
	<xsl:when test="parent::tei:row[tei:match(@rend,'thead')]">th</xsl:when>
	<xsl:when test="parent::tei:row[@role='label' and not(preceding::tei:row)]">th</xsl:when>
	<xsl:otherwise>td</xsl:otherwise>
      </xsl:choose>
    </xsl:variable>
    <xsl:element name="{$cellname}">
      <xsl:for-each select="@*">
	<xsl:choose>
	  <xsl:when test="name(.) eq 'style'">
	    <xsl:copy-of select="."/>
	  </xsl:when>
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
	  <xsl:when test="local-name(.)='align'">
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
	  <xsl:attribute name="style">
	    <xsl:text>text-align:</xsl:text>
	    <xsl:value-of select="$cellAlign"/>
	  </xsl:attribute>
	</xsl:when>
      </xsl:choose>
      <xsl:if test="@role and not (@rend)">
	<xsl:attribute name="class">
	  <xsl:value-of select="@role"/>
	</xsl:attribute>
      </xsl:if>
      <xsl:if test="ancestor::tei:table[tei:match(@rend,'rules')] and not(@rend)">
	<xsl:attribute name="style">
	  <xsl:text>border: 1px solid black; padding: 2px;</xsl:text>
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
  <xsl:template match="tei:figDesc">
    <xsl:if test="count(parent::tei:figure/*)=1">
      <i>
	<xsl:text>[</xsl:text>
	<xsl:value-of select="."/>
	<xsl:text>]</xsl:text>
      </i>
    </xsl:if>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>Process element figure</desc>
   </doc>
  <xsl:template match="tei:figure">
    <xsl:choose>
      <xsl:when test="tei:match(@rend,'inline') or @place='inline'">
	<xsl:apply-templates/>
      </xsl:when>
      <xsl:when test="parent::tei:hi/parent::tei:date or parent::tei:ref or parent::tei:label">
	<xsl:apply-templates/>
      </xsl:when>
      <xsl:otherwise>
	<xsl:element name="{if ($outputTarget='html5') then 'figure'
			   else if (tei:isInline(.)) then 'span'
			   else 'div'}">
          <xsl:call-template name="makeRendition">
	    <xsl:with-param name="auto">figure</xsl:with-param>
	  </xsl:call-template>
	  <xsl:if test="@xml:id">
	    <xsl:attribute name="id">
	      <xsl:value-of select="@xml:id"/>
	    </xsl:attribute>
	  </xsl:if>
	  <xsl:call-template name="figureHook"/>
	  <xsl:choose>
	    <xsl:when test="not(*)">
	      <i>
		<xsl:text>[figure]</xsl:text>
	      </i>
	    </xsl:when>
	    <xsl:otherwise>
	      <xsl:apply-templates/>
	    </xsl:otherwise>
	  </xsl:choose>
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
          <xsl:call-template name="makeRendition">
	    <xsl:with-param name="default">caption</xsl:with-param>
	  </xsl:call-template>
	  <xsl:copy-of select="$captionlabel"/>
	  <xsl:if test="not($captionlabel='')">
	    <xsl:text>. </xsl:text>
	  </xsl:if>
	  <xsl:apply-templates/>
	</figcaption>
      </xsl:when>
      <xsl:otherwise>
	<xsl:element name="{if (ancestor::tei:q) then 'span' else 'div'}">
          <xsl:call-template name="makeRendition">
	    <xsl:with-param name="default">caption</xsl:with-param>
	  </xsl:call-template>
	  <xsl:copy-of select="$captionlabel"/>
	  <xsl:if test="not($captionlabel='')">
	    <xsl:text>. </xsl:text>
	  </xsl:if>
	  <xsl:apply-templates/>
	</xsl:element>
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
  <xsl:template match="tei:graphic|tei:binaryObject|tei:media">
      <xsl:call-template name="showGraphic"/>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>Process element row</desc>
   </doc>
  <xsl:template match="tei:row">
      <tr>
	<xsl:call-template name="makeRendition">
	  <xsl:with-param name="default">false</xsl:with-param>
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
	   <xsl:call-template name="makeRendition">
	    <xsl:with-param name="default">false</xsl:with-param>
	  </xsl:call-template>
	   <xsl:if test="tei:match(@rend,'frame') or tei:match(@rend,'rules')">
	     <xsl:attribute name="style">border-collapse:collapse;border-spacing:0;</xsl:attribute>
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
	     <xsl:when test="tei:row[tei:match(@rend,'thead')]">
	       <thead>
		 <xsl:apply-templates
		     select="tei:row[tei:match(@rend,'thead')]"/>
	       </thead>
	       <tbody>
		 <xsl:apply-templates select="tei:row[not(tei:match(@rend,'thead'))]"/>
	       </tbody>
	     </xsl:when>
	     <xsl:when test="tei:row[@role='label' and not(preceding::tei:row)]">
	       <thead>
		 <xsl:apply-templates
		     select="tei:row[@role='label' and not(preceding::tei:row)]"/>
	       </thead>
	       <tbody>
		 <xsl:apply-templates
		     select="tei:row[not(@role='label' and not(preceding::tei:row))]"/>
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
      <desc>Process element table[tei:match(@rend,'simple')]</desc>
   </doc>
  <xsl:template match="tei:table[tei:match(@rend,'simple')]">
      <table>
	<xsl:call-template name="makeRendition">
	  <xsl:with-param name="default">false</xsl:with-param>
	</xsl:call-template>
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
      <desc>[html] display graphic file</desc>
   </doc>
  <xsl:template name="showGraphic">
      <xsl:variable name="File">
         <xsl:choose>
	   <xsl:when test="self::tei:binaryObject"/>
            <xsl:when test="@url">
               <xsl:sequence select="tei:resolveURI(.,@url)"/>
               <xsl:if test="not(contains(@url,'.'))">
                  <xsl:value-of select="$graphicsSuffix"/>
               </xsl:if>
            </xsl:when>
            <xsl:otherwise>
               <xsl:message>Found binaryObject without @url.</xsl:message>
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
	      <xsl:variable name="sizes">
		<xsl:if test="@width">
		  <xsl:text> width:</xsl:text>
		  <xsl:value-of select="@width"/>
		  <xsl:text>;</xsl:text>
		</xsl:if>
		<xsl:if test="@height">
		  <xsl:text> height:</xsl:text>
		  <xsl:value-of select="@height"/>
		  <xsl:text>;</xsl:text>
		</xsl:if>
	      </xsl:variable>
	      <xsl:variable name="i">
		<img>
		  <xsl:attribute name="src">
		    <xsl:choose>
		      <xsl:when test="self::tei:binaryObject">
		        <xsl:variable name="mime" select="if (@mimeType) then @mimeType else 'image/*'"/>
		        <xsl:variable name="enc" select="if (@encoding) then @encoding else 'base64'"/>
		        <xsl:value-of select="concat('data:', $mime, ';', $enc, ',', normalize-space(text()))"/>
		      </xsl:when>
		      <xsl:otherwise>
			<xsl:value-of
			    select="concat($graphicsPrefix,$File)"/>
		      </xsl:otherwise>
		    </xsl:choose>
		  </xsl:attribute>
		  <xsl:attribute name="alt">
		    <xsl:value-of select="$Alt"/>
		  </xsl:attribute>
		  <xsl:call-template name="imgHook"/>
		  <xsl:if test="@xml:id">
		    <xsl:attribute name="id">
		      <xsl:value-of select="@xml:id"/>
		    </xsl:attribute>
		  </xsl:if>
		  <xsl:call-template name="makeRendition"/>
		</img>
	      </xsl:variable>
	      <xsl:for-each select="$i/*">
		<xsl:copy>
		  <xsl:copy-of select="@*[not(name()='style')]"/>
		  <xsl:choose>
		    <xsl:when test="$sizes=''">
		      <xsl:copy-of select="@style"/>
		    </xsl:when>
		    <xsl:when test="not(@style)">
		      <xsl:attribute name="style" select="$sizes"/>
		    </xsl:when>
		    <xsl:otherwise>
		      <xsl:attribute name="style"
				     select="concat(@style,';' ,$sizes)"/>
		    </xsl:otherwise>
		  </xsl:choose>
		</xsl:copy>
	      </xsl:for-each>
	    </xsl:otherwise>
	  </xsl:choose>	  
         </xsl:when>
         <xsl:otherwise>
            <div class="altfigure">
	      <xsl:sequence select="tei:i18n('figureWord')"/>
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

  <xsl:template match="svg:*">
      <xsl:copy-of select="."/>
  </xsl:template>

  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>[html] figures containing SVG markup </desc>
   </doc>

    <xsl:template match="tei:figure[svg:svg]">
      <xsl:copy-of select="svg:svg"/>
    </xsl:template>

  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc>[html] figures containing MathML Markup </desc>
   </doc>

    <xsl:template match="tei:formula[m:math]">
      <xsl:choose>
	<xsl:when test="@xml:id and (tei:match(@rend,'display') or
			tei:match(@rend,'equation') or tei:match(@rend,'subeqn'))">
	  <div id="{@xml:id}">
	    <xsl:apply-templates select="m:math" mode="math"/>
	  </div>
	</xsl:when>
	<xsl:when test="@xml:id">
	  <span id="{@xml:id}">
	    <xsl:apply-templates select="m:math" mode="math"/>
	  </span>
	</xsl:when>
	<xsl:otherwise>
	  <xsl:apply-templates select="m:math" mode="math"/>
	</xsl:otherwise>
      </xsl:choose>
    </xsl:template>

</xsl:stylesheet>
