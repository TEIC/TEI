<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet
    version="2.0"
    xmlns:xs="http://www.w3.org/2001/XMLSchema"
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    xmlns:teidocx="http://www.tei-c.org/ns/teidocx/1.0"
    xmlns:chart="urn:oasis:names:tc:opendocument:xmlns:chart:1.0"
    xmlns:dc="http://purl.org/dc/elements/1.1/"
    xmlns:dom="http://www.w3.org/2001/xml-events"
    xmlns:dr3d="urn:oasis:names:tc:opendocument:xmlns:dr3d:1.0"
    xmlns:draw="urn:oasis:names:tc:opendocument:xmlns:drawing:1.0"
    xmlns:fo="urn:oasis:names:tc:opendocument:xmlns:xsl-fo-compatible:1.0"
    xmlns:form="urn:oasis:names:tc:opendocument:xmlns:form:1.0"
    xmlns:math="http://www.w3.org/1998/Math/MathML"
    xmlns:meta="urn:oasis:names:tc:opendocument:xmlns:meta:1.0"
    xmlns:number="urn:oasis:names:tc:opendocument:xmlns:datastyle:1.0"
    xmlns:office="urn:oasis:names:tc:opendocument:xmlns:office:1.0"
    xmlns:m="http://www.w3.org/1998/Math/MathML"
    xmlns:atom="http://www.w3.org/2005/Atom"  
    xmlns:estr="http://exslt.org/strings"
    xmlns:xlink="http://www.w3.org/1999/xlink"
    xmlns:xhtml="http://www.w3.org/1999/xhtml"
    xmlns:dbk="http://docbook.org/ns/docbook"
    xmlns:rng="http://relaxng.org/ns/structure/1.0"
    xmlns:ooo="http://openoffice.org/2004/office"
    xmlns:oooc="http://openoffice.org/2004/calc"
    xmlns:ooow="http://openoffice.org/2004/writer"
    xmlns:script="urn:oasis:names:tc:opendocument:xmlns:script:1.0"
    xmlns:style="urn:oasis:names:tc:opendocument:xmlns:style:1.0"
    xmlns:svg="urn:oasis:names:tc:opendocument:xmlns:svg-compatible:1.0"
    xmlns:table="urn:oasis:names:tc:opendocument:xmlns:table:1.0"
    xmlns:text="urn:oasis:names:tc:opendocument:xmlns:text:1.0"
    xmlns:xforms="http://www.w3.org/2002/xforms"
    xmlns:xsd="http://www.w3.org/2001/XMLSchema"
    xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
    xmlns:tei="http://www.tei-c.org/ns/1.0"
    xmlns:teix="http://www.tei-c.org/ns/Examples"
    office:version="1.0"
    >

  <xsl:import href="../common2/core.xsl"/>
  <xsl:param name="useFixedDate">false</xsl:param>

    <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" scope="stylesheet" type="stylesheet">
      <desc>
         <p> TEI stylesheet for making OpenOffice files from TEI XML.
	 Originally derived from the OpenOffice /Docbook
	 conversion, but largely rewritten </p>
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
  
  <xsl:strip-space elements="teix:* rng:* xsl:* xhtml:* atom:* m:*"/>
    <xsl:key name="W" match="image" use="@url"/>
    <xsl:key name="H" match="image" use="@url"/>


  <xsl:output method="xml" omit-xml-declaration="no"/>

  <xsl:decimal-format name="staff" digit="D"/>
  <xsl:variable name="doc_type">TEI</xsl:variable>
  <xsl:param name="useHeaderFrontMatter">false</xsl:param>
  <xsl:param name="postQuote">’</xsl:param>
  <xsl:param name="preQuote">‘</xsl:param>
  <xsl:param name="outputDir">.</xsl:param>
  <xsl:param name="freestanding">false</xsl:param>
  <xsl:key name='IDS' match="tei:*[@xml:id]" use="@xml:id"/>
  <xsl:key name='GRAPHICS' match="tei:graphic" use="1"/>
  <xsl:key name="Page" match="style:page-layout-properties" use="1"/>

  <xsl:template match="/">
    <xsl:choose>
      <xsl:when test="$freestanding='true'">
	<xsl:result-document href="{concat($outputDir,'/meta.xml')}">
	  <xsl:call-template name="META"/>
	</xsl:result-document>
	<office:document-content>
	  <xsl:if test="$freestanding='true'">
	  <xsl:for-each select="document(concat($outputDir,'/content.xml'))/office:document-content">
	    <xsl:copy-of select="office:scripts"/>
	    <xsl:copy-of select="office:font-face-decls"/>
	    <xsl:copy-of select="office:automatic-styles"/>
	  </xsl:for-each>
	  </xsl:if>
	  <office:body>
	    <office:text>
	      <xsl:apply-templates/>
	    </office:text>
	  </office:body>
	</office:document-content>
      </xsl:when>
      <xsl:otherwise>
	<office:document>
	  <xsl:call-template name="META"/>
	  <office:body>
	    <office:text>
	      <xsl:apply-templates/>
	    </office:text>
	  </office:body>
	</office:document>
      </xsl:otherwise>
    </xsl:choose>
    <xsl:result-document href="{concat($outputDir,'/META-INF/manifest.xml')}">
      <manifest:manifest xmlns:manifest="urn:oasis:names:tc:opendocument:xmlns:manifest:1.0">
	<manifest:file-entry manifest:media-type="application/vnd.oasis.opendocument.text" manifest:version="1.2" manifest:full-path="/"/>
	<manifest:file-entry manifest:media-type="" manifest:full-path="Configurations2/statusbar/"/>
	<manifest:file-entry manifest:media-type="" manifest:full-path="Configurations2/accelerator/current.xml"/>
	<manifest:file-entry manifest:media-type="" manifest:full-path="Configurations2/accelerator/"/>
	<manifest:file-entry manifest:media-type="" manifest:full-path="Configurations2/floater/"/>
	<manifest:file-entry manifest:media-type="" manifest:full-path="Configurations2/popupmenu/"/>
	<manifest:file-entry manifest:media-type="" manifest:full-path="Configurations2/progressbar/"/>
	<manifest:file-entry manifest:media-type="" manifest:full-path="Configurations2/menubar/"/>
	<manifest:file-entry manifest:media-type="" manifest:full-path="Configurations2/toolbar/"/>
	<manifest:file-entry manifest:media-type="" manifest:full-path="Configurations2/images/Bitmaps/"/>
	<manifest:file-entry manifest:media-type="" manifest:full-path="Configurations2/images/"/>
	<manifest:file-entry manifest:media-type="application/vnd.sun.xml.ui.configuration" manifest:full-path="Configurations2/"/>
	<manifest:file-entry manifest:media-type="text/xml" manifest:full-path="content.xml"/>
	<manifest:file-entry manifest:media-type="application/rdf+xml" manifest:full-path="manifest.rdf"/>
	<manifest:file-entry manifest:media-type="text/xml" manifest:full-path="styles.xml"/>
	<manifest:file-entry manifest:media-type="text/xml" manifest:full-path="meta.xml"/>
	<manifest:file-entry manifest:media-type="" manifest:full-path="Thumbnails/thumbnail.png"/>
	<manifest:file-entry manifest:media-type="" manifest:full-path="Thumbnails/"/>
	<manifest:file-entry manifest:media-type="text/xml"
			     manifest:full-path="settings.xml"/>
	<xsl:if test="count(key('GRAPHICS',1))&gt;0">
	<manifest:file-entry manifest:media-type="" manifest:full-path="Pictures/"/>
	<xsl:for-each select="key('GRAPHICS',1)">
	  <xsl:variable name="imagetype"
			select="tokenize(@url,'\.')[last()]"/>
	  <manifest:file-entry>
	    <xsl:attribute name="manifest:full-path">
	      <xsl:text>Pictures/resource</xsl:text>
	      <xsl:number level="any"/>
	      <xsl:text>.</xsl:text>
	      <xsl:value-of select="$imagetype"/>
	    </xsl:attribute>
	    <xsl:attribute name="manifest:media-type">
	      <xsl:text>image/</xsl:text>
	      <xsl:choose>
		<xsl:when test="$imagetype='png'">png</xsl:when>
		<xsl:when test="$imagetype='gif'">gif</xsl:when>
		<xsl:when test="$imagetype='jpg'">jpeg</xsl:when>
		<xsl:when test="$imagetype='jpeg'">jpg</xsl:when>
		<xsl:when test="$imagetype='tiff'">tiff</xsl:when>
		<xsl:when test="$imagetype='tif'">tiff</xsl:when>
		<xsl:otherwise>jpeg</xsl:otherwise>
	      </xsl:choose>
	    </xsl:attribute>
	  </manifest:file-entry>
	</xsl:for-each>
	</xsl:if>
      </manifest:manifest>
    </xsl:result-document>

  </xsl:template>

  <xsl:template name="META">
    <xsl:for-each select="*">
    <office:meta>
      <meta:generator>TEI to OpenOffice XSLT</meta:generator>
      <dc:title>
	<xsl:call-template name="generateTitle"/>
      </dc:title>
      <dc:description/>
      <dc:subject/>
      <meta:creation-date>
	<xsl:call-template name="generateDate"/>
      </meta:creation-date>
      <dc:date>
	<xsl:call-template name="generateRevDate"/>
      </dc:date>
      <dc:language>
	<xsl:choose>
	  <xsl:when test="/tei:TEI/@xml:lang">
	    <xsl:value-of select="/tei:TEI/@xml:lang"/>
	  </xsl:when>
	  <xsl:otherwise>en</xsl:otherwise>
	</xsl:choose>
      </dc:language>
      <meta:editing-cycles>1</meta:editing-cycles>
      <meta:editing-duration>PT00H00M00S</meta:editing-duration>
      <meta:user-defined meta:name="Info 1"/>
      <meta:user-defined meta:name="Info 2"/>
      <meta:user-defined meta:name="Info 3"/>
      <meta:user-defined meta:name="Info 4"/>
      <meta:document-statistic meta:table-count="1" meta:image-count="0" meta:object-count="0" meta:page-count="1" meta:paragraph-count="42" meta:word-count="144" meta:character-count="820"/>
    </office:meta>
    </xsl:for-each>
  </xsl:template>

  <!-- base structure -->
  <xsl:template match="tei:TEI">
    <xsl:apply-templates select="tei:text"/>
  </xsl:template>

  <xsl:template match="tei:body|tei:front|tei:back">
	<xsl:apply-templates/>
  </xsl:template>


  <xsl:template match="tei:head">
    <xsl:choose>
      <xsl:when test="parent::tei:figure"/>
      <xsl:when test="parent::tei:list"/>
      <xsl:when test="parent::tei:div"/>
      <xsl:when test="parent::tei:div1"/>
      <xsl:when test="parent::tei:div2"/>
      <xsl:when test="parent::tei:div3"/>
      <xsl:when test="parent::tei:div4"/>
      <xsl:when test="parent::tei:div5"/>
      <xsl:when test="parent::tei:div6"/>
      <xsl:when test="parent::tei:table"/>

      <xsl:otherwise>
	<xsl:choose>
	  <xsl:when test="parent::tei:appendix">
	    <text:p text:style-name="Appendix">
	      <xsl:apply-templates/>	   
	    </text:p>	
	  </xsl:when>
	  <xsl:when test="parent::tei:body">
	    <text:h text:outline-level="1">
	      <xsl:apply-templates/>
	    </text:h>
	  </xsl:when>
	  <xsl:otherwise>
	    <text:p>
	      <xsl:apply-templates/>	   
	    </text:p>
	  </xsl:otherwise>
	</xsl:choose>

      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>



  <xsl:template match="tei:div">
    <xsl:variable name="depth">
      <xsl:value-of select="count(ancestor::tei:div)"/>
    </xsl:variable>
    <text:h>
      <xsl:attribute name="text:outline-level">
	<xsl:value-of select="$depth + 1"/>
      </xsl:attribute>
      <xsl:call-template name="test.id"/>
      <xsl:apply-templates select="tei:head" mode="show"/>
    </text:h>
    <xsl:apply-templates/>
  </xsl:template>

  <xsl:template match="tei:div1|tei:div2|tei:div3|tei:div4|tei:div5|tei:div6">
    <xsl:variable name="depth">
      <xsl:value-of select="substring-after(name(.),'div')"/>
    </xsl:variable>
    <text:h>
      <xsl:attribute name="text:outline-level">
	<xsl:value-of select="$depth + 1"/>
      </xsl:attribute>
      <xsl:call-template name="test.id"/>
      <xsl:apply-templates select="tei:head" mode="show"/>
    </text:h>
    <xsl:apply-templates/>
  </xsl:template>



<!-- paragraphs -->
  <xsl:template match="tei:pb">
    <text:soft-page-break/>
  </xsl:template>

  <xsl:template match="tei:p">
    <xsl:variable name="style">
      <xsl:choose>
	<xsl:when test="ancestor::tei:note[@place='foot']">
	    <xsl:text>Footnote</xsl:text>
	</xsl:when>
	<xsl:otherwise>
	  <xsl:text>Text_20_body</xsl:text>
	</xsl:otherwise>
      </xsl:choose>
    </xsl:variable>
    <xsl:choose>
<!--      
     <xsl:when test="tei:pb">
	<xsl:for-each-group select="node()" group-adjacent="boolean(self::tei:pb)">
	  <xsl:choose>
	    <xsl:when test="current-grouping-key()">
		<text:p text:style-name="P1"/>
	    </xsl:when>
	    <xsl:otherwise>
	    <text:p text:style-name="{$style}">
	      <xsl:call-template name="test.id"/>
	      <xsl:apply-templates select="current-group()"/>
	    </text:p>
	  </xsl:otherwise>  
        </xsl:choose>
	</xsl:for-each-group>
      </xsl:when>
-->
      <xsl:when test="tei:list">
	<xsl:for-each-group select="node()" group-adjacent="boolean(self::tei:list)">
	  <xsl:choose>
	    <xsl:when test="current-grouping-key()">
	      <xsl:apply-templates select="current-group()"/>
	    </xsl:when>
	    <xsl:otherwise>
	    <text:p text:style-name="{$style}">
	      <xsl:call-template name="test.id"/>
	      <xsl:apply-templates select="current-group()"/>
	    </text:p>
	  </xsl:otherwise>  
        </xsl:choose>
	</xsl:for-each-group>
      </xsl:when>
      <xsl:otherwise>
	<text:p text:style-name="{$style}">
	  <xsl:call-template name="test.id"/>
	  <xsl:apply-templates/>
	</text:p>
      </xsl:otherwise>  
    </xsl:choose>
  </xsl:template>



<!-- figures -->
  <xsl:template match="tei:figure">
    <xsl:call-template name="startHook"/>
    <text:p text:style-name="Standard">
      <xsl:call-template name="test.id"/>
      <xsl:apply-templates select="tei:graphic"/>
    </text:p>
    <xsl:if test="tei:head">
      <text:p text:style-name="Caption">
	<text:span text:style-name="Figurenum">
	  <xsl:text>Figure </xsl:text>
	  <text:sequence 
	      text:ref-name="refFigure0" 
	      text:name="Figure"
	      text:formula="Figure+1"
	      style:num-format="1">
	  <xsl:number level="any"/>
	  <xsl:text>.</xsl:text>
	  </text:sequence>
	</text:span>
	<xsl:text> </xsl:text>
	<xsl:apply-templates select="tei:head" mode="show"/>
      </text:p>
    </xsl:if>
    <xsl:call-template name="endHook"/>

  </xsl:template>

  <xsl:template match="tei:graphic">
    <xsl:variable name="id">
      <xsl:choose>
	<xsl:when test="@xml:id">
	  <xsl:value-of select="@xml:id"/>
	</xsl:when>
	<xsl:otherwise>
	  <xsl:text>Figure</xsl:text><xsl:number level="any"/>
	</xsl:otherwise>
      </xsl:choose>
    </xsl:variable>

	<xsl:variable name="filename">
	  <xsl:text>Pictures/resource</xsl:text>
	  <xsl:number level="any"/>
	  <xsl:text>.</xsl:text>
	  <xsl:value-of select="tokenize(@url,'\.')[last()]"/>
	</xsl:variable>
	<xsl:variable name="origheight">
	  <xsl:choose>
	    <xsl:when test="@teidocx:height">
	      <xsl:value-of select="@teidocx:height"/>
	    </xsl:when>
	    <xsl:when test="doc-available(concat($outputDir,'/image-size-info.xml'))">
		<xsl:for-each select="document(concat($outputDir,'/image-size-info.xml'))">
		  <xsl:value-of select="(number(key('H',$filename)/height) div 72) * 9144"/>
		</xsl:for-each>
	    </xsl:when>
	    <xsl:otherwise>0</xsl:otherwise>
	  </xsl:choose>
	</xsl:variable>

	<xsl:variable name="origwidth">
	  <xsl:choose>
	    <xsl:when test="@teidocx:width">
	      <xsl:value-of select="@teidocx:width"/>
	    </xsl:when>
	    <xsl:when test="doc-available(concat($outputDir,'/image-size-info.xml'))">
		<xsl:for-each select="document(concat($outputDir,'/image-size-info.xml'))">
		  <xsl:value-of select="(number(key('W',$filename)/width) div 72) * 9144"/>
		</xsl:for-each>
	    </xsl:when>
	    <xsl:otherwise>0</xsl:otherwise>
	  </xsl:choose>
	</xsl:variable>

    <xsl:choose>
      <xsl:when test="$filename and  ( ($origwidth and $origheight) or (@width and @height))">
	
	<!-- work out page width / height and subtract 1inch on all sides -->
	<xsl:variable name="pageWidth">
	  <xsl:for-each
	      select="document(concat($outputDir,'/styles.xml'))/office:document-styles/office:automatic-styles/style:page-layout/style:page-layout-properties">
	    <xsl:value-of
		select="number(tei:convert-dim-pt(@fo:page-width) - 144)"/>
	  </xsl:for-each>
	</xsl:variable>
	<xsl:variable name="pageHeight">
	  <xsl:for-each
	      select="document(concat($outputDir,'/styles.xml'))/office:document-styles/office:automatic-styles/style:page-layout/style:page-layout-properties">
	    <xsl:value-of 
		select="number(tei:convert-dim-pt(@fo:page-height) - 144)"/>
	  </xsl:for-each>
	</xsl:variable>
	
	<xsl:variable name="Width">
	  <xsl:choose>
	    <xsl:when test="contains(@width,'%')">
	      <xsl:value-of select="(($pageWidth div 100) * number(substring-before(@width,'%'))) cast as xs:integer"/>
	    </xsl:when>
	    <xsl:when test="@width">
	      <xsl:value-of select="tei:convert-dim-pt(@width)"/>
	    </xsl:when>
	    <xsl:when test="@scale and $origwidth">
	      <xsl:value-of select="number($origwidth * number(@scale)) div 127 cast as xs:integer"/>
	    </xsl:when>
	    <xsl:when test="@height[not(contains(.,'%'))] and $origheight">
	      <xsl:variable name="h">
		<xsl:value-of select="number(tei:convert-dim-pt(@height))"/>
	      </xsl:variable>
	      <xsl:value-of select="($h * number($origwidth)) div number($origheight)"/>
	    </xsl:when>
	    <xsl:when test="$origwidth">
	      <xsl:value-of select="number($origwidth) div 127"/>
	    </xsl:when>
	    <xsl:otherwise>
	      <xsl:message terminate="yes">no way to work out image width for
	      <xsl:value-of select="$filename"/>
	      </xsl:message>
	    </xsl:otherwise>
	  </xsl:choose>
	</xsl:variable>
	
	<xsl:variable name="Height">
	  <xsl:choose>
	    <xsl:when test="contains(@height,'%')">
	      <xsl:value-of select="(($pageHeight div 100) * (number(substring-before(@height,'%')))) cast as xs:integer"/>
	    </xsl:when>
	    <xsl:when test="@height">
	      <xsl:value-of select="tei:convert-dim-pt(@height)"/>
	    </xsl:when>
	    <xsl:when test="@scale and $origheight">
	      <xsl:value-of select="($origheight *
				    number(@scale)) div 127 cast as xs:integer"/>
	    </xsl:when>
	    <xsl:when test="@width[not(contains(.,'%'))] and $origheight and $origwidth">
	      <xsl:variable name="w">
		<xsl:value-of select="number(tei:convert-dim-pt(@width))"/>
	      </xsl:variable>
	      <xsl:value-of select="($w * number($origheight)) div number($origwidth)"/>
	    </xsl:when>
	    <xsl:when test="$origheight">
	      <xsl:value-of select="number($origheight) div 127"/>
	    </xsl:when>
	    <xsl:otherwise>
	      <xsl:message terminate="yes">no way to work out image height for
	      <xsl:value-of select="$filename"/>
	      </xsl:message>
	    </xsl:otherwise>
	  </xsl:choose>
	</xsl:variable>
	
	<!-- check for sense -->
	<xsl:variable name="imageHeight">
	  <xsl:choose>
	    <xsl:when test="$Height &lt; 0">
	      <xsl:value-of select="$pageHeight"/>
	    </xsl:when>
	    <xsl:when test="number($Height) &gt; $pageHeight">
	      <xsl:value-of select="$pageHeight"/>
	    </xsl:when>
	    <xsl:otherwise>
	      <xsl:value-of select="$Height"/>
	    </xsl:otherwise>
	  </xsl:choose>
	</xsl:variable>
	<xsl:variable name="imageWidth">
	  <xsl:choose>
	    <xsl:when test="$Width &lt; 0">
	      <xsl:value-of select="$pageWidth"/>
	    </xsl:when>
	    <xsl:when test="number($Width) &gt; $pageWidth">
	      <xsl:value-of select="$pageWidth"/>
	    </xsl:when>
	    <xsl:otherwise>
	      <xsl:value-of select="$Width"/>
	    </xsl:otherwise>
	  </xsl:choose>
	</xsl:variable>
<!--
<xsl:message>
  <xsl:for-each select="@*">
    - @<xsl:value-of select="name(.)"/>: <xsl:value-of select="."/>
</xsl:for-each>
    - pageWidth: <xsl:value-of select="$pageWidth"/>
    - pageHeight: <xsl:value-of select="$pageHeight"/>
    - Width: <xsl:value-of select="$Width"/>
    - Height: <xsl:value-of select="$Height"/>
    - imageWidth: <xsl:value-of select="$imageWidth"/>
    - imageHeight: <xsl:value-of select="$imageHeight"/>
</xsl:message>
-->
	<draw:frame draw:style-name="fr1" 
		    draw:name="{$id}"
		    draw:z-index="0">
	  <xsl:attribute name="text:anchor-type">
	    <xsl:choose>
	      <xsl:when test="parent::tei:figure">
		<xsl:text>paragraph</xsl:text>
	      </xsl:when>
	      <xsl:otherwise>
		<xsl:text>as-char</xsl:text>
	      </xsl:otherwise>
	    </xsl:choose>
	  </xsl:attribute>
	  
	  <xsl:attribute name="svg:width">
	    <xsl:value-of select="$imageWidth"/>
	    <xsl:text>pt</xsl:text>
	  </xsl:attribute>
	  <xsl:attribute name="svg:height">
	    <xsl:value-of select="$imageHeight"/>
	    <xsl:text>pt</xsl:text>
	  </xsl:attribute>
	  <draw:image
	      xlink:href="{$filename}" 
	      xlink:type="simple" 
	      xlink:show="embed"
	      xlink:actuate="onLoad" 
	      draw:filter-name="&lt;All formats&gt;"/>
	</draw:frame>
      </xsl:when>	
      <xsl:otherwise>
	<xsl:message terminate="yes">ERROR. no image size info for  <xsl:value-of select="$filename"/>, cannot proceed</xsl:message>
	
      </xsl:otherwise>
    </xsl:choose>
    
  </xsl:template>


<!-- lists -->
  <xsl:template match="tei:list|tei:listBibl">
    <xsl:if test="tei:head">
      <text:p>
	<xsl:attribute name="text:style-name">
	  <xsl:choose>
	    <xsl:when  test="@type='ordered'">P2</xsl:when>
	    <xsl:otherwise>P1</xsl:otherwise>
	  </xsl:choose>
      </xsl:attribute>
      <xsl:apply-templates select="tei:head" mode="show"/>
      </text:p>
    </xsl:if>
    <text:list>
      <xsl:attribute name="text:style-name">
	<xsl:choose>
	    <xsl:when test="self::tei:listBibl">L2</xsl:when>
            <xsl:when test="not(@type)">L1</xsl:when>
            <xsl:when test="@type='ordered'">L2</xsl:when>
            <xsl:when test="@type='unordered'">L1</xsl:when>
	</xsl:choose>
      </xsl:attribute>
      <xsl:apply-templates/>
    </text:list>
  </xsl:template>



  <xsl:template match="tei:list[@type='gloss' or @rend='valList']" priority="10">
    <xsl:apply-templates/>
  </xsl:template>

  <xsl:template match="tei:list[@type='gloss' or @rend='valList']/tei:item">
    <text:p text:style-name="List_20_Contents">
      <xsl:apply-templates/>
    </text:p>
  </xsl:template>

  <xsl:template match="tei:list[@type='gloss' or @rend='valList']/tei:label">
    <text:p text:style-name="List_20_Heading">
      <xsl:apply-templates/>
    </text:p>
  </xsl:template>

  <xsl:template match="tei:item/tei:p">
    <xsl:apply-templates/>
  </xsl:template>

  <xsl:template match="tei:item">
    <text:list-item>
      <xsl:choose>
	<xsl:when test="tei:list">
	  <xsl:apply-templates/>
	</xsl:when>
	<xsl:otherwise>
	  <text:p>
	    <xsl:attribute name="text:style-name">
	      <xsl:choose>
		<xsl:when  test="parent::tei:list/@type='ordered'">P2</xsl:when>
		<xsl:otherwise>P1</xsl:otherwise>
	      </xsl:choose>
	    </xsl:attribute>
	    <xsl:apply-templates/>
	  </text:p>
	</xsl:otherwise>
      </xsl:choose>
    </text:list-item>
  </xsl:template>

<!-- inline stuff -->
  <xsl:template match="tei:emph">
    <text:span text:style-name="Emphasis">
      <xsl:apply-templates/>
    </text:span>
  </xsl:template>

  <xsl:template  match="tei:gi">
    <xsl:text>&lt;</xsl:text>
    <xsl:apply-templates/>
    <xsl:text>&gt;</xsl:text>
  </xsl:template>

  <xsl:template  match="tei:caesura">
    <xsl:text>&#160;&#160;&#160;</xsl:text>
  </xsl:template>

  <xsl:template match="tei:q">
    <text:span text:style-name="q">
      <xsl:text>&#x2018;</xsl:text>
      <xsl:apply-templates/>
      <xsl:text>&#x2019;</xsl:text>
    </text:span>
  </xsl:template>


  <xsl:template match="tei:note">
    <text:note text:note-class="footnote">
    <text:note-citation>
      <xsl:number level="any" count="tei:note[@place='foot']"/>
    </text:note-citation>
      <text:note-body>
	<text:p text:style-name="Footnote">
	  <xsl:apply-templates/>
	</text:p>
      </text:note-body>
    </text:note>  
  </xsl:template>

  <xsl:template match="tei:note[@place='end']">
    <text:note text:note-class="endnote">
    <text:note-citation>
      <xsl:number format="i" level="any" count="tei:note[@place='end']"/>
    </text:note-citation>
      <text:note-body>
	<text:p text:style-name="Endnote">
	  <xsl:apply-templates/>
	</text:p>
      </text:note-body>
    </text:note>
  </xsl:template>


  <xsl:template match="tei:ref">
    <text:a xlink:type="simple" xlink:href="{@target}">
      <xsl:apply-templates/>
    </text:a>
  </xsl:template>

  <xsl:template match="tei:ptr">
    <text:a xlink:type="simple" xlink:href="{@target}">
      <xsl:choose>
	<xsl:when test="starts-with(@target,'#')">
	  <xsl:for-each select="id(substring(@target,2))">
	    <xsl:apply-templates select="." mode="crossref"/>
	  </xsl:for-each>
	</xsl:when>
	<xsl:otherwise>
	  <xsl:value-of select="@target"/>
	</xsl:otherwise>
      </xsl:choose>
    </text:a>
  </xsl:template>

  <xsl:template match="tei:table|tei:figure|tei:item" mode="crossref">
    <xsl:number level="any"/>
  </xsl:template>

  <xsl:template match="tei:div" mode="crossref">
    <xsl:number format="1.1.1.1.1"
		level="multiple" 
		count="tei:div"/>
  </xsl:template>

  <xsl:template
      match="tei:div1|tei:div2|tei:div3|tei:div4|tei:div5|tei:div6" 
      mode="crossref">
    <xsl:number format="1.1.1.1.1"
        count="tei:div1|tei:div2|tei:div3|tei:div4|tei:div5|tei:div6"
        level="multiple"/>
  </xsl:template>

  <xsl:template name="test.id">
    <xsl:if test="@xml:id">
      <text:bookmark text:name="{@xml:id}"/>
    </xsl:if>
  </xsl:template>

  <xsl:template match="tei:unclear">
    <text:span text:style-name="Highlight">
      <xsl:apply-templates/>
    </text:span>
  </xsl:template>

  <xsl:template match="tei:hi">
    <text:span>
      <xsl:attribute name="text:style-name">
	<xsl:choose>
	  <xsl:when test="@rend='normalweight'">
	    <xsl:text>Standard</xsl:text>
	  </xsl:when>
	  <xsl:when test="@rend='code' or @rend='typewriter'">
	    <xsl:text>Source_20_Text</xsl:text>
	  </xsl:when>
	  <xsl:when test="@rend='sup' or @rend='superscript'">
	    <xsl:text>Superscript</xsl:text>
	  </xsl:when>
	  <xsl:when test="@rend='sub' or @rend='subscript'">
	    <xsl:text>Subscript</xsl:text>
	  </xsl:when>
	  <xsl:when test="@rend='bold'">
	    <xsl:text>Highlight</xsl:text>
	  </xsl:when>
	  <xsl:when test="@rend='label'">
	    <xsl:text>Highlight</xsl:text>
	  </xsl:when>
	  <xsl:when test="@rend='it' or @rend='i' or @rend='italic'">
	    <xsl:text>Emphasis</xsl:text>
	  </xsl:when>
	  <xsl:when test="@rend='underline'">
	    <xsl:text>Underline</xsl:text>
	  </xsl:when>
	  <xsl:when test="@rend='sc' or @rend='smallcaps'">
	    <xsl:text>SmallCaps</xsl:text>
	  </xsl:when>
	  <xsl:when test="@rend='sc' or @rend='capsall'">
	    <xsl:text>AllCaps</xsl:text>
	  </xsl:when>
	  <xsl:when test="@rend='strikethrough'">
	    <xsl:text>StrikeThrough</xsl:text>
	  </xsl:when>
	  <xsl:when test="@rend='strikedoublethrough'">
	    <xsl:text>StrikeDoubleThrough</xsl:text>
	  </xsl:when>
	  <xsl:when test="@rend='underline'">
	    <xsl:text>UnderLine</xsl:text>
	  </xsl:when>
	  <xsl:when test="@rend='underdoubleline'">
	    <xsl:text>UnderDoubleLine</xsl:text>
	  </xsl:when>
	  <xsl:otherwise>
	    <xsl:text>Emphasis</xsl:text>
	  </xsl:otherwise>
	</xsl:choose>
      </xsl:attribute>
      <xsl:apply-templates/>
    </text:span>
  </xsl:template>

  <xsl:template match="tei:term">
    <text:span>
      <xsl:attribute name="text:style-name">
	<xsl:text>Highlight</xsl:text>
      </xsl:attribute>
      <xsl:apply-templates/>
    </text:span>
</xsl:template>

  <xsl:template match="tei:index"/>

  <xsl:template match="tei:eg">
    <xsl:call-template name="startHook"/>
    <xsl:call-template name="Literal">
      <xsl:with-param name="Text">
	<xsl:value-of select="."/>
      </xsl:with-param>
    </xsl:call-template>
    <xsl:call-template name="endHook"/>
  </xsl:template>

  <xsl:template match="teix:egXML">
    <xsl:call-template name="startHook"/>
    <xsl:call-template name="Literal">
      <xsl:with-param name="Text">
	<xsl:apply-templates mode="verbatim"/>
      </xsl:with-param>
    </xsl:call-template>
    <xsl:call-template name="endHook"/>
  </xsl:template>

  <!-- safest to drop comments entirely, I think -->
  <xsl:template match="comment()"/>

  <xsl:template match="tei:head" mode="show">
    <xsl:apply-templates/>
  </xsl:template>

  <xsl:template match="tei:lb">
    <text:line-break/>
  </xsl:template>

  <xsl:template match="tei:biblStruct">
    <text:list-item>
      <text:p text:style-name="P2">
	<xsl:apply-templates/>
      </text:p>
    </text:list-item>
  </xsl:template>

  <xsl:template match="tei:bibl|tei:signed|tei:docTitle|tei:byline|tei:docImprint">
    <text:p text:style-name="tei_{local-name(.)}">
      <xsl:apply-templates/>
    </text:p>
  </xsl:template>

  <xsl:template match="tei:lg">
    <text:p text:style-name="lg">
      <xsl:apply-templates/>
    </text:p>
  </xsl:template>

  <xsl:template match="tei:l">
    <xsl:choose>
      <xsl:when test="parent::tei:q">
	<xsl:apply-templates/>
	<text:line-break/>
      </xsl:when>
      <xsl:when test="parent::tei:lg">
	<xsl:apply-templates/>
	<text:line-break/>
      </xsl:when>
      <xsl:otherwise>
	<text:p text:style-name="lg">
	  <xsl:apply-templates/>
	</text:p>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>

  <xsl:template name="Literal">
    <xsl:param name="Text"/>
    <xsl:choose>
      <xsl:when test="contains($Text,'&#10;')">
	<text:p text:style-name="Preformatted_20_Text">
	  <xsl:value-of
	      select="translate(substring-before($Text,'&#10;'),' ','&#160;')"/>
	</text:p>
	<xsl:call-template name="Literal">
	  <xsl:with-param name="Text">
	    <xsl:value-of select="substring-after($Text,'&#10;')"/>
	  </xsl:with-param>
	</xsl:call-template>
      </xsl:when>
      <xsl:otherwise>
	<text:p text:style-name="Preformatted_20_Text">
	  <xsl:value-of select="translate($Text,' ','&#160;')"/>
	</text:p>
      </xsl:otherwise>
    </xsl:choose>
    <!-- text:s c="6" to ident 6 spaces -->
  </xsl:template>


  <xsl:template match="tei:sp">
    <xsl:apply-templates/>
  </xsl:template>

  <xsl:template match="tei:stage">
    <xsl:choose>
      <xsl:when test="parent::tei:sp or parent::tei:div">
	<text:p text:style-name="Stage">
	  <xsl:apply-templates/>
	</text:p>
      </xsl:when>
      <xsl:otherwise>
	<text:span text:style-name="Stage">
	  <xsl:apply-templates/>
	</text:span>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>

  <xsl:template match="tei:speaker">
      <text:p text:style-name="Speaker">
	<xsl:apply-templates/>
      </text:p>
  </xsl:template>

  <xsl:template match="tei:ab">
      <text:p>
	<xsl:apply-templates/>
      </text:p>
  </xsl:template>

  <!-- tables-->

  <xsl:template match="tei:table">
    <xsl:call-template name="startHook"/>
    <xsl:variable name="tablenum">
      <xsl:choose>
	<xsl:when test="@xml:id"><xsl:value-of select="@xml:id"/></xsl:when>
	<xsl:otherwise>table<xsl:number level="any"/></xsl:otherwise>
      </xsl:choose>
    </xsl:variable>
    <table:table
	table:name="{$tablenum}"
	table:style-name="Table1">
      <xsl:for-each select="1 to max(for $i in tei:row return count($i/tei:cell))">
	<table:table-column
	    table:style-name="Table{.}.col{.}">
	</table:table-column>
      </xsl:for-each>
      <xsl:apply-templates/>
    </table:table>
    <xsl:if test="tei:head">
      <text:p text:style-name="Caption">
	<xsl:apply-templates select="tei:head" mode="show"/>
      </text:p>
    </xsl:if>
    <xsl:call-template name="endHook"/>
  </xsl:template>


  <xsl:template match="tei:row[@role='label']">
    <table:table-header-rows>
      <table:table-row>
	<xsl:apply-templates/>
      </table:table-row>
    </table:table-header-rows>
  </xsl:template>


  <xsl:template match="tei:row">
    <table:table-row>
      <xsl:apply-templates/>
    </table:table-row>
  </xsl:template>



  <xsl:template match="tei:seg">
    <text:span>
      <xsl:apply-templates/>
    </text:span>
  </xsl:template>

  <xsl:template match="tei:seg[@rend='parent']">
      <xsl:apply-templates/>
  </xsl:template>

  <xsl:template match="tei:ident">
    <text:span>
      <xsl:apply-templates/>
    </text:span>
  </xsl:template>

  <xsl:template match="tei:cit[@rend='display' or not(@rend)]">
    <text:p text:style-name="tei_cit">
      <xsl:apply-templates/>
    </text:p>
  </xsl:template>

  <xsl:template match="tei:mentioned">
    <text:span>
      <xsl:apply-templates/>
    </text:span>
  </xsl:template>

  <xsl:template match="tei:code">
    <text:span text:style-name="User_20_Entry">
      <xsl:apply-templates/>
    </text:span>
  </xsl:template>

  <xsl:template match="tei:cell">
    <table:table-cell>
      <xsl:if test="@cols">
	<xsl:attribute name="table:number-columns-spanned"
		       select="@cols"/>
      </xsl:if>
      <xsl:if test="@rows">
	<xsl:attribute name="table:number-rows-spanned"
		       select="@rows"/>
      </xsl:if>
      <xsl:variable name="cellContents">
	<xsl:apply-templates/>
      </xsl:variable>
      <xsl:for-each-group select="$cellContents/node()"			  
			  group-adjacent="if (self::draw:frame or self::text:span or self::text() or self::text:a)
					  then 1
					  else 2">      
      <xsl:choose>
	<xsl:when test="current-grouping-key()=1">
	    <text:p>
	      <xsl:copy-of select="current-group()"/>
	    </text:p>
	  </xsl:when>
	  <xsl:otherwise>
	    <xsl:copy-of select="current-group()"/>
	  </xsl:otherwise>
	</xsl:choose>
      </xsl:for-each-group>
    </table:table-cell>
    <xsl:if test="@cols">
      <xsl:for-each select="2 to @cols">
	<table:covered-table-cell/>
      </xsl:for-each>
    </xsl:if>
  </xsl:template>

  <xsl:param name="startComment"></xsl:param>
  <xsl:param name="endComment"></xsl:param>
  <xsl:param name="startElement"></xsl:param>
  <xsl:param name="endElement"></xsl:param>
  <xsl:param name="startElementName"></xsl:param>
  <xsl:param name="endElementName"></xsl:param>
  <xsl:param name="startAttribute"></xsl:param>
  <xsl:param name="endAttribute"></xsl:param>
  <xsl:param name="startAttributeValue"></xsl:param>
  <xsl:param name="endAttributeValue"></xsl:param>
  <xsl:param name="startNamespace"></xsl:param>
  <xsl:param name="endNamespace"></xsl:param>

  <xsl:param name="spaceCharacter">&#160;</xsl:param>
  <xsl:param name="showNamespaceDecls">true</xsl:param>

  <xsl:param name="wrapLength">65</xsl:param>

  <xsl:param name="attsOnSameLine">3</xsl:param>
  <xsl:key name="Namespaces" match="*[ancestor::teix:egXML]" use="namespace-uri()"/>

  <xsl:key name="Namespaces" match="*[not(ancestor::*)]" use="namespace-uri()"/>


  <xsl:template name="newLine">&#10;</xsl:template>

  <xsl:template name="lineBreak">
    <xsl:param name="id"/>
    <xsl:text>&#10;</xsl:text>
  </xsl:template>

  <xsl:template match="comment()" mode="verbatim">
    <xsl:choose>
      <xsl:when test="ancestor::Wrapper"/>
      <xsl:when test="ancestor::xhtml:Wrapper"/>
      <xsl:otherwise>
    <xsl:call-template name="lineBreak">
      <xsl:with-param name="id">21</xsl:with-param>
    </xsl:call-template>
    <xsl:value-of  disable-output-escaping="yes" select="$startComment"/>
    <xsl:text>&lt;!--</xsl:text>
    <xsl:value-of select="."/>
    <xsl:text>--&gt;</xsl:text>
    <xsl:value-of  disable-output-escaping="yes"
		   select="$endComment"/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>

  <xsl:template match="text()" mode="verbatim">
    <xsl:choose>
      <xsl:when test="not(preceding-sibling::node() or contains(.,'&#10;'))">
	<xsl:call-template name="Text">
	  <xsl:with-param name="words">
	    <xsl:value-of select="."/>
	  </xsl:with-param>
	</xsl:call-template>
<!--	
        <xsl:if test="substring(.,string-length(.))=' '">
	  <xsl:text> </xsl:text>
	</xsl:if>
-->
      </xsl:when>
      <xsl:when test="normalize-space(.)=''">
        <xsl:for-each select="following-sibling::*[1]">
          <xsl:call-template name="lineBreak">
            <xsl:with-param name="id">7</xsl:with-param>
          </xsl:call-template>
          <xsl:call-template name="makeIndent"/>
        </xsl:for-each>
      </xsl:when>
      <xsl:otherwise>
<!--
	<xsl:if test="starts-with(.,' ')">
	  <xsl:text> </xsl:text>
	</xsl:if>
-->
        <xsl:call-template name="wraptext">
          <xsl:with-param name="count">0</xsl:with-param>
          <xsl:with-param name="indent">
            <xsl:for-each select="parent::*">
              <xsl:call-template name="makeIndent"/>
            </xsl:for-each>
          </xsl:with-param>
          <xsl:with-param name="text">
	    <xsl:choose>
	      <xsl:when test="starts-with(.,'&#10;') and not
			      (preceding-sibling::node())">
		<xsl:call-template name="Text">
		  <xsl:with-param name="words">
		    <xsl:value-of select="substring(.,2)"/>
		  </xsl:with-param>
		</xsl:call-template>
		
	      </xsl:when>
	      <xsl:otherwise>
		<xsl:call-template name="Text">
		  <xsl:with-param name="words">
		    <xsl:value-of select="."/>
		  </xsl:with-param>
		</xsl:call-template>
	      </xsl:otherwise>
	    </xsl:choose>
          </xsl:with-param>
        </xsl:call-template>
	<!--
	<xsl:if test="substring(.,string-length(.))=' '">
	  <xsl:text> </xsl:text>
	</xsl:if>
	-->
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>

  <xsl:template name="wraptext">
    <xsl:param name="indent"/>
    <xsl:param name="text"/>
    <xsl:param name="count">0</xsl:param>
    <xsl:choose>
      <xsl:when test="normalize-space($text)=''"/>
      <xsl:when test="contains($text,'&#10;')">
	<xsl:if test="$count &gt; 0">
	  <xsl:value-of select="$indent"/>
	  <xsl:text> </xsl:text>
	</xsl:if>
	<xsl:call-template name="Text">
	  <xsl:with-param name="words">
	    <xsl:value-of
		select="substring-before($text,'&#10;')"/>
	  </xsl:with-param>
	</xsl:call-template>
<!--	<xsl:if test="not(substring-after($text,'&#10;')='')">-->
	  <xsl:call-template name="lineBreak">
	    <xsl:with-param name="id">6</xsl:with-param>
	  </xsl:call-template>
	  <xsl:value-of select="$indent"/>
	  <xsl:call-template name="wraptext">
	    <xsl:with-param name="indent">
	      <xsl:value-of select="$indent"/>
	    </xsl:with-param>
	    <xsl:with-param name="text">
	      <xsl:value-of select="substring-after($text,'&#10;')"/>
	    </xsl:with-param>
	    <xsl:with-param name="count">
	      <xsl:value-of select="$count + 1"/>
	    </xsl:with-param>
	  </xsl:call-template>

      </xsl:when>
      <xsl:otherwise>
	<xsl:if test="$count &gt; 0 and parent::*">
	  <xsl:value-of select="$indent"/>
	  <xsl:text> </xsl:text>
	</xsl:if>
	<xsl:call-template name="Text">
	  <xsl:with-param name="words">
	    <xsl:value-of select="$text"/>
	  </xsl:with-param>
	</xsl:call-template>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>

  <xsl:template name="Text">
    <xsl:param name="words"/>
    <xsl:choose>
      <xsl:when test="contains($words,'&amp;')">
	<xsl:value-of
	    select="substring-before($words,'&amp;')"/>
	<xsl:text>&amp;amp;</xsl:text>
	<xsl:call-template name="Text">
	  <xsl:with-param name="words">
	    <xsl:value-of select="substring-after($words,'&amp;')"/>
	  </xsl:with-param>
	</xsl:call-template>
      </xsl:when>
      <xsl:otherwise>
	<xsl:value-of select="$words"/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>

  <xsl:template match="*" mode="verbatim">
    <xsl:choose>
      <xsl:when test="parent::xhtml:Wrapper"/>
<!--      <xsl:when test="child::node()[last()]/self::text()[not(.='')] and child::node()[1]/self::text()[not(.='')]"/>-->
      <xsl:when test="not(parent::*)  or parent::teix:egXML">
	<xsl:choose>
	  <xsl:when test="preceding-sibling::node()[1][self::text()]
			  and following-sibling::node()[1][self::text()]"/>
	  <xsl:when test="preceding-sibling::*">
	    <xsl:call-template name="lineBreak">
	      <xsl:with-param name="id">-1</xsl:with-param>
	    </xsl:call-template>
	  </xsl:when>
	  <xsl:otherwise>
	    <xsl:call-template name="newLine"/>
        <!-- <xsl:call-template name="makeIndent"/>-->
	  </xsl:otherwise>
	</xsl:choose>
      </xsl:when>
      <xsl:when test="not(preceding-sibling::node())">
	<xsl:call-template name="lineBreak">
          <xsl:with-param name="id">-2</xsl:with-param>
	</xsl:call-template>
        <xsl:call-template name="makeIndent"/>
      </xsl:when>
      <xsl:when test="preceding-sibling::node()[1]/self::*">
        <xsl:call-template name="lineBreak">
          <xsl:with-param name="id">1</xsl:with-param>
        </xsl:call-template>
        <xsl:call-template name="makeIndent"/>
      </xsl:when>
      <xsl:when test="preceding-sibling::node()[1]/self::text()">
	</xsl:when>
      <xsl:otherwise>
        <xsl:call-template name="lineBreak">
          <xsl:with-param name="id">9</xsl:with-param>
        </xsl:call-template>
        <xsl:call-template name="makeIndent"/>
      </xsl:otherwise>
    </xsl:choose>

    <xsl:value-of disable-output-escaping="yes" select="$startElement"/>
    <xsl:text>&lt;</xsl:text>
    <xsl:call-template name="makeElementName">
      <xsl:with-param name="start">true</xsl:with-param>
    </xsl:call-template>
    <xsl:apply-templates select="@*" mode="verbatim"/>
    <xsl:if test="$showNamespaceDecls='true' or parent::teix:egXML[@rend='full']">
      <xsl:choose>
      <xsl:when test="not(parent::*)">
	<xsl:apply-templates select="." mode="ns"/>
      </xsl:when>
      <xsl:when test="parent::teix:egXML and not(preceding-sibling::*)">
	<xsl:apply-templates select="." mode="ns"/>
      </xsl:when>
      </xsl:choose>
    </xsl:if>

    <xsl:choose>
      <xsl:when test="child::node()">
        <xsl:text>&gt;</xsl:text>
        <xsl:value-of disable-output-escaping="yes" select="$endElement"/>
        <xsl:apply-templates mode="verbatim"/>
        <xsl:choose>
          <xsl:when test="child::node()[last()]/self::text() and child::node()[1]/self::text()"/>

	  <xsl:when test="not(parent::*)  or parent::teix:egXML">
            <xsl:call-template name="lineBreak">
              <xsl:with-param name="id">23</xsl:with-param>
            </xsl:call-template>
	  </xsl:when>
          <xsl:when test="child::node()[last()]/self::text()[normalize-space(.)='']">
            <xsl:call-template name="lineBreak">
              <xsl:with-param name="id">3</xsl:with-param>
            </xsl:call-template>
            <xsl:call-template name="makeIndent"/>
          </xsl:when>
          <xsl:when test="child::node()[last()]/self::comment()">
            <xsl:call-template name="lineBreak">
              <xsl:with-param name="id">4</xsl:with-param>
            </xsl:call-template>
            <xsl:call-template name="makeIndent"/>
          </xsl:when>
          <xsl:when test="child::node()[last()]/self::*">
            <xsl:call-template name="lineBreak">
              <xsl:with-param name="id">5</xsl:with-param>
            </xsl:call-template>
            <xsl:call-template name="makeIndent"/>
          </xsl:when>
        </xsl:choose>
        <xsl:value-of disable-output-escaping="yes" select="$startElement"/>
        <xsl:text>&lt;/</xsl:text>
	<xsl:call-template name="makeElementName">
	  <xsl:with-param name="start">false</xsl:with-param>
	</xsl:call-template>
        <xsl:text>&gt;</xsl:text>
        <xsl:value-of disable-output-escaping="yes" select="$endElement"/>
      </xsl:when>
      <xsl:otherwise>
        <xsl:text>/&gt;</xsl:text>
        <xsl:value-of disable-output-escaping="yes" select="$endElement"/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>


  <xsl:template name="makeElementName">
    <xsl:param name="start"/>
    <xsl:choose>

      <xsl:when
	  test="namespace-uri()='http://docbook.org/ns/docbook'">
	<xsl:value-of disable-output-escaping="yes" select="$startNamespace"/>
	<xsl:text>dbk:</xsl:text>
	<xsl:value-of disable-output-escaping="yes" select="$endNamespace"/>
	<xsl:value-of disable-output-escaping="yes" select="$startElementName"/>
	<xsl:value-of select="local-name(.)"/>
	<xsl:value-of disable-output-escaping="yes" select="$endElementName"/>
      </xsl:when>

      <xsl:when
	  test="namespace-uri()='http://www.w3.org/2001/XMLSchema'">
	<xsl:value-of disable-output-escaping="yes" select="$startNamespace"/>
	<xsl:text>xsd:</xsl:text>
	<xsl:value-of disable-output-escaping="yes"
		      select="$endNamespace"/>
	<xsl:value-of disable-output-escaping="yes" select="$startElementName"/>
	
	<xsl:value-of select="local-name(.)"/>
	<xsl:value-of disable-output-escaping="yes" select="$endElementName"/>
	
      </xsl:when>

      <xsl:when
	  test="namespace-uri()='http://www.ascc.net/xml/schematron'">
	<xsl:value-of disable-output-escaping="yes" select="$startNamespace"/>
	<xsl:text>sch:</xsl:text>
	<xsl:value-of disable-output-escaping="yes"
		      select="$endNamespace"/>
	<xsl:value-of disable-output-escaping="yes" select="$startElementName"/>
	
	<xsl:value-of select="local-name(.)"/>
	<xsl:value-of disable-output-escaping="yes" select="$endElementName"/>
	
      </xsl:when>

      <xsl:when
	  test="namespace-uri()='http://www.w3.org/1998/Math/MathML'">
	<xsl:value-of disable-output-escaping="yes" select="$startNamespace"/>
	<xsl:text>m:</xsl:text>
	<xsl:value-of disable-output-escaping="yes"
		      select="$endNamespace"/>
	<xsl:value-of disable-output-escaping="yes" select="$startElementName"/>
	
	<xsl:value-of select="local-name(.)"/>
	<xsl:value-of disable-output-escaping="yes" select="$endElementName"/>
	
      </xsl:when>

      <xsl:when
	  test="namespace-uri()='http://purl.oclc.org/dsdl/nvdl/ns/structure/1.0'">
	<xsl:value-of disable-output-escaping="yes" select="$startNamespace"/>
	<xsl:text>nvdl:</xsl:text>
	<xsl:value-of disable-output-escaping="yes"
		      select="$endNamespace"/>
	<xsl:value-of disable-output-escaping="yes" select="$startElementName"/>
	
	<xsl:value-of select="local-name(.)"/>
	<xsl:value-of disable-output-escaping="yes" select="$endElementName"/>
	
      </xsl:when>

      <xsl:when
	  test="namespace-uri()='http://relaxng.org/ns/compatibility/annotations/1.0'">
	<xsl:value-of disable-output-escaping="yes" select="$startNamespace"/>
	<xsl:text>a:</xsl:text>
	<xsl:value-of disable-output-escaping="yes"
		      select="$endNamespace"/>
	<xsl:value-of disable-output-escaping="yes" select="$startElementName"/>
	
	<xsl:value-of select="local-name(.)"/>
	<xsl:value-of disable-output-escaping="yes" select="$endElementName"/>
	
      </xsl:when>
      <xsl:when
	  test="namespace-uri()='http://www.w3.org/1999/xhtml'">
	<xsl:value-of disable-output-escaping="yes" select="$startNamespace"/>
	<xsl:text>xhtml:</xsl:text>
	<xsl:value-of disable-output-escaping="yes"
		      select="$endNamespace"/>
	<xsl:value-of disable-output-escaping="yes" select="$startElementName"/>
	
	<xsl:value-of select="local-name(.)"/>
	<xsl:value-of disable-output-escaping="yes" select="$endElementName"/>
	
      </xsl:when>

      <xsl:when
	  test="namespace-uri()='http://www.w3.org/1999/xlink'">
	<xsl:value-of disable-output-escaping="yes" select="$startNamespace"/>
	<xsl:text>xlink:</xsl:text>
	<xsl:value-of disable-output-escaping="yes"
		      select="$endNamespace"/>
	<xsl:value-of disable-output-escaping="yes" select="$startElementName"/>
	
	<xsl:value-of select="local-name(.)"/>
	<xsl:value-of disable-output-escaping="yes" select="$endElementName"/>
	
      </xsl:when>

      <xsl:when
	  test="namespace-uri()='http://relaxng.org/ns/structure/1.0'">
	<xsl:value-of disable-output-escaping="yes" select="$startNamespace"/>
	<xsl:text>rng:</xsl:text>
	<xsl:value-of disable-output-escaping="yes"
		      select="$endNamespace"/>
	<xsl:value-of disable-output-escaping="yes" select="$startElementName"/>
	<xsl:value-of select="local-name(.)"/>
	<xsl:value-of disable-output-escaping="yes" select="$endElementName"/>
	
      </xsl:when>

      <xsl:when
	  test="namespace-uri()='http://earth.google.com/kml/2.1'">
	<xsl:value-of disable-output-escaping="yes" select="$startNamespace"/>
	<xsl:text>kml:</xsl:text>
	<xsl:value-of disable-output-escaping="yes"
		      select="$endNamespace"/>
	<xsl:value-of disable-output-escaping="yes" select="$startElementName"/>
	<xsl:value-of select="local-name(.)"/>
	<xsl:value-of disable-output-escaping="yes" select="$endElementName"/>
	
      </xsl:when>

      <xsl:when
	  test="namespace-uri()='http://www.w3.org/2005/11/its'">
	<xsl:value-of disable-output-escaping="yes" select="$startNamespace"/>
	<xsl:text>its:</xsl:text>
	<xsl:value-of disable-output-escaping="yes"
		      select="$endNamespace"/>
	<xsl:value-of disable-output-escaping="yes" select="$startElementName"/>
	<xsl:value-of select="local-name(.)"/>
	<xsl:value-of disable-output-escaping="yes" select="$endElementName"/>
      </xsl:when>

      <xsl:when test="namespace-uri()='http://www.w3.org/1999/XSL/Transform'">
        <xsl:value-of disable-output-escaping="yes" select="$startNamespace"/>
        <xsl:text>xsl:</xsl:text>
        <xsl:value-of select="local-name(.)"/>
        <xsl:value-of disable-output-escaping="yes" select="$endNamespace"/>
      </xsl:when>

      <xsl:when
	  test="namespace-uri()='http://www.tei-c.org/ns/Examples'">
	<xsl:value-of disable-output-escaping="yes" select="$startElementName"/>
	<xsl:value-of select="local-name(.)"/>
	<xsl:value-of disable-output-escaping="yes" select="$endElementName"/>
      </xsl:when>

      <xsl:when
	  test="namespace-uri()='http://www.w3.org/2005/Atom'">
	<xsl:value-of disable-output-escaping="yes" select="$startNamespace"/>
	<xsl:text>atom:</xsl:text>
	<xsl:value-of disable-output-escaping="yes"
		      select="$endNamespace"/>
	<xsl:value-of disable-output-escaping="yes" select="$startElementName"/>
	<xsl:value-of select="local-name(.)"/>
	    <xsl:value-of disable-output-escaping="yes" select="$endElementName"/>
      </xsl:when>

      <xsl:when
	  test="namespace-uri()='http://purl.org/rss/1.0/modules/event/'">
	<xsl:value-of disable-output-escaping="yes" select="$startNamespace"/>
	<xsl:text>ev:</xsl:text>
	<xsl:value-of disable-output-escaping="yes"
		      select="$endNamespace"/>
	<xsl:value-of disable-output-escaping="yes" select="$startElementName"/>
	<xsl:value-of select="local-name(.)"/>
	<xsl:value-of disable-output-escaping="yes" select="$endElementName"/>
	
      </xsl:when>

      <xsl:when test="not(namespace-uri()='')">
	<xsl:value-of disable-output-escaping="yes" select="$startElementName"/>
	<xsl:value-of select="local-name(.)"/>
	<xsl:value-of disable-output-escaping="yes" select="$endElementName"/>
	<xsl:if test="$start='true'">
	  <xsl:text> xmlns="</xsl:text>
	  <xsl:value-of select="namespace-uri()"/>
	  <xsl:text>"</xsl:text>
	</xsl:if>
      </xsl:when>

      <xsl:otherwise>
	<xsl:value-of disable-output-escaping="yes" select="$startElementName"/>
	<xsl:value-of select="local-name(.)"/>
	<xsl:value-of disable-output-escaping="yes" select="$endElementName"/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>


    <xsl:template name="makeIndent">
      <xsl:variable name="depth"
		    select="count(ancestor::*[not(namespace-uri()='http://www.tei-c.org/ns/1.0')])"/>
      <xsl:call-template name="makeSpace">
	<xsl:with-param name="d">
	  <xsl:value-of select="$depth"/>
	</xsl:with-param>
      </xsl:call-template>
  </xsl:template>

  <xsl:template name="makeSpace">
    <xsl:param name="d"/>
    <xsl:if test="number($d)&gt;1">
      <xsl:value-of select="$spaceCharacter"/>
      <xsl:call-template name="makeSpace">
	<xsl:with-param name="d">
	  <xsl:value-of select="$d -1"/>
	</xsl:with-param>
      </xsl:call-template>
    </xsl:if>
  </xsl:template>

<xsl:template match="@*" mode="verbatim">
  <xsl:variable name="L">
    <xsl:for-each select="../@*">
      <xsl:value-of select="."/>
    </xsl:for-each>
  </xsl:variable>
    <xsl:if test="count(../@*)&gt;$attsOnSameLine or string-length($L)&gt;40 or
		  namespace-uri()='http://www.w3.org/2005/11/its' or
		  string-length(.)+string-length(name(.)) &gt; 40">
    <xsl:call-template name="lineBreak">
      <xsl:with-param name="id">5</xsl:with-param>
    </xsl:call-template>
    <xsl:call-template name="makeIndent"/>
  </xsl:if>
  <xsl:value-of select="$spaceCharacter"/>
  <xsl:value-of disable-output-escaping="yes" select="$startAttribute"/>
  <xsl:choose>
    <xsl:when test="namespace-uri()='http://www.w3.org/2005/11/its'">
      <xsl:text>its:</xsl:text>
    </xsl:when>
    <xsl:when
	test="namespace-uri()='http://www.w3.org/XML/1998/namespace'">
      <xsl:text>xml:</xsl:text>
    </xsl:when>
    <xsl:when test="namespace-uri()='http://www.w3.org/1999/xlink'">
      <xsl:text>xlink:</xsl:text>
    </xsl:when>
    <xsl:when
	test="namespace-uri()='http://www.example.org/ns/nonTEI'">
      <xsl:text>my:</xsl:text>
    </xsl:when>
    <xsl:when
	test="namespace-uri()='http://relaxng.org/ns/compatibility/annotations/1.0'">
      <xsl:text>a:</xsl:text>
    </xsl:when>
<!--    <xsl:otherwise>
    <xsl:for-each select="namespace::*">
      <xsl:if test="not(name(.)='')">
	  <xsl:value-of select="name(.)"/>
	  <xsl:text>:</xsl:text>
      </xsl:if>
    </xsl:for-each>
    </xsl:otherwise>
-->
  </xsl:choose>
  <xsl:value-of select="local-name(.)"/>
  <xsl:value-of disable-output-escaping="yes" select="$endAttribute"/>
  <xsl:text>="</xsl:text>
  <xsl:value-of disable-output-escaping="yes" select="$startAttributeValue"/>
  <xsl:apply-templates select="." mode="attributetext"/>
  <xsl:value-of disable-output-escaping="yes" select="$endAttributeValue"/>
  <xsl:text>"</xsl:text>
</xsl:template>

<xsl:template match="@*" mode="attributetext">
  <xsl:choose>
    <xsl:when test="string-length(.)&gt;50">
      <xsl:choose>
	<xsl:when test="contains(.,'|')">
	  <xsl:call-template name="breakMe">
	    <xsl:with-param name="text">
	      <xsl:value-of select="."/>
	    </xsl:with-param>
	    <xsl:with-param name="sep">
	      <xsl:text>|</xsl:text>
	    </xsl:with-param>
	  </xsl:call-template>
	</xsl:when>
	<xsl:otherwise>
	  <xsl:call-template name="breakMe">
	    <xsl:with-param name="text">
	      <xsl:value-of select="."/>
	    </xsl:with-param>
	    <xsl:with-param name="sep">
	      <xsl:text> </xsl:text>
	    </xsl:with-param>
	  </xsl:call-template>
	</xsl:otherwise>
      </xsl:choose>
    </xsl:when>
    <xsl:otherwise>
      <xsl:value-of select="."/>
    </xsl:otherwise>
  </xsl:choose>
</xsl:template>

<xsl:template name="breakMe">
  <xsl:param name="text"/>
  <xsl:param name="sep"/>
  <xsl:choose>
    <xsl:when test="string-length($text)&lt;50">
      <xsl:value-of select="$text"/>
    </xsl:when>
    <xsl:otherwise>
      <xsl:value-of select="substring-before($text,$sep)"/>
      <xsl:text>&#10;</xsl:text>
      <xsl:value-of select="$sep"/>
      <xsl:call-template name="breakMe">
	<xsl:with-param name="text">
	  <xsl:value-of select="substring-after($text,$sep)"/>
	</xsl:with-param>
	<xsl:with-param name="sep">
	  <xsl:value-of select="$sep"/>
	</xsl:with-param>
      </xsl:call-template>
    </xsl:otherwise>
  </xsl:choose>
</xsl:template>


<xsl:template match="text()|comment()|processing-instruction()" mode="ns"/>

<xsl:template match="*" mode="ns">
  <xsl:param name="list"/>
  <xsl:variable name="used">
    <xsl:for-each select="namespace::*">
      <xsl:variable name="ns" select="."/>
      <xsl:choose>
	<xsl:when test="contains($list,$ns)"/>
	<xsl:when test=".='http://relaxng.org/ns/structure/1.0'"/>
	<xsl:when test=".='http://www.w3.org/2001/XInclude'"/>
	<xsl:when test=".='http://www.tei-c.org/ns/Examples'"/>
	<xsl:when test=".='http://relaxng.org/ns/compatibility/annotations/1.0'"/>
	<xsl:when test="name(.)=''"/>
	<xsl:when test=".='http://www.w3.org/XML/1998/namespace'"/>
	<xsl:otherwise>
	  <xsl:call-template name="lineBreak">
	    <xsl:with-param name="id">22</xsl:with-param>
	  </xsl:call-template>
	  <xsl:text>&#160;&#160;&#160;</xsl:text>
	  <xsl:text>xmlns:</xsl:text>
	  <xsl:value-of select="name(.)"/>
	  <xsl:text>="</xsl:text>
	  <xsl:value-of select="."/>
	  <xsl:text>"</xsl:text>
	</xsl:otherwise>
      </xsl:choose>
    </xsl:for-each>
    </xsl:variable>
  <xsl:copy-of select="$used"/>
  <xsl:apply-templates mode="ns">
    <xsl:with-param name="list">
      <xsl:value-of select="$list"/>
      <xsl:value-of select="$used"/>
    </xsl:with-param>
  </xsl:apply-templates>
</xsl:template>


  <xsl:template name="italicize"/>

  <xsl:template match="tei:soCalled">
    <text:span>
      <xsl:value-of select="$preQuote"/>
      <xsl:apply-templates/>
      <xsl:value-of select="$postQuote"/>
    </text:span>
  </xsl:template>

  <xsl:function name="tei:convert-dim-pt" as="xs:integer">
    <xsl:param name="dim"/>
    <xsl:choose>
      <xsl:when test="ends-with($dim,'cm')">
	<xsl:value-of select="number(number(substring($dim,0,string-length($dim)-1))*28.3464567) cast as xs:integer"/>
      </xsl:when>
      <xsl:when test="ends-with($dim,'in')">
	<xsl:value-of select="number(number(substring($dim,0,string-length($dim)-1))*72) cast as xs:integer"/>
      </xsl:when>
      
      <xsl:when test="ends-with($dim,'mm')">
	<xsl:value-of select="number(number(substring($dim,0,string-length($dim)-1))*2.83464567) cast as xs:integer"/>
      </xsl:when>
      <xsl:when test="ends-with($dim,'pt')">
	<xsl:value-of select="number(substring($dim,0,string-length($dim)-1)) cast as xs:integer"/>
      </xsl:when>
      <xsl:when test="ends-with($dim,'px')">
	<xsl:value-of select="number(number(substring($dim,0,string-length($dim)-1))*0.75) cast as xs:integer"/>
      </xsl:when>
      
      <xsl:otherwise>
	-1
      </xsl:otherwise>
    </xsl:choose>
  </xsl:function>

  <xsl:template name="makeExternalLink">
    <xsl:param name="ptr" as="xs:boolean" select="false()"/>
    <xsl:param name="dest"/>
    <xsl:param name="class">link_<xsl:value-of select="local-name(.)"/></xsl:param>
    <text:a xlink:type="simple" xlink:href="{$dest}">
      <xsl:choose>
	<xsl:when test="$ptr">
	  <xsl:value-of select="$dest"/>
	</xsl:when>
	<xsl:otherwise>
	  <xsl:apply-templates/>
	</xsl:otherwise>
      </xsl:choose>
    </text:a>
  </xsl:template>


  <xsl:template name="startHook">
    <xsl:choose>
      <xsl:when test="self::tei:list and parent::tei:item">
	<xsl:text disable-output-escaping="yes">&lt;/text:p&gt;</xsl:text>
	<xsl:text disable-output-escaping="yes">&lt;/text:list&gt;</xsl:text>
      </xsl:when>
      <xsl:when test="parent::tei:p">
      <xsl:text disable-output-escaping="yes">&lt;/text:p&gt;</xsl:text>
      </xsl:when>
    </xsl:choose>
  </xsl:template>

  <xsl:template name="endHook">
    <xsl:choose>
      <xsl:when test="self::tei:list and parent::tei:item">
	<xsl:text disable-output-escaping="yes">&lt;text:list
	</xsl:text>
	<xsl:choose>
	  <xsl:when test="parent::tei:list[@type='ordered']">
	  </xsl:when>
	</xsl:choose>
	<xsl:text>&gt;</xsl:text>
	<xsl:text disable-output-escaping="yes">&lt;text:p  text:style-name="List Contents&gt;</xsl:text>
      </xsl:when>
      <xsl:when test="parent::tei:p">
      <xsl:text disable-output-escaping="yes">&lt;text:p&gt;</xsl:text>
      </xsl:when>
    </xsl:choose>
  </xsl:template>

  <xsl:template name="generateRevDate">
      <xsl:variable name="when">
         <xsl:choose>
            <xsl:when test="ancestor-or-self::tei:TEI/tei:teiHeader/tei:revisionDesc/descendant::tei:date">
               <xsl:value-of select="ancestor-or-self::tei:TEI/tei:teiHeader/tei:revisionDesc/descendant::tei:date[1]"/>
            </xsl:when>
            <xsl:when
		test="ancestor-or-self::tei:TEI/tei:teiHeader/tei:fileDesc/descendant::tei:date">
	      <xsl:value-of select="ancestor-or-self::tei:TEI/tei:teiHeader/tei:fileDesc/descendant::tei:date"/>
	    </xsl:when>	    
         </xsl:choose>
      </xsl:variable>
      <xsl:choose>
         <xsl:when test="starts-with($when,'$Date')">
        <!-- it's RCS -->
        <xsl:value-of select="substring($when,16,2)"/>
            <xsl:text>/</xsl:text>
            <xsl:value-of select="substring($when,13,2)"/>
            <xsl:text>/</xsl:text>
            <xsl:value-of select="substring($when,8,4)"/>
         </xsl:when>
         <xsl:when test="starts-with($when,'$LastChangedDate')">
        <!-- it's Subversion-->
        <xsl:value-of select="substring-before(substring-after($when,'('),')')"/>
         </xsl:when>
         <xsl:otherwise>
            <xsl:value-of select="$when"/>
         </xsl:otherwise>
      </xsl:choose>
  </xsl:template>
  <xsl:template name="generateDate">
      <xsl:choose>
	 <xsl:when test="$useFixedDate='true'">1970-01-01</xsl:when>
         <xsl:when
	     test="ancestor-or-self::tei:TEI/tei:teiHeader/tei:fileDesc/tei:editionStmt/descendant::tei:date[@when]">
            <xsl:value-of select="ancestor-or-self::tei:TEI/tei:teiHeader/tei:fileDesc/tei:editionStmt/descendant::tei:date[@when][1]/@when"/>
         </xsl:when>
         <xsl:when test="ancestor-or-self::tei:TEI/tei:teiHeader/tei:fileDesc/tei:editionStmt/descendant::tei:date">
            <xsl:value-of select="ancestor-or-self::tei:TEI/tei:teiHeader/tei:fileDesc/tei:editionStmt/descendant::tei:date[1]"/>
         </xsl:when>
         <xsl:when test="ancestor-or-self::tei:TEI/tei:teiHeader/tei:fileDesc/tei:publicationStmt/tei:date">
            <xsl:value-of select="ancestor-or-self::tei:TEI/tei:teiHeader/tei:fileDesc/tei:publicationStmt/tei:date"/>
         </xsl:when>
         <xsl:when test="ancestor-or-self::tei:TEI/tei:teiHeader/tei:fileDesc/tei:editionStmt/tei:edition">
            <xsl:apply-templates select="ancestor-or-self::tei:TEI/tei:teiHeader/tei:fileDesc/tei:editionStmt/tei:edition"/>
         </xsl:when>
	 <xsl:when
	     test="ancestor-or-self::tei:TEI/tei:teiHeader/tei:revisionDesc/tei:change[@when
		   or tei:date]">
            <xsl:for-each
		select="ancestor-or-self::tei:TEI/tei:teiHeader/tei:revisionDesc/tei:change[1]">
	      <xsl:choose>
		<xsl:when test="@when">
		  <xsl:value-of select="@when"/>
		</xsl:when>
		<xsl:when test="tei:date/@when">
		  <xsl:value-of select="tei:date/@when"/>
		</xsl:when>
		<xsl:when test="tei:date">
		  <xsl:value-of select="tei:date"/>
		</xsl:when>
		<xsl:otherwise>
		  <xsl:value-of select="format-dateTime(current-dateTime(),'[Y]-[M02]-[D02]')"/>
		</xsl:otherwise>
	      </xsl:choose>
	    </xsl:for-each>
	 </xsl:when>
	 <xsl:otherwise>
	   <xsl:value-of select="format-dateTime(current-dateTime(),'[Y]-[M02]-[D02]')"/>
	 </xsl:otherwise>
      </xsl:choose>
  </xsl:template>

  <xsl:template name="generateTitle">
    <xsl:for-each
	select="ancestor-or-self::tei:TEI/tei:teiHeader/tei:fileDesc/tei:titleStmt">
	<xsl:apply-templates select="tei:title"/>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="makeSpan">
    <xsl:apply-templates/>
  </xsl:template>

  <xsl:template name="tei:makeText">
    <xsl:param name="letters"/>
    <xsl:value-of select="$letters"/>
  </xsl:template>


    <xsl:template name="emphasize">
      <xsl:param name="class"/>
      <xsl:param name="content"/>
      <text:span>
	<xsl:choose>
	  <xsl:when test="$class='titlem'">
	    <xsl:attribute name="text:style-name">Emphasis</xsl:attribute>
	  </xsl:when>
	  <xsl:when test="$class='titlej'">
	    <xsl:attribute name="text:style-name">Emphasis</xsl:attribute>
	  </xsl:when>
	</xsl:choose>
	<xsl:attribute name="xml:space">preserve</xsl:attribute>
	  <xsl:choose>
	    <xsl:when test="$class='titles'">
	      <xsl:text>, </xsl:text>
	    </xsl:when>
	    <xsl:when test="$class='titleu'">
	      <xsl:text>‘</xsl:text>
	    </xsl:when>
	    <xsl:when test="$class='titlea'">
	      <xsl:text>‘</xsl:text>
	    </xsl:when>
	  </xsl:choose>
	  <xsl:value-of select="$content"/>
	  <xsl:choose>
	    <xsl:when test="$class='titleu'">
	      <xsl:text>’</xsl:text>
	    </xsl:when>
	    <xsl:when test="$class='titlea'">
	      <xsl:text>’</xsl:text>
	    </xsl:when>
	  </xsl:choose>
      </text:span>
    </xsl:template>


  <xsl:template name="processBlock">
    <xsl:param name="style"/>
    <xsl:apply-templates/>
  </xsl:template>
  <xsl:template name="processInline">
    <xsl:param name="before"/>
    <xsl:param name="after"/>
    <xsl:param name="style"/>
    <xsl:value-of select="$before"/>
    <xsl:apply-templates/>
    <xsl:value-of select="$after"/>
  </xsl:template>
 
</xsl:stylesheet>
