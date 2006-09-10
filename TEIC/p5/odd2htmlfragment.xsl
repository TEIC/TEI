<xsl:stylesheet version="1.0"
  xmlns:a="http://relaxng.org/ns/compatibility/annotations/1.0"
  xmlns:html="http://www.w3.org/1999/xhtml"
  xmlns:teix="http://www.tei-c.org/ns/Examples"
  xmlns:rng="http://relaxng.org/ns/structure/1.0"
  xmlns:estr="http://exslt.org/strings"
  xmlns:pantor="http://www.pantor.com/ns/local"
  xmlns:exsl="http://exslt.org/common"
  xmlns:tei="http://www.tei-c.org/ns/1.0"
  xmlns:edate="http://exslt.org/dates-and-times"
  extension-element-prefixes="exsl estr edate"
  exclude-result-prefixes="exsl rng edate estr tei a pantor teix html" 
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform">

<!--
$Date: 2003/04/14 $ $Author: rahtz $
odd2html.xsl

 XSLT script for converting TEI Odds (XML) to HTML

 Sebastian Rahtz, 26th April 2000, from SGMLSpm Perl version by LDB
                  15th May 2001, revised to deal with full TEI
		  October 2003, for P5

     Copyright 2001 Sebastian Rahtz/Oxford University/TEI Consortium  
      <sebastian.rahtz@oucs.ox.ac.uk>

 Permission is hereby granted, free of charge, to any person obtaining
 a copy of this software and any associated documentation files (the
 ``Software''), to deal in the Software without restriction, including
 without limitation the rights to use, copy, modify, merge, publish,
 distribute, sublicense, and/or sell copies of the Software, and to
 permit persons to whom the Software is furnished to do so, subject to
 the following conditions:
 
 The above copyright notice and this permission notice shall be included
 in all copies or substantial portions of the Software.
-->


<xsl:import href="../xhtml/tei.xsl"/>
<xsl:import href="../xhtml/tagdocs.xsl"/>
<xsl:import href="../odds/teiodds.xsl"/>

<xsl:output 
  method="xml"  
  doctype-public="-//W3C//DTD HTML 4.0 Transitional//EN" 
  doctype-system="http://www.w3.org/TR/html4/loose.dtd"
  indent="yes"/>

<xsl:key name="SPECS"
	 match="tei:classSpec|tei:elementSpec|tei:macroSpec" use="'yes'"/>

<xsl:key name="PUBLICIDS" match="tei:publicID" use="@file"/>
<xsl:param name="documentationLanguage">en</xsl:param>

<!-- parameterization -->

 <xsl:param name="lookupDatabase">true</xsl:param>
 <xsl:template name="metaHook">
   <xsl:param name="title"/>
   <meta name="DC.Title" content="{$title}"/>
   <meta name="DC.Language" content="(SCHEME=iso639) en"/> 
   <meta name="DC.Creator" content="TEI,Oxford University Computing Services, 13 Banbury Road, Oxford OX2 6NN, United Kingdom"/>
   <meta name="DC.Creator.Address" content="tei@oucs.ox.ac.uk"/>
 </xsl:template>
 
 <xsl:template name="bodyHook">
 </xsl:template>


<xsl:param name="indent-width" select="3"/>
<xsl:param name="line-width" select="80"/>

<xsl:key name="NameToID" match="tei:*" use="@ident"/>
<xsl:param name="cssFile">/stylesheet/teic.css</xsl:param>
<xsl:param name="institution">Text Encoding Initiative</xsl:param>
<xsl:param name="STDOUT">true</xsl:param>
<xsl:param name="BITS">Bits</xsl:param>
<xsl:param name="oddmode">html</xsl:param>
<xsl:param name="leftLinks">true</xsl:param>
<xsl:param name="footnoteFile">true</xsl:param>
<xsl:param name="linksWidth">15%</xsl:param>
<xsl:param name="splitBackmatter">yes</xsl:param>
<xsl:param name="splitFrontmatter">yes</xsl:param>
<xsl:param name="splitLevel">-1</xsl:param>
<xsl:param name="verbose"></xsl:param>
<xsl:param name="autoToc">false</xsl:param>
<xsl:param name="topNavigationPanel">true</xsl:param>
<xsl:param name="numberHeadings">1.1.</xsl:param>
<xsl:param name="numberFrontHeadings"></xsl:param>
<xsl:param name="numberBackHeadings">A.1</xsl:param>
<xsl:param name="searchURL"/>
<xsl:param name="searchWords"/>
<xsl:param name="homeLabel">TEI P5 Home</xsl:param>
<xsl:variable name="linkColor"/>
<xsl:param name="parentWords">TEI Home</xsl:param>
<xsl:param name="homeWords">TEI Guidelines</xsl:param>
<xsl:variable name="top" select="/"/>
<xsl:param name="homeURL">http://www.tei-c.org/P5/</xsl:param>
<xsl:param name="parentURL">http://www.tei-c.org/</xsl:param>
<xsl:param name="TEISERVER">http://tei.oucs.ox.ac.uk/Query/</xsl:param>
<xsl:param name="feedbackURL">mailto:tei@oucs.ox.ac.uk</xsl:param>
<xsl:param name="schemaBaseURL">http://www.tei-c.org.uk/schema/relaxng/p5/</xsl:param>
  <xsl:param name="startRed">&lt;span style="color:red"&gt;</xsl:param>
  <xsl:param name="startBold">&lt;span class="element"&gt;</xsl:param>
  <xsl:param name="endBold">&lt;/span&gt;</xsl:param>
  <xsl:param name="startItalic">&lt;span class="attribute"&gt;</xsl:param>
  <xsl:param name="endItalic">&lt;/span&gt;</xsl:param>
  <xsl:param name="endRed">&lt;/span&gt;</xsl:param>
  <xsl:param name="spaceCharacter">&#xA0;</xsl:param>
  <xsl:template name="lineBreak">
    <xsl:param name="id"/>
    <xsl:text>&#10;</xsl:text>
  </xsl:template>


<!-- the level of division for each mini TOCs are built -->
<xsl:param name="subTocDepth">-1</xsl:param>

<xsl:param name="tocDepth">3</xsl:param>

<xsl:template name="refdoc">
  <xsl:apply-templates select="." mode="weavebody"/>
</xsl:template>

<xsl:template name="stdheader">
  <xsl:param name="title"/>
  <div id="hdr">
  <h2 class="institution">
    <xsl:value-of select="$institution"/>
  </h2>
  <h1>
    <xsl:value-of select="$title"/>
  </h1>
 </div>
</xsl:template>

<xsl:template match="html:*">
     <xsl:element name="{local-name()}">
       <xsl:copy-of select="@*"/>
       <xsl:apply-templates/>
     </xsl:element>
</xsl:template>


<xsl:template match="tei:classSpec|tei:elementSpec|tei:macroSpec">
  <table class="wovenodd" border='1'>
    <xsl:apply-templates select="." mode="weavebody"/>
  </table>
</xsl:template>

<xsl:template match="element">
  <table class="wovenodd" border='1'>
    <xsl:for-each select="equiv">
      <tr>
	<td><xsl:value-of select="@xml:lang"/></td>
	<td><xsl:value-of select="@value"/></td>
	<td><xsl:apply-templates select="following-sibling::desc[@xml:lang=current()/@xml:lang]"/></td>
      </tr>
    </xsl:for-each>
  </table>
</xsl:template>
 
<xsl:template match="/">
  <html>
    <xsl:call-template name="addLangAtt"/>
    <xsl:comment>THIS FILE IS GENERATED FROM AN XML MASTER</xsl:comment>
    <xsl:variable name="pagetitle">
      <xsl:for-each select="key('SPECS','yes')[1]">
	<xsl:choose>
	  <xsl:when test="self::tei:elementSpec">
	    <xsl:call-template name="i18n">
	      <xsl:with-param name="word">Element</xsl:with-param>
	    </xsl:call-template>
	    <xsl:text> &lt;</xsl:text>
	    <xsl:value-of select="@ident"/>
	    <xsl:text>&gt;</xsl:text>
	  </xsl:when>
	  <xsl:when test="self::tei:classSpec[@type='model']">
	    <xsl:call-template name="i18n">
	      <xsl:with-param name="word">Class</xsl:with-param>
	    </xsl:call-template>
	    <xsl:text> </xsl:text>
	    <xsl:value-of select="@ident"/>
	  </xsl:when>
	  <xsl:when test="self::tei:classSpec[@type='atts']">
	    <xsl:call-template name="i18n">
	      <xsl:with-param name="word">Class</xsl:with-param>
	    </xsl:call-template>
	    <xsl:text> </xsl:text>
	    <xsl:value-of select="@ident"/>
	  </xsl:when>
	  <xsl:otherwise>
	    <xsl:value-of select="@ident"/>
	    </xsl:otherwise>
	</xsl:choose>
      </xsl:for-each>
    </xsl:variable>
    <head>
      <title><xsl:value-of select="$pagetitle"/></title>
      <xsl:call-template name="headHook"/>
      <xsl:call-template name="metaHTML">
	<xsl:with-param name="title" select="$pagetitle"/>
      </xsl:call-template>
      <xsl:call-template name="includeCSS"/>
      <xsl:call-template name="cssHook"/>
      <xsl:call-template name="includeJavascript"/>
      <xsl:call-template name="javascriptHook"/>
    </head>
    <body class="simple" id="TOP">
      <xsl:attribute name="onload">
	<xsl:text>startUp()</xsl:text>
      </xsl:attribute>
        <xsl:call-template name="bodyHook"/>
	<xsl:call-template name="bodyJavascriptHook"/>
	<xsl:if test="not(tei:text/tei:front/tei:titlePage)">
	  <xsl:call-template name="stdheader">
	    <xsl:with-param name="title" select="$pagetitle"/>
	  </xsl:call-template>
	</xsl:if>
	<xsl:call-template name="startHook"/>
	<xsl:apply-templates/>
	<xsl:call-template name="stdfooter"/>
      </body>
    </html>
</xsl:template>

<xsl:template match="tei:TEI">
    <xsl:apply-templates
	select=".//tei:classSpec|.//tei:elementSpec|.//tei:macroSpec"/>
</xsl:template>

</xsl:stylesheet>

