<?xml version="1.0"?>
<xsl:stylesheet xmlns:a="http://relaxng.org/ns/compatibility/annotations/1.0" xmlns:html="http://www.w3.org/1999/xhtml" xmlns:teix="http://www.tei-c.org/ns/Examples" xmlns:rng="http://relaxng.org/ns/structure/1.0" xmlns:estr="http://exslt.org/strings" xmlns:pantor="http://www.pantor.com/ns/local" xmlns:exsl="http://exslt.org/common" xmlns:tei="http://www.tei-c.org/ns/1.0" xmlns:edate="http://exslt.org/dates-and-times" xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0" extension-element-prefixes="exsl estr edate" exclude-result-prefixes="exsl rng edate estr tei a pantor teix html">
<!--
     Copyright 2008 Sebastian Rahtz/Oxford University/TEI Consortium  
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
  <xsl:import href="teiodds.xsl"/>
  <xsl:import href="../xhtml/tei.xsl"/>
  <xsl:import href="../xhtml/tagdocs.xsl"/>
  <xsl:import href="RngToRnc.xsl"/>
  <xsl:key name="SPECS" match="tei:classSpec|tei:elementSpec|tei:macroSpec" use="'yes'"/>
  <xsl:param name="xhtml">true</xsl:param>
  <xsl:param name="STDOUT">true</xsl:param>
  <xsl:param name="oddmode">html</xsl:param>
  <xsl:param name="topNavigationPanel">false</xsl:param>
  <xsl:param name="bottomNavigationPanel">false</xsl:param>
  <xsl:param name="homeURL">http://www.tei-c.org/P5/</xsl:param>
  <xsl:param name="parentURL"/>
  <xsl:param name="parentWords"/>
  <xsl:param name="feedbackURL">mailto:tei@oucs.ox.ac.uk</xsl:param>
  <xsl:param name="institution">Text Encoding Initiative</xsl:param>
  <xsl:param name="searchURL"/>
  <xsl:param name="searchWords"/>
  <xsl:param name="cssFile">http://www.tei-c.org/release/xml/tei/stylesheet/tei.css</xsl:param>
  <xsl:param name="cssSecondaryFile">http://www.tei-c.org/release/xml/tei/stylesheet/odd.css</xsl:param>
  <xsl:variable name="top" select="/"/>
  <xsl:template name="bodyHook"> </xsl:template>
  <xsl:template name="lineBreak">
    <xsl:param name="id"/>
    <xsl:text disable-output-escaping="yes">&lt;br/&gt;</xsl:text>
  </xsl:template>
  <xsl:template name="makeSectionHead">
    <xsl:param name="name"/>
    <xsl:param name="id"/>
  </xsl:template>
  <xsl:template match="tei:classSpec|tei:elementSpec|tei:macroSpec">
    <table class="wovenodd" border="1">
      <xsl:apply-templates select="." mode="weavebody"/>
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
        <title>
          <xsl:value-of select="$pagetitle"/>
        </title>
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
    <xsl:apply-templates select=".//tei:classSpec|.//tei:elementSpec|.//tei:macroSpec"/>
  </xsl:template>
  <xsl:template name="copyrightStatement"/>
</xsl:stylesheet>
