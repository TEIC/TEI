<?xml version="1.0" encoding="utf-8"?>
<!--
$Date$ $Author$ odd2roma.xsl

 XSLT script for extracting summary table for Roma, etc from ODD files

 Lou Burnard and Sebastian Rahtz <sebastian.rahtz@oucs.ox.ac.uk>

 December 2003

 Copyright 2003 TEI Consortium

 Permission is hereby granted, free of charge, to any person obtaining
 a copy of this software and any associated documentation gfiles (the
 ``Software''), to deal in the Software without restriction, including
 without limitation the rights to use, copy, modify, merge, publish,
 distribute, sublicense, and/or sell copies of the Software, and to
 permit persons to whom the Software is furnished to do so, subject to
 the following conditions:
 
 The above copyright notice and this permission notice shall be included
 in all copies or substantial portions of the Software.
-->

<xsl:stylesheet 
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
  xmlns:a="http://relaxng.org/ns/compatibility/annotations/1.0"
  xmlns:edate="http://exslt.org/dates-and-times"
  xmlns:estr="http://exslt.org/strings"
  xmlns:exsl="http://exslt.org/common"
  xmlns:rng="http://relaxng.org/ns/structure/1.0"
  xmlns:t="http://www.thaiopensource.com/ns/annotations"
  xmlns:tei="http://www.tei-c.org/ns/1.0"
  exclude-result-prefixes="exsl" 
  extension-element-prefixes="exsl edate"
  version="1.0">
 
  <xsl:import href="../../web/Stylesheets/P5/odds/teiodds.xsl"/>
  <xsl:output method="xml" indent="yes"/>
  <xsl:variable name="oddmode">dtd</xsl:variable>       
 <xsl:key name="SPECGRPS" match="tei:specGrpRef" use="@target"/>
 <xsl:key name="CLASSDOCS" match="tei:classSpec" use="@ident"/>
 <xsl:key name="PATTERNDOCS" match="tei:macroSpec" use="@ident"/>

<xsl:variable name="uc">ABCDEFGHIJKLMNOPQRSTUVWXYZ</xsl:variable>
<xsl:variable name="lc">abcdefghijklmnopqrstuvwxyz</xsl:variable>

<xsl:template match="/tei:TEI">
  <xsl:variable name="files">    
    <files>
      <xsl:for-each select=".//tei:moduleSpec[not(@type='decls')]">
	<xsl:sort select="@name"/>
	<name id="{@name}"/>
      </xsl:for-each>
    </files>
  </xsl:variable>

 <!-- for each file, locate all the chunks which write to it -->
<xsl:variable name="Master" select="."/>
  <Table>
    <xsl:for-each select=".//tei:classSpec[@type='both' or @type='model']">
      <xsl:sort select="@ident"/>
      <xsl:if
       test="generate-id()=generate-id(key('CLASSDOCS',@ident)[1])">
	<xsl:variable name="filelocation">
	  <xsl:call-template name="filename">
	    <xsl:with-param name="suffix">.odd</xsl:with-param>
	  </xsl:call-template>
	</xsl:variable>
      <Class id="{@id}" type="{@type}">
	<xsl:call-template name="info"/>
	<ident><xsl:value-of select="@ident"/></ident>
	<desc><xsl:value-of select="tei:desc"/></desc>
      </Class>
      </xsl:if>
    </xsl:for-each>
    <xsl:for-each select=".//tei:classSpec[@type='both' or @type='atts']">
      <xsl:sort select="@ident"/>
      <xsl:if test="not(@ident='tei.TEIform')">

      <xsl:if test="generate-id()=generate-id(key('CLASSDOCS',@ident)[1])">

      <AttClass id="{@id}" type="{@type}">
	<xsl:call-template name="info"/>
	<ident><xsl:value-of select="@ident"/></ident>
	<desc><xsl:value-of select="tei:desc"/></desc>
	<xsl:if test="tei:attList">
	  <Attributes>
	    <xsl:for-each select="tei:attList//tei:attDef">
	      <Att n="{@ident}"/>
	    </xsl:for-each>
	  </Attributes>
	</xsl:if>
      </AttClass>
      </xsl:if>
      </xsl:if>
    </xsl:for-each>
    <xsl:for-each select=".//tei:macroSpec[starts-with(@ident,'macro.')]">
      <xsl:sort select="@id"/>
      <xsl:if test="generate-id()=generate-id(key('PATTERNDOCS',@ident)[1])">
	<Pattern id="{@id}">
	<xsl:call-template name="info"/>
	<ident><xsl:value-of select="@ident"/></ident>
	<desc><xsl:value-of select="tei:desc"/></desc>
      </Pattern>
      </xsl:if>
    </xsl:for-each>
    <xsl:for-each select=".//tei:elementSpec">
      <xsl:sort select="@id"/>
      <Tag id="{@id}">
	<xsl:call-template name="info"/>
	<ident><xsl:value-of select="@ident"/></ident>
	<Desc><xsl:value-of select="normalize-space(tei:desc)"/></Desc>
	<Content><xsl:copy-of select="tei:content/*"/></Content>
	<xsl:if test="tei:attList">
	  <Attributes>
	    <xsl:for-each select="tei:attList//tei:attDef">
	      <Att n="{@ident}"/>
	    </xsl:for-each>
	  </Attributes>
	</xsl:if>
      </Tag>
    </xsl:for-each>
</Table>
</xsl:template>


<xsl:template name="specGrpfile">
  <xsl:choose>
    <xsl:when test="parent::tei:moduleSpec">
      <xsl:value-of select="parent::tei:moduleSpec/@ident"/>
    </xsl:when>
    <xsl:otherwise>
	<xsl:for-each select="key('SPECGRPS',parent::tei:specGrp/@id)[1]">
	  <xsl:call-template name="specGrpfile"/>
	</xsl:for-each>
    </xsl:otherwise>
  </xsl:choose>
</xsl:template>

<xsl:template name="filename">
  <xsl:param name="suffix"/>
  <xsl:value-of select="ancestor::tei:div1/@id"/>
  <xsl:text>/</xsl:text>
  <xsl:value-of select="translate(@id,$uc,$lc)"/>
  <xsl:value-of select="$suffix"/>
</xsl:template>

<xsl:template name="info">
  <xsl:variable name="filename">
    <xsl:choose>
      <xsl:when test="parent::tei:specGrp">
	<xsl:for-each select="key('SPECGRPS',parent::tei:specGrp/@id)">
	  <xsl:call-template name="specGrpfile"/>
	</xsl:for-each>
      </xsl:when>
      <xsl:when test="parent::tei:moduleSpec">
	<xsl:value-of select="parent::tei:moduleSpec/@ident"/>
      </xsl:when>
      <xsl:otherwise>core</xsl:otherwise>
    </xsl:choose>
  </xsl:variable>
  <xsl:variable name="filelocation">
    <xsl:call-template name="filename">
      <xsl:with-param name="suffix">.odd</xsl:with-param>
    </xsl:call-template>
  </xsl:variable>

<xsl:attribute name="filelocation">
  <xsl:value-of select="$filelocation"/>
</xsl:attribute>
<Tagset><xsl:value-of select="@module"/></Tagset>
</xsl:template>

</xsl:stylesheet>

