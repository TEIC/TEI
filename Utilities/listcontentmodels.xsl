<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet 
     xmlns:s="http://www.ascc.net/xml/schematron" 
     xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
     xmlns:tei="http://www.tei-c.org/ns/1.0"
     xmlns:estr="http://exslt.org/strings"
     xmlns:t="http://www.thaiopensource.com/ns/annotations"
     xmlns:a="http://relaxng.org/ns/compatibility/annotations/1.0"
     xmlns:edate="http://exslt.org/dates-and-times"
     xmlns:exsl="http://exslt.org/common"
     xmlns:rng="http://relaxng.org/ns/structure/1.0"
     extension-element-prefixes="exsl estr edate"
     exclude-result-prefixes="exsl edate estr tei t a rng s" 
     version="1.0">


<xsl:import href="/usr/share/xml/tei/stylesheet/odds/teiodds.xsl"/>

<xsl:param name="TEISERVER">http://www.tei-c.org.uk/Query/</xsl:param>

<xsl:param name="displayMode">rnc</xsl:param>

<xsl:param name="oddmode">tei</xsl:param>

<xsl:variable name="top" select="/"/>

<xsl:key name="REFS" match="tei:elementSpec|tei:macroSpec"
	  use="tei:content//rng:ref/@name"/>

<xsl:key  name="CLASSREFS" match="tei:elementSpec|tei:classSpec"
	  use="tei:classes/tei:memberOf/@key"/>

<xsl:template match="/">
<TEI.2>
   <teiHeader>
      <fileDesc>
         <titleStmt>
            <title>TEI element catalogue</title>
            <author>Sebastian Rahtz</author>
         </titleStmt>
         <editionStmt>
            <edition>
               <date>$Date$</date>
            </edition>
         </editionStmt>
         <sourceDesc><p>derived from P5 sources</p></sourceDesc>
      </fileDesc>
   </teiHeader>
   <text>
      <body>
<div id="elementcontent">
<head>Content models for elements</head>
<table rend="rules">
<row role="label">
  <cell>Name</cell>
  <cell>Module</cell>
  <cell>Content Model</cell>
</row>
<xsl:for-each select="//tei:elementSpec">
<xsl:sort select="@ident"/>
<row rend="leave">
<cell><anchor>
<xsl:attribute name="id">
  <xsl:value-of select="@ident"/>
</xsl:attribute>
</anchor>
<xsl:value-of select="@ident"/></cell>
<cell><xsl:value-of select="@module"/></cell>
<cell><q rend="eg">
<xsl:call-template name="make-body-from-r-t-f">
  <xsl:with-param name="schema">
    <xsl:for-each  select="tei:content">
      <xsl:call-template name="make-compact-schema"/>
    </xsl:for-each>
  </xsl:with-param>
</xsl:call-template>
</q></cell></row>
</xsl:for-each>
</table>
</div>

<div id="macrocontent">
<head>Content models for macros</head>
<table rend="rules">
<row role="label">
  <cell>Name</cell>
  <cell>Content Model</cell>
</row>
<xsl:for-each select="//tei:macroSpec">
<xsl:sort select="@ident"/>
<row>
<cell ><anchor>
<xsl:attribute name="id">
  <xsl:value-of select="@ident"/>
</xsl:attribute>
</anchor>
<xsl:value-of select="@ident"/>
</cell>
<cell ><q rend="eg">
<xsl:call-template name="make-body-from-r-t-f">
  <xsl:with-param name="schema">
    <xsl:for-each  select="tei:content">
      <xsl:call-template name="make-compact-schema"/>
    </xsl:for-each>
  </xsl:with-param>
</xsl:call-template>
</q></cell></row>
</xsl:for-each>
</table>
</div>

<div id="classusage">
<head>Members of each class</head>
<table rules="all" border="1">
<row role="label">
<cell>Name</cell>
<cell>Type</cell>
<cell>Used in content model of</cell>
<cell>Members of club</cell>
</row>
<xsl:for-each select="//tei:classSpec">
<xsl:sort select="@ident"/>
<row>
<cell >
<anchor>
<xsl:attribute name="id">
  <xsl:value-of select="@ident"/>
</xsl:attribute>
</anchor>
<xsl:value-of select="@ident"/></cell>
<cell ><xsl:value-of select="@type"/></cell>
<cell >
<xsl:variable name="ID" select="@ident"/>
<xsl:for-each select="key('REFS',@ident)">
<xsl:if test="not(generate-id(.)=generate-id(key('REFS',$ID)[1]))"> |
</xsl:if>
<xsl:value-of select="@ident"/> 
</xsl:for-each>
</cell>
<cell >
<xsl:variable name="ID" select="@ident"/>
<xsl:for-each select="key('CLASSREFS',@ident)">
<xsl:if test="not(generate-id(.)=generate-id(key('CLASSREFS',$ID)[1]))"> |
</xsl:if>
 <xsl:value-of select="@ident"/> 
</xsl:for-each>
</cell>
</row>
</xsl:for-each>
</table>
</div>
      </body>
   </text>
</TEI.2>
</xsl:template>

<xsl:template name="showElement">
  <xsl:param name="name"/>
  <ref target="{$name}"><xsl:value-of select="$name"/></ref>
</xsl:template>

<xsl:template name="linkTogether">
  <xsl:param name="name"/>
  <xsl:param name="reftext"/>
  <xsl:variable name="link">
    <xsl:choose>
      <xsl:when test="$reftext=''"><xsl:value-of select="$name"/></xsl:when>
      <xsl:otherwise><xsl:value-of select="$reftext"/></xsl:otherwise>
    </xsl:choose>
  </xsl:variable>
      <ref target="{$name}"><xsl:value-of select="$link"/></ref>
</xsl:template>
</xsl:stylesheet>


