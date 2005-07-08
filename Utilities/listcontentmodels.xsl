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

<xsl:key  name="REFS" match="tei:elementSpec|tei:macroSpec"
	  use="tei:content//rng:ref/@name"/>

<xsl:key  name="CLASSREFS" match="tei:elementSpec|tei:classSpec"
	  use="tei:classes/tei:memberOf/@key"/>

<xsl:template match="/">
<xsl:processing-instruction name="xml-stylesheet">href="summary.xsl" type="text/xsl"</xsl:processing-instruction>
<TEI xmlns="http://www.tei-c.org/ns/1.0">
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
         <sourceDesc><p></p></sourceDesc>
      </fileDesc>
   </teiHeader>
   <text>
      <body>
<div>
<head>Content models for each element</head>
<table rend="rules">
<xsl:for-each select="//tei:elementSpec">
<xsl:sort select="@ident"/>
<row rend="leave">
<cell><xsl:value-of select="@ident"/></cell>
<cell><xsl:value-of select="@module"/></cell>
<cell><eg>
<xsl:call-template name="make-body-from-r-t-f">
  <xsl:with-param name="schema">
    <xsl:for-each  select="tei:content">
      <xsl:call-template name="make-compact-schema"/>
    </xsl:for-each>
  </xsl:with-param>
</xsl:call-template>
</eg></cell></row>
</xsl:for-each>
</table>
</div>

<div>
<head>Content model for each macro</head>
<table rend="rules">
<xsl:for-each select="//tei:macroSpec">
<xsl:sort select="@ident"/>
<row>
<cell ><xsl:value-of select="@ident"/></cell>
<cell ><eg>
<xsl:call-template name="make-body-from-r-t-f">
  <xsl:with-param name="schema">
    <xsl:for-each  select="tei:content">
      <xsl:call-template name="make-compact-schema"/>
    </xsl:for-each>
  </xsl:with-param>
</xsl:call-template>
</eg></cell></row>
</xsl:for-each>
</table>

<h1>Usage for each class</h1>
<table rules="all" border="1">
<cell>&#160;</cell><cell>&#160;</cell>
<cell><hi>Used in content model of</hi></cell>
<cell><hi>Members of club</hi></cell>
<xsl:for-each select="//tei:classSpec">
<xsl:sort select="@type"/>
<xsl:sort select="@ident"/>
<row>
<cell ><xsl:value-of select="@ident"/></cell>
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
</TEI>
</xsl:template>

</xsl:stylesheet>


