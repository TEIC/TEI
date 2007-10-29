<?xml version="1.0" encoding="utf-8"?>
<!--
** This is the stylesheet for generating EDW84, the "TEI element catalogue".
** It reads in the Guidelines (i.e., the complete XML source), and writes 
** out a TEI file which, in theory, if it had a DOCTYPE declaration would be
** a valid TEI P4 Lite file which includes tables for cross-referncing 
** elements and classes.
** A typical static invocation would be something like the following (all on 
** one line, double hyphen escaped with numeric character references):
**   xmllint &#x2D;&#x2D;noent &#x2D;&#x2D;xinclude
**   Source/Guidelines/en/guidelines-en.xml
**   | xsltproc Utilities/listcontentmodels.xsl -
**   | xsltproc /usr/share/xml/teip4/stylesheet/teic/teihtml-teic-printable.xsl -
**   > /tmp/edw84.html 
**
** It was written for the 2005-09 "class" meeting in Oxford by Sebastian
** Rahtz.
-->

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

<xsl:key name="IDENTS" match="tei:*[@ident]"	  use="@ident"/>

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
  <cell>Classes</cell>
  <cell>Used in</cell>
  <cell>Content Model</cell>
</row>
<xsl:for-each select="//tei:elementSpec">
<!--<xsl:sort select="tei:content//rng:ref[1]/@name"/>-->
<xsl:sort select="@module"/>
<xsl:sort select="@ident"/>
<row rend="leave">
<cell><anchor>
<xsl:attribute name="id">
  <xsl:value-of select="@ident"/>
</xsl:attribute>
</anchor>
<xref n="{normalize-space(tei:desc)}"
      url="http://tei.oucs.ox.ac.uk/Query/tag.xq?name={@ident}">
<xsl:value-of select="@ident"/>
</xref>
</cell>
<cell><xsl:value-of select="@module"/></cell>
<cell><xsl:for-each select="tei:classes">
  <xsl:for-each select="tei:memberOf">
  <xsl:sort select="@key"/>
    <ref>
      <xsl:attribute name="target">
	<xsl:value-of select="@key"/>
      </xsl:attribute>
      <xsl:for-each select="key('IDENTS',@key)">
      <xsl:choose>
	<xsl:when test="@type='model'">
	  <hi><xsl:value-of select="@ident"/></hi>
	</xsl:when>
	<xsl:otherwise>
	  <xsl:value-of select="@ident"/>
	</xsl:otherwise>
      </xsl:choose>
      </xsl:for-each>
    </ref>
    <xsl:text> </xsl:text>
  </xsl:for-each>
</xsl:for-each>
</cell>
<cell >
<xsl:variable name="ID" select="@ident"/>
<xsl:for-each select="key('REFS',@ident)">
<xsl:if test="not(generate-id(.)=generate-id(key('REFS',$ID)[1]))"> |
</xsl:if>
    <ref>
      <xsl:attribute name="target">
	<xsl:value-of select="@ident"/>
      </xsl:attribute>
      <xsl:value-of select="@ident"/> 
    </ref>
</xsl:for-each>
</cell>
<cell><q rend="eg">
<xsl:call-template name="make-body-from-r-t-f">
  <xsl:with-param name="schema">
    <xsl:for-each  select="tei:content">
      <xsl:call-template name="make-compact-schema"/>
    </xsl:for-each>
  </xsl:with-param>
</xsl:call-template>
</q></cell>
</row>
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
<!--cell>Type</cell-->
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
<xref n="{normalize-space(tei:desc)}"
      url="http://tei.oucs.ox.ac.uk/Query/tag.xq?name={@ident}">
  <xsl:value-of select="@ident"/>
</xref>
</cell>
<!--cell ><xsl:value-of select="@type"/></cell-->
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
<xsl:sort select="@ident"/>
<ref target="{@ident}"><xsl:value-of select="@ident"/> </ref>
<xsl:text> </xsl:text>
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


