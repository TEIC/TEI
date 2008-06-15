<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet 
     xmlns:s="http://www.ascc.net/xml/schematron" 
     xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
     xmlns:html="http://www.w3.org/1999/xhtml"
     xmlns:tei="http://www.tei-c.org/ns/1.0"
     xmlns="http://www.tei-c.org/ns/1.0"
     xmlns:estr="http://exslt.org/strings"
     xmlns:t="http://www.thaiopensource.com/ns/annotations"
     xmlns:a="http://relaxng.org/ns/compatibility/annotations/1.0"
     xmlns:edate="http://exslt.org/dates-and-times"
     xmlns:exsl="http://exslt.org/common"
     xmlns:rng="http://relaxng.org/ns/structure/1.0"
     extension-element-prefixes="exsl estr edate"
     exclude-result-prefixes="exsl edate estr tei t a rng s html" 
     version="1.0">

  <xsl:param name="lang">fr</xsl:param>

<xsl:key name="Mo" match="tei:moduleSpec" use="1"/>
<xsl:key name="E"  match="tei:elementSpec" use="@module"/>
<xsl:key name="Ma" match="tei:macroSpec" use="@module"/>
<xsl:key name="AC" match="tei:classSpec[@type='atts']" use="@module"/>
<xsl:key name="MC" match="tei:classSpec[@type='model']" use="@module"/>

<xsl:template match="/">
  <TEI  xmlns="http://www.tei-c.org/ns/1.0">
   <teiHeader>

     <fileDesc>
       <titleStmt>
	 <title>TEI I18N summary: language <xsl:value-of select="$lang"/></title>
       </titleStmt>
       <editionStmt>
	 <edition>
	   <date>June 2008</date>
	 </edition>
       </editionStmt>
       <publicationStmt>
	 <authority>The Text Encoding Initiative</authority>
	 <p>TEI Web</p>      
       </publicationStmt>
       <sourceDesc>
	 <p>No source</p>
       </sourceDesc>
     </fileDesc>
     <profileDesc>

     </profileDesc>
     <html:style type="text/css">
       .missing {
       color: red;
       }
       table.inner {
         width: 100%;
	 border: solid black 1pt;
       }
       td.label {
        width: 10%;
       }
       td.trans1 {
          width: 40%;
	  background-color: #dddddd;
       }
       td.trans2 {
          width: 40%;
       }
       td.transall {
          width: 90%;
       }
     </html:style>
     <revisionDesc>
       <change>
	 <p><date>$Date: 2005-10-18 14:54:32 +0100 (Tue, 18 Oct 2005) $.</date>
	 <name>$Author: rahtz $</name>
	 Revision: 934 $</p>
       </change>
     </revisionDesc>
   </teiHeader>
   <text>
     <body>
       <xsl:apply-templates select="key('Mo',1)"/>
     </body>
   </text>
  </TEI>
</xsl:template>
  
  <xsl:template match="tei:moduleSpec">
    
    <div>
      <xsl:attribute name="xml:id">
	<xsl:value-of select="@ident"/> 
      </xsl:attribute>
      <head>[<xsl:value-of select="@ident"/>]
      <xsl:value-of select="tei:desc"/>
      </head>
      <xsl:if test="count(key('AC',@ident))&gt;0">
	<div>
	  <head>Attribute classes</head>
	  <table rend="rules">
	    <xsl:apply-templates select="key('AC',@ident)">
	      <xsl:sort select="@ident"/>
	    </xsl:apply-templates>
	  </table>
	</div>
      </xsl:if>
      <xsl:if test="count(key('AC',@ident))&gt;0">
	<div>
	  <head>Model classes</head>
	  <table rend="rules">
	    <xsl:apply-templates select="key('AC',@ident)">
	      <xsl:sort select="@ident"/>
	    </xsl:apply-templates>
	  </table>
	</div>
      </xsl:if>
      <xsl:if test="count(key('E',@ident))&gt;0">
	<div>
	  <head>Elements</head>
	  <table rend="rules">
	    <xsl:apply-templates select="key('E',@ident)">
	      <xsl:sort select="@ident"/>
	    </xsl:apply-templates>
	  </table>
	</div>
      </xsl:if>
      <xsl:if test="count(key('Ma',@ident))&gt;0">
	<div>
	  <head>Macros</head>
	  <table rend="rules">
	    <xsl:apply-templates select="key('Ma',@ident)">
	      <xsl:sort select="@ident"/>
	    </xsl:apply-templates>
	  </table>
	</div>
      </xsl:if>
    </div>
  </xsl:template>


  <xsl:template match="tei:classSpec|tei:macroSpec|tei:elementSpec">
    <row>
      <cell rend="label">
	<hi>
	  <xsl:value-of select="@ident"/>
	</hi>
      </cell>
      <cell rend="transall">
	<xsl:call-template name="show"/>
      </cell>
    </row>
    <xsl:if test=".//tei:attDef">
      <row>
	<cell>&#160;</cell>
	<cell>
	  <table rend="rules">
	    <xsl:for-each select=".//tei:attDef">
	      <row>
		<cell rend="label">&#160;@<xsl:value-of select="@ident"/></cell>
		<cell rend="transall">
		  <xsl:call-template name="show"/>
		</cell>
	      </row>
	      <xsl:if test="tei:valList">
		<row>
		  <cell>&#160;</cell>
		  <cell>
		    <table rend="rules">
		      <xsl:for-each
			  select="tei:valList/tei:valItem">
			<row>
			  <cell rend="label">&#160;&#160;<emph><xsl:value-of select="@ident"/></emph></cell>
			  <cell rend="transall">
			    <xsl:call-template name="show"/>
			  </cell>
			</row>
		      </xsl:for-each>
		    </table>
		  </cell>
		</row>
	      </xsl:if>
	    </xsl:for-each>
	  </table>
	</cell>
      </row>
      </xsl:if>
</xsl:template>

<xsl:template name="show">
<xsl:if test="tei:gloss[not(@xml:lang)] or tei:desc[not(@xml:lang)]">
  <table rend="inner">
    <xsl:if test="tei:gloss[not(@xml:lang)]">
      <row>
	<cell rend="trans1">
	  <xsl:value-of select="tei:gloss[not(@xml:lang)]"/>
	</cell>
	<cell rend="trans2">
	  <xsl:choose>
	    <xsl:when test="tei:gloss[@xml:lang=$lang]">
	      <xsl:value-of select="tei:gloss[@xml:lang=$lang]"/>
	    </xsl:when>
	    <xsl:otherwise>
	      <hi rend="missing">No <xsl:value-of select="$lang"/> for
	      "<xsl:value-of select="tei:gloss[not(@xml:lang)]"/>"
	      </hi>
	    </xsl:otherwise>
	  </xsl:choose>
	</cell>
      </row>
    </xsl:if>
    <xsl:if test="tei:desc[not(@xml:lang)]">
      <row>
	<cell rend="trans1">
	  <xsl:value-of select="tei:desc[not(@xml:lang)]"/>
	</cell>
	<cell rend="trans2">
	  <xsl:choose>
	    <xsl:when test="tei:desc[@xml:lang=$lang]">
	      <xsl:value-of select="tei:desc[@xml:lang=$lang]"/>
	    </xsl:when>
	    <xsl:otherwise>
	      <hi rend="missing">No <xsl:value-of select="$lang"/> for
	      "<xsl:value-of select="tei:desc[not(@xml:lang)]"/>"
	      </hi>
	    </xsl:otherwise>
	  </xsl:choose>
	</cell>
      </row>
    </xsl:if>
  </table>
</xsl:if>
</xsl:template>
</xsl:stylesheet>


