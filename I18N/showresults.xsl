<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet 
     xmlns:s="http://www.ascc.net/xml/schematron" 
     xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
     xmlns:tei="http://www.tei-c.org/ns/1.0"
     xmlns="http://www.tei-c.org/ns/1.0"
     xmlns:estr="http://exslt.org/strings"
     xmlns:t="http://www.thaiopensource.com/ns/annotations"
     xmlns:a="http://relaxng.org/ns/compatibility/annotations/1.0"
     xmlns:edate="http://exslt.org/dates-and-times"
     xmlns:exsl="http://exslt.org/common"
     xmlns:rng="http://relaxng.org/ns/structure/1.0"
     extension-element-prefixes="exsl estr edate"
     exclude-result-prefixes="exsl edate estr tei t a rng s" 
     version="1.0">

  <xsl:param name="lang">fr</xsl:param>
  <xsl:key name="Stuff" 
	   match="tei:classSpec|tei:elementSpec|tei:macroSpec"
	   use="1"/>

<xsl:template match="/">
  <TEI  xmlns="http://www.tei-c.org/ns/1.0">
   <teiHeader>
     <fileDesc>
       <titleStmt>
	 <title>TEI I18N summary: language <xsl:value-of select="$lang"/></title>
       </titleStmt>
       <editionStmt>
       <edition><date>April 2008</date></edition></editionStmt>
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
       <table rend="rules">
       <xsl:for-each select="key('Stuff',1)">
	 <xsl:sort select="local-name(.)"/>
	 <xsl:sort select="@ident"/>
	 <row>
	   <cell>
	     <hi>
	       <xsl:value-of select="@ident"/>
	     </hi>
	   </cell>
	   <cell>
	     <xsl:call-template name="show"/>
	   </cell>
	 </row>
	 <xsl:for-each select=".//tei:attDef">
	   <row>
	     <cell>&#160;<xsl:value-of select="@ident"/></cell>
	     <cell>
	       <xsl:call-template name="show"/>
	     </cell>
	   </row>
	   <xsl:for-each select=".//tei:attDef/tei:valItem">
	   <row>
	     <cell>&#160;&#160;<emph><xsl:value-of select="@ident"/></emph></cell>
	     <cell>
	       <xsl:call-template name="show"/>
	     </cell>
	   </row>
	   </xsl:for-each>
	 </xsl:for-each>
       </xsl:for-each>
       </table>
     </body>
   </text>
  </TEI>
</xsl:template>

<xsl:template name="show">
  <table>
    <xsl:if test="tei:gloss">
      <row>
	<cell>
	  <xsl:value-of select="tei:gloss[not(@xml:lang)]"/>
	</cell>
	<cell>
	  <xsl:value-of select="tei:gloss[@xml:lang=$lang]"/>
	</cell>
      </row>
    </xsl:if>
      <row>
	<cell>
	  <xsl:value-of select="tei:desc[not(@xml:lang)]"/>
	</cell>
	<cell>
	  <xsl:value-of select="tei:desc[@xml:lang=$lang]"/>
	</cell>
      </row>
  </table>
</xsl:template>
</xsl:stylesheet>


