<?xml version="1.0" encoding="utf-8"?>
<!-- 
Text Encoding Initiative Consortium XSLT stylesheet family
$Date$, $Revision$, $Author$

XSL stylesheet to process TEI documents using ODD markup

 
##LICENSE
--> 
<xsl:stylesheet 
 xmlns:rng="http://relaxng.org/ns/structure/1.0"
  xmlns:eg="http://www.tei-c.org/ns/Examples"
  xmlns:tei="http://www.tei-c.org/ns/1.0"
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
  xmlns:exsl="http://exslt.org/common"
  extension-element-prefixes="exsl"
  exclude-result-prefixes="tei exsl" 
  version="1.0">
<xsl:param name="TEISERVER">http://localhost:8080/exist/TEI/Roma/xquery/</xsl:param>
<xsl:key name="MODS" match="tei:moduleSpec" use="@ident"/>
<xsl:key name="SPECS" match="tei:specGrp" use="@id"/>
<xsl:key name="LOCAL"
	 match="tei:classSpec|tei:elementSpec|tei:macroSpec" use="@ident"/>
<xsl:key name="LOCALATT"
	 match="tei:attDef" use="concat(../../@ident,'::',@ident)"/>
<xsl:output method="xml" indent="yes"/>
<xsl:param name="verbose"></xsl:param>

<xsl:variable name="MAIN" select="/"/>

<xsl:template match="tei:body">
<tei:body>
<tei:div0>
<tei:head>ODD SUBSET</tei:head>

<tei:div1>
<tei:head>Documentation</tei:head>
<xsl:apply-templates/>
</tei:div1>

<tei:div1 id="REFCLA">
<tei:head>Class catalogue</tei:head>
<tei:divGen type="classcat"/>
</tei:div1>
<tei:div1 id="REFENT">
<tei:head>Pattern catalogue</tei:head>
<tei:divGen type="patterncat"/>
</tei:div1>
<tei:div1 id="REFTAG">
<tei:head>Element catalogue</tei:head>
<tei:divGen type="tagcat"/>
</tei:div1>

</tei:div0>
</tei:body>
</xsl:template>

<xsl:template match="tei:schemaSpec">
	  <xsl:apply-templates select="tei:specGrp"/>
  	  <xsl:apply-templates select="tei:moduleRef"/>
  	  <xsl:apply-templates select="tei:elementSpec[@mode='add']"/>
</xsl:template>


<xsl:template match="tei:moduleRef">
  <xsl:variable name="test" select="@key"/>
  <xsl:call-template name="findNames">
    <xsl:with-param name="modname">
      <xsl:value-of select="$test"/>
    </xsl:with-param>
  </xsl:call-template>
</xsl:template>

<xsl:template name="findNames">
  <xsl:param name="modname"/>
  <xsl:variable name="HERE" select="."/>
  <xsl:variable name="loc">
    <xsl:value-of select="$TEISERVER"/>
    <xsl:text>allbymod.xq?module=</xsl:text>
    <xsl:value-of select="$modname"/>
  </xsl:variable>
  <xsl:for-each select="document($loc)/List/*">
      <xsl:call-template name="processThing"/>
  </xsl:for-each>
</xsl:template>

<xsl:template name="processThing">
  <xsl:variable name="me" select="@ident"/>
  <xsl:variable name="here" select="."/>
  <xsl:for-each select="$MAIN">
  <xsl:choose>
    <xsl:when test="key('LOCAL',$me)">
      <xsl:for-each select="key('LOCAL',$me)">
	<xsl:choose>
	  <xsl:when test="@mode='delete'"/>
	  <xsl:when test="@mode='change'">
	    <xsl:for-each select="$here">
	      <xsl:apply-templates select="." mode="change"/>
	    </xsl:for-each>
	  </xsl:when>
	  <xsl:otherwise>
	    <xsl:for-each select="$here">
	      <xsl:apply-templates select="." mode="copy"/>
	    </xsl:for-each>
	  </xsl:otherwise>
	</xsl:choose>
      </xsl:for-each>
    </xsl:when>
    <xsl:otherwise>
      <xsl:for-each select="$here">
	<xsl:apply-templates select="."  mode="copy"/>
      </xsl:for-each>
    </xsl:otherwise>
  </xsl:choose>
  </xsl:for-each>
</xsl:template>

<xsl:template match="@mode"/>

<xsl:template match="tei:elementSpec|tei:classSpec|tei:patternSpec" mode="change">
  <xsl:variable name="me" select="@ident"/>
  <xsl:copy>
    <xsl:apply-templates select="@*" mode="change"/>
    <xsl:for-each select="$MAIN">
      <xsl:for-each select="key('LOCAL',$me)">
	<xsl:choose>
	  <xsl:when test="@mode='delete'"/>
	  <xsl:when test="@mode='replace'">
	    <xsl:copy-of select="."/>
	  </xsl:when>
	  <xsl:otherwise>
	    <xsl:copy-of select="tei:altIdent"/>
	  </xsl:otherwise>
	</xsl:choose>
      </xsl:for-each>
    </xsl:for-each>
    <xsl:apply-templates select="*|text()|comment()" mode="change"/>
  </xsl:copy>
</xsl:template>

<xsl:template match="tei:attDef"      mode="change">
  <xsl:variable name="me" select="concat(../../@ident,'::',@ident)"/>
  <xsl:copy>
    <xsl:apply-templates select="@*" mode="change"/>
    <xsl:for-each select="$MAIN">
      <xsl:for-each select="key('LOCALATT',$me)">
	<xsl:choose>
	  <xsl:when test="@mode='delete'"/>
	  <xsl:when test="@mode='replace'">
	    <xsl:copy-of select="."/>
	  </xsl:when>
	  <xsl:otherwise>
	    <xsl:copy-of select="tei:altIdent"/>
	  </xsl:otherwise>
	</xsl:choose>
      </xsl:for-each>
    </xsl:for-each>
    <xsl:apply-templates select="tei:*|eg:*|text()|comment()" mode="change"/>
  </xsl:copy>
</xsl:template>

<xsl:template match="tei:attList"  mode="change">
  <xsl:variable name="me" select="../@ident"/>
  <xsl:copy>
    <xsl:apply-templates select="@*" mode="change"/>
    <xsl:for-each select="$MAIN">
      <xsl:for-each select="key('LOCAL',$me)/tei:attList">
	<xsl:copy-of select="tei:attDef[@mode='add']"/>
      </xsl:for-each>
    </xsl:for-each>
    <xsl:apply-templates select="*|text()|comment()" mode="change"/>
  </xsl:copy>
</xsl:template>

<xsl:template match="tei:*">
  <xsl:copy>
    <xsl:apply-templates select="@*|tei:*|rng:*|eg:*|text()|comment()"/>
  </xsl:copy>
</xsl:template>

<xsl:template match="@*|comment()|text()" mode="change">
  <xsl:copy/>
</xsl:template>

<xsl:template match="@*|comment()">
  <xsl:copy/>
</xsl:template>

<xsl:template match="text()">
  <xsl:copy-of select="."/>
</xsl:template>

<xsl:template match="eg:*" mode="change">
<xsl:copy-of select="."/>
</xsl:template>

<xsl:template match="eg:*">
<xsl:copy-of select="."/>
</xsl:template>


<xsl:template match="tei:*|rng:*" mode="change">
  <xsl:if test="not(@mode='delete')">
    <xsl:copy>
      <xsl:apply-templates select="@*|*|text()|comment()" mode="change"/>
    </xsl:copy>
  </xsl:if>
</xsl:template>


<xsl:template match="*|@*|processing-instruction()|text()" mode="copy">
 <xsl:copy>
  <xsl:apply-templates
   select="*|@*|processing-instruction()|comment()|text()" mode="copy"/>
 </xsl:copy>
</xsl:template>

</xsl:stylesheet>
