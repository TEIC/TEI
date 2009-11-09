<xsl:stylesheet 
    exclude-result-prefixes="tei" 
    xmlns:tei="http://www.tei-c.org/ns/1.0"
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform" 
  xmlns="http://www.w3.org/1999/xhtml"
  version="2.0"
>
<xsl:import href="isoutils.xsl"/>

<xsl:key name="frontDiv" match="tei:div[ancestor::tei:front]" use="1"/>
<xsl:key name="bodyDiv" match="tei:div[ancestor::tei:body]" use="1"/>
<xsl:key name="backDiv" match="tei:div[ancestor::tei:back]" use="1"/>
<xsl:output method="xhtml" encoding="utf-8"/>
<xsl:template match="/tei:TEI">
  <xsl:variable name="today">
    <xsl:call-template name="whatsTheDate"/>
  </xsl:variable>
  <xsl:variable name="isotitle">
    <xsl:call-template name="generateTitle"/>
  </xsl:variable>
  <xsl:variable name="isonumber">
    <xsl:call-template name="getiso_documentNumber"/>
  </xsl:variable>
  <xsl:variable name="isopart">
    <xsl:call-template name="getiso_partNumber"/>
  </xsl:variable>
  <xsl:variable name="isoyear">
    <xsl:call-template name="getiso_year"/>
  </xsl:variable>
<html>
  <head><title>Report on 
      <xsl:value-of select="$isotitle"/>:
      <xsl:value-of select="$isoyear"/>:
      <xsl:value-of select="$isonumber"/>:
      <xsl:value-of select="$isopart"/>
</title>
      <link href="iso.css" rel="stylesheet" type="text/css"/>

  </head>
<body>
  <h1 class="maintitle">
    
	<xsl:value-of select="$isotitle"/>:
	<xsl:value-of select="$isoyear"/>:
	<xsl:value-of select="$isonumber"/>:
	<xsl:value-of select="$isopart"/>
  </h1>

  <xsl:for-each select="tei:text/tei:front">
    <xsl:apply-templates select="key('frontDiv',1)"/>
    <hr/>
  </xsl:for-each>
  <xsl:for-each select="tei:text/tei:body">
    <xsl:apply-templates select="key('bodyDiv',1)"/>
    <hr/>
  </xsl:for-each>
  <xsl:for-each select="tei:text/tei:back">
    <xsl:apply-templates select="key('backDiv',1)"/>
  </xsl:for-each>
</body>
</html>
</xsl:template>

<xsl:template match="tei:div[not(@type='termHeading')]">
  <xsl:variable name="depth" select="count(ancestor::tei:div)+2"/>
  <xsl:element name="h{$depth}">
    <xsl:call-template name="head"/>
  </xsl:element>
  <table border="1">
  <xsl:for-each select=".//tei:add|.//tei:del">
    <tr class="change_{local-name()}">
      <td>
	<xsl:for-each select="parent::*">
	  <xsl:value-of select="local-name()"/>
	  <xsl:text> </xsl:text>
	  <xsl:number/>
	</xsl:for-each>
      </td>
      <td><xsl:value-of select="local-name()"/></td>
      <td><xsl:value-of select="@when"/></td>
      <td><xsl:value-of select="substring-after(@resp,'#')"/></td>
      <td><xsl:value-of select="."/></td>
    </tr>
  </xsl:for-each>
  </table>
</xsl:template>

<xsl:template name="head">
  <xsl:choose>
    <xsl:when test="ancestor::tei:front">
      <xsl:number count="tei:div" from="tei:front" format="i" level="multiple"/>
    </xsl:when>
    <xsl:when test="ancestor::tei:body">
      <xsl:number count="tei:div" from="tei:body" format="1" level="multiple"/>
    </xsl:when>
    <xsl:when test="ancestor::tei:back">
      <xsl:number count="tei:div" from="tei:back" format="A.1.1" level="multiple"/>
    </xsl:when>
  </xsl:choose>
    <xsl:text> </xsl:text>
	<xsl:choose>
	  <xsl:when test="@type='other'">
	    <xsl:value-of select="tei:head"/>
	  </xsl:when>
	  <xsl:otherwise>
	    <span style="color:red">
	      <xsl:value-of select="tei:head"/>
	    </span>
	  </xsl:otherwise>
	</xsl:choose>
</xsl:template>
</xsl:stylesheet>

