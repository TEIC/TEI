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

<xsl:key  name="ATTS" match="tei:elementSpec//tei:attDef"  use="@ident"/>

<xsl:key  name="CLATTS" match="tei:classSpec//tei:attDef"  use="@ident"/>

<xsl:key  name="A" match="tei:elementSpec//tei:attDef"  use="'all'"/>


<xsl:template match="/">
<table>
<row role="label">
<cell>Attribute</cell>
<cell>Datatype</cell>
<cell>Elements</cell>
<cell>Class</cell>
</row>
<xsl:for-each select="key('A','all')">
  <xsl:sort select="@ident"/>
  <xsl:if test="generate-id()=generate-id(key('ATTS',@ident)[1]) and count(key('ATTS',@ident))&gt;1">
    <row>
      <cell><xsl:value-of select="@ident"/></cell>
      <cell><xsl:value-of select="tei:datatype/rng:ref/@name"/></cell>
      <cell>
	<xsl:for-each select="key('ATTS',@ident)">
	  <xsl:value-of select="ancestor::tei:elementSpec/@ident"/>:
      </xsl:for-each>
      </cell>
      <cell>
	<xsl:for-each select="key('CLATTS',@ident)">
	  <xsl:value-of select="ancestor::tei:classSpec/@ident"/>:
	</xsl:for-each>
      </cell>
    </row>
  </xsl:if>
</xsl:for-each>
</table>
</xsl:template>

</xsl:stylesheet>


