<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet xmlns="http://www.tei-c.org/ns/1.0"
    xmlns:teix="http://www.tei-c.org/ns/Examples"
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
    exclude-result-prefixes="exsl edate estr tei t a rng s teix" 
    version="1.0">
  
  <xsl:output indent="yes"/>
  <xsl:key name="E" match="tei:elementSpec" use="'1'"/>

<xsl:template match="/">
  <choice xmlns="http://relaxng.org/ns/structure/1.0">
    <xsl:for-each select="key('E','1')">
      <xsl:sort select="@ident"/>
      <name><xsl:value-of select="@ident"/></name>
    </xsl:for-each>
  </choice>
</xsl:template>

</xsl:stylesheet>



