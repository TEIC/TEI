<?xml version="1.0" encoding="utf-8"?>

<!-- Extract list of chapter titles, etc., from the Guidelines source -->

<!-- Copyleft 2006 Syd Bauman and the Text Encoding Initiative Consortium -->

<!-- usage: -->
<!-- xmllint &#2D;&#2D;noent &#x2D;&#x2D;xinclude /path/to/P5/Source/Guidelines/en/guidelines-en.xml | xsltproc /path/to/me.xslt -->

<xsl:stylesheet version="1.0"
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform" 
  xmlns:rng="http://relaxng.org/ns/structure/1.0"
  xmlns:tei="http://www.tei-c.org/ns/1.0" >

  <xsl:output method="text"/>

  
  <xsl:template match="/">
    <!-- only process <div1>s; ignore everything else -->
    <xsl:apply-templates select="/tei:TEI/tei:text/tei:body/tei:div0/tei:div1"/>
  </xsl:template>
  
  <xsl:template match="/tei:TEI/tei:text/tei:body/tei:div0/tei:div1">
    <!-- When we hit a <div1>, remember it's position and ... -->
    <xsl:variable name="num" select="position()"/>
    <!-- ... write out information about it. -->
    <xsl:text>The </xsl:text>
    <!-- if this <div1> comes before the 10th, output a blank so -->
    <!-- that things line up nicer -->
    <xsl:if test="$num&lt;10"><xsl:text> </xsl:text></xsl:if>
    <!-- write out the ordinal number of this <div1> -->
    <xsl:choose>
      <xsl:when test="$num=1">1st</xsl:when>
      <xsl:when test="$num=2">2nd</xsl:when>
      <xsl:when test="$num=3">3rd</xsl:when>
      <xsl:otherwise><xsl:value-of select="$num"/><xsl:text>th</xsl:text></xsl:otherwise>
    </xsl:choose>
    <xsl:text> &lt;div1> (</xsl:text>
    <!-- in parens put the it's unique identifier, i.e. that which used -->
    <!-- to be its filename -->
    <xsl:value-of select="@xml:id"/>
    <xsl:text>) has n='</xsl:text>
    <!-- folks may also be interested in the value of its n= attribute -->
    <xsl:value-of select="@n"/>
    <xsl:text>': "</xsl:text>
    <!-- give the title of the chapter in quotes -->
    <xsl:value-of select="tei:head"/>
    <xsl:text>"&#10;</xsl:text>
  </xsl:template>
</xsl:stylesheet>
