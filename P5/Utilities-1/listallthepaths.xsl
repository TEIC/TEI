<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    xmlns:rng="http://relaxng.org/ns/structure/1.0"
    version="1.0">
<xsl:key name="E" match="rng:define" use="@name"/>
<xsl:param name="root">TEI</xsl:param>
<xsl:variable
   name="dots">........................................................................</xsl:variable>


<xsl:template match="/">
<xsl:for-each select="key('E',$root)">
  <xsl:message>START: <xsl:value-of select="@name"/></xsl:message>
  <xsl:for-each select=".//rng:ref">
    <xsl:call-template name="doRef">
    <xsl:with-param name="depth">1</xsl:with-param>
    <xsl:with-param name="trail"> TEI </xsl:with-param>
    </xsl:call-template>
  </xsl:for-each>
</xsl:for-each>
</xsl:template>

<xsl:template name="doRef">
    <xsl:param name="depth"/>
    <xsl:param name="trail"/>
<!--  <xsl:message>[ follow <xsl:value-of select="@name"/> noting <xsl:value-of select="$trail"/>]</xsl:message>-->

<xsl:if test="not(contains(@name,'.attribute'))">
  <xsl:for-each select="key('E',@name)">
    <xsl:if test="rng:element">
      <xsl:message><xsl:value-of select="substring($dots,1,$depth)"/>Element <xsl:value-of select="@name"/></xsl:message>
    </xsl:if>
    <xsl:for-each select=".//rng:ref">
      <xsl:if test="not(contains($trail,concat(' ',@name, ' ')))">
	<xsl:call-template name="doRef">
	  <xsl:with-param name="depth">
	    <xsl:value-of select="$depth + 1"/>
	  </xsl:with-param>
	  <xsl:with-param name="trail">
	    <xsl:value-of select="$trail"/>
	    <xsl:text> </xsl:text>
	    <xsl:value-of select="@name"/>
	  </xsl:with-param>
	</xsl:call-template>
      </xsl:if>
    </xsl:for-each>
  </xsl:for-each>
</xsl:if>
</xsl:template>

</xsl:stylesheet> 