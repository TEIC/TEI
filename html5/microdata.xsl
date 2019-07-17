<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet 
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform" 
    version="2.0"
    xpath-default-namespace="http://www.tei-c.org/ns/1.0">

  <xsl:template name="microdata">
    <xsl:choose>
      <xsl:when test="self::p"/>
      <xsl:when test="starts-with(local-name(),'div')"/>
      <!--
	  <xsl:when test="starts-with(local-name(),'div')">
	  <xsl:attribute name="itemscope">itemscope</xsl:attribute>
	  <xsl:attribute name="itemtype">http://www.tei-c.org/ns/1.0/</xsl:attribute>
	  </xsl:when>
      -->
      <xsl:when test="self::docDate|self::date">
	<xsl:attribute name="itemprop">date</xsl:attribute>
      </xsl:when>
      <xsl:otherwise>
	<xsl:attribute name="itemprop" select="local-name()"/>
      </xsl:otherwise>
    </xsl:choose>
    <xsl:if test="@when">
      <xsl:attribute name="data-when">
	<xsl:value-of select="@when"/>
      </xsl:attribute>
    </xsl:if>
  </xsl:template>

  <xsl:template name="bodyMicroData">
    <xsl:attribute name="itemscope">itemscope</xsl:attribute>
    <xsl:attribute name="itemtype">http://www.tei-c.org/ns/1.0/</xsl:attribute>
    <xsl:attribute name="itemprop">TEI</xsl:attribute>
  </xsl:template>

</xsl:stylesheet>
