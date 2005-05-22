<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet 
 xmlns:rng="http://relaxng.org/ns/structure/1.0"
  xmlns:tei="http://www.tei-c.org/ns/1.0"
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
  xmlns:exsl="http://exslt.org/common"
  extension-element-prefixes="exsl"
  exclude-result-prefixes="tei exsl" 
  version="1.0">
<xsl:output method="xml" indent="no"/>
<xsl:param name="lang">es</xsl:param>
<xsl:key name="ELEMENTS" 
	 match="element/equiv"
	 use="concat(@xml:lang,':',@value)"/>
<xsl:key name="ATTRIBUTES" 
	 match="attribute/equiv" 
	 use="concat(@xml:lang,':',@value)"/>
<xsl:param name="verbose">true</xsl:param>
<xsl:param name="TEISERVER">http://localhost/Query/i18n.xq</xsl:param>

<xsl:template match="comment()|text()|processing-instruction()">
  <xsl:copy/>
</xsl:template>

<xsl:template match="tei:*">
  <xsl:variable name="oldname"><xsl:value-of select="local-name()"/></xsl:variable>
  <xsl:variable name="search">
    <xsl:value-of select="$lang"/>:<xsl:value-of select="local-name()"/>
  </xsl:variable>
  <xsl:variable name="newname">
    <xsl:for-each select="document($TEISERVER)/i18n">
      <xsl:value-of select="key('ELEMENTS',$search)/parent::element/@ident"/>
    </xsl:for-each>
  </xsl:variable>
  <xsl:choose>
    <xsl:when test="$newname=''">
      <xsl:element name="{$oldname}"
		   xmlns="http://www.tei-c.org/ns/1.0">
	<xsl:apply-templates select="@*|*|text()|comment()"/>
      </xsl:element>
    </xsl:when>
    <xsl:otherwise>
      <xsl:element name="{$newname}"
		   xmlns="http://www.tei-c.org/ns/1.0">
	<xsl:apply-templates select="@*|*|text()|comment()"/>
      </xsl:element>
    </xsl:otherwise>
  </xsl:choose>
</xsl:template>

<xsl:template match="@*">
  <xsl:variable name="oldname" select="name(.)"/>
  <xsl:variable name="search">
    <xsl:value-of select="$lang"/>:<xsl:value-of select="name()"/>
  </xsl:variable>
  <xsl:variable name="newname">
    <xsl:for-each
     select="document($TEISERVER)/i18n">
      <xsl:value-of select="key('ATTRIBUTES',$search)/parent::attribute/@ident"/>
    </xsl:for-each>
  </xsl:variable>
  <xsl:choose>
    <xsl:when test="$newname=''">
      <xsl:attribute name="{$oldname}">
	<xsl:value-of select="."/>
      </xsl:attribute>
    </xsl:when>
    <xsl:otherwise>
      <xsl:attribute name="{$newname}">
	<xsl:value-of select="."/>
      </xsl:attribute>
    </xsl:otherwise>
  </xsl:choose>
</xsl:template>



</xsl:stylesheet>
