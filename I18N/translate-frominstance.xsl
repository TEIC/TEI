<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet 
 xmlns:rng="http://relaxng.org/ns/structure/1.0"
  xmlns:tei="http://www.tei-c.org/ns/1.0"
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
  xmlns:exsl="http://exslt.org/common"
  extension-element-prefixes="exsl"
  exclude-result-prefixes="tei exsl" 
  version="1.0">
<xsl:output method="xml" indent="yes"/>
<xsl:param name="lang">es</xsl:param>
<xsl:param name="ODDROOT">/TEI/P5/</xsl:param>
<xsl:key name="ELEMENTS" match="element"
	 use="concat(concat(equiv/@lang,':'),equiv/@value)"/>
<xsl:key name="ATTRIBUTES" match="attribute" use="concat(concat(equiv/@lang,':'),equiv/@value)"/>
<xsl:param name="verbose">true</xsl:param>
<xsl:variable name="TEINAMES">http://www.tei-c.org.uk/tei-bin/files.pl?name=teinames.xml</xsl:variable>
<xsl:template match="comment()|text()|processing-instruction()">
  <xsl:copy/>
</xsl:template>

<xsl:template match="tei:*">
  <xsl:variable name="oldname" select="name(.)"/>
  <xsl:variable name="newname">
    <xsl:for-each
     select="document($TEINAMES)/i18n">
      <xsl:value-of select="key('ELEMENTS',concat(concat($lang,':'),$oldname))/@ident"/>
    </xsl:for-each>
  </xsl:variable>
  <xsl:choose>
    <xsl:when test="$newname=''">
      <xsl:element name="{$oldname}" xmlns="http://www.tei-c.org/ns/1.0">
	<xsl:apply-templates select="@*|*|text()|comment()"/>
      </xsl:element>
    </xsl:when>
    <xsl:otherwise>
      <xsl:element name="{$newname}" xmlns="http://www.tei-c.org/ns/1.0">
	<xsl:apply-templates select="@*|*|text()|comment()"/>
      </xsl:element>
    </xsl:otherwise>
  </xsl:choose>
</xsl:template>

<xsl:template match="@*">
  <xsl:variable name="oldname" select="name(.)"/>
  <xsl:variable name="newname">
    <xsl:for-each
     select="document($TEINAMES)/i18n">
      <xsl:value-of select="key('ATTRIBUTES',concat(concat($lang,':'),$oldname))/@ident"/>
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