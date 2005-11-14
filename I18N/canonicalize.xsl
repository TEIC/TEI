<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet 
    xmlns:tei="http://www.tei-c.org/ns/1.0" 
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform" 
    version="1.0" exclude-result-prefixes="tei">
<xsl:output method="xml"/>

<xsl:param name="oddFile"/>
<xsl:key name="ALTS" match="tei:elementSpec/tei:altIdent" use="."/>
<xsl:key name="ALTATTS" match="tei:attDef/tei:altIdent"
	 use="concat(ancestor::tei:elementSpec/@ident,':',.)"/>
<xsl:key name="ALTATTS2" match="tei:attDef/tei:altIdent" 
	 use="concat(ancestor::tei:elementSpec/tei:altIdent,':',.)"/>


<xsl:template match="/">
  <xsl:if test="$oddFile=''">
    <xsl:message terminate="yes">Error, oddFile must be supplied</xsl:message>
  </xsl:if>
  <xsl:apply-templates/>
</xsl:template>

<xsl:template match="*">
  <xsl:variable name="n">
    <xsl:value-of select="local-name()"/>
  </xsl:variable>
  <xsl:variable name="e">
    <xsl:for-each select="document($oddFile)">
      <xsl:choose>
	<xsl:when test="key('ALTS',$n)">
	  <xsl:value-of select="key('ALTS',$n)/../@ident"/>
	</xsl:when>
	<xsl:otherwise>
	  <xsl:value-of select="$n"/>
	</xsl:otherwise>
      </xsl:choose>
    </xsl:for-each>
  </xsl:variable>
  <xsl:element name="{$e}">
    <xsl:apply-templates select="*|@*|processing-instruction()|comment()|text()"/>
  </xsl:element>
</xsl:template>

<xsl:template match="@*">
  <xsl:variable name="n">
    <xsl:value-of select="local-name()"/>
  </xsl:variable>
  <xsl:variable name="composite">
    <xsl:value-of select="local-name(..)"/>
    <xsl:text>:</xsl:text>
    <xsl:value-of select="local-name(.)"/>
  </xsl:variable>
  <xsl:variable name="e">
    <xsl:for-each select="document($oddFile)">
      <xsl:choose>
	<xsl:when test="key('ALTATTS',$composite)">
	  <xsl:for-each select="key('ALTATTS',$composite)">
	    <xsl:value-of select="../@ident"/>
	  </xsl:for-each>
	</xsl:when>
	<xsl:when test="key('ALTATTS2',$composite)">
	  <xsl:for-each select="key('ALTATTS2',$composite)">
	    <xsl:value-of select="../@ident"/>
	  </xsl:for-each>
	</xsl:when>
	<xsl:otherwise>
	  <xsl:value-of select="$n"/>
	</xsl:otherwise>
      </xsl:choose>
    </xsl:for-each>
  </xsl:variable>
  <xsl:attribute name="{$e}">
    <xsl:value-of select="."/>
  </xsl:attribute>
</xsl:template>

<xsl:template match="text()|processing-instruction()|comment()">
  <xsl:copy/>
</xsl:template>

</xsl:stylesheet>

