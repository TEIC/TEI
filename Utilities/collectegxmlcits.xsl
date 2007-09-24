<xsl:stylesheet 
  version="1.0"
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform" 
  xmlns:rng="http://relaxng.org/ns/structure/1.0"
  xmlns:teix="http://www.tei-c.org/ns/Examples"
  xmlns:tei="http://www.tei-c.org/ns/1.0"
  exclude-result-prefixes="tei teix rng">

<xsl:output method="xml" indent="yes" encoding="utf-8"/>


<xsl:key match="teix:egXML" use="'1'" name="EGXMLS"/>

<xsl:template match="/">
<div xmlns="http://www.tei-c.org/ns/1.0">
 <listBibl>

  <xsl:for-each select="key('EGXMLS',1)">
    <xsl:variable name="x">
      <xsl:value-of select="ancestor::tei:div[@xml:id][1]/@xml:id"/>
      <xsl:text>-</xsl:text>
      <xsl:number level="any"/>
    </xsl:variable>
    <xsl:variable name="content">
      <xsl:call-template name="comment"/>
    </xsl:variable>
    <xsl:if test="not(normalize-space($content) = '')">
      <bibl>
	<xsl:attribute name="xml:id">
	  <xsl:value-of select="$x"/>
	</xsl:attribute>
	<xsl:value-of select="$content"/>
      </bibl>
    </xsl:if>
  </xsl:for-each>
 </listBibl>
</div>
</xsl:template>

<xsl:template name="comment">
  <xsl:for-each select="following-sibling::node()[1]">
    <xsl:choose>
      <xsl:when test="self::comment()">
	<xsl:value-of select="."/>
	<xsl:call-template name="comment"/>
      </xsl:when>
      <xsl:when test="self::text() and normalize-space(.)=''">
	<xsl:call-template name="comment"/>
      </xsl:when>
    </xsl:choose>
  </xsl:for-each>
</xsl:template>
</xsl:stylesheet>