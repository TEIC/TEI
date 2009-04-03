<xsl:stylesheet 
  version="1.0"
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform" 
  xmlns:rng="http://relaxng.org/ns/structure/1.0"
  xmlns:teix="http://www.tei-c.org/ns/Examples"
  xmlns:tei="http://www.tei-c.org/ns/1.0"
  exclude-result-prefixes="tei teix rng">

<xsl:output method="xml" indent="yes" encoding="utf-8"/>


<xsl:key match="tei:bibl" use="'1'" name="BIBLS"/>

<xsl:template match="/">
<div xmlns="http://www.tei-c.org/ns/1.0">
 <listBibl>
  <xsl:for-each select="key('BIBLS',1)">
    <xsl:if test="ancestor::tei:body">
      <xsl:copy>
	<xsl:copy-of select="@*"/>
	<xsl:attribute name="xml:id">
	  <xsl:value-of select="ancestor::tei:div/@xml:id"/>
	  <xsl:text>-BIBL-</xsl:text>
	  <xsl:number level="any" from="tei:body/tei:div"/>
	</xsl:attribute>
	<xsl:copy-of select="*|text()|comment()"/>
      </xsl:copy>
    </xsl:if>
  </xsl:for-each>
 </listBibl>
</div>
</xsl:template>
</xsl:stylesheet>