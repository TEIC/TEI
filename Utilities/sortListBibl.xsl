<xsl:stylesheet version="1.0"
	  xmlns:tei="http://www.tei-c.org/ns/1.0"
	  xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
<xsl:output method="xml" encoding="utf-8" indent="yes"/>

<xsl:template match="tei:listBibl|comment()">
  <xsl:copy>
    <xsl:for-each select="tei:bibl">
      <xsl:sort select="(tei:author|tei:title)[1]"/>
      <xsl:copy-of select="."/>
    </xsl:for-each>
  </xsl:copy>
</xsl:template>
</xsl:stylesheet>
