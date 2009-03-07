<xsl:stylesheet version="2.0"
  xmlns:a="http://relaxng.org/ns/compatibility/annotations/1.0"
  xmlns:teix="http://www.tei-c.org/ns/Examples"
  xmlns:rng="http://relaxng.org/ns/structure/1.0"
  xmlns:pantor="http://www.pantor.com/ns/local"
  xmlns:exsl="http://exslt.org/common"
  xmlns:tei="http://www.tei-c.org/ns/1.0"
  exclude-result-prefixes="rng tei a pantor teix" 
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
<xsl:template match="/">
  <xsl:for-each select=".//tei:div1">
    <xsl:result-document href="p5-{@xml:id}.xml">
      <xsl:copy-of select="."/>
    </xsl:result-document>
  </xsl:for-each>
</xsl:template>
</xsl:stylesheet>

