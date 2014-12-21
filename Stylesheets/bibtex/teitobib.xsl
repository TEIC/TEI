<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                xpath-default-namespace="http://www.tei-c.org/ns/1.0"
                version="2.0">
  <xsl:import href="convertbib.xsl"/>
  <xsl:output method="text"/>
  <xsl:strip-space elements="*"/>
  <xsl:key name="B" match="biblStruct" use="1"/>
    <xsl:strip-space elements="*"/>

  <xsl:template match="/">
    <xsl:for-each select="key('B',1)">
      <xsl:call-template name="biblStruct2bibtex"/>
    </xsl:for-each>
  </xsl:template>

</xsl:stylesheet>
