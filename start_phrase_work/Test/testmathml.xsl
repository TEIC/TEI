<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet 
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
  xmlns:svg="http://www.w3.org/2000/svg"
  version="1.0">

  <xsl:import href="/P4/web/oucsng-preview/stylesheets/teihtml.xsl"/>

  <xsl:template match="svg:*">
    <xsl:message>
      SVG <xsl:value-of select="name()"/>
    </xsl:message>
    <xsl:copy-of select="."/>
  </xsl:template>
</xsl:stylesheet>
