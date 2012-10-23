<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet                 
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    version="2.0">


  <xsl:template match="text()|@*">
      <xsl:copy-of select="."/>
  </xsl:template>

  <xsl:template match="*">
      <xsl:copy>
         <xsl:apply-templates select="*|@*|text()"/>
      </xsl:copy>
  </xsl:template>
  
</xsl:stylesheet>