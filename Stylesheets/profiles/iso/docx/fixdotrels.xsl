<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet 
    version="2.0"
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    xmlns:r="http://schemas.openxmlformats.org/package/2006/relationships"
    exclude-result-prefixes="r">

  <xsl:output  encoding="UTF-8" standalone="yes" method="xml"/>

  <!-- identity transform -->
  
  <xsl:template match="@*|text()|comment()|processing-instruction()" >
    <xsl:copy-of select="."/>
  </xsl:template>
  
  <xsl:template match="*" >
    <xsl:copy>
      <xsl:apply-templates select="*|@*|processing-instruction()|comment()|text()" 
			   />
    </xsl:copy>
  </xsl:template>

  <!-- do not copy custom xml rel -->
  <xsl:template match="*[@Target='docProps/custom.xml']"/>
</xsl:stylesheet>
