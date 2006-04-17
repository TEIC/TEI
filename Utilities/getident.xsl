<xsl:stylesheet version="1.0"
	  xmlns:tei="http://www.tei-c.org/ns/1.0"
	  xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
<xsl:output method="text"/>
<xsl:template match="/">
  <xsl:value-of select="*[1]/@ident"/>
</xsl:template>
</xsl:stylesheet>
