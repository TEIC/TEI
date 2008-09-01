<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
 version="1.0"
  xmlns:saxon="http://icl.com/saxon"
  extension-element-prefixes="saxon">
<xsl:output indent="yes"/>
<xsl:template match="/">
  <xsl:for-each select="//eg">
    <saxon:output  method="xml" file="gob{generate-id()}.xml">
    <xsl:value-of 
    disable-output-escaping="yes" select="."/>    
    </saxon:output>
  </xsl:for-each>
</xsl:template>
</xsl:stylesheet>
