<xsl:stylesheet 
    xmlns:tei="http://www.tei-c.org/ns/1.0"
    xmlns:kml="http://earth.google.com/kml/2.2"
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform" 
    xmlns:rng="http://relaxng.org/ns/structure/1.0"
    version="1.0"
    >
  <xsl:template match="/">
    <Document 
	xmlns="http://earth.google.com/kml/2.2"
	xmlns:xlink="http://www.w3/org/1999/xlink">
      <xsl:for-each select=".//tei:place">
      <name><xsl:value-of select="tei:head"/></name>
      <visibility>1</visibility>
	<xsl:copy-of select="tei:location/kml:*"/>
      </xsl:for-each>
    </Document>
  </xsl:template>

</xsl:stylesheet>
  




