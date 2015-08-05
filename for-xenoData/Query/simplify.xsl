<xsl:stylesheet version="1.0"
  xmlns:a="http://relaxng.org/ns/compatibility/annotations/1.0"
  xmlns:teix="http://www.tei-c.org/ns/Examples"
  xmlns:rng="http://relaxng.org/ns/structure/1.0"
  xmlns:estr="http://exslt.org/strings"
  xmlns:exsl="http://exslt.org/common"
  xmlns:tei="http://www.tei-c.org/ns/1.0"
  xmlns:edate="http://exslt.org/dates-and-times"
  extension-element-prefixes="exsl estr edate"
  exclude-result-prefixes="exsl rng edate estr tei a teix" 
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
  <xsl:template match="/tei:TEI">
    <xsl:copy>
    <xsl:copy-of select="tei:teiHeader"/>
      <xsl:for-each select="tei:text/tei:body/tei:div">
	<xsl:copy>
      <xsl:copy-of select="@*"/>
      <xsl:copy-of
	  select=".//tei:elementSpec|.//tei:moduleSpec|.//tei:classSpec|.//tei:macroSpec"/>
	</xsl:copy>
      </xsl:for-each>
    </xsl:copy>
  </xsl:template>
</xsl:stylesheet>
