<xsl:stylesheet version="1.0"
  xmlns:a="http://relaxng.org/ns/compatibility/annotations/1.0"
  xmlns:teix="http://www.tei-c.org/ns/Examples"
  xmlns:rng="http://relaxng.org/ns/structure/1.0"
  xmlns:estr="http://exslt.org/strings"
  xmlns:pantor="http://www.pantor.com/ns/local"
  xmlns:exsl="http://exslt.org/common"
  xmlns:tei="http://www.tei-c.org/ns/1.0"
  xmlns:edate="http://exslt.org/dates-and-times"
  extension-element-prefixes="exsl estr edate"
  exclude-result-prefixes="exsl rng edate estr tei a pantor teix" 
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
<xsl:template match="/">
  <xsl:for-each select=".//tei:elementSpec">
    <exsl:document href="Split/{@ident}.xml">
      <xsl:copy-of select="."/>
    </exsl:document>
  </xsl:for-each>
  <xsl:for-each select=".//tei:macroSpec">
    <exsl:document href="Split/{@ident}.xml">
      <xsl:copy-of select="."/>
    </exsl:document>
  </xsl:for-each>
  <xsl:for-each select=".//tei:classSpec">
    <exsl:document href="Split/{@ident}.xml">
      <xsl:copy-of select="."/>
    </exsl:document>
  </xsl:for-each>
  <xsl:for-each select=".//tei:moduleSpec">
    <exsl:document href="Split/{@ident}.xml">
      <xsl:copy-of select="."/>
    </exsl:document>
  </xsl:for-each>
</xsl:template>
</xsl:stylesheet>

