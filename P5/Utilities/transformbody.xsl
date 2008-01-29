<xsl:stylesheet 
    xmlns:s="http://www.ascc.net/xml/schematron" 
    xmlns:teix="http://www.tei-c.org/ns/Examples"
    xmlns:tei="http://www.tei-c.org/ns/1.0"
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    xmlns:rng="http://relaxng.org/ns/structure/1.0"
    extension-element-prefixes="exsl"
    exclude-result-prefixes="s exsl teix rng tei"
    xmlns:exsl="http://exslt.org/common"
    version="1.0">

<xsl:output 
   method="xml"
   indent="yes"
   encoding="utf-8"
   cdata-section-elements="tei:eg"/>


<xsl:template match="tei:moduleSpec">
  <moduleSpec xmlns="http://www.tei-c.org/ns/1.0">
    <xsl:apply-templates select="@*"/>
    <xsl:apply-templates select="*|comment()|text()"/>
    <xsl:variable name="ID">
      <xsl:value-of select="ancestor::tei:div[last()]/@xml:id"/>
    </xsl:variable>
    <xsl:for-each select="document('heads.xml')//head[@corresp=$ID]">
<xsl:message>doing for <xsl:value-of select="$ID"/></xsl:message>
      <desc xmlns="http://www.tei-c.org/ns/1.0">
	<xsl:copy-of select="@xml:lang"/>
	<xsl:value-of select="."/>
      </desc>
  </xsl:for-each>
  </moduleSpec>
</xsl:template>

</xsl:stylesheet>
