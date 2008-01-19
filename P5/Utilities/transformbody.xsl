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


<xsl:template match="tei:div[@type='div1']/tei:head">
  <head xmlns="http://www.tei-c.org/ns/1.0">
    <xsl:apply-templates select="@*"/>
      <xsl:value-of select="."/>
  </head>
  <xsl:variable name="ID">
    <xsl:value-of select="parent::tei:div/@xml:id"/>
  </xsl:variable>
  <xsl:for-each select="document('frheads.xml')/x/head[@xml:id=$ID]">
    <head xmlns="http://www.tei-c.org/ns/1.0" xml:lang="fr">
      <xsl:value-of select="."/>
    </head>
  </xsl:for-each>
</xsl:template>
</xsl:stylesheet>
