<xsl:stylesheet 
 xmlns:teix="http://www.tei-c.org/ns/Examples"
 xmlns:tei="http://www.tei-c.org/ns/1.0"
 xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
 xmlns:rng="http://relaxng.org/ns/structure/1.0"
 extension-element-prefixes="exsl"
 exclude-result-prefixes="exsl teix rng tei"
 xmlns:exsl="http://exslt.org/common"
 version="1.0">

<xsl:key name="MODS" match="tei:moduleSpec" use="1"/>


<xsl:template match="/">
  <head>
  <xsl:for-each select="key('MODS',1)">
    <xsl:sort select="@ident"/>
    <head xml:lang="zh-tw" corresp="{ancestor::tei:div[last()]/@xml:id}">
      <xsl:value-of select="tei:desc"/>
    </head>
  </xsl:for-each>
  </head>
</xsl:template>

</xsl:stylesheet>
