<xsl:stylesheet 
 xmlns:tei="http://www.tei-c.org/ns/1.0"
 xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
 xmlns:rng="http://relaxng.org/ns/structure/1.0"
 extension-element-prefixes="exsl"
 xmlns:exsl="http://exslt.org/common"
 version="1.0">

<xsl:output method="text"/>
<xsl:key name="CLASSDOCS" match="tei:classDoc" use="1"/>
<xsl:key name="PATDOCS" match="tei:tagDoc" use="1"/>
<xsl:key name="TAGDOCS" match="tei:patternDoc" use="1"/>

<xsl:template match="/">
  <xsl:for-each select="key('CLASSDOCS',1)">
    <xsl:value-of select="@id"/><xsl:text> </xsl:text>  <xsl:value-of select="tei:ident"/>
    <xsl:text> 
</xsl:text>
</xsl:for-each>
  <xsl:for-each select="key('PATDOCS',1)">
    <xsl:value-of select="@id"/><xsl:text> </xsl:text>  <xsl:value-of select="tei:ident"/>
    <xsl:text> 
</xsl:text>
</xsl:for-each>
  <xsl:for-each select="key('TAGDOCS',1)">
    <xsl:value-of select="@id"/><xsl:text> </xsl:text>  <xsl:value-of select="tei:ident"/>
    <xsl:text> 
</xsl:text>
</xsl:for-each>
</xsl:template>
</xsl:stylesheet>







