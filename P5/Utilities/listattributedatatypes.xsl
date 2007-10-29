<xsl:stylesheet version="1.0"
  xmlns:tei="http://www.tei-c.org/ns/1.0"
  xmlns:teix="http://www.tei-c.org/ns/Examples"
  xmlns:rng="http://relaxng.org/ns/structure/1.0"
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
  xmlns:XSL="http://www.w3.org/1999/xsl/TransformAlias" >

  <xsl:key name='A' match="tei:attDef" use="'1'"/>
  <xsl:key name='A2' match="tei:attDef" use="@ident"/>
  <xsl:output method="text"/>
<xsl:template match="tei:TEI">
  <xsl:for-each select="key('A',1)">
    <xsl:sort select="@ident"/>
    <xsl:if test="count(key('A2',@ident))&gt;1">
    <xsl:if test="tei:datatype/rng:ref/@name">
      <xsl:value-of select="@ident"/>
      <xsl:text>: </xsl:text>
      <xsl:value-of select="tei:datatype/rng:ref/@name"/>
      <xsl:text>: </xsl:text>
      <xsl:value-of select="ancestor::tei:elementSpec/@ident"/>
      <xsl:value-of select="ancestor::tei:classSpec/@ident"/>
      <xsl:text>&#10;</xsl:text>
    </xsl:if>
    </xsl:if>
  </xsl:for-each>
</xsl:template>
</xsl:stylesheet>

