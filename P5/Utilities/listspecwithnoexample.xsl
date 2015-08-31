<xsl:stylesheet 
    version="2.0"
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform" 
    xmlns:teix="http://www.tei-c.org/ns/Examples"
    xmlns:tei="http://www.tei-c.org/ns/1.0"
    exclude-result-prefixes="tei teix">
  
  <xsl:output method="text" indent="yes" encoding="utf-8"/>
  
  
  <xsl:key match="tei:elementSpec" use="1" name="E"/>
  <xsl:key match="teix:*[not(ancestor::tei:elementSpec)]" 
	   use="local-name()" name="EX"/>
  <xsl:key match="teix:*[ancestor::tei:elementSpec]" 
	   use="local-name()" name="EXEL"/>
  
  <xsl:template match="/">
    <xsl:for-each select="key('E',1)">
      <xsl:sort select="@module"/>
      <xsl:sort select="@ident"/>
      <xsl:if test="not(@ident='egXML') and not(tei:exemplum/teix:egXML/*)">
	<xsl:text>* Error: elementSpec for </xsl:text>
	<xsl:value-of select="@ident"/>
	<xsl:text> has no proper example&#10;</xsl:text>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>
</xsl:stylesheet>
