<xsl:stylesheet version="2.0"
	  xmlns:tei="http://www.tei-c.org/ns/1.0"
	  xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
<xsl:output method="text"/>
<xsl:key name="IDENTS" match="tei:elementSpec[@ident]" use="1"/>
<xsl:key name="IDENTS" match="tei:classSpec[@ident]" use="1"/>
<xsl:key name="IDENTS" match="tei:macroSpec[@ident]" use="1"/>
<xsl:template match="/">
 <xsl:for-each select="key('IDENTS',1)">
   <xsl:value-of select="@module"/>
<xsl:text> </xsl:text>
   <xsl:value-of select="@ident"/>
   <xsl:text>&#10;</xsl:text>
 </xsl:for-each>
</xsl:template>
</xsl:stylesheet>
