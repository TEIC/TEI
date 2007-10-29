<xsl:stylesheet version="1.0"
	  xmlns:tei="http://www.tei-c.org/ns/1.0"
	  xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
<xsl:output method="text"/>
<xsl:template match="/tei:moduleSpec">
 <xsl:for-each select="*">
   <xsl:value-of select="@ident"/>
   <xsl:text>&#10;</xsl:text>
 </xsl:for-each>
</xsl:template>
</xsl:stylesheet>
