<xsl:stylesheet
 xmlns="http://www.tei-c.org/ns/1.0"
 xmlns:tei="http://www.tei-c.org/ns/1.0"
 xmlns:tite="http://www.tei-c.org/ns/tite/1.0"
 xmlns:xsl="http://www.w3.org/1999/XSL/Transform" 
 exclude-result-prefixes="xsl tei"    
 version="1.0">

<xsl:template name="b">
  <hi rend="bold">
    <xsl:apply-templates 
	select="@*|*|text()|comment()|processing-instruction"/>  
  </hi>
</xsl:template>

<xsl:template name="i">
  <hi rend="italic">
    <xsl:apply-templates 
	select="@*|*|text()|comment()|processing-instruction"/>  
  </hi>
</xsl:template>

<xsl:template name="ul">
  <hi rend="underline">
    <xsl:apply-templates 
	select="@*|*|text()|comment()|processing-instruction"/>  
  </hi>
</xsl:template>

<xsl:template name="sup">
  <hi rend="sup">
    <xsl:apply-templates 
	select="@*|*|text()|comment()|processing-instruction"/>  
  </hi>
</xsl:template>

<xsl:template name="sub">
  <hi rend="sub">
    <xsl:apply-templates 
	select="@*|*|text()|comment()|processing-instruction"/>  
  </hi>
</xsl:template>

<xsl:template name="smcap">
  <hi rend="smcap">
    <xsl:apply-templates 
	select="@*|*|text()|comment()|processing-instruction"/>  
  </hi>
</xsl:template>

</xsl:stylesheet>
