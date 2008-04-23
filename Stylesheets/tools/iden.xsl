<xsl:stylesheet 
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform" 
  version="1.0"
>
<xsl:output method="xml" indent="yes"/>

<xsl:template match="@*|text()|comment()|processing-instruction()">
 <xsl:copy-of select="."/>
</xsl:template>


<xsl:template match="*">
  <xsl:copy>
    <xsl:apply-templates 
	select="*|@*|processing-instruction()|comment()|text()"/>
  </xsl:copy>
</xsl:template>


</xsl:stylesheet>
