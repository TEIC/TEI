<xsl:stylesheet 
    xmlns:svg="http://www.w3.org/2000/svg" 
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform" 
  version="1.0"
>
<xsl:output method="xml" indent="yes"/>

<xsl:template match="*|@*|processing-instruction()">
 <xsl:copy>
  <xsl:apply-templates select="*|@*|processing-instruction()|comment()|text()"/>
 </xsl:copy>
</xsl:template>

<xsl:template match="text()">
    <xsl:value-of select="."/> <!-- could normalize() here -->
</xsl:template>

<xsl:template match="svg:rect">
  <xsl:copy>
    <xsl:copy-of select="@x"/>
    <xsl:copy-of select="@y"/>
    <xsl:copy-of select="@width"/>
    <xsl:copy-of select="@height"/>
    <xsl:copy-of select="@style"/>
  </xsl:copy>
  <svg:view
	id="{@id}" 
	viewBox="{@x} {@y} {@width} {@height}"/>

</xsl:template>

</xsl:stylesheet>
