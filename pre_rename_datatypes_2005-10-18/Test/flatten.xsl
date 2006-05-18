<xsl:stylesheet
 xmlns:tei="http://www.tei-c.org/ns/1.0"
 xmlns:xsl="http://www.w3.org/1999/XSL/Transform" 
 xmlns="http://www.w3.org/1999/xhtml"
 version="1.0">

  <xsl:template match="tei:teiHeader">
    <xsl:copy-of select="."/>
  </xsl:template>

  <xsl:template match="tei:TEI">
    <xsl:copy>
      <xsl:apply-templates select="@*|*|text()|comment()"/>
    </xsl:copy>
  </xsl:template>

  <xsl:template match="tei:*">
    <xsl:element name="{local-name()}"
		 xmlns="http://www.tei-c.org/ns/1.0">
    <xsl:apply-templates select="@*"/>
    </xsl:element>
    <xsl:apply-templates select="*|text()|comment()"/>
    <xsl:if test="*|text()">
      <xsl:element name="{concat(local-name(),'_')}"
		   xmlns="http://www.tei-c.org/ns/1.0"/>
    </xsl:if>
  </xsl:template>
  
  <xsl:template match="@*|text()|comment()">
    <xsl:copy-of select="."/>
  </xsl:template>
  
</xsl:stylesheet>
