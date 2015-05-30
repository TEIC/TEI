<xsl:stylesheet version="1.0"
	  xmlns:rng="http://relaxng.org/ns/structure/1.0"
	  xmlns:tei="http://www.tei-c.org/ns/1.0"
	  exclude-result-prefixes="rng tei"
	  xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
<xsl:output 
   method="xml"
   encoding="utf-8"
   indent="yes"
   cdata-section-elements="tei:eg"/>

<xsl:template match="tei:gloss[not(@xml:lang)  and
		     string-length(.)=0]"/>

<xsl:template match="tei:xgloss[@xml:lang]">
  <xsl:choose>
  <xsl:when test="../tei:gloss[not(@xml:lang) and
		string-length(.)=0] and not(contains(.),' ')">
    <xsl:message>zap <xsl:value-of 
select="@xml:lang"/> gloss on <xsl:value-of select="../@ident"/></xsl:message>
  </xsl:when>
  <xsl:otherwise>
    <xsl:copy>
      <xsl:apply-templates select="@*"/>
      <xsl:apply-templates 
	  select="*|comment()|processing-instruction()|text()"/>
    </xsl:copy>
  </xsl:otherwise>
  </xsl:choose>
</xsl:template>

<xsl:template match="*">
 <xsl:copy>
  <xsl:apply-templates select="@*"/>
  <xsl:apply-templates 
      select="*|comment()|processing-instruction()|text()"/>
 </xsl:copy>
</xsl:template>

<xsl:template match="@*|processing-instruction()|text()|comment()">
  <xsl:copy/>
</xsl:template>

</xsl:stylesheet>
