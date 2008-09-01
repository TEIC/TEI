<xsl:stylesheet
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0"
  xmlns:fo="http://www.w3.org/XSL/Format/1.0"
  indent-result="yes"
  result-ns="">

<xsl:template match="math">
  <xsl:apply-templates mode="math"/>
</xsl:template>

<xsl:template match="*|@*|comment()|processing-instruction()|text()" mode="math">
      <xsl:copy>
          <xsl:apply-templates mode="math" select="*|@*|processing-instruction()|text()"/>
      </xsl:copy>
</xsl:template>

</xsl:stylesheet>
