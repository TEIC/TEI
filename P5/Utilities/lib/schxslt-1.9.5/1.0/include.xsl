<xsl:transform version="1.0"
               xmlns:sch="http://purl.oclc.org/dsdl/schematron"
               xmlns:xsl="http://www.w3.org/1999/XSL/Transform">

  <xsl:template match="node() | @*">
    <xsl:copy>
      <xsl:apply-templates select="node() | @*"/>
    </xsl:copy>
  </xsl:template>

  <xsl:template match="sch:extends[@href]">
    <xsl:choose>
      <xsl:when test="contains(@href, '#')">
        <xsl:if test="local-name(..) = local-name(document(substring-before(@href, '#'), @href)//*[@xml:id = substring-after(current()/@href, '#')])">
          <xsl:if test="namespace-uri(document(substring-before(@href, '#'), @href)//*[@xml:id = substring-after(current()/@href, '#')]) = 'http://purl.oclc.org/dsdl/schematron'">
            <xsl:apply-templates select="document(substring-before(@href, '#'), @href)//*[@xml:id = substring-after(current()/@href, '#')]/*"/>
          </xsl:if>
        </xsl:if>
      </xsl:when>
      <xsl:otherwise>
        <xsl:if test="local-name(..) = local-name(document(@href)/*) and namespace-uri(document(@href)/*) = 'http://purl.oclc.org/dsdl/schematron'">
          <xsl:apply-templates select="document(@href)/*/*"/>
        </xsl:if>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>

  <xsl:template match="sch:include">
    <xsl:choose>
      <xsl:when test="contains(@href, '#')">
        <xsl:apply-templates select="document(substring-before(@href, '#'), @href)//*[@xml:id = substring-after(current()/@href, '#')]"/>
      </xsl:when>
      <xsl:otherwise>
        <xsl:apply-templates select="document(@href)"/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>

</xsl:transform>
