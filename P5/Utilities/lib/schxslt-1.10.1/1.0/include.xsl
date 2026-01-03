<xsl:transform version="1.0"
               xmlns:schxslt="https://doi.org/10.5281/zenodo.1495494"
               xmlns:sch="http://purl.oclc.org/dsdl/schematron"
               xmlns:xsl="http://www.w3.org/1999/XSL/Transform">

  <xsl:template match="*">
    <xsl:param name="sourceLang"/>

    <xsl:variable name="thisLang">
      <xsl:call-template name="schxslt:in-scope-language"/>
    </xsl:variable>

    <xsl:copy>
      <xsl:if test="not(@xml:lang) and $thisLang != $sourceLang">
        <xsl:attribute name="xml:lang">
          <xsl:value-of select="$thisLang"/>
        </xsl:attribute>
      </xsl:if>
      <xsl:apply-templates select="@*"/>
      <xsl:apply-templates select="node()"/>
    </xsl:copy>
  </xsl:template>

  <xsl:template match="comment() | processing-instruction() | @*">
    <xsl:copy>
      <xsl:apply-templates select="node() | @*"/>
    </xsl:copy>
  </xsl:template>

  <xsl:template match="sch:extends[@href][contains(@href, '#')]" priority="10">
    <xsl:variable name="fragment" select="substring-after(current()/@href, '#')"/>
    <xsl:variable name="sourceLang">
      <xsl:call-template name="schxslt:in-scope-language"/>
    </xsl:variable>
    <xsl:choose>
      <xsl:when test="document(substring-before(@href, '#'), @href)//*[@xml:id = $fragment]">
        <xsl:if test="local-name(..) = local-name(document(substring-before(@href, '#'), @href)//*[@xml:id = $fragment])">
          <xsl:if test="namespace-uri(document(substring-before(@href, '#'), @href)//*[@xml:id = $fragment]) = 'http://purl.oclc.org/dsdl/schematron'">
            <xsl:apply-templates select="document(substring-before(@href, '#'), @href)//*[@xml:id = $fragment]/node()">
              <xsl:with-param name="sourceLang" select="$sourceLang"/>
            </xsl:apply-templates>
          </xsl:if>
        </xsl:if>
      </xsl:when>
      <xsl:otherwise>
        <xsl:if test="local-name(..) = local-name(document(substring-before(@href, '#'), @href)//*[@id = $fragment])">
          <xsl:if test="namespace-uri(document(substring-before(@href, '#'), @href)//*[@id = $fragment]) = 'http://purl.oclc.org/dsdl/schematron'">
            <xsl:apply-templates select="document(substring-before(@href, '#'), @href)//*[@id = $fragment]/node()">
              <xsl:with-param name="sourceLang" select="$sourceLang"/>
            </xsl:apply-templates>
          </xsl:if>
        </xsl:if>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>

  <xsl:template match="sch:extends[@href]">
    <xsl:variable name="sourceLang">
      <xsl:call-template name="schxslt:in-scope-language"/>
    </xsl:variable>
    <xsl:apply-templates select="document(@href)/*/node()">
      <xsl:with-param name="sourceLang" select="$sourceLang"/>
    </xsl:apply-templates>
  </xsl:template>

  <xsl:template match="sch:include[contains(@href, '#')]">
    <xsl:variable name="fragment" select="substring-after(current()/@href, '#')"/>
    <xsl:variable name="sourceLang">
      <xsl:call-template name="schxslt:in-scope-language"/>
    </xsl:variable>

    <xsl:choose>
      <xsl:when test="document(substring-before(@href, '#'), @href)//*[@xml:id = $fragment]">
        <xsl:apply-templates select="document(substring-before(@href, '#'), @href)//*[@xml:id = $fragment]">
          <xsl:with-param name="sourceLang" select="$sourceLang"/>
        </xsl:apply-templates>
      </xsl:when>
      <xsl:otherwise>
        <xsl:apply-templates select="document(substring-before(@href, '#'), @href)//*[@id = $fragment]">
          <xsl:with-param name="sourceLang" select="$sourceLang"/>
        </xsl:apply-templates>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>

  <xsl:template match="sch:include">
    <xsl:variable name="sourceLang">
      <xsl:call-template name="schxslt:in-scope-language"/>
    </xsl:variable>
    <xsl:apply-templates select="document(@href)">
      <xsl:with-param name="sourceLang" select="$sourceLang"/>
    </xsl:apply-templates>
  </xsl:template>

  <xsl:template name="schxslt:in-scope-language">
    <xsl:param name="context" select="."/>
    <xsl:value-of select="$context/ancestor-or-self::*[@xml:lang][1]/@xml:lang"/>
  </xsl:template>

</xsl:transform>
