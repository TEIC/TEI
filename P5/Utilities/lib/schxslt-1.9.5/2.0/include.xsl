<xsl:transform version="2.0" exclude-result-prefixes="#all"
               xmlns:sch="http://purl.oclc.org/dsdl/schematron"
               xmlns:xs="http://www.w3.org/2001/XMLSchema"
               xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
               xmlns:schxslt="https://doi.org/10.5281/zenodo.1495494">

  <!-- Entry for recursive inclusion -->
  <xsl:template match="sch:schema">
    <xsl:call-template name="schxslt:include">
      <xsl:with-param name="schematron" select="."/>
    </xsl:call-template>
  </xsl:template>

  <!-- Recursive inclusion -->
  <xsl:template name="schxslt:include">
    <xsl:param name="schematron" as="element(sch:schema)" required="yes"/>
    <xsl:apply-templates mode="schxslt:include" select="."/>
  </xsl:template>

  <xsl:template match="node() | @*" mode="schxslt:include">
    <xsl:copy>
      <xsl:sequence select="@*"/>
      <xsl:apply-templates select="node()" mode="#current"/>
    </xsl:copy>
  </xsl:template>

  <xsl:template match="sch:include | sch:extends[@href]" mode="schxslt:include">
    <xsl:param name="base-uri" as="xs:string?" tunnel="yes"/>
    <xsl:variable name="url" as="xs:string" select="if (contains(@href, '#')) then substring-before(@href, '#') else @href"/>
    <xsl:variable name="fragment" as="xs:string?" select="substring-after(@href, '#')"/>
    <xsl:variable name="document" select="document(if ($base-uri) then resolve-uri($url, $base-uri) else $url, @href)"/>
    <xsl:variable name="element" select="if ($fragment) then ($document//sch:*[@id = $fragment], $document//sch:*[@xml:id = $fragment])[1] else $document/*[1]"/>
    <xsl:choose>
      <xsl:when test="self::sch:include">
        <xsl:apply-templates select="$element" mode="schxslt:include">
          <xsl:with-param name="base-uri" as="xs:string?" tunnel="yes" select="base-uri($element)"/>
        </xsl:apply-templates>
      </xsl:when>
      <xsl:otherwise>
        <xsl:if test="(local-name($element) eq local-name(..)) and (namespace-uri($element) eq 'http://purl.oclc.org/dsdl/schematron')">
          <xsl:apply-templates mode="schxslt:include" select="$element/*">
            <xsl:with-param name="base-uri" as="xs:string?" tunnel="yes" select="base-uri($element)"/>
          </xsl:apply-templates>
        </xsl:if>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>

</xsl:transform>
