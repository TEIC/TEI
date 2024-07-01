<xsl:transform version="2.0"
               xmlns:schxslt="https://doi.org/10.5281/zenodo.1495494"
               xmlns:svrl="http://purl.oclc.org/dsdl/svrl"
               xmlns:xs="http://www.w3.org/2001/XMLSchema"
               xmlns:xsl="http://www.w3.org/1999/XSL/Transform">

  <xsl:output indent="yes"/>

  <!-- Normalize SVRL report document -->

  <xsl:template match="svrl:schematron-output">
    <xsl:variable as="node()*" name="svrl-header" select="node()[not(self::svrl:active-pattern)][empty(preceding-sibling::svrl:active-pattern)]"/>

    <xsl:copy>
      <xsl:sequence select="@*"/>
      <xsl:sequence select="$svrl-header"/>

      <xsl:for-each-group select="node() except $svrl-header" group-starting-with="svrl:active-pattern">
        <xsl:sort select="normalize-space(concat(@id, ' ', @documents))"/>
        <xsl:sort select="count(current-group()[self::svrl:fired-rule])"/>
        <xsl:sequence select="."/>
        <xsl:for-each-group select="current-group()[preceding-sibling::svrl:active-pattern]" group-starting-with="svrl:fired-rule">
          <xsl:sort select="@id"/>
          <xsl:sort select="@context"/>
          <xsl:sequence select="."/>
          <xsl:for-each select="current-group()[preceding-sibling::fired-rule]">
            <xsl:sort select="local-name()"/>
            <xsl:sort select="@id"/>
            <xsl:sort select="@test"/>
            <xsl:sequence select="current-group()"/>
          </xsl:for-each>
        </xsl:for-each-group>
      </xsl:for-each-group>

    </xsl:copy>
  </xsl:template>

</xsl:transform>
