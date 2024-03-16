<xsl:transform version="2.0"
               xmlns:sch="http://purl.oclc.org/dsdl/schematron"
               xmlns:schxslt="https://doi.org/10.5281/zenodo.1495494"
               xmlns:xsl="http://www.w3.org/1999/XSL/Transform">

  <xsl:include href="compile/compile-2.0.xsl"/>
  <xsl:include href="include.xsl"/>
  <xsl:import href="expand.xsl"/>

  <xsl:template match="/sch:schema" priority="100">
    <xsl:call-template name="schxslt:compile">
      <xsl:with-param name="schematron" as="element(sch:schema)">
        <xsl:call-template name="schxslt:expand">
          <xsl:with-param name="schema" as="element(sch:schema)">
            <xsl:call-template name="schxslt:include">
              <xsl:with-param name="schematron" as="element(sch:schema)" select="."/>
            </xsl:call-template>
          </xsl:with-param>
        </xsl:call-template>
      </xsl:with-param>
    </xsl:call-template>
  </xsl:template>

</xsl:transform>
