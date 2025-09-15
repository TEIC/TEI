<xsl:transform xmlns:schxslt="https://doi.org/10.5281/zenodo.1495494" xmlns:xs="http://www.w3.org/2001/XMLSchema" xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="2.0">

  <xsl:template name="schxslt:version">
    <rdf:Description xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#"
                     xmlns:dct="http://purl.org/dc/terms/"
                     xmlns:dc="http://purl.org/dc/elements/1.1/"
                     xmlns:skos="http://www.w3.org/2004/02/skos/core#">
      <dct:creator>
        <dct:Agent>
          <skos:prefLabel><xsl:value-of select="schxslt:user-agent()"/></skos:prefLabel>
          <schxslt.compile.typed-variables xmlns="https://doi.org/10.5281/zenodo.1495494#">
            <xsl:value-of select="$schxslt.compile.typed-variables"/>
          </schxslt.compile.typed-variables>
        </dct:Agent>
      </dct:creator>
      <dct:created><xsl:value-of select="current-dateTime()"/></dct:created>
    </rdf:Description>
  </xsl:template>

  <xsl:function name="schxslt:user-agent" as="xs:string">
    <xsl:variable name="schxslt-ident" as="xs:string">SchXslt/1.10.1</xsl:variable>
    <xsl:variable name="xslt-ident" as="xs:string">
      <xsl:value-of separator="/" select="(system-property('xsl:product-name'), system-property('xsl:product-version'))"/>
    </xsl:variable>
    <xsl:value-of separator=" " select="($schxslt-ident, $xslt-ident)"/>
  </xsl:function>

</xsl:transform>
