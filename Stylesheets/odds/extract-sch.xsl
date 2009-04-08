<xsl:stylesheet version="1.0"
  xmlns:rng="http://relaxng.org/ns/structure/1.0"
  xmlns:tei="http://www.tei-c.org/ns/1.0"
  xmlns:s="http://www.ascc.net/xml/schematron"
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
  <xsl:include href="teiodds.xsl"/>
  <xsl:key name="SCHEMATRON" match="s:*[parent::tei:constraint or parent::tei:content]" use="1"/>
  <xsl:template match="/">
    <s:schema xmlns:s="http://www.ascc.net/xml/schematron" >
      <s:title>Schematron rules for TEI</s:title>
      <xsl:for-each select="key('SCHEMATRON',1)">
	<xsl:call-template name="processSchematron"/>
      </xsl:for-each>
    </s:schema>
</xsl:template>
</xsl:stylesheet>

