<xsl:stylesheet version="2.0"
    xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
    xmlns:sch="http://purl.oclc.org/dsdl/schematron"
    xmlns:s="http://www.ascc.net/xml/schematron"
    xmlns:a="http://relaxng.org/ns/compatibility/annotations/1.0"
    xmlns:rng="http://relaxng.org/ns/structure/1.0"
    xmlns:tei="http://www.tei-c.org/ns/1.0"
    xmlns:teix="http://www.tei-c.org/ns/Examples"
    xmlns:xd="http://www.pnp-software.com/XSLTdoc"
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform" 
    exclude-result-prefixes="teix a s tei rng xd sch xsi">
  <xsl:key name="ALL" use="1" 
    match="tei:elementSpec|tei:macroSpec|tei:classSpec|tei:moduleSpec"/>
  <xsl:template match="/">
    <TEI xmlns="http://www.tei-c.org/ns/1.0">
      <xsl:apply-templates select="tei:TEI/tei:teiHeader"/>
      <xsl:for-each select="tei:TEI/tei:text/tei:front">
	<xsl:copy>
	  <xsl:call-template name="subdivs"/>
	</xsl:copy>
      </xsl:for-each>
      <xsl:for-each select="tei:TEI/tei:text/tei:body">
	<xsl:copy>
	  <xsl:call-template name="subdivs"/>
	</xsl:copy>
      </xsl:for-each>
      <xsl:for-each select="tei:TEI/tei:text/tei:back">
	<xsl:copy>
	  <xsl:call-template name="subdivs"/>
	</xsl:copy>
      </xsl:for-each>
      <xsl:apply-templates select="key('ALL',1)"/>
    </TEI>
  </xsl:template>

  <xsl:template name="subdivs">
    <xsl:for-each select="tei:div">
      <xsl:copy>
	<xsl:copy-of select="@*"/>
	<xsl:call-template name="subdivs"/>
      </xsl:copy>
    </xsl:for-each>
  </xsl:template>
  
  <xsl:template match="@*|text()|processing-instruction()">
    <xsl:copy-of select="."/>
  </xsl:template>
  
  
  <xsl:template match="*">
    <xsl:copy>
      <xsl:apply-templates 
	  select="*|@*|processing-instruction()|text()"/>
    </xsl:copy>
  </xsl:template>
	

</xsl:stylesheet>