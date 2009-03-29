<xsl:stylesheet version="1.0"
  xmlns:rng="http://relaxng.org/ns/structure/1.0"
  xmlns:tei="http://www.tei-c.org/ns/1.0"
  xmlns:teix="http://www.tei-c.org/ns/Examples"
  xmlns:s="http://www.ascc.net/xml/schematron"
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
  <xsl:key name="SCHEMATRON" match="s:*[parent::tei:constraint]" use="1"/>
  <xsl:template match="/">
    <schema xmlns="http://www.ascc.net/xml/schematron" >
      <title>Schematron rules for TEI</title>
      <xsl:for-each select="key('SCHEMATRON',1)">
	<xsl:choose>
	  <xsl:when test="ancestor::teix:egXML"/>
	  <xsl:when test="self::s:ns">
	    <xsl:copy-of select="."/>
	  </xsl:when>
	  <xsl:when test="self::s:pattern">
	    <xsl:copy-of select="."/>
	  </xsl:when>
	  <xsl:when test="(self::s:report or self::s:assert) and ancestor::tei:elementSpec">
	    <pattern>
	      <xsl:attribute name="name">
		<xsl:choose>
		  <xsl:when test="tei:head">
		    <xsl:value-of select="tei:head"/>
		  </xsl:when>
		  <xsl:otherwise>
		    <xsl:value-of select="generate-id()"/>
		  </xsl:otherwise>
		</xsl:choose>
	      </xsl:attribute>
	      <rule>
		<xsl:attribute name="context">
		  <xsl:text>tei:</xsl:text>
		  <xsl:value-of select="ancestor::tei:elementSpec/@ident"/>
		</xsl:attribute>
		<xsl:copy-of select="."/>
	      </rule>
	    </pattern>
	  </xsl:when>
	</xsl:choose>
      </xsl:for-each>
    </schema>

  </xsl:template>
</xsl:stylesheet>

