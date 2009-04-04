<xsl:stylesheet version="2.0"
		xmlns:teix="http://www.tei-c.org/ns/Examples"
		xmlns:rng="http://relaxng.org/ns/structure/1.0"
		xmlns:tei="http://www.tei-c.org/ns/1.0"
		xmlns:sch="http://purl.oclc.org/dsdl/schematron"
		xmlns="http://purl.oclc.org/dsdl/schematron"
		exclude-result-prefixes="rng tei sch #default"
		xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
  <xsl:output encoding="utf-8" indent="yes" method="xml"/>
  <xsl:key name="SCHEMATRON" match="sch:*[parent::tei:constraint]" use="1"/>
  <xsl:template match="/">
    <schema>
      <title>ISO Schematron rules for TEI</title>
      <xsl:for-each select="key('SCHEMATRON',1)">
	<xsl:choose>
	  <xsl:when test="ancestor::teix:egXML"/>
	  <xsl:when test="self::sch:ns">
	    <xsl:copy-of select="."/>
	  </xsl:when>
	  <xsl:when test="self::sch:pattern">
	    <xsl:copy-of select="."/>
	  </xsl:when>
	  <xsl:when test="(self::sch:report or self::sch:assert) and ancestor::tei:elementSpec">
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

