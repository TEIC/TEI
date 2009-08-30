<xsl:stylesheet version="1.0"
  xmlns:s="http://www.ascc.net/xml/schematron"
  xmlns:teix="http://www.tei-c.org/ns/Examples"
  xmlns:xi="http://www.w3.org/2001/XInclude"
  xmlns:rng="http://relaxng.org/ns/structure/1.0"
  xmlns:tei="http://www.tei-c.org/ns/1.0"
  xmlns="http://www.ascc.net/xml/schematron"
  exclude-result-prefixes="tei rng teix s xi #default"
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
  <xsl:output encoding="utf-8" indent="yes" method="xml"/>
  <xsl:key name="SCHEMATRONNS" match="s:ns[parent::tei:constraint or parent::rng:*]" use="1"/>
  <xsl:key name="SCHEMATRON" match="s:pattern[parent::tei:constraint or
				    parent::rng:*]" use="1"/>
  <xsl:key name="SCHEMATRON" match="s:rule[parent::tei:constraint or
				    parent::rng:*]" use="1"/>
  <xsl:key name="SCHEMATRON" match="s:assert[parent::tei:constraint or
				    parent::rng:*]" use="1"/>
  <xsl:key name="SCHEMATRON" match="s:report[parent::tei:constraint or parent::rng:*]" use="1"/>
  <xsl:template match="/">
    <schema ns="http://www.ascc.net/xml/schematron">
      <title>Schematron 1.5 rules</title>
      <xsl:for-each select="key('SCHEMATRONNS',1)">
	<xsl:choose>
	  <xsl:when test="ancestor::teix:egXML"/>
	  <xsl:otherwise>
	    <xsl:apply-templates select="."/>
	  </xsl:otherwise>
	</xsl:choose>
      </xsl:for-each>
      <xsl:for-each select="key('SCHEMATRON',1)">
	<xsl:choose>
	  <xsl:when test="ancestor::teix:egXML"/>
	  <xsl:when test="self::s:pattern">
	    <xsl:apply-templates select="."/>
	  </xsl:when>
	  <xsl:when test="(self::s:report or self::s:assert) and ancestor::tei:elementSpec">
	    <pattern name="{ancestor::tei:elementSpec/@ident}-constraint-{parent::tei:constraintSpec/@ident}">
	      <rule>
		<xsl:attribute name="context">
		  <xsl:text>tei:</xsl:text>
		  <xsl:value-of select="ancestor::tei:elementSpec/@ident"/>
		</xsl:attribute>
		<xsl:apply-templates select="."/>
	      </rule>
	    </pattern>
	  </xsl:when>
	</xsl:choose>
      </xsl:for-each>
    </schema>
</xsl:template>


<xsl:template match="@*|text()|comment()|processing-instruction()">
 <xsl:copy-of select="."/>
</xsl:template>


<xsl:template match="s:*">
  <xsl:element name="{local-name()}" xmlns="http://www.ascc.net/xml/schematron">
    <xsl:apply-templates 
	select="*|@*|processing-instruction()|comment()|text()"/>
  </xsl:element>
</xsl:template>

</xsl:stylesheet>

