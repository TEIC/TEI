<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet 
    xmlns="http://www.tei-c.org/ns/1.0"
    xmlns:teix="http://www.tei-c.org/ns/Examples"
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    xpath-default-namespace="http://www.tei-c.org/ns/1.0"
    exclude-result-prefixes="#all"
    version="2.0">
  <!-- import base conversion style -->

  <xsl:import href="/usr/share/xml/tei/stylesheet/common/verbatim.xsl"/>
  <xsl:param name="attLength">35</xsl:param>
  <xsl:param name="spaceCharacter">&#160;</xsl:param>

  <xsl:output indent="yes"/>

  <xsl:template match="*">
    <xsl:copy>
      <xsl:apply-templates select="@*|*|processing-instruction()|comment()|text()" />
    </xsl:copy>
  </xsl:template>

  <xsl:template match="comment()|@*|processing-instruction()|text()">
    <xsl:copy-of select="."/>
  </xsl:template>

  <xsl:template match="teix:egXML">
      <xsl:param name="simple">false</xsl:param>
      <xsl:param name="highlight"/>
      <code>
	<xsl:apply-templates mode="verbatim">
	  <xsl:with-param name="highlight">
	    <xsl:value-of select="$highlight"/>
	  </xsl:with-param>
	</xsl:apply-templates>
      </code>
  </xsl:template>

</xsl:stylesheet>
