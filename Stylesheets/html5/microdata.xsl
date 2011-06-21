<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet 
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform" 
    version="2.0"
    xpath-default-namespace="http://www.tei-c.org/ns/1.0">

  <xsl:template name="microdata">
    <xsl:choose>
      <xsl:when test="self::p"/>
      <xsl:when test="self::docDate">
	<xsl:attribute name="itemprop">date</xsl:attribute>
      </xsl:when>
      <xsl:otherwise>
	<xsl:attribute name="itemprop" select="local-name()"/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>

</xsl:stylesheet>