<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet xmlns="http://www.tei-c.org/ns/1.0"
		xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
		xmlns:html="http://www.w3.org/1999/xhtml"
		version="2.0">
  <xsl:key name="IDS" use="@id" match="*"/>
  <xsl:template match="/">
    <xsl:for-each select="//html:a[starts-with(@href,'#')]">
      <xsl:if test="not(key('IDS',substring(@href,2)))">
	<xsl:message>Error: no target for link <xsl:value-of select="@href"/></xsl:message>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>
</xsl:stylesheet>
