<xsl:stylesheet version="1.0"
	  xmlns:rng="http://relaxng.org/ns/structure/1.0"
	  xmlns:tei="http://www.tei-c.org/ns/1.0"
	  exclude-result-prefixes="rng tei"
	  xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
<xsl:output method="xml" encoding="utf-8" indent="yes"/>
<xsl:param name="lang">ja</xsl:param>

<xsl:template match="/i18n">
  <i18n>
    <xsl:for-each select="entry">
      <xsl:if test="not(text[@xml:lang=$lang])">
	<entry key="@key">
	  <xsl:copy-of select="text[@xml:lang='en']"/>
	  <text xml:lang="{$lang}"></text>
	</entry>
      </xsl:if>
    </xsl:for-each>
  </i18n>
</xsl:template>
</xsl:stylesheet>
