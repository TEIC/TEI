<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet xmlns:rng="http://relaxng.org/ns/structure/1.0"
                xmlns:tei="http://www.tei-c.org/ns/1.0"
                xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                version="1.0"
                exclude-result-prefixes="rng tei">
   <xsl:output method="xml" encoding="utf-8" indent="yes"/>
   <xsl:param name="lang">ja</xsl:param>

   <xsl:template match="/i18n">
      <i18n>
         <xsl:for-each select="entry">
            <xsl:if test="not(text[@xml:lang=$lang])">
	              <entry>
	                 <xsl:copy-of select="key"/>
	                 <xsl:copy-of select="text[@xml:lang='en']"/>
	                 <text xml:lang="{$lang}"/>
	              </entry>
            </xsl:if>
         </xsl:for-each>
      </i18n>
   </xsl:template>
</xsl:stylesheet>