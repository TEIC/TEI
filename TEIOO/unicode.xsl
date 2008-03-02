<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                version="1.0">

  <xsl:output method="xml" omit-xml-declaration="yes" indent="yes"/>

  <xsl:template match="charlist">
    <list type="gloss">
    <xsl:for-each select="character">
      <xsl:if test="not(contains(@id,'-') or description='' or @dec &lt; 31 or @dec &gt; 5000)">
      <label><xsl:value-of select="@id"/></label>
      <item><xsl:value-of select="@dec"/> 
        (<xsl:value-of select="description"/>):
    <xsl:text disable-output-escaping="yes">&amp;#x</xsl:text>
    <xsl:value-of select="@dec"/>
    <xsl:text>;</xsl:text>
       </item> 
      </xsl:if>

    </xsl:for-each>
  </list>
  </xsl:template>

</xsl:stylesheet>
