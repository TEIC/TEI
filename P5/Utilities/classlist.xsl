<xsl:stylesheet 
  xmlns:tei="http://www.tei-c.org/ns/1.0"
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform" 
  version="1.0"
>
<xsl:output method="xml" indent="yes"/>

<xsl:key name="MEMBERS"  match="tei:classSpec" use="tei:classes/tei:memberOf/@key"/>

<xsl:template match="/">
  <xsl:for-each select="//tei:classSpec">
    <xsl:if test="not(tei:classes/tei:memberOf)">
      * <xsl:value-of select="@ident"/>: <xsl:value-of
      select="@xml:id"/> <xsl:value-of
      select="@module"/>
      <xsl:for-each select="key('MEMBERS',@ident)">
         ** <xsl:value-of select="@ident"/>: <xsl:value-of
	 select="@xml:id"/>  <xsl:value-of
      select="@module"/>
      </xsl:for-each>
    </xsl:if>
  </xsl:for-each>
</xsl:template>
</xsl:stylesheet>
