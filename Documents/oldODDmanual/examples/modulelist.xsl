<xsl:stylesheet 
  xmlns:tei="http://www.tei-c.org/ns/1.0"
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform" 
  version="1.0"
  exclude-result-prefixes="tei" 
>
<xsl:output method="xml" encoding="utf-8" indent="yes"/>

<xsl:template match="/">

<tei:table rend="rules" xml:id="tab_modlist">
<tei:head>The TEI modules.</tei:head>
  <xsl:for-each select="//tei:moduleSpec[not(@type='decls')]">
    <xsl:sort select="@ident"/>
    <tei:row>
      <tei:cell>
	<xsl:choose>
      <xsl:when test="@ident='core' or @ident='tei' or
      @ident='textstructure' or @ident='header'">
	<tei:hi><xsl:value-of  select="@ident"/></tei:hi>
      </xsl:when>
      <xsl:otherwise>
	<xsl:value-of  select="@ident"/>
      </xsl:otherwise>
	</xsl:choose>
      </tei:cell>
      <tei:cell><xsl:apply-templates select="tei:desc"/></tei:cell>
    </tei:row>
  </xsl:for-each>
</tei:table>
</xsl:template>
</xsl:stylesheet>
