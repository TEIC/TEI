<xsl:stylesheet version="2.0"
    xmlns:tei="http://www.tei-c.org/ns/1.0"
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform" 
    xmlns:teix="http://www.tei-c.org/ns/Examples" >

  <xsl:key name="V" match="teix:egXML[@valid='true' or not(@valid)]" use="1"/>
  <xsl:key name="F" match="teix:egXML[@valid='feasible']" use="1"/>

  <xsl:template match="/">
    <xsl:for-each select="key('V',1)">
      <xsl:variable name="N">
	<xsl:call-template name="loc"/>
      </xsl:variable>
      <xsl:result-document href="valid/{$N}">
	<xsl:copy-of select="."/>
      </xsl:result-document>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="loc">
    <xsl:for-each select="ancestor::tei:*|ancestor-or-self::teix:*">
      <xsl:value-of select="name(.)"/>
      <xsl:text>_</xsl:text>
      <xsl:choose>
	<xsl:when test="@ident">
	  <xsl:text></xsl:text>
	  <xsl:value-of select="replace(@ident,':','')"/>
	  <xsl:text></xsl:text>
	</xsl:when>
	<xsl:when test="@xml:id">
	  <xsl:text></xsl:text>
	  <xsl:value-of select="replace(@xml:id,':','')"/>
	  <xsl:text></xsl:text>
	</xsl:when>
	<xsl:otherwise>
	  <xsl:number/>
	</xsl:otherwise>
      </xsl:choose>
      <xsl:text>-</xsl:text>
    </xsl:for-each>
</xsl:template>

</xsl:stylesheet>