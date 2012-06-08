<xsl:stylesheet version="2.0"
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform" 
    xpath-default-namespace="http://www.tei-c.org/ns/1.0">

  <xsl:template match="/">
    <TEI xmlns="http://www.tei-c.org/ns/1.0">
      <xsl:copy-of select="TEI/teiHeader"/>
      <xsl:for-each select="TEI/text/front">
	<xsl:copy>
	  <xsl:call-template name="subdivs"/>
	</xsl:copy>
      </xsl:for-each>
      <xsl:for-each select="TEI/text/body">
	<xsl:copy>
	  <xsl:call-template name="subdivs"/>
	</xsl:copy>
      </xsl:for-each>
      <xsl:for-each select="TEI/text/back">
	<xsl:copy>
	  <xsl:call-template name="subdivs"/>
	</xsl:copy>
      </xsl:for-each>
    </TEI>
  </xsl:template>

  <xsl:template name="subdivs">
    <xsl:apply-templates/>
  </xsl:template>
  
  <xsl:template match="@*|text()|processing-instruction()"/>
  
  <xsl:template match="*">
    <xsl:choose>
      <xsl:when test="self::div">
	<xsl:copy>
	  <xsl:copy-of select="@*"/>
	  <xsl:apply-templates/>
	</xsl:copy>
      </xsl:when>
      <xsl:when test="self::elementSpec|self::macroSpec|self::classSpec|self::moduleSpec">
	<xsl:copy-of select="."/>
      </xsl:when>
      <xsl:otherwise>
	<xsl:apply-templates/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
	

</xsl:stylesheet>