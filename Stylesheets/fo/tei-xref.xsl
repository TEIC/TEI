<!-- 
TEI XSLT stylesheet family
$Date$, $Revision$, $Author$

XSL FO stylesheet to format TEI XML documents 

##LICENSE
-->
<xsl:stylesheet
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0"
  xmlns:tei="http://www.tei-c.org/ns/1.0"
  xmlns:fo="http://www.w3.org/1999/XSL/Format"
  >

<xsl:template name="makeInternalLink">
  <xsl:param name="ptr"/>
  <xsl:param name="target"/>
  <xsl:param name="dest"/>
  <xsl:param name="body"/>
  <xsl:variable name="W">
    <xsl:choose>
      <xsl:when test="$target"><xsl:value-of select="$target"/></xsl:when>
      <xsl:when test="contains($dest,'#')">
	<xsl:value-of select="substring-after($dest,'#')"/>
      </xsl:when>
      <xsl:otherwise>
	<xsl:value-of select="$dest"/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:variable>
  <fo:basic-link internal-destination="{$W}">
    <xsl:call-template name="linkStyle"/>
    <xsl:choose>
      <xsl:when test="not($body='')">
	<xsl:value-of select="$body"/>
      </xsl:when>
      <xsl:when test="$ptr='true'">
	<xsl:apply-templates mode="xref" select="key('IDS',$W)">
	  <xsl:with-param name="minimal" select="$minimalCrossRef"/>
	</xsl:apply-templates>
      </xsl:when>
      <xsl:otherwise>
	<xsl:apply-templates/>
      </xsl:otherwise>
    </xsl:choose>
  </fo:basic-link>
</xsl:template>

<xsl:template name="makeExternalLink">
  <xsl:param name="ptr"/>
  <xsl:param name="dest"/>
  <fo:basic-link external-destination="{$dest}">
	<xsl:choose>
	  <xsl:when test="$ptr='true'">
	    <xsl:call-template name="showXrefURL">
	      <xsl:with-param name="dest">
		<xsl:value-of select="$dest"/>
	      </xsl:with-param>
	    </xsl:call-template>
	  </xsl:when>
	  <xsl:otherwise>
	    <xsl:apply-templates/>
	  </xsl:otherwise>
	</xsl:choose>
  </fo:basic-link>
</xsl:template>

<xsl:template name="generateEndLink">
  <xsl:param name="where"/>
  <xsl:apply-templates select="$where"/>
</xsl:template>

</xsl:stylesheet>
