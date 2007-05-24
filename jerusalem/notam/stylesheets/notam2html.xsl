<?xml version="1.0"?>
<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
<xsl:param name="headline"/>

  <xsl:template match="/">
    <xsl:if test="//notamList/notam">
      <p class="notamList">
	<table>
	  <xsl:if test="not($headline='')">
	    <tr><td class="notamHeadline"><xsl:value-of
	    select="$headline"/></td></tr>
	  </xsl:if>
	  <xsl:call-template name="notam"/>
	</table>
      </p>
    </xsl:if>
  </xsl:template>

  
  <xsl:template name="notam">
    <xsl:for-each select="//notamList/notam">
      <span><xsl:attribute name="class"><xsl:value-of select="status"/></xsl:attribute>
	<tr>
	  <td class="headline"><xsl:value-of select="headline"/></td>
	</tr>
	<tr>
	  <td class="message"><xsl:value-of select="message"/></td>
	</tr>
      </span>
    </xsl:for-each>
  </xsl:template>

</xsl:stylesheet>