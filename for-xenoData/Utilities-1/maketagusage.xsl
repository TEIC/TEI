<xsl:stylesheet 
 xmlns:tei="http://www.tei-c.org/ns/1.0"
 xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
 xmlns:rng="http://relaxng.org/ns/structure/1.0"
 extension-element-prefixes="exsl"
 xmlns:exsl="http://exslt.org/common"
 version="1.0">

<xsl:output method="xml"/>
<xsl:key name="TAGLIST" match="tei:*" use="name(.)"/>

<xsl:template match="/">
  <xsl:variable name="HERE" select="."/>
  <p>
    <xsl:for-each select="document('tags.xml')//gi">
      <xsl:variable name="me" select="."/>
      <xsl:for-each select="$HERE">
	<xsl:variable name="n">
	  <xsl:value-of select="count(key('TAGLIST',$me))"/>
	</xsl:variable>
	<xsl:if test="$n &gt; 0">
	  <tagUsage>
	    <xsl:attribute name="gi">
	      <xsl:value-of select="$me"/>
	    </xsl:attribute>
	    <xsl:attribute name="occurs">
	      <xsl:value-of select="$n"/>
	    </xsl:attribute>
	  </tagUsage>
	</xsl:if>
      </xsl:for-each>
    </xsl:for-each>
  </p>
</xsl:template>
</xsl:stylesheet>







