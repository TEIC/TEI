<xsl:stylesheet 
 xmlns:tei="http://www.tei-c.org/ns/1.0"
 xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
 xmlns:rng="http://relaxng.org/ns/structure/1.0"
 extension-element-prefixes="exsl"
 xmlns:exsl="http://exslt.org/common"
 version="1.0">

<xsl:output method="text"/>
<xsl:key name="EMPTYELEMENTS" match="*[not(text())]" use="local-name(.)"/>
<xsl:key name="CONTENTELEMENTS" match="*[text()]" use="local-name(.)"/>
<xsl:key name="ELEMENTS" match="*" use="local-name(.)"/>

<xsl:template match="/">
 <xsl:for-each select="//*">
     <xsl:if
	 test="generate-id(.)=generate-id(key('ELEMENTS',local-name())[1])">
       <xsl:value-of select="local-name()"/>: <xsl:value-of
       select="count(key('EMPTYELEMENTS',local-name()))"/>: <xsl:value-of select="count(key('CONTENTELEMENTS',local-name()))"/>:
     </xsl:if>
 </xsl:for-each>
</xsl:template>

</xsl:stylesheet>







