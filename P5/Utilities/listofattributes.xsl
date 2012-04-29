<xsl:stylesheet 
    version="2.0"
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform" 
    xmlns:tei="http://www.tei-c.org/ns/1.0"
    xmlns:rng="http://relaxng.org/ns/structure/1.0"
    xmlns:teix="http://www.tei-c.org/ns/Examples"
    xpath-default-namespace="http://www.tei-c.org/ns/1.0">
  
  <xsl:output method="text"/>
  
  <xsl:variable name="doc" select="/"/>
  
  <xsl:template match="/">
    <xsl:for-each select=".//attDef">
      <xsl:for-each
	  select="ancestor::elementSpec|ancestor::classSpec">
	<xsl:value-of select="@ident"/>
	<xsl:text>,</xsl:text>
	<!--
	<xsl:value-of select="name()"/>
	<xsl:text>,</xsl:text>
	-->
      </xsl:for-each>
      <xsl:value-of select="@ident"/>
      <xsl:text>,</xsl:text>
      <xsl:choose>
	<xsl:when test="valList[@type='closed']">valList</xsl:when>
	<xsl:when test="datatype/rng:text">TEXT</xsl:when>
	<xsl:when test="datatype//rng:data"><xsl:value-of select="datatype//rng:data/@type"/></xsl:when>
	<xsl:otherwise>
	  <xsl:for-each select="datatype//rng:ref">
	    <xsl:value-of select="@name"/>
	    <xsl:if test="following-sibling::rng:ref">|</xsl:if>
	  </xsl:for-each>
	</xsl:otherwise>
      </xsl:choose>
      <xsl:text>,</xsl:text>
      <xsl:choose>
	<xsl:when
	    test="datatype/@maxOccurs='unbounded'">multiple</xsl:when>
	<xsl:when
	    test="number(datatype/@maxOccurs)&gt;1">multiple</xsl:when>
	<xsl:otherwise>single</xsl:otherwise>
      </xsl:choose>
      <xsl:text>&#10;</xsl:text>      
    </xsl:for-each>
</xsl:template>
</xsl:stylesheet>
