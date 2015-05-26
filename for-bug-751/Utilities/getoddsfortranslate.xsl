<xsl:stylesheet 
 xmlns:teix="http://www.tei-c.org/ns/Examples"
 xmlns:tei="http://www.tei-c.org/ns/1.0"
 xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
 xmlns:rng="http://relaxng.org/ns/structure/1.0"
 exclude-result-prefixes="teix rng tei"
 version="2.0">

<xsl:param name="translang">ru</xsl:param>

<xsl:param name="transdate">2012-10-11</xsl:param>

<xsl:key name="MOD" match="tei:moduleSpec" use="1"/>
<xsl:output method="xml"/>

<xsl:template match="/">
  <xsl:for-each select="key('MOD',1)">
    <xsl:variable name="module" select="@ident"/>
    <xsl:result-document         
	method="xml"
	cdata-section-elements="tei:eg teix:egXML" 
	indent="yes"
	href="fortranslate-{$translang}/{@ident}.xml">
      <xsl:copy>
	<xsl:copy-of select="@ident"/>
	<xsl:for-each select="//tei:elementSpec[@module=$module]|//tei:classSpec[@module=$module]|//tei:macroSpec[@module=$module]">
	  
	  <xsl:copy>
	    <xsl:apply-templates select="@ident"/>
	    <xsl:apply-templates select="@module"/>
	    <xsl:apply-templates
		select="*|text()"/>
	  </xsl:copy>
	</xsl:for-each>
      </xsl:copy>
    </xsl:result-document>
  </xsl:for-each>
</xsl:template>

<xsl:template match="tei:valDesc|tei:desc|tei:gloss">
  <xsl:choose>
    <xsl:when test="@xml:lang and not(@xml:lang='en')">
    </xsl:when>
    <xsl:otherwise>
      <xsl:copy>
	<xsl:apply-templates select="@*"/>
	<xsl:apply-templates
	    select="*|text()"/>
      </xsl:copy>
      <xsl:copy>
	<xsl:attribute name="xml:lang">
	  <xsl:value-of select="$translang"/>
	</xsl:attribute>
	<xsl:attribute name="notBefore">
	  <xsl:value-of select="$transdate"/>
	</xsl:attribute>
	<xsl:if test="string-length(.)&gt;0">
	  <xsl:text>TO BE TRANSLATED</xsl:text>
	</xsl:if>
      </xsl:copy>
    </xsl:otherwise>
  </xsl:choose>
</xsl:template>

<xsl:template match="tei:exemplum"/>
<xsl:template match="tei:remarks"/>
<xsl:template match="tei:classes"/>
<xsl:template match="tei:equiv"/>
<xsl:template match="tei:content"/>
<xsl:template match="tei:listRef"/>
<xsl:template match="tei:datatype"/>
<xsl:template match="tei:defaultVal"/>

<xsl:template match="@*|text()">
  <xsl:copy/>
</xsl:template>

<xsl:template match="*">
 <xsl:copy>
  <xsl:apply-templates select="@*"/>
  <xsl:apply-templates       
      select="*|text()"/>
 </xsl:copy>
</xsl:template>

</xsl:stylesheet>







