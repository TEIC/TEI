<xsl:stylesheet
    xmlns:tei="http://www.tei-c.org/ns/1.0"
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform" 
    xmlns:XSL="http://www.w3.org/1999/XSL/TransformAlias" 
    exclude-result-prefixes="xsl" 
    version="1.0"
>
<xsl:key name="EQUIVFILES" match="tei:equiv" use="@filter"/>
<xsl:key name="CLASSMEMBERS" match="tei:memberOf" use="@key"/>

<xsl:output method="xml" indent="yes" encoding="utf-8"/>

<xsl:namespace-alias stylesheet-prefix="XSL" result-prefix="xsl"/>

<xsl:template match="/">
  
  <XSL:stylesheet version="1.0"
		  exclude-result-prefixes="tei" 
		  xmlns:xsl="http://www.w3.org/1999/XSL/Transform" 
		  xmlns:tei="http://www.tei-c.org/ns/1.0">
    
    <xsl:for-each select="//tei:equiv[@filter]">
      <xsl:if test="generate-id(.)=generate-id(key('EQUIVFILES',@filter)[1])">
	<XSL:import href="{@filter}"/>  
      </xsl:if>
    </xsl:for-each>
    
    <XSL:template match="*">
      <XSL:copy>
	<XSL:apply-templates select="*|@*|processing-instruction()|comment()|text()"/>
      </XSL:copy>
    </XSL:template>
    
    <XSL:template match="text()|comment()|@*|processing-instruction()">
      <XSL:copy/>
    </XSL:template>
    
    <xsl:for-each select="//tei:elementSpec/tei:equiv[@filter]">
      <XSL:template>
	<xsl:attribute name="match">
	  <xsl:text>tei:</xsl:text>
	  <xsl:value-of select="parent::tei:elementSpec/@ident"/>
	</xsl:attribute>
	<XSL:call-template name="{@name}"/>
      </XSL:template>
    </xsl:for-each>

    <xsl:for-each select="//tei:elementSpec/tei:altIdent">
      <XSL:template>
	<xsl:attribute name="match">
	  <xsl:text>tei:</xsl:text>
	  <xsl:value-of select="."/>
	</xsl:attribute>
	<XSL:element name="{parent::tei:elementSpec/@ident}" xmlns="http://www.tei-c.org/ns/1.0">
	  <XSL:apply-templates select="*|@*|processing-instruction()|comment()|text()"/>
	</XSL:element>
      </XSL:template>
    </xsl:for-each>
    
    <xsl:for-each select="//tei:elementSpec/tei:attList//tei:attDef/tei:altIdent">
      <XSL:template>
	<xsl:attribute name="match">
	  <xsl:text>tei:</xsl:text>
	  <xsl:choose>
	    <xsl:when test="ancestor::tei:elementSpec/tei:altIdent">
	      <xsl:value-of
		  select="ancestor::tei:elementSpec/tei:altIdent"/>
	    </xsl:when>
	    <xsl:otherwise>
	      <xsl:value-of
		  select="ancestor::tei:elementSpec/@ident"/>
	    </xsl:otherwise>
	  </xsl:choose>
	  <xsl:text>/@</xsl:text>
	  <xsl:value-of select="."/>
	</xsl:attribute>
	<XSL:attribute name="{parent::tei:attDef/@ident}">
	  <XSL:value-of select="."/>
	</XSL:attribute>
      </XSL:template>
    </xsl:for-each>
    
    <xsl:for-each
	select="//tei:classSpec/tei:attList/tei:attDef/tei:altIdent">
      <xsl:variable name="n">
	<xsl:value-of select="."/>
      </xsl:variable>
      <xsl:variable name="real">
	<xsl:value-of select="parent::tei:attDef/@ident"/>
      </xsl:variable>
      <xsl:for-each
	  select="key('CLASSMEMBERS',ancestor::tei:classSpec/@ident)">
	<xsl:for-each select="ancestor::tei:elementSpec">
	  <XSL:template>
	    <xsl:attribute name="match">
	      <xsl:text>tei:</xsl:text>
	      <xsl:choose>
		<xsl:when test="tei:altIdent">
		  <xsl:value-of select="tei:altIdent"/>
	      </xsl:when>
	      <xsl:otherwise>
		<xsl:value-of  select="@ident"/>
	      </xsl:otherwise>
	    </xsl:choose>
	    <xsl:text>/@</xsl:text>
	    <xsl:value-of select="$n"/>
	  </xsl:attribute>
	  <XSL:attribute name="{$real}">
	    <XSL:value-of select="."/>
	  </XSL:attribute>
	</XSL:template>
      </xsl:for-each>
    </xsl:for-each>
    </xsl:for-each>
  </XSL:stylesheet>
</xsl:template>

</xsl:stylesheet>
