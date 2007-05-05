<XSL:stylesheet
    xmlns:tei="http://www.tei-c.org/ns/1.0"
    xmlns:XSL="http://www.w3.org/1999/XSL/Transform" 
    xmlns:xsl="http://www.w3.org/1999/XSL/TransformAlias" 
    version="1.0"
>
<XSL:key name="EQUIVFILES" match="tei:equiv" use="@filter"/>
<XSL:key name="EQUIV" match="tei:equiv" use="1"/>

<XSL:output method="xml" indent="yes" encoding="utf-8"/>
<XSL:key name="NS" match="tei:elementSpec|tei:attDef" use="@ns"/>

<XSL:namespace-alias stylesheet-prefix="xsl" result-prefix="XSL"/>

<XSL:template match="/">
  
  <xsl:stylesheet version="1.0"
		  cdata-section-elements="tei:eg"
		  exclude-result-prefixes="xsl tei" 
		  xmlns:XSL="http://www.w3.org/1999/XSL/Transform" 
		  xmlns:tei="http://www.tei-c.org/ns/1.0">
    
    <XSL:for-each select="key('EQUIV','1')">
     <XSL:if test="../@ns and generate-id(.)=generate-id(key('EQUIVFILES',@filter)[1])">
      <XSL:variable name="N">
	<XSL:text>NS</XSL:text>
	<XSL:number level="any"/>
      </XSL:variable>
      <XSL:attribute 
	  name="{concat($N,':','dummy')}" 
	  namespace="{../@ns}">
	<XSL:value-of select="../@ns"/>
      </XSL:attribute>
     </XSL:if>
    </XSL:for-each>

    <XSL:for-each select="key('EQUIV','1')">
      <XSL:if test="generate-id(.)=generate-id(key('EQUIVFILES',@filter)[1])">
	<xsl:import href="{@filter}"/>  
      </XSL:if>
    </XSL:for-each>
    
    <xsl:template match="*">
      <xsl:copy>
	<xsl:apply-templates select="*|@*|processing-instruction()|comment()|text()"/>
      </xsl:copy>
    </xsl:template>
    
    <xsl:template match="text()|comment()|@*|processing-instruction()">
      <xsl:copy/>
    </xsl:template>
    
    <XSL:for-each select="key('EQUIV','1')">
      <xsl:template>
	<XSL:variable name="f" select="@filter"/>
	<XSL:attribute name="match">
	  <XSL:text>NS</XSL:text>
	  <XSL:for-each select="key('EQUIVFILES',$f)[1]">
	    <XSL:number level="any"/>
	  </XSL:for-each>
	  <XSL:text>:</XSL:text>
	  <XSL:value-of select="parent::tei:elementSpec/@ident"/>
	</XSL:attribute>
	<xsl:call-template name="{@name}"/>
      </xsl:template>
    </XSL:for-each>
    
  </xsl:stylesheet>
</XSL:template>

</XSL:stylesheet>
