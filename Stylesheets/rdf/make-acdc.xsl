<XSL:stylesheet
    xpath-default-namespace="http://www.tei-c.org/ns/1.0"
    xmlns:XSL="http://www.w3.org/1999/XSL/Transform" 
    xmlns:xsl="http://www.w3.org/1999/XSL/TransformAlias" 
    version="2.0"
>
<XSL:key name="EQUIVFILES" match="equiv" use="@filter"/>
<XSL:key name="EQUIV" match="equiv[@filter]" use="1"/>

<XSL:output method="xml" indent="yes" encoding="utf-8"/>
<XSL:key name="NS" match="elementSpec|attDef" use="@ns"/>

<XSL:namespace-alias stylesheet-prefix="xsl" result-prefix="XSL"/>

<XSL:template match="/">
  
  <xsl:stylesheet version="2.0"
		  xpath-default-namespace="http://www.tei-c.org/ns/1.0"
		  xmlns:XSL="http://www.w3.org/1999/XSL/Transform">

    <XSL:for-each select="key('EQUIV',1)">
      <XSL:if test="generate-id(.)=generate-id(key('EQUIVFILES',@filter)[1])">
	<xsl:import href="{@filter}"/>  
      </XSL:if>
    </XSL:for-each>
    
    <xsl:template match="*">
	<xsl:apply-templates select="*|@*|processing-instruction()|comment()|text()"/>
    </xsl:template>
    
    <xsl:template match="text()|comment()|@*|processing-instruction()"/>
    
    <XSL:for-each select="key('EQUIV',1)">
      <xsl:template>
	<XSL:variable name="f" select="@filter"/>
	<XSL:attribute name="match">
	  <XSL:value-of select="ancestor::elementSpec/@ident"/>
	</XSL:attribute>
	<xsl:call-template name="{@name}"/>
      </xsl:template>
    </XSL:for-each>
    
  </xsl:stylesheet>
</XSL:template>

</XSL:stylesheet>
