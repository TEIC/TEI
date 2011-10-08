<?xml version="1.0"?>
<xsl:stylesheet 
    xmlns:dc="http://purl.org/dc/elements/1.1/"
    xmlns:xs="http://www.w3.org/2001/XMLSchema" 
    xmlns="http://purl.org/NET/crm-owl#" 
    xmlns:tei="http://www.tei-c.org/ns/1.0" 
    xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#" 
    xmlns:rdfs="http://www.w3.org/2000/01/rdf-schema#" 
    xmlns:owl="http://www.w3.org/2002/07/owl#" 
    xmlns:xsd="http://www.w3.org/2001/XMLSchema#" 
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    version="2.0" 
    xpath-default-namespace="http://www.tei-c.org/ns/1.0" 
    exclude-result-prefixes="dc tei rdf rdfs owl xsd xsl xs">
  
  <xsl:output encoding="utf-8" method="xml" indent="yes"/>

  <xsl:template name="dc_header">
    <rdf:Description xmlns:dc="http://purl.org/dc/elements/1.1/"
	rdf:about="{.//idno[1]}">
      <xsl:apply-templates/>
    </rdf:Description>
  </xsl:template>

  <xsl:template name="dc_title">
    <dc:title>
      <xsl:value-of select="."/>
    </dc:title>
  </xsl:template>
  
  <xsl:template name="dc_creator">
    <dc:creator>
      <xsl:value-of select="."/>
    </dc:creator>
  </xsl:template>
  
  <xsl:template name="dc_publisher">
    <xsl:if test="publisher">
      <dc:publisher>
	<xsl:value-of select="publisher"/>
	<xsl:if test="pubPlace">
	  <xsl:text>, </xsl:text>
	  <xsl:value-of select="pubPlace"/>
	</xsl:if>
	<xsl:if test="date">
	  <xsl:text>, </xsl:text>
		<xsl:value-of select="date"/>
	</xsl:if>
      </dc:publisher>
    </xsl:if>
  </xsl:template>

  <xsl:template name="dc_date">
    <dc:date>
      <xsl:value-of select="."/>
    </dc:date>
  </xsl:template>
  
  <xsl:template name="dc_rights">
    <dc:rights>
      <xsl:value-of select="."/>
    </dc:rights>
  </xsl:template>

  <xsl:template name="dc_subject">
      <dc:subject>
	<xsl:value-of select="."/>
      </dc:subject>
  </xsl:template>
  
  <xsl:template name="dc_contributor">
    <dc:contributor>
      <xsl:value-of select="."/>
    </dc:contributor>
  </xsl:template>
</xsl:stylesheet>
