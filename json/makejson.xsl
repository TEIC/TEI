<xsl:stylesheet
    xpath-default-namespace="http://www.tei-c.org/ns/1.0"
    xmlns:tei="http://www.tei-c.org/ns/1.0"
    xmlns="http://www.tei-c.org/ns/1.0"
    xmlns:xs="http://www.w3.org/2001/XMLSchema"                
    exclude-result-prefixes="tei xs"
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    version="2.0"
    >
<xsl:include href="jsonlib.xsl"/>

<xsl:strip-space elements="*"/>
<xsl:output method="text" encoding="utf-8" />
  
<xsl:template match="/">
  <xsl:call-template name="main"/>
</xsl:template>

<xsl:template name="main">
  <xsl:variable name="docs" select="collection('.?select=*.xml;recurse=yes;on-error=warning')"/> 
  <xsl:variable name="all" as="xs:string+">
    <xsl:for-each select="$docs/TEI/teiHeader/fileDesc">
      <xsl:variable name="p"  as="xs:string+">
	<xsl:value-of select="tei:json('date',sourceDesc//publicationStmt//date,true())"/>
	<xsl:value-of
	    select="tei:json('title',tei:jsonString(titleStmt/title[1]),true())"/>
      </xsl:variable>
      <xsl:value-of select="tei:jsonObject(($p))"/>
    </xsl:for-each>
  </xsl:variable>
  <xsl:value-of select="tei:jsonObject(tei:jsonArray('data',($all),false()))"/>
</xsl:template>

</xsl:stylesheet>
