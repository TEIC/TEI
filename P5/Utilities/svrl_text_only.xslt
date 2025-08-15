<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
  xmlns:xs="http://www.w3.org/2001/XMLSchema"
  xmlns:svrl="http://purl.oclc.org/dsdl/svrl"
  xpath-default-namespace="http://purl.oclc.org/dsdl/svrl"
  exclude-result-prefixes="#all"
  version="3.0">

  <!--
      ** extracts only the messages and location of node for which message
      ** was generated from SVRL output. 
      ** Written by Syd Bauman
      ** Copyleft 2023 Syd Bauman and the TEI Consortium
  -->

  <xsl:variable name="Qtei"  select="'Q\{http://www\.tei-c\.org/ns/1\.0\}'"/>
  <xsl:variable name="Qteix" select="'Q\{http://www\.tei-c\.org/ns/Examples\}'"/>

  <xsl:output method="text"/>
  <xsl:mode on-no-match="shallow-skip"/>
  
  <xsl:template match="svrl:successful-report/svrl:text|svrl:failed-assert/svrl:text">
    <!-- Use prefixes instead of EQName notation for the most common namespaces,
         just to make the logs easier for humans to read. -->
    <xsl:variable name="loc" select="../@location => replace( $Qtei, 'tei:') => replace( $Qteix, 'teix:')"/>
    <!-- I used <value-of> because <sequence> gave me some odd whitespace;
         not that it matters. -->
    <xsl:value-of select="'at '||$loc||' —&#x0A;'"/>
    <xsl:value-of select="'   '||normalize-space(.)||'&#x0A;'"/>
  </xsl:template>
  
</xsl:stylesheet>
