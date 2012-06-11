<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
  xmlns:xs="http://www.w3.org/2001/XMLSchema"
  xmlns:xd="http://www.oxygenxml.com/ns/doc/xsl"
  exclude-result-prefixes="xs xd"
  version="2.0">
  <xd:doc scope="stylesheet">
    <xd:desc>
      <xd:p><xd:b>Created on:</xd:b> May 31, 2012</xd:p>
      <xd:p><xd:b>Author:</xd:b> mholmes</xd:p>
      <xd:p>This transformation is designed to modify the main config.xml file for TEI Jenkins. It 
        currently does only one thing:
      <xd:ul>
        <xd:li>Replaces the existing Jenkins version number in the file with the current 
        version extracted using the Jenkins CLI tool.</xd:li>
      </xd:ul>
        This is basically an identity transform with one active template.
      </xd:p>
    </xd:desc>
  </xd:doc>
  
  <xsl:output method="xml" encoding="UTF-8" indent="yes"/>
  
  <xsl:param name="jinksVersion"></xsl:param>
  
  <xsl:template match="/">
    <xsl:apply-templates/>
  </xsl:template>
  
<!-- Insert the correct Jenkins version number, as specified in the parameter. -->
  
  <xsl:template match="version"><version><xsl:value-of select="$jinksVersion"/></version></xsl:template>
  
  
  <xsl:template match="@*|*|processing-instruction()|comment()" priority="-1">
    <xsl:copy>
      <xsl:apply-templates select="*|@*|text()|processing-instruction()|comment()"/>
    </xsl:copy>
  </xsl:template>
  
</xsl:stylesheet>