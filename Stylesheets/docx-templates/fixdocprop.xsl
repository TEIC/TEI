<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet 
    version="2.0"
    xmlns:p="http://schemas.openxmlformats.org/officeDocument/2006/custom-properties" 
    xmlns:vt="http://schemas.openxmlformats.org/officeDocument/2006/docPropsVTypes"    
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    xmlns:w="http://schemas.openxmlformats.org/wordprocessingml/2006/main"
    exclude-result-prefixes="w vt p">

  <xsl:output  encoding="UTF-8" standalone="yes" method="xml"/>

  <xsl:param name="template"/>
  <!-- identity transform -->
  
  <xsl:template match="@*|text()|comment()|processing-instruction()" >
    <xsl:copy-of select="."/>
  </xsl:template>
  
  <xsl:template match="*" >
    <xsl:copy>
      <xsl:apply-templates select="*|@*|processing-instruction()|comment()|text()" />
    </xsl:copy>
  </xsl:template>

  <xsl:template match="p:Properties">
    <xsl:copy>
      <xsl:apply-templates select="@*"/>
      <property pid="1" name="Template">
	<xsl:attribute name="fmtid">
	  <xsl:text>{D5CDD505-2E9C-101B-9397-08002B2CF9AE}</xsl:text>
	</xsl:attribute>
	<vt:lpwstr>
	  <xsl:value-of select="$template"/>
	</vt:lpwstr>
      </property>
    </xsl:copy>
  </xsl:template>
</xsl:stylesheet>
