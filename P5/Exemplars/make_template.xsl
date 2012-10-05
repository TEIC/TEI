<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet xmlns="http://www.tei-c.org/ns/1.0"
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    xpath-default-namespace="http://www.tei-c.org/ns/1.0"
    version="2.0">
  <xsl:param name="file"/>
  <xsl:template name="main">
    <xsl:for-each select="doc(concat($file,'.xml'))">
      <xsl:result-document href="{/TEI/@n}.xml" indent="yes">
      <xsl:processing-instruction name="xml-model">
	<xsl:text>href="http://www.tei-c.org/release/xml/tei/custom/schema/relaxng/</xsl:text>
	<xsl:value-of select="$file"/>
	<xsl:text>" type="application/xml" schematypens="http://relaxng.org/ns/structure/1.0"</xsl:text>
      </xsl:processing-instruction>
      <xsl:processing-instruction name="xml-model">
	<xsl:text>href="http://www.tei-c.org/release/xml/tei/custom/schema/relaxng/</xsl:text>
	<xsl:value-of select="$file"/>
	<xsl:text>" type="application/xml"
	schematypens="http://purl.oclc.org/dsdl/schematron"</xsl:text>
      </xsl:processing-instruction>
      <xsl:apply-templates select="*|text()|comment|processing-instruction()"/>
    </xsl:result-document>

    <xsl:result-document href="{/TEI/@n}.properties">
smallIcon=../icons/TEI_16.gif
bigIcon=../icons/TEI_48.png
    </xsl:result-document>
    </xsl:for-each>
  </xsl:template>
  
  <xsl:template match="TEI/@n"/>

  <xsl:template match="*">
    <xsl:copy>
      <xsl:apply-templates select="@*|*|text()|comment|processing-instruction()"/>
    </xsl:copy>
  </xsl:template>

  <xsl:template match="@*|text()|comment|processing-instruction()">
    <xsl:copy-of select="."/>
  </xsl:template>

</xsl:stylesheet>



