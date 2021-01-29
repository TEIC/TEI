<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet xmlns="http://www.tei-c.org/ns/1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
  xpath-default-namespace="http://www.tei-c.org/ns/1.0" version="2.0">
  <xsl:param name="file"/>
  <xsl:template name="main">
    <xsl:for-each select="doc(concat($file, '.template'))">
      <xsl:variable name="Name">
        <xsl:choose>
          <xsl:when test="*/processing-instruction()[name() = 'name']">
            <xsl:value-of select="*/processing-instruction()[name() = 'name']"/>
          </xsl:when>
          <xsl:otherwise>
            <xsl:value-of select="/*/@n"/>
          </xsl:otherwise>
        </xsl:choose>
      </xsl:variable>
      <xsl:if test="normalize-space($Name) = ''">
        <xsl:message>ERROR: The template <xsl:value-of select="$file"/>.template has no name; it
          needs the @n attribute on its root element before it can be turned into a working
          template.</xsl:message>
      </xsl:if>
      <xsl:message>Create template <xsl:value-of select="$Name"/> from <xsl:value-of select="$file"
        /></xsl:message>
      <xsl:result-document href="{$Name}.xml" indent="yes">
        <xsl:processing-instruction name="xml-model">
	<xsl:text>href="http://www.tei-c.org/release/xml/tei/custom/schema/relaxng/</xsl:text>
	<xsl:value-of select="$file"/>
	<xsl:text>.rng" type="application/xml" schematypens="http://relaxng.org/ns/structure/1.0"</xsl:text>
      </xsl:processing-instruction>
        <xsl:text>&#10;</xsl:text>
        <xsl:processing-instruction name="xml-model">
	<xsl:text>href="http://www.tei-c.org/release/xml/tei/custom/schema/relaxng/</xsl:text>
	<xsl:value-of select="$file"/>
	<xsl:text>.rng" type="application/xml"
	schematypens="http://purl.oclc.org/dsdl/schematron"</xsl:text>
      </xsl:processing-instruction>
        <xsl:text>&#10;</xsl:text>
        <xsl:apply-templates select="* | text() | comment() | processing-instruction()"/>
      </xsl:result-document>

      <xsl:result-document href="{$Name}.properties"> smallIcon=../icons/TEI_16.gif
        bigIcon=../icons/TEI_48.png </xsl:result-document>
    </xsl:for-each>
  </xsl:template>

  <xsl:template match="/*/processing-instruction()[name() = 'name']"/>

  <xsl:template match="/*/@n"/>

  <xsl:template match="*">
    <xsl:copy>
      <xsl:apply-templates select="@* | * | text() | comment() | processing-instruction()"/>
    </xsl:copy>
  </xsl:template>

  <xsl:template match="@* | text() | comment() | processing-instruction()">
    <xsl:copy-of select="."/>
  </xsl:template>

</xsl:stylesheet>
