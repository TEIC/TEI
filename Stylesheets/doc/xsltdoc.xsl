<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet xmlns:xdt="http://www.pnp-software.com/XSLTdocTemplate" xmlns:util="http://www.pnp-software.com/util" xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xmlns:xd="http://www.pnp-software.com/XSLTdoc" exclude-result-prefixes="xdt util xd" version="2.0">
  <xsl:import href="../xsltdoc/xsl/core.xsl"/>
  <xsl:include href="../xsltdoc/xsl/properties/author.xsl"/>
  <xsl:include href="../xsltdoc/xsl/properties/cvsId.xsl"/>
  <xsl:include href="../xsltdoc/xsl/properties/svnId.xsl"/>
  <xsl:include href="../xsltdoc/xsl/properties/copyright.xsl"/>
  <xsl:include href="../xsltdoc/xsl/inlineTags/xml.xsl"/>

  <xsl:template match="xsl:param" mode="printProperties">
  <xsl:text>Default: </xsl:text>
  <span xmlns="http://www.w3.org/1999/xhtml" class="paramProperties">
    <xsl:choose>
      <xsl:when test="*">
	<xsl:apply-templates select="*|text()" mode="verbatim"/>
      </xsl:when>
      <xsl:otherwise>
	<xsl:value-of select="."/>
      </xsl:otherwise>
    </xsl:choose>
  </span>
</xsl:template>
  <xd:doc type="stylesheet">
    Frontend for core.xsl
    <xd:author>ibirrer</xd:author>
    <xd:cvsId>$Id$</xd:cvsId>
    <xd:copyright>2004, P&amp;P Software GmbH</xd:copyright>
  </xd:doc>
  <xsl:output name="xhtml" omit-xml-declaration="yes" method="xml" doctype-public="-//W3C//DTD XHTML 1.0 Transitional//EN" doctype-system="http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd" indent="no" encoding="iso-8859-1"/>
  <xsl:template match="/">
    <xsl:apply-templates>
      <xsl:with-param name="config" tunnel="yes" as="element()">
        <config>
          <htmlTemplate href="../../doc/doctemplate.html"/>
          <page id="mainPage" menu="yes" label="Summary" targetFilename="index.html"/>
          <page id="stylesheetList" menu="yes" label="Stylesheet List"/>
          <page id="functionTemplateList" menu="yes" label="Template List"/>
        </config>
      </xsl:with-param>
    </xsl:apply-templates>
  </xsl:template>
  <xsl:template xmlns="http://www.w3.org/1999/xhtml" match="xdt:menuLinks" mode="htmlTemplate">
    <xsl:param name="config" tunnel="yes" as="element()"/>
    <xsl:param name="currentPage" tunnel="yes" as="element()"/>
    <xsl:for-each select="$config/page[@menu='yes']">
      <span class="menuLink">
        <xsl:choose>
          <xsl:when test="$currentPage is .">
            <a class="menuLinkCurrent" href="{util:getRelativeUriFiles( @uriAbs, $currentPage/@uriAbs, true() )}">
              <xsl:value-of select="@label"/>
            </a>
          </xsl:when>
          <xsl:otherwise>
            <a class="menuLink" href="{util:getRelativeUriFiles( @uriAbs, $currentPage/@uriAbs, true() )}">
              <xsl:value-of select="@label"/>
            </a>
          </xsl:otherwise>
        </xsl:choose>
      </span>
    </xsl:for-each>
  </xsl:template>
  <xsl:template match="text()" mode="verbatim">
    <xsl:value-of select="."/>
  </xsl:template>
  <xsl:template match="*" mode="verbatim">
    <xsl:text>&lt;</xsl:text>
    <xsl:value-of select="local-name(.)"/>
    <xsl:for-each select="@*">
      <xsl:text>&#10;</xsl:text>
      <xsl:value-of select="local-name(.)"/>
      <xsl:text>="</xsl:text>
      <xsl:value-of select="."/>
      <xsl:text>"</xsl:text>
    </xsl:for-each>
    <xsl:if test="not(*|text())">/</xsl:if>
    <xsl:text>&gt;</xsl:text>
    <xsl:if test="*|text()">
      <xsl:apply-templates select="*|text()" mode="verbatim"/>
      <xsl:text>&lt;/</xsl:text>
      <xsl:value-of select="local-name(.)"/>
      <xsl:text>&gt;</xsl:text>
    </xsl:if>
  </xsl:template>
</xsl:stylesheet>
