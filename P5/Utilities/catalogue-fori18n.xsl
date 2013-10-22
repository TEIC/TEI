<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet xmlns:tei="http://www.tei-c.org/ns/1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xmlns="http://www.w3.org/1999/xhtml" xpath-default-namespace="http://www.tei-c.org/ns/1.0" version="2.0">
  <xsl:output method="html"/>
  <xsl:param name="lang">fr</xsl:param>
  <xsl:variable name="top" select="/"/>
  <xsl:template match="/">
    <html>
      <head>
        <title>TEI documentation strings</title>
      </head>
      <body>
        <table style="border-collapse:collapse;border-spacing:0;">
          <thead>
            <tr>
              <th>name</th>
              <th>attribute name</th>
              <th>desc</th>
              <th>translation</th>
              <th>gloss</th>
              <th>translation</th>
              <th>remarks</th>
              <th>translation</th>
              <th>responsibility</th>
            </tr>
          </thead>
          <xsl:for-each select="//elementSpec|//macroSpec|//classSpec">
            <xsl:sort select="@ident"/>
            <xsl:message>
              <xsl:value-of select="@ident"/>
            </xsl:message>
            <xsl:call-template name="show">
	      <xsl:with-param name="att" select="false()"/>
	    </xsl:call-template>
            <xsl:for-each select=".//attDef">
              <xsl:message>....<xsl:value-of
	      select="@ident"/></xsl:message>
	      <xsl:call-template name="show">
		<xsl:with-param name="att" select="true()"/>
	      </xsl:call-template>
            </xsl:for-each>
          </xsl:for-each>
        </table>
      </body>
    </html>
  </xsl:template>
  <xsl:template name="check">
    <xsl:param name="where" select="."/>
    <xsl:param name="data"/>
    <xsl:if test="string-length($data)=0 and *[name()=$where and not(@xml:lang)]">
      <xsl:attribute name="style">background-color: red</xsl:attribute>
    </xsl:if>
    <xsl:value-of select="normalize-space($data)"/>
  </xsl:template>
  <xsl:template  name="show">
    <xsl:param name="att" select="false()"/>
    <tr>
      <xsl:choose>
	<xsl:when test="$att">
	  <td style="border: 1px solid black; padding: 2px;vertical-align:top">—</td>
	  <td style="border: 1px solid black; padding: 2px;vertical-align:top">
	    <xsl:call-template name="check">
	      <xsl:with-param name="data" select="@ident"/>
	    </xsl:call-template>
	  </td>
	</xsl:when>
	<xsl:otherwise>
	  <td style="border: 1px solid black; padding: 2px;vertical-align:top">
	    <xsl:call-template name="check">
	      <xsl:with-param name="data" select="@ident"/>
	    </xsl:call-template>
	  </td>
	  <td style="border: 1px solid black; padding: 2px;vertical-align:top"></td>
	</xsl:otherwise>
      </xsl:choose>
      <td style="border: 1px solid black; padding: 2px;vertical-align:top;font-style:italic">
        <xsl:call-template name="check">
          <xsl:with-param name="data" select="desc[not(@xml:lang) or  @xml:lang='en']"/>
        </xsl:call-template>
      </td>
      <td style="border: 1px solid black; padding: 2px;vertical-align:top">
        <xsl:call-template name="check">
	  <xsl:with-param name="where">desc</xsl:with-param>
          <xsl:with-param name="data" select="desc[@xml:lang=$lang]"/>
        </xsl:call-template>
      </td>
      <td style="border: 1px solid black; padding: 2px;vertical-align:top;font-style:italic">
        <xsl:call-template name="check">
          <xsl:with-param name="data" select="gloss[not(@xml:lang) or  @xml:lang='en']"/>
        </xsl:call-template>
      </td>
      <td style="border: 1px solid black; padding: 2px;vertical-align:top">
        <xsl:call-template name="check">
	  <xsl:with-param name="where">gloss</xsl:with-param>
          <xsl:with-param name="data" select="gloss[@xml:lang=$lang]"/>
        </xsl:call-template>
      </td>
      <td style="border: 1px solid black; padding: 2px;vertical-align:top;font-style:italic">
        <xsl:call-template name="check">
          <xsl:with-param name="data" select="remarks[not(@xml:lang) or  @xml:lang='en'][1]"/>
        </xsl:call-template>
      </td>
      <td style="border: 1px solid black; padding: 2px;vertical-align:top;">
        <xsl:call-template name="check">
	  <xsl:with-param name="where">remarks</xsl:with-param>
          <xsl:with-param name="data" select="remarks[@xml:lang=$lang][1]"/>
        </xsl:call-template>
      </td>
      <td style="border: 1px solid black; padding: 2px;vertical-align:top"/>
    </tr>
  </xsl:template>
</xsl:stylesheet>
