<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet xmlns:tei="http://www.tei-c.org/ns/1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xmlns="http://www.w3.org/1999/xhtml" xpath-default-namespace="http://www.tei-c.org/ns/1.0" version="2.0">
  <xsl:import href="/usr/share/xml/tei/stylesheet/common/functions.xsl"/>
  <xsl:output method="html"/>
  <xsl:param name="lang">fr</xsl:param>
  <xsl:param name="teiP5RefBase">http://www.tei-c.org/release/doc/tei-p5-doc/en/html/ref-</xsl:param>
  <xsl:variable name="top" select="/"/>
  <xsl:template match="/">
    <html>
      <head>
        <title>TEI documentation strings</title>
	<meta charset="utf-8"/>
      </head>
      <body>
        <table style="border-collapse:collapse;border-spacing:0;">
          <thead>
            <tr>
              <th>name</th>
	      <th>module</th>
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
            <xsl:call-template name="show"/>
            <xsl:for-each select=".//attDef">
	      <xsl:call-template name="show"/>
	      <xsl:for-each select=".//valItem[desc|gloss]">
	      <xsl:call-template name="show"/>
	      </xsl:for-each>
            </xsl:for-each>
          </xsl:for-each>
        </table>
      </body>
    </html>
  </xsl:template>

  <xsl:template name="display">
    <xsl:param name="where" select="."/>
    <xsl:param name="data"/>
    <xsl:param name="older" select="false()"/>
    <xsl:choose>
    <xsl:when test="string-length($data)=0 and *[name()=$where and
		    (@xml:lang='en' or not(@xml:lang))]">
      <xsl:attribute name="style">background-color: red</xsl:attribute>
    </xsl:when>
    <xsl:when test="$older">
      <xsl:attribute name="style">background-color: yellow</xsl:attribute>
    </xsl:when>
    </xsl:choose>
    <xsl:apply-templates select="$data"/>
  </xsl:template>

  <xsl:template  name="show">
    <tr>
      <td style="font-weight:bold;border: 1px solid black; padding: 2px;vertical-align:top">
	<xsl:variable name="defSequence" select="ancestor-or-self::*[@ident]/@ident"/>
	<xsl:message>  <xsl:value-of select="$defSequence" separator="/"/></xsl:message>
	<a href="{concat($teiP5RefBase,$defSequence[1])}">
	  <xsl:value-of select="$defSequence" separator="/"/>
	</a>
      </td>
      <td>
	<xsl:choose>
	  <xsl:when test="self::tei:attDef|self::tei:valItem">
	    <xsl:value-of select="ancestor-or-self::tei:*[@module]/@module"/>
	  </xsl:when>
	  <xsl:otherwise>
	    <xsl:value-of select="@module"/>
	  </xsl:otherwise>
	</xsl:choose>
      </td>
      <td style="border: 1px solid black; padding: 2px;vertical-align:top;font-style:italic">
	<xsl:call-template name="display">
	    <xsl:with-param name="data" select="desc[not(@xml:lang) or  @xml:lang='en']"/>
	</xsl:call-template>
      </td>
      <td style="border: 1px solid black; padding: 2px;vertical-align:top">
	<xsl:call-template name="display">
	    <xsl:with-param name="where">desc</xsl:with-param>
	    <xsl:with-param name="data"
			    select="desc[@xml:lang=$lang]"/>
	    <xsl:with-param name="older" select="if
	      (desc[@xml:lang='en']/@versionDate &gt;
	      desc[@xml:lang=$lang]/@versionDate) then true() else false()"/>
	</xsl:call-template>
      </td>
      
      <td style="border: 1px solid black; padding: 2px;vertical-align:top;font-style:italic">
	<xsl:call-template name="display">
	  <xsl:with-param name="data" select="gloss[not(@xml:lang) or
					      @xml:lang='en']"/>	  
	</xsl:call-template>
      </td>
      <td style="border: 1px solid black; padding: 2px;vertical-align:top">
	<xsl:call-template name="display">
	  <xsl:with-param name="where">gloss</xsl:with-param>
	  <xsl:with-param name="data" select="gloss[@xml:lang=$lang]"/>
	    <xsl:with-param name="older" select="if
	      (gloss[@xml:lang='en']/@versionDate &gt;
	      gloss[@xml:lang=$lang]/@versionDate) then true() else false()"/>
	</xsl:call-template>
      </td>
      <td style="border: 1px solid black; padding: 2px;vertical-align:top;font-style:italic">
	<xsl:call-template name="display">
	  <xsl:with-param name="data" select="remarks[not(@xml:lang) or  @xml:lang='en'][1]"/>
	</xsl:call-template>
      </td>
      <td style="border: 1px solid black; padding: 2px;vertical-align:top;">
	<xsl:call-template name="display">
	  <xsl:with-param name="where">remarks</xsl:with-param>
	  <xsl:with-param name="data" select="remarks[@xml:lang=$lang][1]"/>
	    <xsl:with-param name="older" select="if
	      (remarks[@xml:lang='en']/@versionDate &gt;
	      remarks[@xml:lang=$lang]/@versionDate) then true() else false()"/>
	</xsl:call-template>
      </td>
      <td style="border: 1px solid black; padding: 2px;vertical-align:top"/>
    </tr>
  </xsl:template>

  <xsl:template match="gloss|desc|remarks|remarks/p">
    <xsl:apply-templates/>
  </xsl:template>

  <xsl:template match="@ident">
    <xsl:value-of select="."/>
</xsl:template>

  <xsl:template match="@*">
    <xsl:text> </xsl:text>
    <xsl:value-of select="local-name()"/>
    <xsl:text>="</xsl:text>
    <xsl:value-of select="."/>
    <xsl:text>"</xsl:text>
  </xsl:template>

  <xsl:template match="*">
    <xsl:text>&lt;</xsl:text>
    <xsl:value-of select="local-name()"/>
    <xsl:apply-templates select="@*"/>
    <xsl:text>&gt;</xsl:text>
    <xsl:apply-templates select="*|text()"/>
    <xsl:text>&lt;/</xsl:text>
    <xsl:value-of select="local-name()"/>
    <xsl:text>&gt;</xsl:text>
  </xsl:template>


  <xsl:template match="text()">
    <xsl:choose>
      <xsl:when test="ancestor::*[@xml:space][1]/@xml:space='preserve'">
        <xsl:value-of select="tei:escapeChars(.,parent::*)"/>
      </xsl:when>
      <xsl:otherwise>
        <!-- Retain one leading space if node isn't first, has
	     non-space content, and has leading space.-->
        <xsl:if test="position()!=1 and          matches(.,'^\s') and          normalize-space()!=''">
          <xsl:call-template name="space"/>
        </xsl:if>
        <xsl:value-of select="tei:escapeChars(normalize-space(.),parent::*)"/>
        <xsl:choose>
          <!-- node is an only child, and has content but it's all space -->
          <xsl:when test="last()=1 and string-length()!=0 and      normalize-space()=''">
            <xsl:call-template name="space"/>
          </xsl:when>
          <!-- node isn't last, isn't first, and has trailing space -->
          <xsl:when test="position()!=1 and position()!=last() and matches(.,'\s$')">
            <xsl:call-template name="space"/>
          </xsl:when>
          <!-- node isn't last, is first, has trailing space, and has non-space content   -->
          <xsl:when test="position()=1 and matches(.,'\s$') and normalize-space()!=''">
            <xsl:call-template name="space"/>
          </xsl:when>
        </xsl:choose>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>

</xsl:stylesheet>
