<xsl:stylesheet
    xmlns:xd="http://www.pnp-software.com/XSLTdoc"
    xmlns:tei="http://www.tei-c.org/ns/1.0"
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform" 
    xmlns:XSL="http://www.w3.org/1999/XSL/TransformAlias" 
    exclude-result-prefixes="xsl tei xd" 
    version="2.0"
>

<xsl:output method="xml" indent="yes" encoding="utf-8"/>

<xsl:namespace-alias stylesheet-prefix="XSL" result-prefix="xsl"/>

<xsl:template match="@*|text()|comment()|processing-instruction()">
 <xsl:copy-of select="."/>
</xsl:template>


<xsl:template match="*">
  <xsl:copy>
    <xsl:apply-templates 
	select="*|@*|processing-instruction()|comment()|text()"/>
  </xsl:copy>
</xsl:template>

<xsl:template match="xd:short">
  <xsl:apply-templates/>
</xsl:template>

<xsl:template match="xd:detail">
  <xsl:apply-templates/>
</xsl:template>

<xsl:template match="xd:author">
 <p xmlns="http://www.oxygenxml.com/ns/doc/xsl">Author: <xsl:apply-templates/></p>
</xsl:template>

<xsl:template match="xd:copyright">
 <p xmlns="http://www.oxygenxml.com/ns/doc/xsl">Copyright: <xsl:apply-templates/></p>
</xsl:template>

<xsl:template match="xd:cvsId">
 <p xmlns="http://www.oxygenxml.com/ns/doc/xsl">Id: <xsl:apply-templates/></p>
</xsl:template>

<xsl:template match="xd:param">
 <param xmlns="http://www.oxygenxml.com/ns/doc/xsl" name="{@name}"><xsl:apply-templates/></param>
</xsl:template>

<xsl:template match="xd:doc">
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <xsl:if test="@type='stylesheet'">
      <xsl:attribute name="scope">
	<xsl:value-of select="@type"/>
      </xsl:attribute>
    </xsl:if>
    <xsl:copy-of select="@class"/>
    <xsl:copy-of select="@type"/>
    <desc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <xsl:variable name="stuff">
      <xsl:choose>
	<xsl:when test="xd:detail and xd:short and
			string-length(normalize-space(xd:detail))=0">
	  <xsl:apply-templates select="xd:short"/>
	</xsl:when>
	<xsl:when test="xd:detail and xd:short and xd:detail='&#160;'">
	  <xsl:apply-templates select="xd:short"/>
	</xsl:when>
	<xsl:when test="xd:detail and xd:short">
	  <p xmlns="http://www.oxygenxml.com/ns/doc/xsl"><xsl:apply-templates select="xd:short"/></p>
	  <p xmlns="http://www.oxygenxml.com/ns/doc/xsl"><xsl:apply-templates select="xd:detail"/></p>
	</xsl:when>
	<xsl:otherwise>
	  <xsl:apply-templates select="xd:short|xd:detail|text()"/>
	</xsl:otherwise>
      </xsl:choose>
      </xsl:variable>
      <xsl:choose>
	<xsl:when test="starts-with($stuff,'Process elements  tei:')">
	  <xsl:message>reject <xsl:value-of select="$stuff"/></xsl:message>
	</xsl:when>
	<xsl:otherwise>
	  <xsl:copy-of select="$stuff"/>
	</xsl:otherwise>
      </xsl:choose>
      
      <xsl:apply-templates select="xd:param|xd:author|xd:cvsId|xd:copyright"/>
    </desc>
  </doc>
</xsl:template>

<!--
<doc class="CSS" type="string">
<doc class="figures" type="decimal">
<doc class="figures" type="string">
<doc class="figures">
<doc class="headings" type="boolean">
<doc class="headings" type="string">
<doc class="headings">
<doc class="hook">
<doc class="i18n">
<doc class="layout">
<doc class="links" type="anyURI">
<doc class="links" type="string">
<doc class="localisation" type="string">
<doc class="localisation">
<doc class="misc" type="anyURI">
<doc class="misc" type="boolean">
<doc class="numbering" type="boolean">
<doc class="numbering" type="integer">
<doc class="numbering" type="string">
<doc class="numbering">
<doc class="output" type="boolean">
<doc class="output" type="string">
<doc class="style" type="boolean">
<doc class="style" type="string">
<doc class="style">
<doc class="tables" type="string">
<doc class="tables">
<doc type="anyURI" class="CSS">
<doc type="anyURI" class="layout">
<doc type="anyURI" class="links">
<doc type="anyURI" class="toc">
<doc type="boolean" class="cssFileInclude">
<doc type="boolean" class="figures">
<doc type="boolean" class="layout">
<doc type="boolean" class="links">
<doc type="boolean" class="misc">
<doc type="boolean" class="numbering">
<doc type="boolean" class="output">
<doc type="boolean" class="style">
<doc type="boolean" class="tables">
<doc type="boolean" class="toc">
<doc type="float" class="layout">
<doc type="integer" class="figures">
<doc type="integer" class="layout">
<doc type="integer" class="output">
<doc type="integer" class="toc">
<doc type="string" class="CSS">
<doc type="string" class="figures">
<doc type="string" class="layout">
<doc type="string" class="links">
<doc type="string" class="misc">
<doc type="string" class="output">
<doc type="string" class="style">
<doc type="string" class="tables">
<doc type="string" class="toc">
<doc type="string" class="userpackage">
<doc type="string">
<doc type="stylesheet">
<doc>

-->
</xsl:stylesheet>
