<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet xpath-default-namespace="http://www.tei-c.org/ns/1.0"
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    xmlns="http://www.w3.org/1999/xhtml"
    version="2.0">
  <xsl:output method="html"/>

  <xsl:param name="lang">fr</xsl:param>
  
  <xsl:variable name="top" select="/"/>
  
  
  <xsl:template match="/">
    <html>
      <head>
	<title>TEI documentation strings</title>
      </head>
      <body>
      <table>
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
	</tr></thead>
	<xsl:for-each select="//elementSpec|//macroSpec|//classSpec">
	  <xsl:sort select="@ident"/>
	  <xsl:message><xsl:value-of select="@ident"/></xsl:message>
	  <tr>
	    <td><xsl:value-of select="normalize-space(@ident)"/></td>
	    <td></td>
	    <td><xsl:value-of select="normalize-space(desc[not(@xml:lang) or @xml:lang='en'])"/></td>
	    <td><xsl:value-of select="normalize-space(desc[@xml:lang=$lang])"/></td>
	    <td><xsl:value-of select="normalize-space(gloss[not(@xml:lang) or @xml:lang='en'])"/></td>
	    <td><xsl:value-of select="normalize-space(gloss[@xml:lang=$lang])"/></td>
	    <td><xsl:value-of select="normalize-space(remarks[not(@xml:lang) or @xml:lang='en'][1])"/></td>
	    <td><xsl:value-of select="normalize-space(remarks[@xml:lang=$lang][1])"/></td>
	    <td></td>
	  </tr>
	  <xsl:for-each select=".//attDef">
	  <xsl:message>....<xsl:value-of select="@ident"/></xsl:message>
	  <tr>
	    <td>-</td>
	    <td><xsl:value-of select="normalize-space(@ident)"/></td>
	    <td><xsl:value-of select="normalize-space(desc[not(@xml:lang) or @xml:lang='en'])"/></td>
	    <td><xsl:value-of select="normalize-space(desc[@xml:lang=$lang])"/></td>
	    <td><xsl:value-of select="normalize-space(gloss[not(@xml:lang) or @xml:lang='en'])"/></td>
	    <td><xsl:value-of select="normalize-space(gloss[@xml:lang=$lang])"/></td>
	    <td><xsl:value-of select="normalize-space(remarks[1][not(@xml:lang) or @xml:lang='en'])"/></td>
	    <td><xsl:value-of select="normalize-space(remarks[1][@xml:lang=$lang])"/></td>
	    <td></td>
	  </tr>
	  </xsl:for-each>
	</xsl:for-each>
      </table>
      </body>
    </html>
</xsl:template>
</xsl:stylesheet>



