<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet 
    xmlns:tei="http://www.tei-c.org/ns/1.0"
    xpath-default-namespace="http://www.tei-c.org/ns/1.0"
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
	    <td style="vertical-align:top"><xsl:call-template
					       name="check"><xsl:with-param
					       name="data" select="@ident"/></xsl:call-template></td>
	    <td style="vertical-align:top"></td>
	    <td style="vertical-align:top"><xsl:call-template
					       name="check"><xsl:with-param
					       name="data" select="desc[not(@xml:lang) or  @xml:lang='en']"/></xsl:call-template></td>
	    <td style="vertical-align:top"><xsl:call-template
					       name="check"><xsl:with-param
					       name="data" select="desc[@xml:lang=$lang]"/></xsl:call-template></td>
	    <td style="vertical-align:top"><xsl:call-template
					       name="check"><xsl:with-param
					       name="data" select="gloss[not(@xml:lang) or  @xml:lang='en']"/></xsl:call-template></td>
	    <td style="vertical-align:top"><xsl:call-template
					       name="check"><xsl:with-param
					       name="data" select="gloss[@xml:lang=$lang]"/></xsl:call-template></td>
	    <td style="vertical-align:top"><xsl:call-template
					       name="check"><xsl:with-param
					       name="data" select="remarks[not(@xml:lang) or  @xml:lang='en'][1]"/></xsl:call-template></td>
	    <td style="vertical-align:top"><xsl:call-template
					       name="check"><xsl:with-param
					       name="data" select="remarks[@xml:lang=$lang][1]"/></xsl:call-template></td>
	    <td style="vertical-align:top"></td>
	  </tr>
	  <xsl:for-each select=".//attDef">
	  <xsl:message>....<xsl:value-of select="@ident"/></xsl:message>
	  <tr>
	    <td style="vertical-align:top">-</td>
	    <td style="vertical-align:top"><xsl:call-template
					       name="check"><xsl:with-param
					       name="data" select="@ident"/></xsl:call-template></td>
	    <td style="vertical-align:top"><xsl:call-template
					       name="check"><xsl:with-param
					       name="data" select="desc[not(@xml:lang) or  @xml:lang='en']"/></xsl:call-template></td>
	    <td style="vertical-align:top"><xsl:call-template
					       name="check"><xsl:with-param
					       name="data" select="desc[@xml:lang=$lang]"/></xsl:call-template></td>
	    <td style="vertical-align:top"><xsl:call-template
					       name="check"><xsl:with-param
					       name="data" select="gloss[not(@xml:lang) or  @xml:lang='en']"/></xsl:call-template></td>
	    <td style="vertical-align:top"><xsl:call-template
					       name="check"><xsl:with-param
					       name="data" select="gloss[@xml:lang=$lang]"/></xsl:call-template></td>
	    <td style="vertical-align:top"><xsl:call-template
					       name="check"><xsl:with-param
					       name="data" select="remarks[not(@xml:lang) or  @xml:lang='en'][1]"/></xsl:call-template></td>
	    <td style="vertical-align:top"><xsl:call-template
					       name="check"><xsl:with-param
					       name="data" select="remarks[@xml:lang=$lang][1]"/></xsl:call-template></td>
	    <td style="vertical-align:top"></td>
	  </tr>
	  </xsl:for-each>
	</xsl:for-each>
      </table>
      </body>
    </html>
</xsl:template>

<xsl:template name="check">
  <xsl:param name="data"/>
  <xsl:if test="string-length($data)=0 and @xml:lang=$lang">
    <xsl:attribute name="style">background-color: red</xsl:attribute>
  </xsl:if>
  <xsl:value-of select="normalize-space($data)"/>
</xsl:template>
</xsl:stylesheet>



