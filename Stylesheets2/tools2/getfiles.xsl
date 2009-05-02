<?xml version="1.0"?>
<xsl:stylesheet version="2.0"
      exclude-result-prefixes="n tei xs"
      xpath-default-namespace="http://www.tei-c.org/ns/1.0"
      xmlns:n="www.example.com"
      xmlns:tei="http://www.tei-c.org/ns/1.0"
      xmlns:xs="http://www.w3.org/2001/XMLSchema"
      xmlns:xsl="http://www.w3.org/1999/XSL/Transform">

  <xsl:param name="corpus"/>
  <xsl:param name="base"/>

  <xsl:key name="All" match="*" use="1"/>
  <xsl:key name="E" match="*" use="local-name()"/>

  <xsl:template match="/">
    <xsl:variable name="pathlist">
      <xsl:value-of 
	  select="concat($corpus,
		  '?select=*.xml;recurse=yes;on-error=warning')"/>
    </xsl:variable>
    <xsl:variable name="docs" select="collection($pathlist)"/> 
    <xsl:variable name="all">
      <n:ROOT>
	<xsl:for-each select="$docs/TEI">
	  <TEI xn="{base-uri(.)}">
	    <xsl:copy-of select="@*|*"/>
	  </TEI>
	</xsl:for-each>
	<xsl:for-each select="$docs/teiCorpus">
	  <teiCorpus xn="{base-uri(.)}">
	    <xsl:copy-of select="@*|*"/>
	  </teiCorpus>
	</xsl:for-each>
      </n:ROOT>
    </xsl:variable>
    <xsl:for-each select="$all">
      <xsl:call-template name="processAll"/>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="processAll">
    <html>
      <body>
	<table border="1">
	  <xsl:for-each-group select="key('All',1)" group-by="local-name()">
	    <xsl:sort select="current-grouping-key()"/>
	    <tr valign="top">
	      <td> 
		<xsl:value-of select="current-grouping-key()"/>
	      </td> 
	      <td> 
		<xsl:value-of select="count(current-group())"/>
	      </td> 
	    </tr>
	  </xsl:for-each-group>
	</table>
      </body>
    </html>
  </xsl:template>
</xsl:stylesheet>


