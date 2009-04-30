<?xml version="1.0"?>
<xsl:stylesheet version="2.0"
      exclude-result-prefixes="saxon tei webaudit xs"
      xmlns:saxon="http://saxon.sf.net/"
      xmlns:tei="http://www.tei-c.org/ns/1.0"
      xmlns:webaudit="http://www.oucs.ox.ac.uk/namespaces/isteam/WebAudit"
      xmlns:xs="http://www.w3.org/2001/XMLSchema"
      xmlns:xsl="http://www.w3.org/1999/XSL/Transform">

  <xsl:param name="corpus"/>
  <xsl:param name="base"/>

  <xsl:key name="All" match="tei:*" use="1"/>
  <xsl:key name="E" match="tei:*" use="local-name()"/>

  <xsl:template match="/">
    <xsl:variable name="pathlist">
      <xsl:value-of 
	  select="concat($corpus,
		  '?select=*.xml;recurse=yes;on-error=warning')"/>
    </xsl:variable>
    <xsl:variable name="docs" select="collection($pathlist)"/> 
    <xsl:variable name="all">
      <tei:teiCorpus>
	<xsl:for-each select="$docs/tei:TEI">
	  <tei:TEI n="{base-uri(.)}">
	    <xsl:copy-of select="@*|*"/>
	  </tei:TEI>
	</xsl:for-each>
      </tei:teiCorpus>
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


