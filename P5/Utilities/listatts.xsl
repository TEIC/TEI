<xsl:stylesheet 
    version="2.0"
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform" 
    xmlns:tei="http://www.tei-c.org/ns/1.0"
    xmlns:teix="http://www.tei-c.org/ns/Examples"
    xpath-default-namespace="http://www.tei-c.org/ns/1.0">
  
  <xsl:output method="html" indent="yes" encoding="utf-8"/>
  
  
  <xsl:template match="/">
    <html>
      <body>
	<xsl:call-template name="list">
	  <xsl:with-param name="head">Rend</xsl:with-param>
	  <xsl:with-param name="contents">
	    <xsl:for-each select="//teix:*/@rend">
	      <tei:foo>
		<xsl:value-of select="."/>
		<xsl:text>&gt;</xsl:text>
		<xsl:value-of select="name(..)"/> 
	      </tei:foo> 
	    </xsl:for-each>
	  </xsl:with-param>
	</xsl:call-template>
	<xsl:call-template name="list">
	  <xsl:with-param name="head">N</xsl:with-param>
	  <xsl:with-param name="contents">
	    <xsl:for-each select="//teix:*/@n">
	      <tei:foo>
		<xsl:value-of select="."/>
		<xsl:text>&gt;</xsl:text>
		<xsl:value-of select="name(..)"/> 
	      </tei:foo> 
	    </xsl:for-each>
	  </xsl:with-param>
	</xsl:call-template>
	<xsl:call-template name="list">
	  <xsl:with-param name="head">Place</xsl:with-param>
	  <xsl:with-param name="contents">
	    <xsl:for-each select="//teix:*/@place">
	      <tei:foo>
		<xsl:value-of select="."/>
		<xsl:text>&gt;</xsl:text>
		<xsl:value-of select="name(..)"/> 
	      </tei:foo> 
	    </xsl:for-each>
	  </xsl:with-param>
	</xsl:call-template>
	<xsl:call-template name="list">
	  <xsl:with-param name="head">Type</xsl:with-param>
	  <xsl:with-param name="contents">
	    <xsl:for-each select="//teix:*/@type">
	      <tei:foo>
		<xsl:value-of select="."/>
		<xsl:text>&gt;</xsl:text>
		<xsl:value-of select="name(..)"/> 
	      </tei:foo> 
	    </xsl:for-each>
	  </xsl:with-param>
	</xsl:call-template>
      </body>
    </html>
  </xsl:template>

<xsl:template name="list">
  <xsl:param name="head"/>
  <xsl:param name="contents"/>
  <h1><xsl:value-of select="$head"/></h1>
  <table>
    <xsl:for-each-group select="$contents/foo" group-by=".">
      <xsl:sort select="."/>
      <tr>
	<xsl:for-each select="tokenize(current-grouping-key(),'&gt;')">
	  <td><xsl:value-of select="."/></td>
	</xsl:for-each>
	<td><xsl:value-of select="count(current-group())"/></td>
      </tr>
    </xsl:for-each-group>
  </table>
  </xsl:template>
</xsl:stylesheet>
