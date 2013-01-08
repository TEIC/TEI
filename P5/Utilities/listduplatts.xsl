<xsl:stylesheet 
    version="2.0"
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform" 
    xmlns:tei="http://www.tei-c.org/ns/1.0"
    xmlns:teix="http://www.tei-c.org/ns/Examples"
    xmlns:xs="http://www.w3.org/2001/XMLSchema" 
    xpath-default-namespace="http://www.tei-c.org/ns/1.0">
  
  <xsl:output method="html" indent="yes" encoding="utf-8"/>
  
  <xsl:variable name="doc" select="/"/>
  
  <xsl:template match="/">
    <html>
      <head>
      <title>Attributes duplicated in classes</title>
      </head>
      <style type="text/css">
	td {
	vertical-align: top;
	}
      </style>
      <body>
	<table>
	  <xsl:variable name="x">
	    <xsl:for-each select="//classSpec//attDef[tei:dupl(@ident)]">
	      <xsl:sort select="ancestor::classSpec/@ident"/>
	      <xsl:sort select="@ident"/>
	      <att xmlns="http://www.tei-c.org/ns/1.0" desc="{desc[not(@xml:lang)]}" me="{@ident}" them="{ancestor::classSpec/@ident}"/>
	    </xsl:for-each>
	  </xsl:variable>
	  <xsl:for-each select="$x/att">
	    <xsl:variable name="me" select="@me"/>
	    <tr>
	      <td><xsl:number/></td>
	      <td><xsl:value-of select="@them"/>
	      <xsl:text>/@</xsl:text>
	      <span style="color:red"><xsl:value-of
	      select="$me"/></span>
	      </td>
	      <td><xsl:value-of select="@desc"/></td>
	      <td>
		<table style="border: solid black 1pt">
		  <xsl:for-each
		      select="$doc//elementSpec//attDef[@ident=$me and not(@mode)]">
		    <xsl:sort select="ancestor::elementSpec/@ident"/>
		    <tr>
		      <td><a href="http://www.tei-c.org/release/doc/tei-p5-doc/en/html/ref-{ancestor::elementSpec/@ident}.html"><xsl:value-of select="ancestor::elementSpec/@ident"/></a></td>
		      <td><xsl:value-of select="desc[not(@xml:lang)]"/></td>
		    </tr>
		  </xsl:for-each>
		</table>
	      </td>
	    </tr>
	  </xsl:for-each>
	</table>
      </body>
    </html>
</xsl:template>
<xsl:function name="tei:dupl" as="xs:boolean">
  <xsl:param name="ident"/>
  <xsl:sequence select="if ($doc//elementSpec//attDef[@ident=$ident])
    then true() else false()"/>
</xsl:function>
</xsl:stylesheet>
