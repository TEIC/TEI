<?xml version="1.0"?>
<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">

  <xsl:template match="/">
 	<html>
	 <head>
          <title>Roma - generating validators for the TEI</title>
         </head>
	 <body>
          <font color="#CC0000">
           <h3>Text Encoding Initiative</h3>
           <h1>Roma: generating validators for the TEI</h1>
           <hr noshade="1"/>
          </font>
	  <h4>Tagset:</h4>
	    <xsl:for-each select="/child::root/child::teiElementList">
	      <xsl:value-of select="@module"/>, 
	    </xsl:for-each>
	  <form method="POST"><xsl:attribute name="action"><xsl:value-of select="$action"/></xsl:attribute>
  	   <xsl:apply-templates/>
	  <input type="submit"/>
	  </form>
         </body>
        </html>	 
  </xsl:template>

  <xsl:template match="teiElementList">
     <h2><xsl:value-of select="@module"/></h2><br/>
    	<table border="0">
	  <tr>
           <td></td>
           <td>Include</td>
           <td>Exclude</td>
           <td>Element Name</td>
           <td>Description</td>
	  </tr>
	    <xsl:apply-templates/>
	</table>
  </xsl:template>


  <xsl:template match="teiElement">
	<tr>
	  <xsl:apply-templates/>
	</tr>
  </xsl:template>


  <xsl:template match="elementName">
            <td><xsl:value-of select="."/></td>
	    <td><input type="radio" value="0" checked="1"><xsl:attribute name="name"><xsl:value-of select="."/></xsl:attribute></input></td>
	    <td><input type="radio" value="1"><xsl:attribute name="name"><xsl:value-of select="."/></xsl:attribute></input></td>
	    <td>
              <input type="text">
	          <xsl:attribute name="value"><xsl:value-of select="."/></xsl:attribute>
	      </input>
	    </td>
  </xsl:template>


  <xsl:template match="elementDesc">
	<td>
	  <xsl:value-of select="."/>
	</td>
  </xsl:template>

</xsl:stylesheet>