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

	   Please pick you're Tagsets:<br/><br/>
	  <xsl:apply-templates/>
         </body>
        </html>	 
  </xsl:template>

  <xsl:template match="teiModulesList">
    <form method="POST">
     <xsl:attribute name="action"><xsl:value-of select="$action"/></xsl:attribute>
     <input type="hidden" name="mode" value="evaluateMain"/>
     <table border="0" cellspacing="0" cellpadding="0" width="50%">
       <xsl:apply-templates/>
     </table>
     <h5>configuring the tagset</h5>
     <p>
     Do you want to change the elements from the origninal tagset?
     <select name="changeTagset" size="1">
       <option value="0">Leave elements as they are</option>
       <option value="1">Configure elements, including them by default</option>
       <option value="2">Configure elements, excluding them by default</option>     
     </select><br/>
    </p>
    <p>
     <table border="0" cellspacing="0" cellpadding="0">
      <tr>
       <td>Define some new elements</td>
       <td><input type="checkbox" name="newElements"/></td>
      </tr>
      <tr>
       <td>Add TEI Lite extension</td>
       <td><input type="checkbox" name="teiLite"/></td>
      </tr>
      <tr>
       <td>MathML as content of &lt;formula&gt; (Schema only)</td>
       <td><input type="checkbox" name="mathml"/></td>
      </tr>
      <tr>
       <td>SVG as content of &lt;figure&gt; (Schema only)</td>
       <td><input type="checkbox" name="svg"/></td>
      </tr>
     </table>
    </p>
     <input type="submit"/>
    </form>
  </xsl:template>

  <xsl:template match="teiModule">
     <tr>
	<xsl:apply-templates/>
     </tr>
  </xsl:template>

  <xsl:template match="moduleName">
     <td>
	<xsl:value-of select="."/>
     </td>
     <td align="left">
	<input type="checkbox" value="1">
          <xsl:attribute name="name">module_<xsl:value-of select="."/></xsl:attribute>
        </input>
     </td>
  </xsl:template>

  <xsl:template match="moduleDesc">
     <td>
	<xsl:value-of select="."/>
     </td>
  </xsl:template>


</xsl:stylesheet>