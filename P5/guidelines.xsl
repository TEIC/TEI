<xsl:stylesheet
    xmlns:tei="http://www.tei-c.org/ns/1.0"    
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform" 
    version="1.0"
    xmlns:html="http://www.w3.org/1999/xhtml">
  
<xsl:import href="/usr/share/xml/tei/stylesheet/base/p5/odds/odd2html.xsl"/>
  
<xsl:param name="cssFile">teic.css</xsl:param>
<xsl:param name="parentWords">Text Encoding Initiative Consortium</xsl:param>

<xsl:template name="generateSubTitle">
  <xsl:value-of select="tei:head"/>
</xsl:template>

  
 <xsl:template name="pageTableHeader">
    <xsl:param name="mode"/>
	<table width="100%" border="0">
	  <tr>
	    <td height="100" class="bgimage" onClick="window.location='{$homeURL}'" cellpadding="0">
	      <h1 class="maintitle"><xsl:call-template name="generateTitle"/></h1>
	      <h2 class="subtitle"><xsl:call-template name="generateSubTitle"/></h2>
	    </td>
	  </tr>
	</table>
 </xsl:template>

<xsl:template name="printLink"/>
  

</xsl:stylesheet>


