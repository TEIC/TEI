<xsl:stylesheet
  exclude-result-prefixes="xlink dbk rng tei teix xhtml a edate estr html pantor xd xs xsl"
  extension-element-prefixes="exsl estr edate" 
  version="1.0"
  xmlns="http://www.w3.org/1999/xhtml"
  xmlns:xlink="http://www.w3.org/1999/xlink"
  xmlns:dbk="http://docbook.org/ns/docbook"
  xmlns:rng="http://relaxng.org/ns/structure/1.0"
  xmlns:tei="http://www.tei-c.org/ns/1.0"
  xmlns:teix="http://www.tei-c.org/ns/Examples"
  xmlns:xhtml="http://www.w3.org/1999/xhtml"
  xmlns:a="http://relaxng.org/ns/compatibility/annotations/1.0"
  xmlns:edate="http://exslt.org/dates-and-times"
  xmlns:estr="http://exslt.org/strings" xmlns:exsl="http://exslt.org/common"
  xmlns:html="http://www.w3.org/1999/xhtml"
  xmlns:pantor="http://www.pantor.com/ns/local"
  xmlns:xd="http://www.pnp-software.com/XSLTdoc"
  xmlns:xs="http://www.w3.org/2001/XMLSchema"
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
  
<xsl:import href="odd2htmlp5.xsl"/>
<xsl:param name="lang"/>
<xsl:param name="doclang"/>
<xsl:param name="cssFile">tei.css</xsl:param>
<xsl:param name="cssSecondaryFile">teic.css</xsl:param>

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


<xsl:template match="tei:front/tei:div/tei:div">
  <xsl:call-template name="makeDivPage">
    <xsl:with-param name="depth">1</xsl:with-param>
  </xsl:call-template>
</xsl:template>  

<xsl:template match="tei:body/tei:div/tei:div">
  <xsl:call-template name="makeDivPage">
    <xsl:with-param name="depth">1</xsl:with-param>
  </xsl:call-template>
</xsl:template>  

<xsl:template match="tei:back/tei:div/tei:div">
  <xsl:call-template name="makeDivPage">
    <xsl:with-param name="depth">1</xsl:with-param>
  </xsl:call-template>
</xsl:template>  

</xsl:stylesheet>


