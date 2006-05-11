<xsl:stylesheet
    xmlns="http://www.w3.org/1999/xhtml"
    xmlns:html="http://www.w3.org/1999/xhtml" 
    xmlns:xd="http://www.pnp-software.com/XSLTdoc"
    xmlns:a="http://relaxng.org/ns/compatibility/annotations/1.0"
    xmlns:teix="http://www.tei-c.org/ns/Examples"
    xmlns:xs="http://www.w3.org/2001/XMLSchema" 
    xmlns:rng="http://relaxng.org/ns/structure/1.0"
    xmlns:estr="http://exslt.org/strings"
    xmlns:pantor="http://www.pantor.com/ns/local"
    xmlns:exsl="http://exslt.org/common"
    xmlns:tei="http://www.tei-c.org/ns/1.0"
    xmlns:edate="http://exslt.org/dates-and-times"
    extension-element-prefixes="exsl estr edate"
    exclude-result-prefixes="exsl rng edate estr tei html a pantor teix xs xd" 
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    version="1.0"
>
  
<xsl:import href="odd2htmlp5.xsl"/>
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

<xsl:template match="tei:div0">
  <xsl:apply-templates/>
</xsl:template>

<xsl:template match="tei:div2|tei:div3|tei:div4|tei:div5|tei:div6">
    <xsl:variable name="depth">
      <xsl:apply-templates select="." mode="depth"/>
    </xsl:variable>
  <div>
    <xsl:call-template name="divClassAttribute">
      <xsl:with-param name="depth" select="$depth"/>
    </xsl:call-template>
    <xsl:call-template name="doDivBody">
      <xsl:with-param name="Type" select="$depth"/>
    </xsl:call-template>
  </div>
</xsl:template>

<xsl:template match="tei:div1">
    <xsl:variable name="depth">
      <xsl:apply-templates select="." mode="depth"/>
    </xsl:variable>
  <xsl:call-template name="makeDivPage">
    <xsl:with-param name="depth" select="$depth"/>
  </xsl:call-template>
</xsl:template>  

</xsl:stylesheet>


