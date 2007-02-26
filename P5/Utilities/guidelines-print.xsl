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
<xsl:import href="./odd2htmlp5.xsl"/>
<xsl:output method="xml"
	      doctype-public="//W3C//DTD XHTML 1.1//EN"
	      doctype-system="http://www.w3.org/TR/xhtml11/DTD/xhtml11.dtd"
	      encoding="utf-8"
	      />  
 
<xsl:param name="autoToc">true</xsl:param>
<xsl:param name="splitLevel">-1</xsl:param>
<xsl:param name="footnoteFile">false</xsl:param>
<xsl:param name="pageLayout">Simple</xsl:param>
<xsl:param name="cssFile">tei-print.css</xsl:param>
<xsl:param name="parentWords">Text Encoding Initiative Consortium</xsl:param>
<xsl:param name="topNavigationPanel">true</xsl:param>

<xsl:template name="generateSubTitle"/>

<xsl:template name="maintoc"> 
  <xsl:param name="force"/>
   <xsl:for-each
       select="ancestor-or-self::tei:TEI/tei:text/tei:front">
     <ul class="toc{$force}">
       <xsl:apply-templates 
	   select="tei:div/tei:div" mode="maketoc">
	 <xsl:with-param name="forcedepth" select="$force"/>
       </xsl:apply-templates>
     </ul>
   </xsl:for-each>

   <xsl:for-each select="ancestor-or-self::tei:TEI/tei:text/tei:body">
     <ul class="toc{$force}">
    <xsl:apply-templates 
      select="tei:div/tei:div" mode="maketoc">
     <xsl:with-param name="forcedepth" select="$force"/>
    </xsl:apply-templates>
     </ul>
   </xsl:for-each>

   <xsl:for-each select="ancestor-or-self::tei:TEI/tei:text/tei:back">
     <ul class="toc{$force}">
    <xsl:apply-templates 
      select="tei:div/tei:div" mode="maketoc">
     <xsl:with-param name="forcedepth" select="$force"/>
   </xsl:apply-templates>
     </ul>
   </xsl:for-each>
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

<xsl:template name="header">
 <xsl:param name="minimal">false</xsl:param>
 <xsl:param name="toc"/>
 <xsl:variable name="TOC">
    <xsl:apply-templates select="." mode="generateLink"/>
 </xsl:variable>
 <xsl:variable name="depth">
   <xsl:apply-templates select="." mode="depth"/>
 </xsl:variable>
 <xsl:if test="$numberHeadingsDepth &gt;= $depth">
   <xsl:call-template name="calculateNumber">
     <xsl:with-param name="numbersuffix" select="$headingNumberSuffix"/>
   </xsl:call-template>
 </xsl:if>
 <xsl:if test="$minimal='false'">
   <xsl:value-of select="$headingNumberSuffix"/>
   <xsl:choose>
     <xsl:when test="not($toc='')">
       <xsl:call-template name="makeInternalLink">
          <xsl:with-param name="class">toc</xsl:with-param>
	  <xsl:with-param name="dest"><xsl:value-of select="$TOC"/></xsl:with-param>
	  <xsl:with-param name="body">
	    <xsl:apply-templates mode="plain" select="tei:head"/>
	  </xsl:with-param>
       </xsl:call-template>
     </xsl:when>
     <xsl:otherwise>
       <xsl:apply-templates mode="plain" select="tei:head"/>
     </xsl:otherwise>
   </xsl:choose>
 </xsl:if> 

</xsl:template>

  <xsl:template name="nextLink">
    <xsl:choose>
      <xsl:when test="following-sibling::tei:div">
	<xsl:apply-templates mode="generateNextLink"
	     select="following-sibling::tei:div[1]"/>
      </xsl:when>
      <xsl:when test="parent::tei:div/following-sibling::tei:div/child::tei:div">
	<xsl:apply-templates mode="generateNextLink"
	     select="parent::tei:div/following-sibling::tei:div[1]/child::tei:div[1]"/>
      </xsl:when>
    </xsl:choose>
  </xsl:template>
  
  <xsl:template name="previousLink">
    <xsl:choose>
      <xsl:when test="preceding-sibling::tei:div">
	<xsl:apply-templates mode="generatePreviousLink"
	     select="preceding-sibling::tei:div[1]"/>
      </xsl:when>
      <xsl:when test="parent::tei:div/preceding-sibling::tei:div/child::tei:div">
	<xsl:apply-templates mode="generatePreviousLink"
	     select="parent::tei:div/preceding-sibling::tei:div[1]/child::tei:div[last()]"/>
      </xsl:when>
    </xsl:choose>
  </xsl:template>
  

</xsl:stylesheet>


