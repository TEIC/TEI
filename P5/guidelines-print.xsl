<xsl:stylesheet
    xmlns:tei="http://www.tei-c.org/ns/1.0"    
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform" 
    version="1.0"
    xmlns:html="http://www.w3.org/1999/xhtml">
  
<xsl:import href="http://www.tei-c.org/stylesheet/odds/odd2html.xsl"/>
  
<xsl:param name="autoToc">true</xsl:param>
<xsl:param name="pageLayout">Simple</xsl:param>
<xsl:param name="cssFile">tei-print.css</xsl:param>
<xsl:param name="parentWords">Text Encoding Initiative Consortium</xsl:param>
<xsl:param name="topNavigationPanel">true</xsl:param>

<xsl:template name="generateSubTitle"/>

<xsl:template name="maintoc"> 
  <xsl:param name="force"/>
   <xsl:for-each select="ancestor-or-self::tei:TEI/tei:text/tei:front">
    <xsl:apply-templates 
      select=".//tei:div1" mode="maketoc">
     <xsl:with-param name="forcedepth" select="$force"/>
    </xsl:apply-templates>
   </xsl:for-each>

   <xsl:for-each select="ancestor-or-self::tei:TEI/tei:text/tei:body">
    <xsl:apply-templates 
      select=".//tei:div1" mode="maketoc">
     <xsl:with-param name="forcedepth" select="$force"/>
    </xsl:apply-templates>
   </xsl:for-each>

   <xsl:for-each select="ancestor-or-self::tei:TEI/tei:text/tei:back">
    <xsl:apply-templates 
      select=".//tei:div1" mode="maketoc">
     <xsl:with-param name="forcedepth" select="$force"/>
   </xsl:apply-templates>
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
 <xsl:param name="minimal"/>
 <xsl:param name="toc"/>
 <xsl:variable name="TOC">
    <xsl:apply-templates select="." mode="generateLink"/>
 </xsl:variable>
 <xsl:variable name="depth">
   <xsl:apply-templates select="." mode="depth"/>
 </xsl:variable>
 <xsl:if test="not($depth &gt; $numberHeadingsDepth)">
   <xsl:call-template name="calculateNumber">
     <xsl:with-param name="numbersuffix" select="$headingNumberSuffix"/>
   </xsl:call-template>
 </xsl:if>
 <xsl:if test="not($minimal)">
   <xsl:value-of select="$headingNumberSuffix"/>
   <xsl:choose>
     <xsl:when test="not($TOC='')">
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
      <xsl:when test="following-sibling::tei:div1">
	<xsl:apply-templates mode="generateNextLink"
	     select="following-sibling::tei:div1[1]"/>
      </xsl:when>
      <xsl:when test="parent::tei:div0/following-sibling::tei:div0/child::tei:div1">
	<xsl:apply-templates mode="generateNextLink"
	     select="parent::tei:div0/following-sibling::tei:div0[1]/child::tei:div1[1]"/>
      </xsl:when>
    </xsl:choose>
  </xsl:template>
  
  <xsl:template name="previousLink">
    <xsl:choose>
      <xsl:when test="preceding-sibling::tei:div1">
	<xsl:apply-templates mode="generatePreviousLink"
	     select="preceding-sibling::tei:div1[1]"/>
      </xsl:when>
      <xsl:when test="parent::tei:div0/preceding-sibling::tei:div0/child::tei:div1">
	<xsl:apply-templates mode="generatePreviousLink"
	     select="parent::tei:div0/preceding-sibling::tei:div0[1]/child::tei:div1[last()]"/>
      </xsl:when>
    </xsl:choose>
  </xsl:template>
  

</xsl:stylesheet>


