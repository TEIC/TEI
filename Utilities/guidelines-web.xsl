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
<xsl:param name="cssFile">../guidelines-web.css</xsl:param>
<xsl:param name="STDOUT">true</xsl:param>
<xsl:param name="urlChunkPrefix">.ID=</xsl:param>
<xsl:param name="homeLabel">TEI P5 Guidelines source</xsl:param>
<xsl:param name="homeURL">/Guidelines/en/index.xml</xsl:param>

  <xsl:template name="includeCSS">
    <link href="{$cssFile}" rel="stylesheet" type="text/css"/>
    <xsl:if test="not($cssPrintFile='')">
      <link rel="stylesheet" media="print" type="text/css" href="{$cssPrintFile}"/>
    </xsl:if>
  </xsl:template>

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


<xsl:template match="tei:titlePage" mode="paging">
  <xsl:apply-templates select="."/>
</xsl:template>

  <xsl:template name="i18n">
    <xsl:param name="word"/>
    <xsl:variable name="local">
      <xsl:call-template name="myi18n">
	<xsl:with-param name="word">
	  <xsl:value-of select="$word"/>
	</xsl:with-param>
      </xsl:call-template>
    </xsl:variable>
    <xsl:choose>
     <xsl:when test="string-length($local)&gt;0">
       <xsl:value-of select="$local"/>
     </xsl:when>
     <xsl:otherwise>
       <xsl:for-each select="document('/usr/share/xml/tei/stylesheet/i18n.xml',document(''))">
	 <xsl:choose>
	   <xsl:when
	       test="key('KEYS',normalize-space($word))/text[@xml:lang=$documentationLanguage]">
	     <xsl:value-of
		 select="key('KEYS',normalize-space($word))/text[@xml:lang=$documentationLanguage]"/>
	   </xsl:when>
	   <xsl:otherwise>
	     <xsl:value-of select="$word"/>
	   </xsl:otherwise>
	 </xsl:choose>
       </xsl:for-each>
     </xsl:otherwise>
    </xsl:choose>
  </xsl:template>

  <xsl:template name="crumbPath">
    <div class="breadcrumb">
      <xsl:call-template name="preBreadCrumbPath"/>
      <ul class="breadcrumb">
        <li class="breadcrumb-first">
          <a class="breadcrumb" href="{$homeURL}" target="_top">
            <xsl:value-of select="$homeLabel"/>
          </a>
        </li>
        <xsl:call-template name="walkTree">
          <xsl:with-param name="path">
            <xsl:value-of select="substring-after($REQUEST,'/cocoon/Guidelines/')"/>
          </xsl:with-param>
          <xsl:with-param name="class">breadcrumb</xsl:with-param>
        </xsl:call-template>
      </ul>
    </div>
  </xsl:template>


</xsl:stylesheet>


