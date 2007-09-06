<xsl:stylesheet
  exclude-result-prefixes="xlink dbk rng tei teix xhtml a edate estr html pantor xd xs xsl fo"
  extension-element-prefixes="exsl estr edate" 
  version="1.0"
  xmlns="http://www.w3.org/1999/xhtml"
  xmlns:fo="http://www.w3.org/1999/XSL/Format"
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
<xsl:param name="splitLevel">0</xsl:param>
<xsl:param name="footnoteFile">false</xsl:param>
<xsl:param name="autoToc">false</xsl:param>
<xsl:param name="numberFrontHeadings">true</xsl:param>
<xsl:param name="cssFile">guidelines-beta.css</xsl:param>
<xsl:param name="cssPrintFile">guidelines-print-beta.css</xsl:param>
<xsl:param name="displayMode">both</xsl:param>

  <xsl:template name="includeCSS">
    <link href="{$cssFile}" rel="stylesheet" type="text/css"/>
    <xsl:if test="not($cssPrintFile='')">
      <link rel="stylesheet" media="print" type="text/css" href="{$cssPrintFile}"/>
    </xsl:if>
  </xsl:template>

<xsl:template name="generateSubTitle">
  <xsl:value-of select="tei:head"/>
</xsl:template>

  
<xsl:template name="printLink"/>


<xsl:template match="tei:titlePage" mode="paging">
  <xsl:apply-templates select="."/>
</xsl:template>

<xsl:template match="/div">
</xsl:template>  

<xsl:template name="bitOut">
    <xsl:param name="grammar"/>
    <xsl:param name="content"/>
    <xsl:param name="element">pre</xsl:param>
    <xsl:choose>
      <xsl:when test="$displayMode='both'">
        <div class="displayRelax">
          <button class="displayRelax"
            onclick="togglerelax(this)">Display RNG</button>
          <pre class="eg_rng" style="display:none">
            <xsl:apply-templates mode="verbatim"
              select="exsl:node-set($content)/*/*"/>
          </pre>
          <pre class="eg_rnc" style="display:block">
            <xsl:call-template name="make-body-from-r-t-f">
              <xsl:with-param name="schema">
                <xsl:for-each select="exsl:node-set($content)/*">
                  <xsl:call-template name="make-compact-schema"/>
                </xsl:for-each>
              </xsl:with-param>
            </xsl:call-template>
          </pre>
        </div>
      </xsl:when>
      <xsl:when test="$displayMode='rng'">
        <xsl:element name="{$element}">
          <xsl:attribute name="class">eg</xsl:attribute>
          <xsl:apply-templates mode="verbatim"
            select="exsl:node-set($content)/*/*"/>
        </xsl:element>
      </xsl:when>
      <xsl:when test="$displayMode='rnc'">
        <xsl:element name="{$element}">
          <xsl:attribute name="class">eg</xsl:attribute>
          <xsl:call-template name="make-body-from-r-t-f">
            <xsl:with-param name="schema">
              <xsl:for-each select="exsl:node-set($content)/*">
                <xsl:call-template name="make-compact-schema"/>
              </xsl:for-each>
            </xsl:with-param>
          </xsl:call-template>
        </xsl:element>
      </xsl:when>
      <xsl:otherwise>
        <xsl:element name="{$element}">
          <xsl:attribute name="class">eg</xsl:attribute>
          <xsl:for-each select="exsl:node-set($content)/*">
            <xsl:apply-templates mode="literal"/>
          </xsl:for-each>
        </xsl:element>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  
  
  <xsl:template name="javascriptHook">
    <script type="text/javascript">
      <xsl:comment>
      <xsl:text disable-output-escaping="yes">
    function toggleTOC (el) {
        if (el.innerHTML == 'Display Full Contents') {
        el.innerHTML = 'Display Summary Contents';
        }
        else
        {
        el.innerHTML = 'Display Full Contents';
        }
        var div = el.parentNode; 
        for (j=0;j&lt;div.childNodes.length;j++)
        {
        if (div.childNodes[j].nodeType != 1) continue;
        if (div.childNodes[j].nodeName != 'DIV') continue;
        var thisone=div.childNodes[j];
        var state=thisone.style.display;
        if (state == 'block')
        {  
        thisone.style.display='none'; 
        }
        else
        {  
        thisone.style.display='block';
        }
        }
        }

        function togglerelax (el) {
        if (el.innerHTML == 'Display RNC') {
        el.innerHTML = 'Display RNG';
        }
        else
        {
        el.innerHTML = 'Display RNC';
        }
        var div = el.parentNode; 
        for (j=0;j&lt;div.childNodes.length;j++)
        {
        if (div.childNodes[j].nodeType != 1) continue;
        if (div.childNodes[j].nodeName != 'PRE') continue;
        var thisone=div.childNodes[j];
        var state=thisone.style.display;
        if (state == 'block')
        {  
        thisone.style.display='none'; 
        }
        else
        {  
        thisone.style.display='block';
        }
        }
        }
      </xsl:text>
      </xsl:comment>
    </script>
  </xsl:template>

  
  <xsl:template name="sectionHeadHook">
    <xsl:variable name="ident">
          <xsl:apply-templates mode="ident" select="."/>
    </xsl:variable>
    <span class="permalink">
    <a href="#{$ident}" 
       class="permalink" 
       title="Link to this section"> &#x00B6;</a>
    </span>
  </xsl:template>  
   
  <xsl:template name="startDivHook">
    <xsl:if test="not(parent::tei:div) or not(local-name(preceding::*[1])='head')">
    <table class="miniTOC">
	<tr>
	  <td>
	    <a class="navigation" href="index.html">Home</a> |
	    <a class="navigation" href="index-toc.html">Table of contents</a> 
	    <xsl:for-each select="ancestor::tei:div">
	      <div>
		<xsl:attribute name="style">
		  <xsl:text>margin-left:</xsl:text>
		  <xsl:value-of select="count(ancestor::tei:div) + 1"/>
		  <xsl:text>em;</xsl:text>
		</xsl:attribute>
		<xsl:text>&#x21B3;</xsl:text>
		<a class="UP">
		<xsl:attribute name="href">
		  <xsl:apply-templates mode="generateLink" select="."/>
		</xsl:attribute>
		<xsl:call-template name="headerLink">
		  <xsl:with-param name="minimal" select="$minimalCrossRef"/>
		</xsl:call-template>
		</a>
	      </div>
	    </xsl:for-each>
	  </td>
	</tr>
	<tr>
	  <td>
	    <xsl:call-template name="previousLink"/>
	  </td>
	</tr>
	<tr>
	  <td>
	    <xsl:call-template name="nextLink"/>
	  </td>
	</tr>

	<xsl:if test="not(parent::tei:div) and child::tei:div">  
	  <tr>
	    <td>
	      <xsl:call-template name="subtoc"/>
	    </td>
	  </tr>
	</xsl:if>
      </table>
    </xsl:if>
  </xsl:template>
    

  <xsl:template name="mainPage">
    <xsl:param name="currentID"/>
    <div id="hdr">
      <xsl:call-template name="hdr"/>
    </div>
    <div id="accessibility">
      <span class="tocontent"><a href="{$REQUEST}?style=text">Text only</a>
      | <a class="skiplinks" href="#rh-column"
      title="Go to main page content">Skip links</a></span>
    </div>
    <div id="hdr2">
      <xsl:call-template name="hdr2"/>
    </div>

    <div id="onecol" class="main-content">
      <xsl:call-template name="mainFrame">
	<xsl:with-param name="currentID" select="$currentID"/>
	<xsl:with-param name="minimal">true</xsl:with-param>
      </xsl:call-template>
      <xsl:if test="$currentID=''">
	<ul>
	  <li><a href="index-toc.html">Table of Contents</a></li>
	  <li><a href="SG.html">Gentle Introduction to XML</a></li>
	  <li><a href="REFTAG.html">Catalogue of elements</a></li>
	  <li><a href="USE.html">Using the TEI</a></li>
	</ul>
	  <xsl:variable name="name">
	    TEI Guidelines TOC
	  </xsl:variable>
	<xsl:call-template name="outputChunk">
	  <xsl:with-param name="ident">
	    <xsl:text>index-toc</xsl:text>
	  </xsl:with-param>
	  <xsl:with-param name="content">
            <html>
              <xsl:comment>THIS IS A GENERATED FILE. DO NOT EDIT (7) </xsl:comment>
              <head>
                <title>
		  <xsl:value-of select="$name"/>
                </title>
		<xsl:choose>
		  <xsl:when test="$cssFile = ''"/>
		  <xsl:when test="$cssFileInclude='true'">
		    <style>
		      <include xmlns="http://www.w3.org/2001/XInclude"
			  href="{$cssFile}" 
			  parse="text"/>
		    </style>
		  </xsl:when>
		  <xsl:otherwise>
		    <link href="{$cssFile}" rel="stylesheet" type="text/css"/>
		  </xsl:otherwise>
		</xsl:choose>
                <xsl:if test="not($cssSecondaryFile = '')">
                  <link href="{$cssSecondaryFile}" rel="stylesheet" type="text/css"/>
                </xsl:if>
		<meta name="generator" content="Text Encoding Initiative Consortium XSLT stylesheets"/>
		<meta name="DC.Title" content="{$name}"/>
		<meta http-equiv="Content-Type" 
		      content="application/xhtml+xml; charset=utf-8"/>
                <xsl:call-template name="includeJavascript"/>
                <xsl:call-template name="javascriptHook"/>
              </head>
              <body id="TOP">
                <xsl:attribute name="onload">
                  <xsl:text>startUp()</xsl:text>
                </xsl:attribute>
                <xsl:call-template name="bodyHook"/>
                <div id="hdr">
                  <xsl:call-template name="stdheader">
                    <xsl:with-param name="title">
                      <xsl:value-of select="$name"/>
                    </xsl:with-param>
                  </xsl:call-template>
                </div>

	    <div class="togglingTOCs">
	      <button class="displayRelax"
		      onclick="toggleTOC(this)">Display Full Contents</button>
	      <div style="display: block" class="toggleTOC_summary">
		<h2>Summary Table of Contents</h2>
		<xsl:call-template name="mainTOC">
		  <xsl:with-param name="force">0</xsl:with-param>
		</xsl:call-template>
	      </div>
	      <div style="display: none"  class="toggleTOC_full">	
		<h2>Full Table of Contents</h2>
		<xsl:call-template name="mainTOC"/>
	      </div>
	    </div>
	      </body>
	    </html>
	  </xsl:with-param>
	</xsl:call-template>
    </xsl:if>
    <xsl:call-template name="stdfooter"/>

    </div>
  </xsl:template>

  <xsl:template name="numberFrontDiv">
    <xsl:if test="count(ancestor::tei:div)&lt;2">
      <xsl:number
	  format="i.i"
	  count="tei:div"
	  level="multiple"/>
    </xsl:if>
  </xsl:template>

  <xsl:template name="myi18n">
    <xsl:param name="word"/>
    <xsl:choose>
      <xsl:when test="$word='previousWord'">
	<span class="icon">
	  <xsl:text>&#8656; </xsl:text>
	</span>
      </xsl:when>
      <xsl:when test="$word='upWord'">
	<span class="icon">
	  <xsl:text>&#8657; </xsl:text>
	</span>
      </xsl:when>
      <xsl:when test="$word='nextWord'">
	<span class="icon">
	  <xsl:text>&#8658; </xsl:text>
	</span>
      </xsl:when>
    </xsl:choose>
  </xsl:template>

<xsl:template name="navInterSep">
</xsl:template>

  <xsl:template match="tei:specGrp">
<!--
    <div class="specgrp">
      <xsl:call-template name="makeAnchor"/>
      <xsl:if test="@n">
	<b>
	  <xsl:value-of select="@n"/>
	</b>
      </xsl:if>
      <dl>
        <xsl:apply-templates/>
      </dl>
    </div>
-->
  </xsl:template>

  <xsl:template match="tei:specGrpRef">
  </xsl:template>

 <xsl:template match="a:documentation" mode="verbatim"/>

<xsl:template name="pageHeader">
    <xsl:param name="mode"/>
	<xsl:call-template name="makeHTMLHeading">
	  <xsl:with-param name="class">title</xsl:with-param>
	  <xsl:with-param name="text">
	    <xsl:call-template name="generateTitle"/>
	  </xsl:with-param>
	  <xsl:with-param name="level">1</xsl:with-param>
	</xsl:call-template>

	<xsl:call-template name="makeHTMLHeading">
	  <xsl:with-param name="class">subtitle</xsl:with-param>
	  <xsl:with-param name="text">
	    <xsl:call-template name="generateSubTitle"/>
	  </xsl:with-param>
	  <xsl:with-param name="level">2</xsl:with-param>
	</xsl:call-template>
	
  </xsl:template>

  <xsl:template name="stdheader">
    <xsl:param name="title" select="'(no title)'"/>
    <xsl:call-template name="pageHeader"/>
  </xsl:template>

  <xsl:template match="tei:titlePage">
    <div class="titlePage">
  <h1>
    <xsl:value-of select="/tei:TEI/tei:teiHeader/tei:fileDesc/tei:titleStmt/tei:title"/>
  </h1>
  <h2>
    <xsl:value-of select="tei:docAuthor"/>
  </h2>
    </div>
  </xsl:template>
  
  
  
  <!-- JC: Putting element and desc into table -->
  <xsl:template match="tei:elementSpec" mode="weavebody">
    <xsl:variable name="name">
      <xsl:choose>
        <xsl:when test="tei:altIdent">
          <xsl:value-of select="tei:altIdent"/>
        </xsl:when>
        <xsl:otherwise>
          <xsl:value-of select="@ident"/>
        </xsl:otherwise>
      </xsl:choose>
    </xsl:variable>
    <table border="1" class="wovenodd">
      <tr>
        <td   class="wovenodd-col2" colspan="2">
          <span class="label">&lt;<xsl:value-of
            select="$name"/>&gt; </span>   
      <xsl:call-template name="makeDescription"/>
      <xsl:if test="tei:listRef">
        <xsl:for-each select="tei:listRef/tei:ptr">
          <xsl:text> </xsl:text>
          <xsl:apply-templates select="." mode="weave"/>
        </xsl:for-each>
      </xsl:if></td></tr>
      <xsl:if test="@module">
        <xsl:call-template name="moduleInfo"/>
      </xsl:if>
      <tr>
        <td   class="wovenodd-col1">
          <span class="label">Parents</span>
        </td>
        <td class="wovenodd-col2">
          <xsl:call-template name="generateParents"/>
        </td>
      </tr>
      
      <tr>
        <td  class="wovenodd-col1">
          <span class="label">
            <xsl:call-template name="i18n">
              <xsl:with-param name="word">Attributes</xsl:with-param>
            </xsl:call-template>
          </span>
        </td>
        <td class="wovenodd-col2">
          <xsl:choose>
            <xsl:when test="not(tei:attList)">
              <xsl:call-template name="showAttClasses"/>
            </xsl:when>
            <xsl:otherwise>
              <xsl:for-each select="tei:attList">
                <xsl:call-template name="displayAttList">
                  <xsl:with-param name="mode">all</xsl:with-param>
                </xsl:call-template>
              </xsl:for-each>
            </xsl:otherwise>
          </xsl:choose>
        </td>
      </tr>
      <xsl:apply-templates mode="weave"/>
      
    </table>
    
  </xsl:template>
  
  
  <xsl:template match="tei:hi[@rend='math']">
      <span class="math">
        <xsl:apply-templates/>
      </span>
  </xsl:template>
</xsl:stylesheet>
