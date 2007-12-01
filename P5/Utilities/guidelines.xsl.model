<xsl:stylesheet
  exclude-result-prefixes="xlink dbk rng tei teix xhtml a edate estr html pantor xd xs xsl fo"
  extension-element-prefixes="exsl estr edate" version="1.0"
  xmlns="http://www.w3.org/1999/xhtml"
  xmlns:a="http://relaxng.org/ns/compatibility/annotations/1.0"
  xmlns:dbk="http://docbook.org/ns/docbook"
  xmlns:edate="http://exslt.org/dates-and-times"
  xmlns:estr="http://exslt.org/strings"
  xmlns:exsl="http://exslt.org/common"
  xmlns:fo="http://www.w3.org/1999/XSL/Format"
  xmlns:html="http://www.w3.org/1999/xhtml"
  xmlns:pantor="http://www.pantor.com/ns/local"
  xmlns:rng="http://relaxng.org/ns/structure/1.0"
  xmlns:tei="http://www.tei-c.org/ns/1.0"
  xmlns:teix="http://www.tei-c.org/ns/Examples"
  xmlns:xd="http://www.pnp-software.com/XSLTdoc"
  xmlns:xhtml="http://www.w3.org/1999/xhtml"
  xmlns:xlink="http://www.w3.org/1999/xlink"
  xmlns:xs="http://www.w3.org/2001/XMLSchema"
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform">

  <xsl:import href="/usr/share/xml/tei/stylesheet/odds/odd2html.xsl"/>

  <xsl:param name="lang"/>
  <xsl:param name="doclang"/>
  <xsl:param name="splitLevel">0</xsl:param>
  <xsl:param name="footnoteFile">false</xsl:param>
  <xsl:param name="auto">false</xsl:param>
  <xsl:param name="numberFrontHeadings">true</xsl:param>
  <xsl:param name="cssFile">guidelines.css</xsl:param>
  <xsl:param name="cssSecondaryFile">udm.css</xsl:param>
  <xsl:param name="cssPrintFile">guidelines-print.css</xsl:param>
  <xsl:param name="displayMode">both</xsl:param>

  <xsl:param name="feedbackURL">http://www.tei-c.org/Consortium/contact.xml</xsl:param>
  <xsl:param name="homeLabel">TEI P5 Guidelines</xsl:param>
  <xsl:param name="homeWords">TEI P5</xsl:param>
  <xsl:param name="institution">Text Encoding Initiative</xsl:param>
  <xsl:param name="outputDir">Guidelines</xsl:param>
  <xsl:param name="parentURL">http://www.tei-c.org/Consortium/</xsl:param>
  <xsl:param name="parentWords">TEI Consortium</xsl:param>

  <xsl:template name="copyrightStatement">Copyright TEI Consortium 2007</xsl:template>

  <xsl:template name="metaHook">
    <xsl:param name="title"/>
    <meta name="DC.Title" content="{$title}"/>
    <meta name="DC.Language" content="(SCHEME=iso639) en"/> 
    <meta name="DC.Creator" content="TEI,Oxford University Computing Services, 13 Banbury Road, Oxford OX2 6NN, United Kingdom"/>
    <meta name="DC.Creator.Address" content="tei@oucs.ox.ac.uk"/>
  </xsl:template>
  

  <xsl:template name="startDivHook">
    <xsl:if
      test="not(parent::tei:div) or not(local-name(preceding::*[1])='head')">
      <div>
	<xsl:choose>
        <xsl:when test="not(parent::tei:div) and child::tei:div">
	  <xsl:attribute name="class">
	    <xsl:text>miniTOC miniTOC_left</xsl:text>
	  </xsl:attribute>
	  <xsl:call-template name="subtoc"/>
	</xsl:when>
	<xsl:otherwise>
	  <xsl:attribute name="class">
	    <xsl:text>miniTOC miniTOC_right</xsl:text>
	  </xsl:attribute>
	</xsl:otherwise>
	</xsl:choose>
	<ul class="subtoc">
	  <li class="subtoc"><xsl:call-template name="previousLink"/></li>
	  <li class="subtoc"><xsl:call-template name="nextLink"/></li>
	  <li class="subtoc"><a class="navigation" href="index.html">
	  <xsl:call-template name="i18n">
	    <xsl:with-param name="word">homeWord</xsl:with-param>
	  </xsl:call-template>
	  </a> | <a
	  class="navigation" href="index-toc.html">
	  <xsl:call-template name="i18n">
	    <xsl:with-param name="word">tocWords</xsl:with-param>
	  </xsl:call-template>
	  </a></li>
	  <li class="subtoc">
	  <xsl:choose>
	    <xsl:when test="self::tei:elementSpec">
		<a class="navigation" href="REF-ELEMENTS.html">
		Element catalogue</a>
	    </xsl:when>
	    <xsl:when test="self::tei:classSpec[@type='model']">
		<a class="navigation" href="REF-CLASSES-MODEL.html">
		Model Class catalogue</a>
	    </xsl:when>
	    <xsl:when test="self::tei:classSpec[@type='atts']">
		<a class="navigation" href="REF-CLASSES-ATTS.html">
		Attribute Class catalogue</a>
	    </xsl:when>
	    <xsl:when test="self::tei:macroSpec">
		<a class="navigation" href="REF-MACROS.html">
		Macro and datatype catalogue</a>
	    </xsl:when>
	    <xsl:otherwise>
	      <!--
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
		    <xsl:apply-templates mode="generateLink"
					 select="."/>
		  </xsl:attribute>
		  <xsl:call-template name="headerLink">
		    <xsl:with-param name="minimal"
				    select="$minimalCrossRef"/>
		  </xsl:call-template>
		</a>
		</div>
	      </xsl:for-each>
	      -->
	    </xsl:otherwise>
	  </xsl:choose>
	  </li>
	</ul>
      </div>
    </xsl:if>
  </xsl:template>


  <xsl:template name="mainPage">
    <xsl:param name="currentID"/>
    <xsl:call-template name="teiTOP"/>
    <div id="onecol" class="main-content">
      <xsl:call-template name="mainFrame">
        <xsl:with-param name="currentID" select="$currentID"/>
        <xsl:with-param name="minimal">true</xsl:with-param>
      </xsl:call-template>
      <xsl:if test="$currentID=''">
        <div style="float:left; margin:4%;">
          <h3>Versions of the Guidelines</h3>
          <ul>
            <li>
              <a href="index-toc.html">Table of Contents</a>
            </li>
<!--
            <li>
              <a href="guidelines.html">One single HTML file</a>
            </li>
            <li>
              <a href="guidelines.pdf">One single PDF file </a>
            </li>
-->
<!--
            <li>
              <a
                href="http://books.lulu.com/content/123WeHaveNotSubmittedThemYetSorry/"
                >Hardcopy Printed Version</a>
            </li>
-->
            <li><a
              href="http://www.tei-c.org/Council/tcw06.xml">Getting
              the most recent version</a></li>
            <li><a
              href="http://tei.svn.sourceforge.net/viewvc/tei/">Sourceforge
              Subversion Repository</a></li>
            <li>
              <a
                href="http://sourceforge.net/tracker/?group_id=106328&amp;func=browse">Bug
                Reports, Feature Requests, etc.</a>
            </li>
            
          </ul>
        </div>
        <div style="float:left; margin:4%;">
          <h3>Some Popular Sections</h3>
          <ul>
            <li>
              <a href="AB.html">About These Guidelines</a>
            </li>
            <li>
              <a href="ST.html">The TEI Infrastructure</a>
            </li>
            <li>
              <a href="SG.html">A Gentle Introduction to XML</a>
            </li>
            <li>
              <a href="HD.html">The TEI Header</a>
            </li>
            <li>
              <a href="CO.html">Elements Available in All TEI
                Documents</a>
            </li>
<!--
            <li>
              <a href="DS.html">Default Text Structure</a>
            </li>
            <li>
              <a href="ND.html">Names, Dates, People, and Places</a>
            </li>
-->
            <li>
              <a href="REF-CLASSES-MODEL.html">Model Classes</a>
            </li>
            <li>
              <a href="REF-CLASSES-ATTS.html">Attribute Classes</a>
            </li>
            <li>
              <a href="REF-ELEMENTS.html">Elements</a>
            </li>
            <li>
              <a href="USE.html">Using the TEI</a>
            </li>
<!--
            <li>
              <a href="BIB.html">Bibliography</a>
            </li>
-->
          </ul>
        </div>

        <xsl:variable name="name"> TEI Guidelines TOC </xsl:variable>
        <xsl:call-template name="outputChunk">
          <xsl:with-param name="ident">
            <xsl:text>index-toc</xsl:text>
          </xsl:with-param>
          <xsl:with-param name="content">
            <html>
              <xsl:comment>THIS IS A GENERATED FILE. DO NOT EDIT (99) </xsl:comment>
              <head>
                <title>
                  <xsl:value-of select="$name"/>
                </title>
                <xsl:choose>
                  <xsl:when test="$cssFile = ''"/>
                  <xsl:when test="$cssFileInclude='true'">
                    <style>
		      <include href="{$cssFile}" parse="text" xmlns="http://www.w3.org/2001/XInclude"/>
		    </style>
                  </xsl:when>
                  <xsl:otherwise>
                    <link href="{$cssFile}" rel="stylesheet"
                      type="text/css"/>
                  </xsl:otherwise>
                </xsl:choose>
                <xsl:if test="not($cssSecondaryFile = '')">
                  <link href="{$cssSecondaryFile}" rel="stylesheet"
                    type="text/css"/>
                </xsl:if>
                <meta
                  content="Text Encoding Initiative Consortium XSLT stylesheets"
                  name="generator"/>
                <meta content="{$name}" name="DC.Title"/>
                <meta content="application/xhtml+xml; charset=utf-8"
                  http-equiv="Content-Type"/>
                <xsl:call-template name="includeJavascript"/>
                <xsl:call-template name="javascriptHook"/>
              </head>
              <body id="TOP">
                <xsl:attribute name="onload">
                  <xsl:text>startUp()</xsl:text>
                </xsl:attribute>
                <xsl:call-template name="bodyHook"/>
		<xsl:call-template name="teiTOP"/>
		<div id="onecol" class="main-content">
		  <xsl:call-template name="mainTOC"/>
		</div>
                <xsl:call-template name="stdfooter"/>
              </body>
            </html>
          </xsl:with-param>
        </xsl:call-template>
      </xsl:if>
      <xsl:call-template name="stdfooter"/>

    </div>
  </xsl:template>


  <xsl:template name="numberFrontDiv">
    <xsl:param name="minimal"/>
    <xsl:if test="count(ancestor::tei:div)&lt;2">
      <xsl:number count="tei:div" format="i.i" level="multiple"/>
      <xsl:if test="$minimal='false'">
	<xsl:value-of select="$numberSpacer"/>
      </xsl:if>
    </xsl:if>
  </xsl:template>

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
        <!--  <xsl:value-of
    select="/tei:TEI/tei:teiHeader/tei:fileDesc/tei:titleStmt/tei:title"/>
  -->
        <xsl:apply-templates
          select="tei:docTitle/tei:titlePart/tei:title"/>
      </h1>
      <h2>
        <xsl:value-of select="tei:docAuthor"/>
      </h2>
    </div>
  </xsl:template>



  <!-- JC Adding headings -->
  <xsl:template name="class_toc">
    <xsl:param name="depth"/>
    <xsl:text>toc</xsl:text>
    <xsl:text> </xsl:text>
    <xsl:text>toc</xsl:text>
    <xsl:value-of select="$depth"/>
  </xsl:template>

  <xsl:template name="continuedToc">
    <xsl:if
      test="tei:div|tei:div0|tei:div1|tei:div2|tei:div3|tei:div4|tei:div5|tei:div6">
      <ul class="toc">
        <xsl:apply-templates mode="maketoc"
          select="tei:div|tei:div0|tei:div1|tei:div2|tei:div3|tei:div4|tei:div5|tei:div6"
        />
      </ul>
    </xsl:if>
  </xsl:template>

  <xsl:template match="tei:div" mode="maketoc">
    <xsl:param name="forcedepth"/>
    <xsl:variable name="myName">
      <xsl:value-of select="local-name(.)"/>
    </xsl:variable>
    <xsl:if test="tei:head or $autoHead='true'">
      <xsl:variable name="Depth">
        <xsl:choose>
          <xsl:when test="not($forcedepth='')">
            <xsl:value-of select="$forcedepth"/>
          </xsl:when>
          <xsl:otherwise>
            <xsl:value-of select="$tocDepth"/>
          </xsl:otherwise>
        </xsl:choose>
      </xsl:variable>
      <xsl:variable name="thislevel">
	<xsl:value-of select="count(ancestor::tei:div)"/>
      </xsl:variable>
      <xsl:variable name="pointer">
        <xsl:apply-templates mode="generateLink" select="."/>
      </xsl:variable>
      <li>
	<xsl:choose>
	<xsl:when test="not(ancestor::tei:div) and tei:div">
	  <xsl:attribute name="class">
	    <xsl:text>tocTree</xsl:text>
	  </xsl:attribute>
	  <a class="collapsed" 
	     title="Click here to expand list of sections in this chapter" 
	     href="#" onclick="toggleToc(this);return false;">&#160;</a>
	</xsl:when>
	<xsl:otherwise>
	  <xsl:attribute name="class">
	    <xsl:text>toc</xsl:text>
	  </xsl:attribute>
	  <a class="normaltoc">&#160;</a>
	</xsl:otherwise>
	</xsl:choose>
	<xsl:call-template name="header">
	  <xsl:with-param name="toc" select="$pointer"/>
	  <xsl:with-param name="minimal">false</xsl:with-param>
	  <xsl:with-param name="display">plain</xsl:with-param>
	</xsl:call-template>
	<xsl:if test="$thislevel &lt; $Depth">
	    <xsl:call-template name="continuedToc"/>
        </xsl:if>
      </li>
    </xsl:if>
  </xsl:template>

  <xsl:template name="mainTOC">
    <xsl:param name="force"/>

    <div class="toc_back">
      <h3>Back Matter</h3>
      <xsl:for-each
	  select="ancestor-or-self::tei:TEI/tei:text/tei:back">
	<xsl:if
	    test="tei:div|tei:div0|tei:div1|tei:div2|tei:div3|tei:div4|tei:div5|tei:div6">
	  <ul class="toc{$force} toc_back">
	    <xsl:apply-templates mode="maketoc"
				 select="tei:div|tei:div0|tei:div1|tei:div2|tei:div3|tei:div4|tei:div5|tei:div6">
	      <xsl:with-param name="forcedepth" select="$force"/>
              </xsl:apply-templates>
	  </ul>
	</xsl:if>
      </xsl:for-each>
    </div>

    <div class="toc_front">
      <h3>Front Matter</h3>
      <xsl:for-each
	  select="ancestor-or-self::tei:TEI/tei:text/tei:front">
	<xsl:if
	    test="tei:div|tei:div0|tei:div1|tei:div2|tei:div3|tei:div4|tei:div5|tei:div6">
	  <ul class="toc{$force} toc_front">
	    <xsl:apply-templates mode="maketoc"
				 select="tei:div|tei:div0|tei:div1|tei:div2|tei:div3|tei:div4|tei:div5|tei:div6">
	      <xsl:with-param name="forcedepth" select="$force"/>
	    </xsl:apply-templates>
            </ul>
	</xsl:if>
      </xsl:for-each>
    </div>

    <div class="toc_body">
      <h3>Text Body</h3>
      <xsl:for-each
        select="ancestor-or-self::tei:TEI/tei:text/tei:body">
        <xsl:if
          test="tei:div|tei:div0|tei:div1|tei:div2|tei:div3|tei:div4|tei:div5|tei:div6">
          <ul class="toc{$force}  toc_body">
            <xsl:apply-templates mode="maketoc"
              select="tei:div|tei:div0|tei:div1|tei:div2|tei:div3|tei:div4|tei:div5|tei:div6">
              <xsl:with-param name="forcedepth" select="$force"/>
            </xsl:apply-templates>
          </ul>
        </xsl:if>
      </xsl:for-each>
    </div>

  </xsl:template>


  <xsl:template match="tei:divGen[@type='toc']"/>

</xsl:stylesheet>
