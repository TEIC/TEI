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

  <xsl:import href="odd2htmlp5.xsl"/>
  <xsl:param name="lang"/>
  <xsl:param name="doclang"/>
  <xsl:param name="splitLevel">0</xsl:param>
  <xsl:param name="footnoteFile">false</xsl:param>
  <xsl:param name="auto">false</xsl:param>
  <xsl:param name="numberFrontHeadings">true</xsl:param>
  <xsl:param name="cssFile">guidelines-beta.css</xsl:param>
  <xsl:param name="cssSecondaryFile">udm.css</xsl:param>
  <xsl:param name="cssPrintFile">guidelines-print-beta.css</xsl:param>
  <xsl:param name="displayMode">both</xsl:param>


  <xsl:key name="CLASS-MODULE" match="tei:classSpec"
	   use="@module"/>
  <xsl:key name="ELEMENT-MODULE" match="tei:elementSpec"
	   use="@module"/>
  <xsl:key name="MACRO-MODULE" match="tei:macroSpec"
	   use="@module"/>

  <xsl:key name="ELEMENT-ALPHA" match="tei:elementSpec"
	   use="substring(translate(@ident,'ABCDEFGHIJKLMNOPQRSTUVWXYZ','abcdefghijklmnopqrstuvwxyz'),1,1)"/>


  <xsl:template name="includeCSS">
    <link href="{$cssFile}" rel="stylesheet" type="text/css"/>
    <xsl:if test="not($cssPrintFile='')">
      <link href="{$cssPrintFile}" media="print" rel="stylesheet"
        type="text/css"/>
    </xsl:if>
    <xsl:if test="not($cssSecondaryFile = '')">
      <link href="{$cssSecondaryFile}" rel="stylesheet" type="text/css"/>
    </xsl:if>

  </xsl:template>

  <xsl:template name="generateSubTitle">
    <xsl:value-of select="tei:head"/>
  </xsl:template>


  <xsl:template name="printLink"/>


  <xsl:template match="tei:titlePage" mode="paging">
    <xsl:apply-templates select="."/>
  </xsl:template>

  <xsl:template match="/div"> </xsl:template>

  <xsl:template name="bitOut">
    <xsl:param name="grammar"/>
    <xsl:param name="content"/>
    <xsl:param name="element">pre</xsl:param>
    <xsl:choose>
      <xsl:when test="$displayMode='both'">
        <div class="displayRelax">
          <button class="displayRelax" onclick="togglerelax(this)"
            >Compact to XML format</button>
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

  <script type="text/javascript" src="udm-all.js">
    <xsl:comment>&#160;</xsl:comment>
  </script>

    <script type="text/javascript">
      <xsl:comment>
        <xsl:text disable-output-escaping="yes">
states=new Array()
states[0]="element-a"
states[1]="element-b"
states[2]="element-c"
states[3]="element-d"
states[4]="element-e"
states[5]="element-f"
states[6]="element-g"
states[7]="element-h"
states[8]="element-i"
states[9]="element-j"
states[10]="element-k"
states[11]="element-l"
states[12]="element-m"
states[13]="element-n"
states[14]="element-o"
states[15]="element-p"
states[16]="element-q"
states[17]="element-r"
states[18]="element-s"
states[19]="element-t"
states[20]="element-u"
states[21]="element-v"
states[22]="element-w"
states[23]="element-x"
states[24]="element-y"
states[25]="element-z"

function startUp() {
 hideallExcept('');
}

function hideallExcept(elm) {
for (var i = 0; i &lt; states.length; i++) {
 var layer;
 if (layer = document.getElementById(states[i]) ) {
  if (states[i] != elm) {
    layer.style.display = "none";
  }
  else {
   layer.style.display = "block";
      }
  }
 }
 var mod;
 if ( mod = document.getElementById('elembymod') ) {
     mod.style.display = "none";
 }
}

function showall() {
 for (var i = 0; i &lt; states.length; i++) {
   var layer = document.getElementById(states[i]);
   layer.style.display = "block";
  }
}

function showByMod() {
  hideallExcept('');
  var mod = document.getElementById('elembymod');
  mod.style.display = "block";
}

function toggleToc(el){
   var item = el.parentNode; 
   for (j=0;j&lt;item.childNodes.length;j++)
   {
     if (item.childNodes[j].nodeType != 1) continue;
     if (item.childNodes[j].nodeName != 'UL') continue;
     var thisone=item.childNodes[j];
     var state=thisone.style.display;
     if (state == 'block')
      {  
        thisone.style.display='none'; 
	el.className="collapsed";
	el.title="Click here to expand list";
      }
     else
      {  
        thisone.style.display='block';
	el.className="expanded";
	el.title="Click here to collapse list";
      }
    }
  }

function togglerelax (el) {
    if (el.innerHTML == 'XML format to compact') {
         el.innerHTML = 'Compact to XML format';
      }
   else
     {
      el.innerHTML = 'XML format to compact';
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
    <xsl:variable name="d">
      <xsl:apply-templates mode="depth" select="."/>
    </xsl:variable>
    <xsl:if test="$d &gt; 0">
    <span class="bookmarklink">
      <a class="bookmarklink" href="#{$ident}">
	<xsl:attribute name="title">
	  <xsl:text>link to this section </xsl:text>
	</xsl:attribute>
	<span class="invisible">
	  <xsl:text>TEI: </xsl:text>
	  <xsl:value-of select="tei:head[1]"/>
	</span>
	<xsl:text>&#x00B6;</xsl:text>
      </a>
    </span>
    </xsl:if>
  </xsl:template>

  <xsl:template name="startDivHook">
    <xsl:if
      test="not(parent::tei:div) or not(local-name(preceding::*[1])='head')">
      <table class="miniTOC">
        <xsl:if test="not(parent::tei:div) and child::tei:div">
          <tr>
            <td>
              <xsl:call-template name="subtoc"/>
	      <hr/>
            </td>
          </tr>
        </xsl:if>
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

	<tr>
	  <td>
	    <a class="navigation" href="index.html">Home</a> | <a
	    class="navigation" href="index-toc.html">Table of
	    Contents</a>
	  </td>
	</tr>
        <tr>
          <td>
            <xsl:choose>
	      <xsl:when test="self::tei:elementSpec">
                <xsl:text> | </xsl:text>
                <a class="navigation" href="REFTAG.html">
                  Element catalogue</a>
              </xsl:when>
	      <xsl:when test="self::tei:classSpec">
                <xsl:text> | </xsl:text>
                <a class="navigation" href="REFCLA.html">
                  Class catalogue</a>
              </xsl:when>
	      <xsl:when test="self::tei:macroSpec">
                <xsl:text> | </xsl:text>
                <a class="navigation" href="REFENT.html">
                  Macro and datatype catalogue</a>
              </xsl:when>
              <xsl:otherwise>
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
              </xsl:otherwise>
            </xsl:choose>
          </td>
        </tr>
      </table>
    </xsl:if>
  </xsl:template>


  <xsl:template name="mainPage">
    <xsl:param name="currentID"/>
    <xsl:call-template name="teiTOP"/>
    <div class="main-content" id="onecol">
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
              <a href="REFCLA.html">Appendix A: Classes</a>
            </li>
            <li>
              <a href="REFTAG.html">Appendix B: Elements</a>
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
		<div class="container">
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

  <xsl:template name="navInterSep"> </xsl:template>

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


  <xsl:template name="makeHTMLHeading">
    <xsl:param name="text"/>
    <xsl:param name="class">title</xsl:param>
    <xsl:param name="level">1</xsl:param>
    <xsl:if test="not($text='')">
      <xsl:choose>
        <xsl:when test="$level='1'">
          <xsl:element name="h1">
            <xsl:attribute name="class">
              <xsl:value-of select="$class"/>
            </xsl:attribute>
            <a href="index.html"><xsl:value-of select="$text"/></a>
          </xsl:element>
          </xsl:when>
          <xsl:otherwise>
      <xsl:element name="h{$level}">
        <xsl:attribute name="class">
          <xsl:value-of select="$class"/>
        </xsl:attribute>
        <xsl:value-of select="$text"/>
      </xsl:element>
        </xsl:otherwise>
        </xsl:choose>
    </xsl:if>
  </xsl:template>
  

<xsl:template match="tei:divGen[@type='classcat']">
  <h3>Alphabetical list</h3>
  <xsl:apply-templates mode="weave" select="key('CLASSDOCS',1)">
    <xsl:sort select="@ident"/>
  </xsl:apply-templates>

  <xsl:for-each select="key('CLASSDOCS',1)">
    <xsl:sort select="@module"/>
    <xsl:if
	test="generate-id(.)=generate-id(key('CLASS-MODULE',@module)[1])">
      <div id='class-{@module}'>
      <h3>
	<xsl:for-each select="key('MODULES',@module)">
	  <xsl:value-of select="@ident"/>
	  <xsl:text>: </xsl:text>
	  <xsl:value-of select="tei:desc"/>
	</xsl:for-each>
      </h3>
      <xsl:apply-templates mode="weave"
			   select="key('CLASS-MODULE',@module)">
	<xsl:sort select="@ident"/>
      </xsl:apply-templates>
      </div>
    </xsl:if>
  </xsl:for-each>

</xsl:template>


<xsl:template match="tei:divGen[@type='macrocat']">

  <h3>Alphabetical list</h3>
  <xsl:apply-templates mode="weave" select="key('MACRODOCS',1)">
    <xsl:sort select="@ident"/>
  </xsl:apply-templates>

  <xsl:for-each select="key('MACRODOCS',1)">
    <xsl:sort select="@module"/>
    <xsl:if
	test="generate-id(.)=generate-id(key('MACRO-MODULE',@module)[1])">
      <div id='macro-{@module}'>
      <h3>
	<xsl:for-each select="key('MODULES',@module)">
	  <xsl:value-of select="@ident"/>
	  <xsl:text>: </xsl:text>
	  <xsl:value-of select="tei:desc"/>
	</xsl:for-each>
      </h3>
      <xsl:apply-templates mode="weave"
			   select="key('MACRO-MODULE',@module)">
	<xsl:sort select="@ident"/>
      </xsl:apply-templates>
      </div>
    </xsl:if>
  </xsl:for-each>
</xsl:template>


<xsl:template match="tei:divGen[@type='tagcat']">
  <div id="azindex">
    <span>Elements sorted
    alphabetically, starting with:</span>
      <ul class="index">     
    <li>
	<a onclick="hideallExcept('element-a');" href="#">a</a>
      </li>
      <li>
	<a onclick="hideallExcept('element-b');" href="#">b</a>
      </li>
      <li>
	<a onclick="hideallExcept('element-c');" href="#">c</a>
      </li>
      <li>
	<a onclick="hideallExcept('element-d');" href="#">d</a>
      </li>
      <li>
	<a onclick="hideallExcept('element-e');" href="#">e</a>
      </li>
      <li>
	<a onclick="hideallExcept('element-f');" href="#">f</a>
      </li>
      <li>
	<a onclick="hideallExcept('element-g');" href="#">g</a>
      </li>
      <li>
	<a onclick="hideallExcept('element-h');" href="#">h</a>
      </li>
      <li>
	<a onclick="hideallExcept('element-i');" href="#">i</a>
      </li>
      <li>
	<a onclick="hideallExcept('element-j');" href="#">j</a>
      </li>
      <li>
	<a onclick="hideallExcept('element-k');" href="#">k</a>
      </li>
      <li>
	<a onclick="hideallExcept('element-l');" href="#">l</a>
      </li>
      <li>
	<a onclick="hideallExcept('element-m');" href="#">m</a>
      </li>
      
      <li>
	<a onclick="hideallExcept('element-n');" href="#">n</a>
      </li>
      <li>
	<a onclick="hideallExcept('element-o');" href="#">o</a>
      </li>
      <li>
	<a onclick="hideallExcept('element-p');" href="#">p</a>
      </li>
      <li>
	<a onclick="hideallExcept('element-q');" href="#">q</a>
      </li>
      <li>
	<a onclick="hideallExcept('element-r');" href="#">r</a>
      </li>
      <li>
	<a onclick="hideallExcept('element-s');" href="#">s</a>
      </li>
      <li>
	<a onclick="hideallExcept('element-t');" href="#">t</a>
      </li>
      <li>
	<a onclick="hideallExcept('element-u');" href="#">u</a>
      </li>
      <li>
	<a onclick="hideallExcept('element-v');" href="#">v</a>
      </li>
      <li>
	<a onclick="hideallExcept('element-w');" href="#">w</a>
      </li>
      <li>
	<a onclick="hideallExcept('element-x');" href="#">x</a>
      </li>
      <li>
	<a onclick="hideallExcept('element-y');" href="#">y</a>
      </li>
      <li>
	<a onclick="hideallExcept('element-z');" href="#">z</a>
      </li>
      <li class="showall">
	<a onclick="showall();" href="#">Show All</a>
      </li>
      <li class="showall">
	<a onclick="showByMod();" href="#">Show by Module</a>
      </li>
    </ul>
  </div>

  <br style="clear:both;"/>

    <xsl:for-each select="key('ELEMENTDOCS',1)">
    <xsl:sort select="translate(@ident,$uc,$lc)"/>
    <xsl:variable name="letter">
      <xsl:value-of select="substring(@ident,1,1)"/>
    </xsl:variable>
    <xsl:if
	test="generate-id(.)=generate-id(key('ELEMENT-ALPHA',$letter)[1])">
      <ul class="atoz" id="element-{$letter}">	
	<xsl:for-each select="key('ELEMENT-ALPHA',$letter)">
	  <xsl:sort select="@ident"/>
	  <li>
	    <xsl:apply-templates select="." mode="weave"/>
	  </li>
	</xsl:for-each>
      </ul>
    </xsl:if>
  </xsl:for-each>

  <div id="elembymod">
    <xsl:for-each select="key('ELEMENTDOCS',1)">
      <xsl:sort select="@module"/>
      <xsl:if
	  test="generate-id(.)=generate-id(key('ELEMENT-MODULE',@module)[1])">
	<div>
	  <h3>
	    <xsl:for-each select="key('MODULES',@module)">
	      <xsl:value-of select="@ident"/>
	      <xsl:text>: </xsl:text>
	      <xsl:value-of select="tei:desc"/>
	    </xsl:for-each>
	  </h3>
	  <xsl:apply-templates mode="weave"
			       select="key('ELEMENT-MODULE',@module)">
	    <xsl:sort select="@ident"/>
	  </xsl:apply-templates>
	</div>
      </xsl:if>
    </xsl:for-each>
  </div>
</xsl:template>

<xsl:template name="formatHeadingNumber">
  <xsl:param name="text"/>
  <xsl:param name="toc"/>
  <span class="headingNumber">
    <xsl:choose>
      <xsl:when test="$toc =''">
	<xsl:copy-of select="$text"/>
      </xsl:when>
      <xsl:when test="number(normalize-space($text))&lt;10">
	<xsl:text>&#8194;</xsl:text>
	<xsl:copy-of select="$text"/>
      </xsl:when>
      <xsl:otherwise>
	<xsl:copy-of select="$text"/>
      </xsl:otherwise>
    </xsl:choose>
  </span>
</xsl:template>




<xsl:template name="teiTOP">
<xsl:param name="name"/>
               
<script type="text/javascript" src="udm-dom.js">
    <xsl:comment>&#160;</xsl:comment>
</script>
<script type="text/javascript" src="udm-mod-keyboard.js">
  <xsl:comment>&#160;</xsl:comment>
</script>

<div id="container">
   <a href="#contentstart" 
      title="Skip to content" 
      class="skip">skip to content</a>
   <div id="banner">
     <img src="Images/banner.jpg"
	  alt="Text Encoding Initiative logo and banner"/>
   </div>
   
   <xsl:copy-of select="document('staticnav.xml')/html:ul"/>
</div>
<div
 class="mainhead"><h1>P5: Guidelines for Electronic Text Encoding and Interchange</h1></div>

</xsl:template>
</xsl:stylesheet>
