<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet
  exclude-result-prefixes="exsl estr edate a fo local dbk xlink xhtml rng tei teix xd"
  extension-element-prefixes="exsl estr edate saxon7 saxon6" version="1.0"
  xmlns:xlink="http://www.w3.org/1999/xlink"
  xmlns:dbk="http://docbook.org/ns/docbook"
  xmlns:rng="http://relaxng.org/ns/structure/1.0"
  xmlns:tei="http://www.tei-c.org/ns/1.0"
  xmlns:teix="http://www.tei-c.org/ns/Examples"
  xmlns:xhtml="http://www.w3.org/1999/xhtml"
  xmlns:a="http://relaxng.org/ns/compatibility/annotations/1.0"
  xmlns:edate="http://exslt.org/dates-and-times"
  xmlns:estr="http://exslt.org/strings" xmlns:exsl="http://exslt.org/common"
  xmlns:fo="http://www.w3.org/1999/XSL/Format"
  xmlns:html="http://www.w3.org/1999/xhtml"
  xmlns:local="http://www.pantor.com/ns/local"
  xmlns:saxon6="http://icl.com/saxon" xmlns:saxon7="http://saxon.sf.net/"
  xmlns:xd="http://www.pnp-software.com/XSLTdoc"
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
  <xd:doc type="stylesheet">
    <xd:short> TEI stylesheet dealing with elements from the textstructure
      module, making HTML output. </xd:short>
    <xd:detail> This library is free software; you can redistribute it and/or
      modify it under the terms of the GNU Lesser General Public License as
      published by the Free Software Foundation; either version 2.1 of the
      License, or (at your option) any later version. This library is
      distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY;
      without even the implied warranty of MERCHANTABILITY or FITNESS FOR A
      PARTICULAR PURPOSE. See the GNU Lesser General Public License for more
      details. You should have received a copy of the GNU Lesser General Public
      License along with this library; if not, write to the Free Software
      Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA xs </xd:detail>
    <xd:author>See AUTHORS</xd:author>
    <xd:cvsId>$Id$</xd:cvsId>
    <xd:copyright>2007, TEI Consortium</xd:copyright>
  </xd:doc>
  <xd:doc>
    <xd:short>Process elements * in inner mode</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template match="*" mode="innertext">
    <xsl:apply-templates select="."/>
  </xsl:template>
  <xd:doc>
    <xd:short>Process elements * in paging mode</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template match="*" mode="paging">
    <xsl:choose>
      <xsl:when test="self::tei:divGen[@type='summary']">
        <xsl:call-template name="summaryToc"/>
      </xsl:when>
      <xsl:when test="self::tei:divGen">
        <xsl:apply-templates select="."/>
      </xsl:when>
      <xsl:when test="starts-with(local-name(),'div')">
        <xsl:if
          test="not(preceding-sibling::tei:*) or preceding-sibling::tei:titlePage">
          <h2>
            <xsl:apply-templates mode="xref" select="."/>
	    <xsl:call-template name="sectionHeadHook"/>
          </h2>
          <xsl:if test="$topNavigationPanel='true'">
            <xsl:call-template name="xrefpanel">
              <xsl:with-param name="homepage"
                select="concat($masterFile,$standardSuffix)"/>
              <xsl:with-param name="mode" select="local-name(.)"/>
            </xsl:call-template>
          </xsl:if>
          <xsl:call-template name="doDivBody"/>
          <xsl:if test="$bottomNavigationPanel='true'">
            <xsl:call-template name="xrefpanel">
              <xsl:with-param name="homepage"
                select="concat($masterFile,$standardSuffix)"/>
              <xsl:with-param name="mode" select="local-name(.)"/>
            </xsl:call-template>
          </xsl:if>
        </xsl:if>
      </xsl:when>
      <xsl:when test="local-name(..)='front'">
        <xsl:apply-templates select="."/>
        <xsl:apply-templates mode="paging" select="following-sibling::tei:*[1]"/>
        <xsl:apply-templates mode="paging" select="../../tei:body/tei:*[1]"/>
      </xsl:when>
      <xsl:otherwise>
        <xsl:apply-templates select="."/>
        <xsl:apply-templates mode="paging" select="following-sibling::tei:*[1]"
        />
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  <xd:doc>
    <xd:short>Process elements /</xd:short>
    <xd:detail>
      <p> processors must support `key' </p>
    </xd:detail>
  </xd:doc>
  <xsl:template match="/">
    <xsl:if test="contains($processor,'Clark')">
      <xsl:message terminate="yes"> XT is not supported by the TEI stylesheets,
        as it does not implement the "key" function </xsl:message>
    </xsl:if>
    <xsl:choose>
      <!-- there are various choices of how to proceed, driven by

$pageLayout: Simple, CSS, Table, Frames

$STDOUT: true or false

$splitLevel: -1 to 3

$ID: requests a particular page
-->
      <!-- we are making a composite layout and there is a TEI(2) element -->
      <xsl:when
        test="($pageLayout = 'CSS' or $pageLayout = 'Table') and (tei:TEI or tei:teiCorpus)">
        <xsl:if test="$verbose='true'">
          <xsl:message>case 1: pageLayout <xsl:value-of select="$pageLayout"
          /></xsl:message>
        </xsl:if>
        <xsl:for-each select="tei:TEI|tei:teiCorpus">
          <xsl:call-template name="doPageTable">
            <xsl:with-param name="currentID" select="$ID"/>
          </xsl:call-template>
        </xsl:for-each>
        <xsl:if test="$STDOUT='false'">
          <xsl:call-template name="doDivs"/>
        </xsl:if>
      </xsl:when>
      <!-- we are making a frame-based system -->
      <xsl:when test="$pageLayout='Frames'">
        <xsl:if test="$verbose='true'">
          <xsl:message>case 2: pageLayout <xsl:value-of select="$pageLayout"
          /></xsl:message>
        </xsl:if>
        <xsl:call-template name="doFrames"/>
      </xsl:when>
      <!-- we have been asked for a particular section of the document -->
      <xsl:when test="not($ID='')">
        <xsl:if test="$verbose='true'">
          <xsl:message>case 3: ID <xsl:value-of select="$ID"/>, pageLayout
              <xsl:value-of select="$pageLayout"/></xsl:message>
        </xsl:if>
        <xsl:choose>
          <xsl:when test="$ID='frametoc___'">
            <xsl:call-template name="writeFrameToc"/>
          </xsl:when>
          <xsl:when test="$ID='prelim___'">
            <xsl:apply-templates/>
          </xsl:when>
          <xsl:when test="count(key('IDS',$ID))&gt;0">
            <xsl:for-each select="key('IDS',$ID)">
              <xsl:call-template name="writeDiv"/>
            </xsl:for-each>
          </xsl:when>
          <xsl:otherwise>
            <!-- the passed ID is a pseudo-XPath expression
		 which starts below TEI/text.
		 The real XPath syntax is changed to avoid problems
	    -->
            <xsl:apply-templates mode="xpath" select="tei:TEI/tei:text">
              <xsl:with-param name="xpath" select="$ID"/>
            </xsl:apply-templates>
          </xsl:otherwise>
        </xsl:choose>
      </xsl:when>
      <!-- we want HTML to just splurge out-->
      <xsl:when test="$STDOUT='true'">
        <xsl:if test="$verbose='true'">
          <xsl:message>case 4: write to stdout, pageLayout <xsl:value-of
              select="$pageLayout"/></xsl:message>
        </xsl:if>
        <xsl:apply-templates/>
      </xsl:when>
      <!-- we want the document split up into separate files -->
      <xsl:when test="tei:TEI or tei:teiCorpus and $splitLevel&gt;-1">
        <xsl:if test="$verbose='true'">
          <xsl:message>case 5: split output, <xsl:value-of select="$splitLevel"
            /> pageLayout <xsl:value-of select="$pageLayout"/></xsl:message>
        </xsl:if>
        <xsl:apply-templates mode="split"/>
      </xsl:when>
      <!-- we want the whole document, in an output file -->
      <xsl:otherwise>
        <xsl:if test="$verbose='true'">
          <xsl:message>case 6: one document, pageLayout <xsl:value-of
              select="$pageLayout"/></xsl:message>
        </xsl:if>
        <xsl:choose>
          <xsl:when test="$masterFile='' or $STDOUT='true'">
            <xsl:apply-templates/>
          </xsl:when>
          <xsl:otherwise>
            <xsl:call-template name="outputChunk">
              <xsl:with-param name="ident">
                <xsl:value-of select="$masterFile"/>
              </xsl:with-param>
              <xsl:with-param name="content">
                <xsl:apply-templates/>
              </xsl:with-param>
            </xsl:call-template>
          </xsl:otherwise>
        </xsl:choose>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  <xd:doc>
    <xd:short>Process elements processing-instruction()[name()='xmltex']</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template match="processing-instruction()[name()='xmltex']">
    <xsl:value-of select="."/>
  </xsl:template>
  <xd:doc>
    <xd:short>Process elements tei:*</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template match="tei:*" mode="generateNextLink">
    <span class="nextLink"><xsl:text> </xsl:text><xsl:call-template name="i18n">
        <xsl:with-param name="word">nextWord</xsl:with-param>
      </xsl:call-template>: </span>
      <a class="navigation">
	<xsl:if test="$virtualPages='true'">
	  <xsl:attribute name="onclick">
	    <xsl:text>switchDiv('</xsl:text>
	    <xsl:apply-templates mode="ident" select="."/>
	    <xsl:text>');return false</xsl:text>
	  </xsl:attribute>
	</xsl:if>
      <xsl:attribute name="href">
        <xsl:apply-templates mode="generateLink" select="."/>
      </xsl:attribute>
      <xsl:call-template name="headerLink">
        <xsl:with-param name="minimal" select="$minimalCrossRef"/>
      </xsl:call-template>
    </a>
  </xsl:template>
  <xd:doc>
    <xd:short>Process elements tei:*</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template match="tei:*" mode="generatePreviousLink">
    <span class="previousLink"><xsl:text> </xsl:text><xsl:call-template name="i18n">
        <xsl:with-param name="word">previousWord</xsl:with-param>
      </xsl:call-template>: </span>
    <a class="navigation">
      <xsl:if test="$virtualPages='true'">
        <xsl:attribute name="onclick">
          <xsl:text>switchDiv('</xsl:text>
          <xsl:apply-templates mode="ident" select="."/>
          <xsl:text>');return false</xsl:text>
        </xsl:attribute>
      </xsl:if>
      <xsl:attribute name="href">
        <xsl:apply-templates mode="generateLink" select="."/>
      </xsl:attribute>
      <xsl:call-template name="headerLink">
        <xsl:with-param name="minimal" select="$minimalCrossRef"/>
      </xsl:call-template>
    </a>
  </xsl:template>
  <xd:doc>
    <xd:short>Process any element in xpath mode</xd:short>
    <xd:param name="xpath">xpath</xd:param>
    <xd:param name="action">action</xd:param>
    <xd:detail>
      <p> This nice bit of code is from Jeni Tennison </p>
    </xd:detail>
  </xd:doc>
  <xsl:template match="tei:*" mode="xpath">
    <xsl:param name="xpath"/>
    <xsl:param name="action"/>
    <xsl:choose>
      <!-- if there is a path -->
      <xsl:when test="$xpath">
        <!-- step is the part before the '_' (if there is one) -->
        <xsl:variable name="step">
          <xsl:choose>
            <xsl:when test="contains($xpath, '_')">
              <xsl:value-of select="substring-before($xpath, '_')"/>
            </xsl:when>
            <xsl:otherwise>
              <xsl:value-of select="$xpath"/>
            </xsl:otherwise>
          </xsl:choose>
        </xsl:variable>
        <!-- the child's name is the part before the '.' -->
        <xsl:variable name="childName" select="substring-before($step, '.')"/>
        <!-- and its index is the part after '.' -->
        <xsl:variable name="childIndex" select="substring-after($step, '.')"/>
        <!-- so apply templates to that child, passing in the $xpath
	     left after the first step -->
        <xsl:apply-templates mode="xpath"
          select="*[name() = $childName]          [number($childIndex)]">
          <xsl:with-param name="xpath" select="substring-after($xpath, '_')"/>
          <xsl:with-param name="action" select="$action"/>
        </xsl:apply-templates>
      </xsl:when>
      <!-- if there's no path left, then this is the element we want -->
      <xsl:otherwise>
        <xsl:choose>
          <xsl:when test="$action='header'">
            <xsl:apply-templates mode="xref" select="."/>
          </xsl:when>
          <xsl:when test="$action='notes'">
            <xsl:call-template name="printNotes"/>
          </xsl:when>
          <xsl:when test="$action='toclist'">
            <xsl:call-template name="linkListContents">
              <xsl:with-param name="style" select="'toclist'"/>
            </xsl:call-template>
          </xsl:when>
          <xsl:when
            test="starts-with(local-name(),'div') and      $pageLayout='Table'      or      $pageLayout='CSS'">
            <h2>
              <xsl:apply-templates mode="xref" select="."/>
	      <xsl:call-template name="sectionHeadHook"/>
            </h2>
	    <xsl:if test="$topNavigationPanel='true'">
	      <xsl:call-template name="xrefpanel">
		<xsl:with-param name="homepage"
				select="concat($masterFile,$standardSuffix)"/>
		<xsl:with-param name="mode" select="local-name(.)"/>
	      </xsl:call-template>
	    </xsl:if>
            <xsl:call-template name="doDivBody"/>
            <xsl:if test="$bottomNavigationPanel='true'">
              <xsl:call-template name="xrefpanel">
                <xsl:with-param name="homepage"
                  select="concat($masterFile,$standardSuffix)"/>
                <xsl:with-param name="mode" select="local-name(.)"/>
              </xsl:call-template>
            </xsl:if>
          </xsl:when>
          <xsl:when test="self::tei:divGen[@type='summary']">
            <xsl:call-template name="summaryToc"/>
          </xsl:when>
          <xsl:otherwise>
            <html>
              <xsl:call-template name="addLangAtt"/>
              <xsl:call-template name="htmlFileTop"/>
              <body id="TOP">
                <xsl:attribute name="onload">
                  <xsl:text>startUp()</xsl:text>
                </xsl:attribute>
                <xsl:call-template name="bodyHook"/>
                <xsl:call-template name="bodyJavascriptHook"/>
		<div class="stdheader">
                <xsl:call-template name="stdheader">
                  <xsl:with-param name="title">
                    <xsl:call-template name="generateTitle"/>
                  </xsl:with-param>
                </xsl:call-template>
		</div>
                <h2>
                  <xsl:apply-templates mode="xref" select="."/>
		  <xsl:call-template name="sectionHeadHook"/>
                </h2>
                <xsl:apply-templates/>
                <xsl:call-template name="printNotes"/>
                <xsl:call-template name="htmlFileBottom"/>
              </body>
            </html>
          </xsl:otherwise>
        </xsl:choose>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  <xd:doc>
    <xd:short>Process elements tei:TEI</xd:short>
    <xd:detail>
      <p> *****************************************</p>
    </xd:detail>
  </xd:doc>
  <xsl:template match="tei:TEI">
    <xsl:call-template name="teiStartHook"/>
    <xsl:if test="$verbose='true'">
      <xsl:message>TEI HTML in single document mode </xsl:message>
    </xsl:if>
    <html>
      <xsl:call-template name="addLangAtt"/>
      <xsl:comment>THIS FILE IS GENERATED FROM AN XML MASTER. DO NOT EDIT (5)</xsl:comment>
      <head>
        <xsl:variable name="pagetitle">
          <xsl:call-template name="generateTitle"/>
        </xsl:variable>
        <title>
          <xsl:value-of select="$pagetitle"/>
        </title>
        <xsl:call-template name="headHook"/>
        <xsl:call-template name="metaHTML">
          <xsl:with-param name="title" select="$pagetitle"/>
        </xsl:call-template>
        <xsl:call-template name="includeCSS"/>
        <xsl:call-template name="cssHook"/>
        <xsl:call-template name="includeJavascript"/>
        <xsl:call-template name="javascriptHook"/>
      </head>
      <body class="simple" id="TOP">
        <xsl:attribute name="onload">
          <xsl:text>startUp()</xsl:text>
        </xsl:attribute>
        <xsl:call-template name="bodyHook"/>
        <xsl:call-template name="bodyJavascriptHook"/>
        <xsl:if test="not(tei:text/tei:front/tei:titlePage)">
	  <div class="stdheader">
	    <xsl:call-template name="stdheader">
	      <xsl:with-param name="title">
		<xsl:call-template name="generateTitle"/>
	      </xsl:with-param>
	    </xsl:call-template>
	  </div>
        </xsl:if>
        <xsl:call-template name="startHook"/>
        <xsl:call-template name="simpleBody"/>
        <xsl:call-template name="stdfooter"/>
      </body>
    </html>
    <xsl:if test="$verbose='true'">
      <xsl:message>TEI HTML: run end hook template teiEndHook</xsl:message>
    </xsl:if>
    <xsl:call-template name="teiEndHook"/>
  </xsl:template>
  <xd:doc>
    <xd:short>Process elements tei:TEI</xd:short>
    <xd:detail>
      <p> *****************************************</p>
    </xd:detail>
  </xd:doc>
  <xsl:template match="tei:TEI" mode="split">
    <xsl:variable name="BaseFile">
      <xsl:value-of select="$masterFile"/>
      <xsl:call-template name="addCorpusID"/>
    </xsl:variable>
    <xsl:if test="$verbose='true'">
      <xsl:message>TEI HTML: run start hook template teiStartHook</xsl:message>
    </xsl:if>
    <xsl:call-template name="teiStartHook"/>
    <xsl:if test="$verbose='true'">
      <xsl:message>TEI HTML in splitting mode, base file is <xsl:value-of
          select="$BaseFile"/>
      </xsl:message>
    </xsl:if>
    <xsl:call-template name="outputChunk">
      <xsl:with-param name="ident">
        <xsl:value-of select="$BaseFile"/>
      </xsl:with-param>
      <xsl:with-param name="content">
        <xsl:call-template name="pageLayoutSimple"/>
      </xsl:with-param>
    </xsl:call-template>
    <xsl:if test="$verbose='true'">
      <xsl:message>TEI HTML: run end hook template teiEndHook</xsl:message>
    </xsl:if>
    <xsl:call-template name="teiEndHook"/>
  </xsl:template>
  <xd:doc>
    <xd:short>Process elements tei:body|tei:back</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template match="tei:body|tei:back" mode="split">
    <xsl:for-each select="*">
      <xsl:choose>
        <xsl:when test="starts-with(local-name(.),'div')">
          <xsl:apply-templates mode="split" select="."/>
        </xsl:when>
        <xsl:otherwise>
          <xsl:apply-templates select="."/>
        </xsl:otherwise>
      </xsl:choose>
    </xsl:for-each>
  </xsl:template>
  <xd:doc>
    <xd:short>Process elements tei:body in inner mode</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template match="tei:body" mode="inner">
    <xsl:apply-templates/>
  </xsl:template>
  <xd:doc>
    <xd:short>Process elements tei:closer</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template match="tei:closer">
    <blockquote class="closer">
      <xsl:choose>
        <xsl:when test="tei:p">
          <xsl:apply-templates/>
        </xsl:when>
        <xsl:otherwise>
          <p>
            <xsl:apply-templates/>
          </p>
        </xsl:otherwise>
      </xsl:choose>
    </blockquote>
  </xsl:template>
  <xd:doc>
    <xd:short>Process elements tei:dateline</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template match="tei:dateline">
    <div class="dateline">
      <xsl:apply-templates/>
    </div>
  </xsl:template>
  <xd:doc>
    <xd:short>Process elements tei:divGen[@type='actions']</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template match="tei:divGen[@type='actions']">
    <h3>Actions arising</h3>
    <dl>
      <xsl:for-each select="/tei:TEI/tei:text//tei:note[@type='action']">
        <dt>
          <b>
            <xsl:number count="tei:note[@type='action']" level="any"/>
          </b>
        </dt>
        <dd>
          <xsl:apply-templates/>
        </dd>
      </xsl:for-each>
    </dl>
  </xsl:template>
  <xd:doc>
    <xd:short>Process elements tei:divGen[@type='toc']</xd:short>
    <xd:detail>
      <p> table of contents </p>
    </xd:detail>
  </xd:doc>
  <xsl:template match="tei:divGen[@type='toc']">
    <h2>
      <xsl:call-template name="i18n">
        <xsl:with-param name="word">tocWords</xsl:with-param>
      </xsl:call-template>
    </h2>
    <xsl:call-template name="mainTOC"/>
  </xsl:template>
  <xd:doc>
    <xd:short>Process elements tei:div[@type='canto']</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template match="tei:div[@type='canto']">
    <xsl:variable name="divlevel" select="count(ancestor::tei:div)"/>
    <xsl:element name="h{$divlevel + $divOffset}">
      <xsl:call-template name="makeAnchor"/>
      <xsl:call-template name="header">
	<xsl:with-param name="display">full</xsl:with-param>
      </xsl:call-template>
      <xsl:call-template name="sectionHeadHook"/>
    </xsl:element>
    <xsl:apply-templates/>
  </xsl:template>
  <xd:doc>
    <xd:short>Process elements tei:div*, @type='letter'</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template match="tei:div1[@type='letter']|tei:div[@type='letter']">
    <div class="letter">
      <xsl:apply-templates/>
    </div>
  </xsl:template>
  <xd:doc>
    <xd:short>Process elements tei:div[@type='epistle']</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template match="tei:div[@type='epistle']">
    <div class="epistle">
      <xsl:apply-templates/>
    </div>
  </xsl:template>
  <xd:doc>
    <xd:short>Process elements tei:div[@type='frontispiece']</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template match="tei:div[@type='frontispiece']">
    <xsl:apply-templates/>
  </xsl:template>
  <xd:doc>
    <xd:short>Process elements tei:div[@type='illustration']</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template match="tei:div[@type='illustration']">
    <xsl:apply-templates/>
  </xsl:template>
  <xd:doc>
    <xd:short>Process elements
      tei:div|tei:div0|tei:div1|tei:div2|tei:div3|tei:div4|tei:div5|tei:div6</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template
    match="tei:div|tei:div0|tei:div1|tei:div2|tei:div3|tei:div4|tei:div5|tei:div6">
    <xsl:variable name="depth">
      <xsl:apply-templates mode="depth" select="."/>
    </xsl:variable>
    <!-- depending on depth and splitting level, 
	 we may do one of two things: -->
    <xsl:choose>
      <!-- -1. Override at top level -->
      <xsl:when test="ancestor::tei:TEI/@rend='all'">
        <div>
          <xsl:call-template name="divClassAttribute">
            <xsl:with-param name="depth" select="$depth"/>
          </xsl:call-template>
          <xsl:call-template name="doDivBody">
            <xsl:with-param name="Type" select="$depth"/>
          </xsl:call-template>
        </div>
      </xsl:when>
      <xsl:when test="ancestor::tei:TEI/@rend='frontpage'">
        <div>
          <xsl:call-template name="divClassAttribute">
            <xsl:with-param name="depth" select="$depth"/>
          </xsl:call-template>
          <xsl:call-template name="doDivBody">
            <xsl:with-param name="Type" select="$depth"/>
          </xsl:call-template>
        </div>
      </xsl:when>
      <!-- 0. We have gone far enough -->
      <xsl:when test="$depth = $splitLevel and $STDOUT='true'">
        <xsl:if test="$virtualPages='true'">
          <xsl:call-template name="makeDivPage">
            <xsl:with-param name="depth" select="$depth"/>
          </xsl:call-template>
        </xsl:if>
      </xsl:when>
      <!-- 1. our section depth is below the splitting level -->
      <xsl:when
        test="$depth &gt; $splitLevel or         @rend='nosplit' or         ancestor::tei:TEI/@rend='all' or         ancestor::tei:TEI/@rend='frontpage' or         ancestor::tei:TEI/@rend='nosplit'">
        <div>
          <xsl:call-template name="divClassAttribute">
            <xsl:with-param name="depth" select="$depth"/>
          </xsl:call-template>
          <xsl:call-template name="doDivBody">
            <xsl:with-param name="Type" select="$depth"/>
          </xsl:call-template>
        </div>
      </xsl:when>
      <!-- 2. we are at or above splitting level, 
	   so start a new page  -->
      <xsl:when
        test="$depth &lt;= $splitLevel and parent::tei:front         and $splitFrontmatter='true'">
        <xsl:call-template name="makeDivPage">
          <xsl:with-param name="depth" select="$depth"/>
        </xsl:call-template>
      </xsl:when>
      <xsl:when
        test="$depth &lt;= $splitLevel and parent::tei:back and $splitBackmatter='true'">
        <xsl:call-template name="makeDivPage">
          <xsl:with-param name="depth" select="$depth"/>
        </xsl:call-template>
      </xsl:when>
      <xsl:when test="$depth &lt;= $splitLevel and parent::tei:body">
        <xsl:call-template name="makeDivPage">
          <xsl:with-param name="depth" select="$depth"/>
        </xsl:call-template>
      </xsl:when>
      <xsl:otherwise>
        <div>
          <xsl:call-template name="divClassAttribute">
            <xsl:with-param name="depth" select="$depth"/>
          </xsl:call-template>
          <xsl:call-template name="doDivBody">
            <xsl:with-param name="Type" select="$depth"/>
          </xsl:call-template>
        </div>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  <xd:doc>
    <xd:short>Create a new output page for a section</xd:short>
    <xd:detail> </xd:detail>
    <xd:param name="depth">depth of section (1, 2 3, 4 etc)</xd:param>
  </xd:doc>
  <xsl:template name="makeDivPage">
    <xsl:param name="depth"/>
    <xsl:choose>
      <xsl:when test="$virtualPages='true'">
        <div>
          <xsl:call-template name="divClassAttribute">
            <xsl:with-param name="depth" select="$depth"/>
          </xsl:call-template>
          <xsl:if test="$topNavigationPanel='true'">
            <xsl:call-template name="xrefpanel">
              <xsl:with-param name="mode" select="local-name(.)"/>
            </xsl:call-template>
          </xsl:if>
          <xsl:call-template name="doDivBody">
            <xsl:with-param name="Type" select="$depth"/>
          </xsl:call-template>
          <xsl:if test="$bottomNavigationPanel='true'">
            <xsl:call-template name="xrefpanel">
              <xsl:with-param name="mode" select="local-name(.)"/>
            </xsl:call-template>
          </xsl:if>
        </div>
      </xsl:when>
      <xsl:otherwise>
        <xsl:call-template name="outputChunk">
          <xsl:with-param name="ident">
            <xsl:apply-templates mode="ident" select="."/>
          </xsl:with-param>
          <xsl:with-param name="content">
            <xsl:choose>
              <xsl:when test="$pageLayout='CSS'">
                <xsl:call-template name="pageLayoutCSS">
                  <xsl:with-param name="currentID">
                    <xsl:apply-templates mode="ident" select="."/>
                  </xsl:with-param>
                </xsl:call-template>
              </xsl:when>
              <xsl:when test="$pageLayout='Table'">
                <xsl:call-template name="pageLayoutTable">
                  <xsl:with-param name="currentID">
                    <xsl:apply-templates mode="ident" select="."/>
                  </xsl:with-param>
                </xsl:call-template>
              </xsl:when>
              <xsl:otherwise>
                <xsl:call-template name="writeDiv"/>
              </xsl:otherwise>
            </xsl:choose>
          </xsl:with-param>
        </xsl:call-template>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  <xd:doc>
    <xd:short>Process elements tei:docAuthor in "author" mode"</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template match="tei:docAuthor" mode="author">
    <xsl:if test="preceding-sibling::tei:docAuthor">
      <xsl:text>, </xsl:text>
    </xsl:if>
    <xsl:apply-templates/>
  </xsl:template>
  <xd:doc>
    <xd:short>Process elements tei:docTitle, docAuthor, docImprint and
      tei:docDate</xd:short>
    <xd:detail>
      <p> Translate these to a corresponding HTML div </p>
    </xd:detail>
  </xd:doc>
  <xsl:template match="tei:docTitle|tei:docAuthor|tei:docImprint|tei:docDate">
    <div class="{local-name()}">
      <xsl:apply-templates/>
    </div>
  </xsl:template>
  <xd:doc>
    <xd:short>Process elements tei:opener</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template match="tei:opener">
    <blockquote class="opener">
      <xsl:choose>
        <xsl:when test="tei:p">
          <xsl:apply-templates/>
        </xsl:when>
        <xsl:otherwise>
          <p>
            <xsl:apply-templates/>
          </p>
        </xsl:otherwise>
      </xsl:choose>
    </blockquote>
  </xsl:template>
  <xd:doc>
    <xd:short>Process elements tei:text</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template match="tei:text">
    <xsl:choose>
      <xsl:when test="parent::tei:TEI">
        <xsl:apply-templates/>
      </xsl:when>
      <xsl:when test="parent::tei:group">
        <xsl:apply-templates/>
      </xsl:when>
      <xsl:otherwise>
        <div class="innertext">
          <xsl:apply-templates mode="innertext"/>
        </div>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  <xd:doc>
    <xd:short>Process elements tei:titlePage</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template match="tei:titlePage">
    <div class="titlePage">
      <xsl:apply-templates/>
    </div>
  </xsl:template>
  <xd:doc>
    <xd:short>[html] </xd:short>
    <xd:param name="crumbBody">crumbBody</xd:param>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template name="aCrumb">
    <xsl:param name="crumbBody"/>
    <li class="breadcrumb">
      <xsl:copy-of select="$crumbBody"/>
    </li>
  </xsl:template>
  <xd:doc>
    <xd:short>[html] </xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template name="addCorpusID">
    <xsl:if test="ancestor-or-self::tei:teiCorpus">
      <xsl:for-each select="ancestor-or-self::tei:TEI">
        <xsl:text>-</xsl:text>
        <xsl:choose>
          <xsl:when test="@xml:id">
            <xsl:value-of select="@xml:id"/>
          </xsl:when>
          <xsl:otherwise>
            <xsl:number/>
          </xsl:otherwise>
        </xsl:choose>
      </xsl:for-each>
    </xsl:if>
  </xsl:template>
  <xd:doc>
    <xd:short>[html] </xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template name="addLangAtt">
    <xsl:variable name="documentationLanguage">
    <xsl:choose>
      <xsl:when test="string-length($doclang)&gt;0">
	<xsl:value-of select="$doclang"/>
      </xsl:when>
      <xsl:when test="ancestor-or-self::tei:schemaSpec/@docLang">
	<xsl:value-of select="//tei:schemaSpec[1]/@docLang"/>
      </xsl:when>
      <xsl:otherwise>
	<xsl:text>en</xsl:text>
      </xsl:otherwise>
    </xsl:choose>
    </xsl:variable>

    <xsl:variable name="supplied">
      <xsl:choose>
        <xsl:when test="ancestor-or-self::tei:*[@xml:lang]">
          <xsl:value-of select="ancestor-or-self::tei:*[@xml:lang][1]/@xml:lang"
          />
        </xsl:when>
        <xsl:when test="ancestor-or-self::tei:*[@lang]">
          <xsl:value-of select="ancestor-or-self::tei:*[@lang][1]/@lang"/>
        </xsl:when>
        <xsl:otherwise>
          <xsl:value-of select="$documentationLanguage"/>
        </xsl:otherwise>
      </xsl:choose>
    </xsl:variable>
    <xsl:choose>
      <xsl:when test="$xhtml='true'">
        <xsl:attribute name="xml:lang">
          <xsl:value-of select="$supplied"/>
        </xsl:attribute>
      </xsl:when>
      <xsl:otherwise>
        <xsl:attribute name="lang">
          <xsl:value-of select="$supplied"/>
        </xsl:attribute>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  <xd:doc>
    <xd:short>[html] </xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
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
            <xsl:value-of select="substring-after($REQUEST,'/')"/>
          </xsl:with-param>
          <xsl:with-param name="class">breadcrumb</xsl:with-param>
        </xsl:call-template>
      </ul>
    </div>
  </xsl:template>
  <xd:doc>
    <xd:short>[html] </xd:short>
    <xd:param name="Head">Head</xd:param>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template name="doBody">
    <xsl:param name="Head"/>
    <xsl:variable name="ident">
      <xsl:apply-templates mode="ident" select="."/>
    </xsl:variable>
    <xsl:choose>
      <xsl:when test="parent::tei:div/@rend='multicol'">
        <td valign="top">
          <xsl:if test="not($Head = '')">
            <xsl:element name="h{$Head + $divOffset}">
              <xsl:if test="$xhtml='false'">
                <a name="{$ident}"/>
              </xsl:if>
              <xsl:call-template name="header">
		<xsl:with-param name="display">full</xsl:with-param>
	      </xsl:call-template>
	      <xsl:call-template name="sectionHeadHook"/>
            </xsl:element>
          </xsl:if>
          <xsl:apply-templates select="tei:text/tei:body"/>
        </td>
      </xsl:when>
      <xsl:when test="@rend='multicol'">
        <table>
          <tr>
            <xsl:apply-templates select="tei:text/tei:body"/>
          </tr>
        </table>
      </xsl:when>
      <xsl:otherwise>
        <xsl:if test="not($Head = '')">
          <xsl:element name="h{$Head + $divOffset}">
            <xsl:if test="$xhtml='false'">
              <a name="{$ident}"/>
            </xsl:if>
            <xsl:call-template name="header">
	      <xsl:with-param name="display">full</xsl:with-param>
	    </xsl:call-template>
	    <xsl:call-template name="sectionHeadHook"/>
          </xsl:element>
        </xsl:if>
        <xsl:apply-templates select="tei:text/tei:body"/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  <xd:doc>
    <xd:short>[html] Make a section</xd:short>
    <xd:param name="Type">Type</xd:param>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template name="doDivBody">
    <xsl:param name="Type"/>
    <xsl:call-template name="startDivHook"/>
    <xsl:variable name="ident">
      <xsl:apply-templates mode="ident" select="."/>
    </xsl:variable>
    <xsl:choose>
      <xsl:when test="parent::tei:div/@rend='multicol'">
        <td valign="top">
          <xsl:if test="not($Type = '')">
            <xsl:element name="h{$Type + $divOffset}">
              <xsl:if test="$xhtml='false'">
                <a name="{$ident}"/>
              </xsl:if>
              <xsl:call-template name="header">
		<xsl:with-param name="display">full</xsl:with-param>
	      </xsl:call-template>
	      <xsl:call-template name="sectionHeadHook"/>
            </xsl:element>
          </xsl:if>
          <xsl:apply-templates/>
        </td>
      </xsl:when>
      <xsl:when test="@rend='multicol'">
        <xsl:apply-templates select="*[not(local-name(.)='div')]"/>
        <table>
          <tr>
            <xsl:apply-templates select="tei:div"/>
          </tr>
        </table>
      </xsl:when>
      <xsl:otherwise>
        <xsl:if test="not($Type = '')">
          <xsl:element name="h{$Type + $divOffset}">
            <xsl:if test="$xhtml='false'">
              <a name="{$ident}"/>
            </xsl:if>
            <xsl:call-template name="header">
	      <xsl:with-param name="display">full</xsl:with-param>
	    </xsl:call-template>
	    <xsl:call-template name="sectionHeadHook"/>
          </xsl:element>
        </xsl:if>
        <xsl:apply-templates/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  <xd:doc>
    <xd:short>[html] </xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template name="doDivs">
    <xsl:for-each select="tei:TEI/tei:text">
      <xsl:for-each select="tei:front|tei:body|tei:back">
        <xsl:for-each select="tei:div|tei:div0|tei:div1">
          <xsl:variable name="currentID">
            <xsl:apply-templates mode="ident" select="."/>
          </xsl:variable>
          <xsl:call-template name="doPageTable">
            <xsl:with-param name="currentID" select="$currentID"/>
          </xsl:call-template>
        </xsl:for-each>
      </xsl:for-each>
    </xsl:for-each>
  </xsl:template>
  <xd:doc>
    <xd:short>[html] </xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template name="doFrames">
    <xsl:variable name="BaseFile">
      <xsl:value-of select="$masterFile"/>
      <xsl:call-template name="addCorpusID"/>
    </xsl:variable>
    <xsl:choose>
      <xsl:when test="$ID='toclist___'">
        <xsl:call-template name="writeFrameToc"/>
      </xsl:when>
      <xsl:when test="$STDOUT='true'">
        <xsl:call-template name="pageLayoutSimple"/>
      </xsl:when>
      <xsl:otherwise>
        <xsl:call-template name="outputChunk">
          <xsl:with-param name="ident">
            <xsl:value-of select="concat($BaseFile,'-menubar')"/>
          </xsl:with-param>
          <xsl:with-param name="content">
            <xsl:call-template name="writeFrameToc"/>
          </xsl:with-param>
        </xsl:call-template>
        <xsl:call-template name="outputChunk">
          <xsl:with-param name="ident">
            <xsl:value-of select="concat($BaseFile,'-frames')"/>
          </xsl:with-param>
          <xsl:with-param name="content">
            <xsl:call-template name="pageLayoutSimple"/>
          </xsl:with-param>
        </xsl:call-template>
        <xsl:apply-templates mode="split" select="tei:TEI"/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  <xd:doc>
    <xd:short>[html] </xd:short>
    <xd:param name="currentID">currentID</xd:param>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template name="doPageTable">
    <xsl:param name="currentID"/>
    <xsl:variable name="BaseFile">
      <xsl:value-of select="$masterFile"/>
      <xsl:call-template name="addCorpusID"/>
    </xsl:variable>
    <xsl:call-template name="outputChunk">
      <xsl:with-param name="ident">
        <xsl:choose>
          <xsl:when test="$STDOUT='true'"/>
          <xsl:when test="not($currentID='')">
            <xsl:value-of select="$currentID"/>
          </xsl:when>
          <xsl:otherwise>
            <xsl:value-of select="$BaseFile"/>
          </xsl:otherwise>
        </xsl:choose>
      </xsl:with-param>
      <xsl:with-param name="content">
        <xsl:choose>
          <xsl:when test="$pageLayout='CSS'">
            <xsl:call-template name="pageLayoutCSS">
              <xsl:with-param name="currentID" select="$currentID"/>
            </xsl:call-template>
          </xsl:when>
          <xsl:when test="$pageLayout='Table'">
            <xsl:call-template name="pageLayoutTable">
              <xsl:with-param name="currentID" select="$currentID"/>
            </xsl:call-template>
          </xsl:when>
        </xsl:choose>
      </xsl:with-param>
    </xsl:call-template>
  </xsl:template>
  <xd:doc>
    <xd:short>[html] </xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template name="generateDivheading">
    <xsl:apply-templates mode="xref" select="."/>
  </xsl:template>
  <xd:doc>
    <xd:short>[html] </xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template name="generateDivtitle">
    <xsl:apply-templates select="tei:head/text()"/>
  </xsl:template>
  <xd:doc>
    <xd:short>[html] </xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template name="generateUpLink">
    <xsl:variable name="myName">
      <xsl:value-of select="local-name(.)"/>
    </xsl:variable>
    <xsl:variable name="BaseFile">
      <xsl:value-of select="$masterFile"/>
      <xsl:value-of select="$standardSuffix"/>
      <xsl:call-template name="addCorpusID"/>
    </xsl:variable>
    <xsl:choose>
      <xsl:when test="$myName = 'div' and not(ancestor::tei:div)">
        <xsl:call-template name="upLink">
	  <xsl:with-param name="up" select="$BaseFile"/>
	  <xsl:with-param name="title" select="$homeLabel"/>
        </xsl:call-template>
      </xsl:when>
      <xsl:when test="$myName = 'div'">
        <xsl:call-template name="upLink">
          <xsl:with-param name="up" select="ancestor::tei:div[last()]"/>
        </xsl:call-template>
      </xsl:when>
      <xsl:when test="$myName='div0'">
	<xsl:call-template name="upLink">
	  <xsl:with-param name="up" select="$BaseFile"/>
	  <xsl:with-param name="title" select="$homeLabel"/>
	</xsl:call-template>
      </xsl:when>
      <xsl:when test="$myName='div1'">
	<xsl:call-template name="upLink">
	  <xsl:with-param name="up" select="$BaseFile"/>
	  <xsl:with-param name="title" select="$homeLabel"/>
	</xsl:call-template>
      </xsl:when>
      <xsl:when test="$myName='div2'">
	<xsl:call-template name="upLink">
	  <xsl:with-param name="up" select="ancestor::tei:div1"/>
	</xsl:call-template>
      </xsl:when>
      <xsl:when test="$myName='div3'">
	<xsl:call-template name="upLink">
	  <xsl:with-param name="up" select="ancestor::tei:div2"/>
	</xsl:call-template>
      </xsl:when>
      <xsl:when test="$myName='div4'">
	<xsl:call-template name="upLink">
	  <xsl:with-param name="up" select="ancestor::tei:div3"/>
	</xsl:call-template>
      </xsl:when>
      <xsl:when test="$myName='div5'">
	<xsl:call-template name="upLink">
	  <xsl:with-param name="up" select="ancestor::tei:div4"/>
	</xsl:call-template>
      </xsl:when>
      <xsl:otherwise>
	<xsl:call-template name="upLink">
	  <xsl:with-param name="up"
			  select="(ancestor::tei:div1|ancestor::tei:div)[1]"/>
	</xsl:call-template>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  <xd:doc>
    <xd:short>[html] </xd:short>
    <xd:detail>
      <p> *****************************************</p>
    </xd:detail>
  </xd:doc>
  <xsl:template name="htmlFileBottom">
    <xsl:call-template name="topNavigation"/>
    <xsl:call-template name="stdfooter"/>
  </xsl:template>
  <xd:doc>
    <xd:short>[html] </xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template name="htmlFileTop">
    <xsl:comment>THIS FILE IS GENERATED FROM AN XML MASTER. DO NOT EDIT (6)</xsl:comment>
    <xsl:variable name="pagetitle">
      <xsl:call-template name="generateTitle"/>
    </xsl:variable>
    <head>
      <title>
        <xsl:value-of select="$pagetitle"/>
      </title>
      <xsl:call-template name="headHook"/>
      <xsl:call-template name="metaHTML">
        <xsl:with-param name="title" select="$pagetitle"/>
      </xsl:call-template>
      <xsl:call-template name="includeCSS"/>
      <xsl:call-template name="cssHook"/>
      <xsl:call-template name="includeJavascript"/>
      <xsl:call-template name="javascriptHook"/>
    </head>
  </xsl:template>
  <xd:doc>
    <xd:short>[html] </xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template name="includeCSS">
    <xsl:choose>
      <xsl:when test="$cssFile = ''"/>
      <xsl:otherwise>
	<link href="{$cssFile}" rel="stylesheet" type="text/css"/>
      </xsl:otherwise>
    </xsl:choose>
    <xsl:if test="not($cssSecondaryFile='')">
      <link href="{$cssSecondaryFile}" media="screen" rel="stylesheet"
        type="text/css"/>
    </xsl:if>
    <xsl:if test="not($cssPrintFile='')">
      <link rel="stylesheet" media="print" type="text/css" href="{$cssPrintFile}"/>
    </xsl:if>
  </xsl:template>
  <xd:doc>
    <xd:short>[html] Javascript functions to be declared in HTML header</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template name="includeJavascript">
    <xsl:text>&#10;</xsl:text>
    <xsl:call-template name="writeJavascript">
      <xsl:with-param name="content">
	<xsl:if test="$virtualPages='true'"> 
	  <xsl:text>var thisDiv; var curID = '</xsl:text>
	  <xsl:choose>
            <xsl:when test="not($ID='')">
              <xsl:value-of select="$ID"/>
            </xsl:when>
            <xsl:otherwise>
              <xsl:apply-templates mode="ident" select=".//tei:body/tei:div[1]"
              />
            </xsl:otherwise>
          </xsl:choose>
	  <xsl:text>'; 

	  function switchDiv(id) { thisDiv =
          document.getElementById(curID); thisDiv.style.display = "none";
          curID=id; thisDiv = document.getElementById(curID);
          thisDiv.style.display = "block"; } 
	  </xsl:text>
	  </xsl:if> 
	  <xsl:text>
	    function startUp() {
	  </xsl:text>
          <xsl:if test="$virtualPages='true'"> thisDiv =
          document.getElementById(curID); thisDiv.style.display = "block";
        </xsl:if> 
	<xsl:text>} 

	function openpopup(location){ 
	var newwin =
        window.open(location,"OUCSPopup","status=no,menu=no,toolbar=no,width=350,height=400,resizable=yes,scrollbars=yes")
          }
	</xsl:text>
	<!--	function clearsearch(){
	document.searchform.q.value = "";
	}
	
	function clearsearch2(){
	document.searchform2.q.value = "";
	}
	
	function expandcollapse (postid) { 
	whichpost = document.getElementById(postid); 	
	if (whichpost.className=="posthidden") { 
	  whichpost.className="postshown"; 
	 } 
	else { 
	  whichpost.className="posthidden"; 
	 } 
	} 
	
	function popUpPage(url, parameters, name)
	{
	var day = new Date();
	var pageName = name ? name : day.getTime()
	
	eval("ox"+pageName+" = window.open('"+url+"','"+pageName+"','"+parameters+"')");
	
	if (eval("ox"+pageName) &amp;&amp; window.focus) eval("ox"+pageName).focus();
	}
	-->
	<xsl:if   test="$rawXML='true'"> 
	  <xsl:text>function makeitsoyoubastard(hash){
          alert("Fragment "+hash); 
	  var as = document.all.tags("A"); for (var
          i=0; i &lt; as.length; i++){ if (as[i].name == hash)
          as[i].scrollIntoView(true); } } 
	  
	  function gotoSection(frag,section){
          var s = new ActiveXObject("MSXML2.FreeThreadedDOMDocument"); 
	  var x = document.XMLDocument; 
	  if (x == null){ x = navigator.XMLDocument; s =
          navigator.XSLDocument; }else{ s.async = false;
          s.load(document.XSLDocument.url); x.load(document.XMLDocument.url); }
          var tem = new ActiveXObject("MSXML2.XSLTemplate"); tem.stylesheet = s;
          var proc = tem.createProcessor(); proc.addParameter("ID", section);
          proc.input = x; proc.transform(); var str = proc.output; var newDoc =
          document.open("text/html", "replace"); newDoc.write(str);
          newDoc.close(); navigator.XMLDocument = x; navigator.XSLDocument = s;
          if (frag == '') {} else { makeitsoyoubastard(frag); } }
	    </xsl:text>
      </xsl:if>
    </xsl:with-param>
    </xsl:call-template>
  </xsl:template>
  <xd:doc>
    <xd:short>[html] Write out some Javascript into the HTML</xd:short>
    <xd:param name="content">The code</xd:param>
    <xd:detail>Note that it does not have to commented if the output is
    XHTML</xd:detail>
  </xd:doc>
  <xsl:template name="writeJavascript">
    <xsl:param name="content"/>
    <script type="text/javascript">
      <xsl:choose>
        <xsl:when test="$xhtml='true'">
          <xsl:value-of select="$content"/>
        </xsl:when>
        <xsl:otherwise>
          <xsl:comment>
            <xsl:value-of select="$content"/>
          </xsl:comment>
        </xsl:otherwise>
      </xsl:choose>
    </script>
  </xsl:template>
  <xd:doc>
    <xd:short>[html] Make contents of left-hand column</xd:short>
    <xd:param name="currentID">currentID</xd:param>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template name="leftHandFrame">
    <xsl:param name="currentID"/>
    <xsl:call-template name="makeSidebar"/>
    <xsl:choose>
      <xsl:when test="$currentID=''">
        <xsl:call-template name="linkListContents">
          <xsl:with-param name="style" select="'toclist'"/>
        </xsl:call-template>
      </xsl:when>
      <xsl:otherwise>
        <xsl:choose>
          <xsl:when test="count(key('IDS',$currentID))&gt;0">
            <xsl:for-each select="key('IDS',$currentID)">
              <xsl:call-template name="linkListContents">
                <xsl:with-param name="style" select="'toclist'"/>
              </xsl:call-template>
            </xsl:for-each>
          </xsl:when>
          <xsl:otherwise>
            <xsl:apply-templates mode="xpath"
              select="ancestor-or-self::tei:TEI/tei:text">
              <xsl:with-param name="xpath" select="$currentID"/>
              <xsl:with-param name="action" select="'toclist'"/>
            </xsl:apply-templates>
          </xsl:otherwise>
        </xsl:choose>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  <xd:doc>
    <xd:short>[html] bypass sidebar lists in normal mode</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template match="tei:list[@type='sidebar']"/>
  <xd:doc>
    <xd:short>[html] Summary links in left-hand column</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template name="makeSidebar">
    <xsl:for-each
      select="ancestor-or-self::tei:TEI/tei:text/tei:body/tei:list[@type='sidebar']">
      <xsl:for-each select=".//tei:xref|.//tei:ref">
        <p class="sidebar">
          <a class="toclist" href="{@url}">
            <xsl:apply-templates/>
          </a>
        </p>
      </xsl:for-each>
      <hr/>
    </xsl:for-each>
  </xsl:template>
  <xd:doc>
    <xd:short>[html] Summary table of contents in left-hand column</xd:short>
    <xd:param name="style">style</xd:param>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template name="linkListContents">
    <xsl:param name="style" select="'toc'"/>
    <xsl:variable name="BaseFile">
      <xsl:value-of select="$masterFile"/>
      <xsl:call-template name="addCorpusID"/>
    </xsl:variable>
    <xsl:variable name="thisOne">
      <xsl:value-of select="generate-id()"/>
    </xsl:variable>
    <xsl:for-each select="ancestor-or-self::tei:TEI/tei:text">
      <!-- front matter -->
      <xsl:for-each select="tei:front">
        <div class="tocFront">
          <xsl:element name="{$tocContainerElement}">
            <xsl:attribute name="class">
              <xsl:text>tocContainer</xsl:text>
            </xsl:attribute>
            <xsl:call-template name="tocSection">
              <xsl:with-param name="id" select="$thisOne"/>
              <xsl:with-param name="style" select="$style"/>
              <xsl:with-param name="force" select="$virtualPages"/>
            </xsl:call-template>
          </xsl:element>
        </div>
      </xsl:for-each>
      <!-- body matter -->
      <xsl:for-each select="tei:body">
        <div class="tocBody">
          <xsl:comment>start TOC</xsl:comment>
          <xsl:element name="{$tocContainerElement}">
            <xsl:attribute name="class">
              <xsl:text>tocContainer</xsl:text>
            </xsl:attribute>
            <xsl:comment>TOC components</xsl:comment>
            <xsl:call-template name="tocSection">
              <xsl:with-param name="id" select="$thisOne"/>
              <xsl:with-param name="style" select="$style"/>
              <xsl:with-param name="force" select="$virtualPages"/>
            </xsl:call-template>
          </xsl:element>
        </div>
      </xsl:for-each>
      <!-- back matter -->
      <xsl:for-each select="tei:back">
        <div class="tocBack">
          <xsl:element name="{$tocContainerElement}">
            <xsl:attribute name="class">
              <xsl:text>tocContainer</xsl:text>
            </xsl:attribute>
            <xsl:call-template name="tocSection">
              <xsl:with-param name="id" select="$thisOne"/>
              <xsl:with-param name="style" select="$style"/>
              <xsl:with-param name="force" select="$virtualPages"/>
            </xsl:call-template>
          </xsl:element>
        </div>
      </xsl:for-each>
    </xsl:for-each>
  </xsl:template>
  <xd:doc>
    <xd:short>[html] Main page in right-hand column</xd:short>
    <xd:param name="currentID">currentID</xd:param>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template name="mainFrame">
    <xsl:param name="currentID"/>
    <xsl:param name="minimal">false</xsl:param>
    <xsl:choose>
      <xsl:when test="$currentID='current'">
        <xsl:apply-templates/>
      </xsl:when>
      <xsl:when test="$currentID='' and $splitLevel=-1">
        <xsl:apply-templates/>
      </xsl:when>
      <xsl:when test="$currentID='' and $virtualPages='true'">
        <xsl:apply-templates/>
      </xsl:when>
      <xsl:when test="self::teiCorpus.2">
        <xsl:call-template name="corpusBody"/>
      </xsl:when>
      <xsl:when test="$currentID=''">
        <!-- we need to locate the first interesting object in the file, ie
	     the first grandchild of <text > -->
        <xsl:for-each
          select=" descendant-or-self::tei:TEI/tei:text/tei:*[1]/*[1]">
          <xsl:apply-templates mode="paging" select="."/>
          <xsl:if test="$autoToc='true'">
            <xsl:if test="following-sibling::tei:div/tei:head">
              <xsl:call-template name="contentsHeading"/>
              <ul class="toc">
                <xsl:apply-templates mode="maketoc"
                  select="following-sibling::tei:div">
                  <xsl:with-param name="forcedepth" select="'0'"/>
                </xsl:apply-templates>
              </ul>
            </xsl:if>
          </xsl:if>
        </xsl:for-each>
      </xsl:when>
      <xsl:otherwise>
        <xsl:choose>
          <xsl:when test="count(key('IDS',$currentID))&gt;0">
            <xsl:for-each select="key('IDS',$currentID)">
              <h2>
                <xsl:apply-templates mode="xref" select="."/>
		<xsl:call-template name="sectionHeadHook"/>
              </h2>
	      <xsl:if test="$topNavigationPanel='true'">
		<xsl:call-template name="xrefpanel">
		  <xsl:with-param name="homepage"
				  select="concat($masterFile,$standardSuffix)"/>
		  <xsl:with-param name="mode" select="local-name(.)"/>
		</xsl:call-template>
	      </xsl:if>
              <xsl:call-template name="doDivBody"/>
              <xsl:if test="$bottomNavigationPanel='true'">
                <xsl:call-template name="xrefpanel">
                  <xsl:with-param name="homepage"
                    select="concat($masterFile,$standardSuffix)"/>
                  <xsl:with-param name="mode" select="local-name(.)"/>
                </xsl:call-template>
              </xsl:if>
            </xsl:for-each>
          </xsl:when>
          <xsl:otherwise>
            <!-- the passed ID is a pseudo-XPath expression
		 which starts below TEI/tei:text.
		 The real XPath syntax is changed to avoid problems
	    -->
            <xsl:choose>
              <xsl:when test="ancestor-or-self::tei:TEI/tei:group/tei:text">
                <xsl:apply-templates mode="xpath"
                  select="ancestor-or-self::tei:TEI/tei:group/tei:text">
                  <xsl:with-param name="xpath" select="$currentID"/>
                </xsl:apply-templates>
              </xsl:when>
              <xsl:otherwise>
                <xsl:apply-templates mode="xpath"
                  select="ancestor-or-self::tei:TEI/tei:text">
                  <xsl:with-param name="xpath" select="$currentID"/>
                </xsl:apply-templates>
              </xsl:otherwise>
            </xsl:choose>
          </xsl:otherwise>
        </xsl:choose>
      </xsl:otherwise>
    </xsl:choose>

    <xsl:call-template name="partialFootNotes">
      <xsl:with-param name="currentID" select="$currentID"/>
    </xsl:call-template>
    <xsl:if test="$minimal='false'">
      <xsl:call-template name="stdfooter"/>
    </xsl:if>
  </xsl:template>
  <xd:doc>
    <xd:short>[html] </xd:short>
    <xd:detail>
      <p> *****************************************</p>
    </xd:detail>
  </xd:doc>
  <xsl:template name="mainbody">
    <xsl:comment> process front matter </xsl:comment>
    <xsl:apply-templates select="tei:text/tei:front"/>
    <xsl:if
      test="$autoToc='true' and (descendant::tei:div or descendant::tei:div0 or descendant::tei:div1) and not(descendant::tei:divGen[@type='toc'])">
      <h2>
        <xsl:call-template name="i18n">
          <xsl:with-param name="word">tocWords</xsl:with-param>
        </xsl:call-template>
      </h2>
      <xsl:call-template name="mainTOC"/>
    </xsl:if>
    <xsl:choose>
      <xsl:when test="tei:text/tei:group">
        <xsl:apply-templates select="tei:text/tei:group"/>
      </xsl:when>
      <xsl:otherwise>
        <xsl:comment>process body matter </xsl:comment>
        <xsl:call-template name="startHook"/>
        <xsl:call-template name="doBody"/>
      </xsl:otherwise>
    </xsl:choose>
    <xsl:comment>back matter </xsl:comment>
    <xsl:apply-templates select="tei:text/tei:back"/>
  </xsl:template>
  <xd:doc>
    <xd:short>[html] </xd:short>
    <xd:param name="force">force</xd:param>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template name="mainTOC">
    <xsl:param name="force"/>
    <xsl:if test="$tocFront">
      <xsl:for-each select="ancestor-or-self::tei:TEI/tei:text/tei:front">
        <xsl:if
          test="tei:div|tei:div0|tei:div1|tei:div2|tei:div3|tei:div4|tei:div5|tei:div6">
          <ul class="toc{$force}">
            <xsl:apply-templates mode="maketoc"
              select="tei:div|tei:div0|tei:div1|tei:div2|tei:div3|tei:div4|tei:div5|tei:div6">
              <xsl:with-param name="forcedepth" select="$force"/>
            </xsl:apply-templates>
          </ul>
        </xsl:if>
      </xsl:for-each>
    </xsl:if>
    <xsl:for-each select="ancestor-or-self::tei:TEI/tei:text/tei:body">
      <xsl:if
        test="tei:div|tei:div0|tei:div1|tei:div2|tei:div3|tei:div4|tei:div5|tei:div6">
        <ul class="toc{$force}">
          <xsl:apply-templates mode="maketoc"
            select="tei:div|tei:div0|tei:div1|tei:div2|tei:div3|tei:div4|tei:div5|tei:div6">
            <xsl:with-param name="forcedepth" select="$force"/>
          </xsl:apply-templates>
        </ul>
      </xsl:if>
    </xsl:for-each>
    <xsl:if test="$tocBack">
      <xsl:for-each select="ancestor-or-self::tei:TEI/tei:text/tei:back">
        <xsl:if
          test="tei:div|tei:div0|tei:div1|tei:div2|tei:div3|tei:div4|tei:div5|tei:div6">
          <ul class="toc{$force}">
            <xsl:apply-templates mode="maketoc"
              select="tei:div|tei:div0|tei:div1|tei:div2|tei:div3|tei:div4|tei:div5|tei:div6">
              <xsl:with-param name="forcedepth" select="$force"/>
            </xsl:apply-templates>
          </ul>
        </xsl:if>
      </xsl:for-each>
    </xsl:if>
  </xsl:template>
  <xd:doc>
    <xd:short>[html] </xd:short>
    <xd:detail>
      <p> xref to previous and last sections </p>
    </xd:detail>
  </xd:doc>
  <xsl:template name="nextLink">
    <xsl:variable name="myName">
      <xsl:value-of select="local-name(.)"/>
    </xsl:variable>
    <xsl:choose>
      <xsl:when test="following-sibling::tei:TEI">
        <xsl:apply-templates mode="generateNextLink"
          select="following-sibling::tei:TEI[1]"/>
      </xsl:when>
      <xsl:when test="following-sibling::tei:div[tei:head or $autoHead='true']">
        <xsl:apply-templates mode="generateNextLink"
          select="following-sibling::tei:div[1]"/>
      </xsl:when>
      <xsl:when
        test="parent::tei:body/following-sibling::tei:back/tei:div[tei:head or $autoHead='true']">
        <xsl:apply-templates mode="generateNextLink"
          select="parent::tei:body/following-sibling::tei:back/tei:div[1]"/>
      </xsl:when>
      <xsl:when
        test="parent::tei:front/following-sibling::tei:body/tei:div[tei:head or $autoHead='true']">
        <xsl:apply-templates mode="generateNextLink"
          select="parent::tei:front/following-sibling::tei:body/tei:div[1]"/>
      </xsl:when>
      <xsl:when
        test="$myName='div0' and following-sibling::tei:div0[tei:head or $autoHead='true']">
        <xsl:apply-templates mode="generateNextLink"
          select="following-sibling::tei:div0[1]"/>
      </xsl:when>
      <xsl:when
        test="$myName='div1' and following-sibling::tei:div1[tei:head or $autoHead='true']">
        <xsl:apply-templates mode="generateNextLink"
          select="following-sibling::tei:div1[1]"/>
      </xsl:when>
      <xsl:when
        test="$myName='div2' and following-sibling::tei:div2[tei:head or $autoHead='true']">
        <xsl:apply-templates mode="generateNextLink"
          select="following-sibling::tei:div2[1]"/>
      </xsl:when>
      <xsl:when
        test="$myName='div3' and following-sibling::tei:div3[tei:head or $autoHead='true']">
        <xsl:apply-templates mode="generateNextLink"
          select="following-sibling::tei:div3[1]"/>
      </xsl:when>
    </xsl:choose>
  </xsl:template>
  <xd:doc>
    <xd:short>[html] Generate a chunk of output</xd:short>
    <xd:param name="ident">ident</xd:param>
    <xd:param name="content">content</xd:param>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template name="outputChunk">
    <xsl:param name="ident"/>
    <xsl:param name="content"/>
    <xsl:variable name="outName">
      <xsl:choose>
        <xsl:when test="not($outputDir ='')">
          <xsl:value-of select="$outputDir"/>
          <xsl:if
            test="not(substring($outputDir,string-length($outputDir),string-length($outputDir))='/')">
            <xsl:text>/</xsl:text>
          </xsl:if>
        </xsl:when>
      </xsl:choose>
      <xsl:value-of select="$ident"/>
      <xsl:value-of select="$outputSuffix"/>
    </xsl:variable>
    <xsl:choose>
      <xsl:when test="$ident=''">
        <xsl:copy-of select="$content"/>
      </xsl:when>
      <xsl:when test="element-available('exsl:document')">
        <xsl:if test="$verbose='true'">
          <xsl:message>Opening <xsl:value-of select="$outName"/> with
            exsl:document</xsl:message>
        </xsl:if>
        <xsl:if test="element-available('exsl:document')">
          <exsl:document doctype-public="{$doctypePublic}"
            doctype-system="{$doctypeSystem}" encoding="{$outputEncoding}"
            href="{$outName}" method="{$outputMethod}">
            <xsl:copy-of select="$content"/>
          </exsl:document>
        </xsl:if>
        <xsl:if test="$verbose='true'">
          <xsl:message>Closing file <xsl:value-of select="$outName"
          /></xsl:message>
        </xsl:if>
      </xsl:when>
      <xsl:when test="contains($processor,'SAXON 7')">
        <xsl:if test="$verbose='true'">
          <xsl:message>Opening <xsl:value-of select="$outName"/> with Saxon
          8</xsl:message>
        </xsl:if>
        <saxon7:output doctype-public="{$doctypePublic}"
          doctype-system="{$doctypeSystem}" encoding="{$outputEncoding}"
          href="{$outName}" method="{$outputMethod}">
          <xsl:copy-of select="$content"/>
          <xsl:fallback>
            <xsl:copy-of select="$content"/>
          </xsl:fallback>
        </saxon7:output>
        <xsl:if test="$verbose='true'">
          <xsl:message>Closing file <xsl:value-of select="$outName"
          /></xsl:message>
        </xsl:if>
      </xsl:when>
      <xsl:when test="contains($processor,'SAXON 6')">
        <xsl:if test="$verbose='true'">
          <xsl:message>Opening <xsl:value-of select="$outName"/> with Saxon
          6</xsl:message>
        </xsl:if>
        <saxon6:output doctype-public="{$doctypePublic}"
          doctype-system="{$doctypeSystem}" encoding="{$outputEncoding}"
          href="{$outName}" method="{$outputMethod}">
          <xsl:copy-of select="$content"/>
          <xsl:fallback>
            <xsl:copy-of select="$content"/>
          </xsl:fallback>
        </saxon6:output>
        <xsl:if test="$verbose='true'">
          <xsl:message>Closing file <xsl:value-of select="$outName"
          /></xsl:message>
        </xsl:if>
      </xsl:when>
      <xsl:when test="contains($processor,'Apache')">
        <xsl:if test="$verbose='true'">
          <xsl:message>Opening <xsl:value-of select="$outName"/> with
          Xalan</xsl:message>
        </xsl:if>
        <xalan:write file="{$outName}"
          xmlns:xalan="org.apache.xalan.xslt.extensions.Redirect"
          xsl:extension-element-prefixes="xalan">
          <xsl:copy-of select="$content"/>
          <xsl:fallback>
            <xsl:copy-of select="$content"/>
          </xsl:fallback>
        </xalan:write>
        <xsl:if test="$verbose='true'">
          <xsl:message>Closing file <xsl:value-of select="$outName"
          /></xsl:message>
        </xsl:if>
      </xsl:when>
      <xsl:otherwise>
        <xsl:if test="$verbose='true'">
          <xsl:message>Creation of <xsl:value-of select="$outName"/> not
            possible with <xsl:value-of select="$processor"/></xsl:message>
        </xsl:if>
        <xsl:copy-of select="$content"/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  <xd:doc>
    <xd:short>[html] Make a new page using CSS layout </xd:short>
    <xd:param name="currentID">current ID</xd:param>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template name="pageLayoutCSS">
    <xsl:param name="currentID"/>
    <html>
      <xsl:call-template name="addLangAtt"/>
      <xsl:comment>THIS FILE IS GENERATED FROM AN XML MASTER. DO NOT EDIT (4)</xsl:comment>
      <xsl:text>&#10;</xsl:text>
      <head>
        <xsl:variable name="pagetitle">
          <xsl:choose>
            <xsl:when test="$currentID=''">
              <xsl:call-template name="generateTitle"/>
            </xsl:when>
            <xsl:otherwise><xsl:call-template name="generateTitle"/>: <xsl:choose>
                <xsl:when test="$currentID='current'">
                  <xsl:apply-templates mode="xref" select="."/>
                </xsl:when>
                <xsl:when test="count(key('IDS',$currentID))&gt;0">
                  <xsl:for-each select="key('IDS',$currentID)">
                    <xsl:apply-templates mode="xref" select="."/>
                  </xsl:for-each>
                </xsl:when>
                <xsl:otherwise>
                  <xsl:apply-templates mode="xpath" select="descendant::text">
                    <xsl:with-param name="xpath" select="$currentID"/>
                    <xsl:with-param name="action" select="'header'"/>
                  </xsl:apply-templates>
                </xsl:otherwise>
              </xsl:choose></xsl:otherwise>
          </xsl:choose>
        </xsl:variable>
        <title>
          <xsl:value-of select="$htmlTitlePrefix"/>
          <xsl:text> </xsl:text>
          <xsl:value-of select="$pagetitle"/>
        </title>
        <link href="/favicon.ico" rel="icon" type="image/x-icon"/>
        <link href="/favicon.ico" rel="shortcut icon" type="image/x-icon"/>
        <xsl:call-template name="headHook"/>
        <xsl:call-template name="metaHTML">
          <xsl:with-param name="title" select="$pagetitle"/>
        </xsl:call-template>
        <xsl:call-template name="includeCSS"/>
        <xsl:call-template name="cssHook"/>
        <xsl:call-template name="includeJavascript"/>
        <xsl:call-template name="javascriptHook"/>
      </head>
      <body>
        <xsl:attribute name="onload">
	  <xsl:choose>
	    <xsl:when test="tei:text/tei:body/@onload">
	      <xsl:value-of select="tei:text/tei:body/@onload"/>
	    </xsl:when>
	    <xsl:otherwise>
	      <xsl:text>startUp()</xsl:text>
	    </xsl:otherwise>
	  </xsl:choose>
        </xsl:attribute>
	<xsl:copy-of select="tei:text/tei:body/@onunload"/>
        <xsl:call-template name="bodyHook"/>
        <xsl:call-template name="bodyJavascriptHook"/>
	<xsl:call-template name="mainPage">
	  <xsl:with-param name="currentID">
	    <xsl:value-of select="$currentID"/>
	  </xsl:with-param>
	</xsl:call-template>
        <xsl:call-template name="bodyEndHook"/>
      </body>
    </html>
  </xsl:template>

  <xd:doc>
    <xd:short>[html] the main page structure</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template name="mainPage">
    <xsl:param name="currentID"/>
    <!-- header -->
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
    <xsl:if test="not($contentStructure='all' or @rend='all')">
      <div id="hdr3">
	<xsl:call-template name="hdr3"/>
      </div>
    </xsl:if>
    <xsl:choose>
      <xsl:when test="$contentStructure='all' or @rend='all'">
	<div class="column-wrapper">
	  <xsl:call-template name="col1"/>
	  <xsl:call-template name="col2"/>
	  <xsl:call-template name="col3"/>
	</div>
      </xsl:when>
      <xsl:when test="@rend='frontpage'">
	<div class="column-wrapper">
	  <div id="rh-col">
	    <xsl:for-each
		select="descendant-or-self::tei:TEI/tei:text/tei:body">
	      <xsl:apply-templates/>
	    </xsl:for-each>
	  </div>
	  <div id="lh-col">
	    <xsl:for-each
		select="descendant-or-self::tei:TEI/tei:text/tei:front">
	      <xsl:apply-templates/>
	    </xsl:for-each>
	  </div>
	</div>
      </xsl:when>
      <xsl:when test="$contentStructure='body'">
	<xsl:call-template name="bodyLayout">
	  <xsl:with-param name="currentID" select="$currentID"/>
	</xsl:call-template>
      </xsl:when>
    </xsl:choose>
  </xsl:template>

  <xd:doc>
    <xd:short>[html] what to do in column 1 of 3 column arrangement </xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template name="col1">
    <div id="col1">
      <xsl:for-each select="descendant-or-self::tei:TEI/tei:text/tei:front">
        <xsl:apply-templates/>
      </xsl:for-each>
    </div>
  </xsl:template>
  <xd:doc>
    <xd:short>[html] what to do in column 2 of 3 column arrangement </xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template name="col2">
    <div id="col2">
      <xsl:for-each select="descendant-or-self::tei:TEI/tei:text/tei:body">
        <xsl:apply-templates/>
      </xsl:for-each>
    </div>
  </xsl:template>
  <xd:doc>
    <xd:short>[html] what to do in column 3 of 3 column arrangement </xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template name="col3">
    <div id="col3">
      <xsl:for-each select="descendant-or-self::tei:TEI/tei:text/tei:back">
        <xsl:apply-templates/>
      </xsl:for-each>
    </div>
  </xsl:template>
  <xd:doc>
    <xd:short>[html] arrangment of page as HTML divs </xd:short>
    <xd:param name="currentID">currentID</xd:param>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template name="bodyLayout">
    <xsl:param name="currentID"/>
    <div class="column-wrapper">
      <div id="lh-col">
        <div id="lh-col-top">
          <xsl:comment>top of left-hand column</xsl:comment>
          <xsl:call-template name="lh-col-top"/>
        </div>
        <div id="lh-col-bottom">
          <xsl:comment>bottom of left-hand column</xsl:comment>
          <xsl:call-template name="lh-col-bottom">
            <xsl:with-param name="currentID" select="$currentID"/>
          </xsl:call-template>
        </div>
      </div>
      <div id="rh-col">
	<xsl:choose>
	  <xsl:when test="$xhtml='false'">
	    <a name="rh-column">
	      <xsl:comment>real top </xsl:comment>
	    </a>
	  </xsl:when>
	  <xsl:otherwise>
	    <a id="rh-column"><xsl:comment>real top </xsl:comment></a>

	  </xsl:otherwise>
	</xsl:choose>
	<div id="rh-col-top">
	  <xsl:comment>top of right-hand column</xsl:comment>
	  <xsl:call-template name="rh-col-top"/>
        </div>
        <div id="rh-col-bottom">
          <xsl:comment>bottom of right-hand column</xsl:comment>
          <xsl:call-template name="rh-col-bottom">
            <xsl:with-param name="currentID" select="$currentID"/>
          </xsl:call-template>
        </div>
      </div>
    </div>
  </xsl:template>
  <xd:doc>
    <xd:short>[html] Generate a page using simple layout </xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template name="pageLayoutSimple">
    <html>
      <xsl:call-template name="addLangAtt"/>
      <xsl:call-template name="htmlFileTop"/>
      <body class="simple" id="TOP">
        <xsl:attribute name="onload">
          <xsl:text>startUp()</xsl:text>
        </xsl:attribute>
        <xsl:call-template name="bodyHook"/>
        <xsl:call-template name="bodyJavascriptHook"/>
        <xsl:if test="not(tei:text/tei:front/tei:titlePage)">
	  <div class="stdheader">
	    <xsl:call-template name="stdheader">
	      <xsl:with-param name="title">
		<xsl:call-template name="generateTitle"/>
	      </xsl:with-param>
	    </xsl:call-template>
	  </div>
        </xsl:if>
        <xsl:call-template name="mainbody"/>
        <xsl:call-template name="printNotes"/>
        <xsl:call-template name="htmlFileBottom"/>
      </body>
    </html>
  </xsl:template>
  <xd:doc>
    <xd:short>[html] Generate a page using table layout</xd:short>
    <xd:param name="currentID">currentID</xd:param>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template name="pageLayoutTable">
    <xsl:param name="currentID"/>
    <html>
      <xsl:call-template name="addLangAtt"/>
      <xsl:comment>THIS FILE IS GENERATED FROM AN XML MASTER. DO NOT EDIT (1)</xsl:comment>
      <xsl:text>&#10;</xsl:text>
      <head>
        <xsl:variable name="pagetitle">
          <xsl:choose>
            <xsl:when test="$currentID=''">
              <xsl:call-template name="generateTitle"/>
            </xsl:when>
            <xsl:otherwise><xsl:call-template name="generateTitle"/>: <xsl:choose>
                <xsl:when test="count(key('IDS',$currentID))&gt;0">
                  <xsl:for-each select="key('IDS',$currentID)">
                    <xsl:apply-templates mode="xref" select="."/>
                  </xsl:for-each>
                </xsl:when>
                <xsl:otherwise>
                  <xsl:apply-templates mode="xpath"
                    select="descendant-or-self::tei:TEI/tei:text">
                    <xsl:with-param name="xpath" select="$currentID"/>
                    <xsl:with-param name="action" select="'header'"/>
                  </xsl:apply-templates>
                </xsl:otherwise>
              </xsl:choose></xsl:otherwise>
          </xsl:choose>
        </xsl:variable>
        <title>
          <xsl:value-of select="$htmlTitlePrefix"/>
          <xsl:value-of select="$pagetitle"/>
        </title>
        <link href="/favicon.ico" rel="icon" type="image/x-icon"/>
        <link href="/favicon.ico" rel="shortcut icon" type="image/x-icon"/>
        <xsl:call-template name="headHook"/>
        <xsl:call-template name="metaHTML">
          <xsl:with-param name="title" select="$pagetitle"/>
        </xsl:call-template>
        <xsl:call-template name="includeCSS"/>
        <xsl:call-template name="cssHook"/>
        <xsl:call-template name="includeJavascript"/>
        <xsl:call-template name="javascriptHook"/>
      </head>
      <body class="pagetable">
        <xsl:attribute name="onload">
          <xsl:text>startUp()</xsl:text>
        </xsl:attribute>
        <xsl:call-template name="bodyHook"/>
        <xsl:call-template name="bodyJavascriptHook"/>
        <xsl:call-template name="pageHeader">
          <xsl:with-param name="mode">table</xsl:with-param>
        </xsl:call-template>
        <xsl:call-template name="pageLayoutTableBody">
          <xsl:with-param name="currentID" select="$currentID"/>
        </xsl:call-template>
      </body>
    </html>
  </xsl:template>
  <xd:doc>
    <xd:short>[html] The page body, when using table layout</xd:short>
    <xd:param name="currentID">currentID</xd:param>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template name="pageLayoutTableBody">
    <xsl:param name="currentID"/>
    <table>
      <tr>
        <td class="hdr" colspan="2">
          <xsl:call-template name="hdr"/>
        </td>
      </tr>
      <tr>
        <td class="hdr2" colspan="2">
          <xsl:call-template name="hdr2"/>
        </td>
      </tr>
      <tr>
        <td class="hdr3" colspan="2">
          <xsl:call-template name="hdr3"/>
        </td>
      </tr>
      <tr>
        <td align="left" class="sidetext" rowspan="2" valign="top"
          width="{$linksWidth}">
          <xsl:call-template name="searchbox"/>
          <xsl:call-template name="leftHandFrame">
            <xsl:with-param name="currentID" select="$ID"/>
          </xsl:call-template>
          <hr/>
        </td>
      </tr>
      <tr>
        <td class="maintext" colspan="2" valign="top">
          <xsl:call-template name="mainFrame">
            <xsl:with-param name="currentID" select="$currentID"/>
          </xsl:call-template>
        </td>
      </tr>
    </table>
  </xsl:template>
  <xd:doc>
    <xd:short>[html] </xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template name="previousLink">
    <xsl:variable name="myName">
      <xsl:value-of select="local-name(.)"/>
    </xsl:variable>
    <xsl:choose>
      <xsl:when test="preceding-sibling::tei:TEI">
        <xsl:apply-templates mode="generatePreviousLink"
          select="preceding-sibling::tei:TEI[1]"/>
      </xsl:when>
      <xsl:when test="preceding-sibling::tei:div[tei:head or $autoHead='true']">
        <xsl:apply-templates mode="generatePreviousLink"
          select="preceding-sibling::tei:div[1]"/>
      </xsl:when>
      <xsl:when
        test="parent::tei:body/preceding-sibling::tei:front/tei:div[tei:head or $autoHead='true']">
        <xsl:apply-templates mode="generatePreviousLink"
          select="parent::tei:body/preceding-sibling::tei:front/tei:div[last()]"/>
      </xsl:when>
      <xsl:when
        test="parent::tei:back/preceding-sibling::tei:body/tei:div[tei:head or $autoHead='true']">
        <xsl:apply-templates mode="generatePreviousLink"
          select="parent::tei:front/preceding-sibling::tei:body/tei:div[last()]"/>
      </xsl:when>
      <xsl:when
        test="$myName='div0' and preceding-sibling::tei:div0[tei:head or $autoHead='true']">
        <xsl:apply-templates mode="generatePreviousLink"
          select="preceding-sibling::tei:div0[1]"/>
      </xsl:when>
      <xsl:when
        test="$myName='div1' and preceding-sibling::tei:div1[tei:head or $autoHead='true']">
        <xsl:apply-templates mode="generatePreviousLink"
          select="preceding-sibling::tei:div1[1]"/>
      </xsl:when>
      <xsl:when
        test="$myName='div2' and preceding-sibling::tei:div2[tei:head or $autoHead='true']">
        <xsl:apply-templates mode="generatePreviousLink"
          select="preceding-sibling::tei:div2[1]"/>
      </xsl:when>
      <xsl:when
        test="$myName='div3' and preceding-sibling::tei:div3[tei:head or $autoHead='true']">
        <xsl:apply-templates mode="generatePreviousLink"
          select="preceding-sibling::tei:div3[1]"/>
      </xsl:when>
    </xsl:choose>
  </xsl:template>
  <xsl:template name="simpleBody">
    <!-- front matter -->
    <xsl:apply-templates select="tei:text/tei:front"/>
    <xsl:if
      test="$autoToc='true' and (descendant::tei:div or descendant::tei:div0 or descendant::tei:div1) and not(descendant::tei:divGen[@type='toc'])">
      <h2>
        <xsl:call-template name="i18n">
          <xsl:with-param name="word">tocWords</xsl:with-param>
        </xsl:call-template>
      </h2>
      <xsl:call-template name="mainTOC"/>
    </xsl:if>
    <!-- main text -->
    <xsl:choose>
      <xsl:when test="tei:text/tei:group">
        <xsl:apply-templates select="tei:text/tei:group"/>
      </xsl:when>
      <xsl:otherwise>
        <xsl:apply-templates select="tei:text/tei:body"/>
      </xsl:otherwise>
    </xsl:choose>
    <!-- back matter -->
    <xsl:apply-templates select="tei:text/tei:back"/>
    <xsl:call-template name="printNotes"/>
  </xsl:template>
  <xd:doc>
    <xd:short>[html] </xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template name="stdfooter">
    <xsl:param name="style" select="'plain'"/>
    <xsl:variable name="date">
      <xsl:call-template name="generateDate"/>
    </xsl:variable>
    <xsl:variable name="author">
      <xsl:call-template name="generateAuthor"/>
    </xsl:variable>
    <div class="stdfooter">
      <hr/>
      <xsl:if test="$linkPanel='true'">
        <div class="footer">
          <xsl:if test="not($parentURL='')"><a class="{$style}"
              href="{$parentURL}" target="_top">
              <xsl:value-of select="$parentWords"/>
            </a> | </xsl:if>
          <a class="{$style}" href="{$homeURL}" target="_top">
            <xsl:value-of select="$homeWords"/>
          </a>
          <xsl:if test="$searchURL"> | <a class="{$style}" href="{$searchURL}"
              target="_top">
              <xsl:call-template name="searchWords"/>
            </a>
          </xsl:if>
          <xsl:if test="$feedbackURL"> | <a class="{$style}" href="{$feedbackURL}"
              target="_top">
              <xsl:call-template name="feedbackWords"/>
            </a>
          </xsl:if>
        </div>
        <hr/>
      </xsl:if>
      <xsl:call-template name="preAddressHook"/>
      <address>
        <xsl:if test="not($author='')"><xsl:text> </xsl:text><xsl:value-of select="$author"/>.
      </xsl:if>
        <xsl:call-template name="i18n">
          <xsl:with-param name="word">dateWord</xsl:with-param>
        </xsl:call-template>
        <xsl:text>: </xsl:text>
        <xsl:value-of select="$date"/>
        <br/>
        <xsl:call-template name="copyrightStatement"/>
        <xsl:comment><xsl:text>
	  Generated </xsl:text><xsl:if test="not($masterFile='index')"><xsl:text>from </xsl:text><xsl:value-of select="$masterFile"/></xsl:if><xsl:text> using an XSLT version </xsl:text><xsl:value-of select="system-property('xsl:version')"/> stylesheet
	  based on <xsl:value-of select="$teixslHome"/>tei.xsl
	  processed using <xsl:value-of select="system-property('xsl:vendor')"/>
	  on <xsl:call-template name="whatsTheDate"/></xsl:comment>
      </address>
    </div>
  </xsl:template>
  <xd:doc>
    <xd:short>[html] </xd:short>
    <xd:param name="style">CSS style</xd:param>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template name="stdfooterFrame">
    <xsl:param name="style" select="'plain'"/>
    <hr/>
    <xsl:variable name="BaseFile">
      <xsl:value-of select="$masterFile"/>
      <xsl:call-template name="addCorpusID"/>
    </xsl:variable>
    <xsl:if test="$linkPanel='true'">
      <div class="footer">
        <a class="{$style}" target="_top">
          <xsl:attribute name="href">
            <xsl:value-of select="concat($BaseFile,$standardSuffix)"/>
            <xsl:text>?style=printable</xsl:text>
          </xsl:attribute>
          <xsl:call-template name="singleFileLabel"/>
        </a>
      </div>
      <hr/>
      <div class="footer">
        <xsl:if test="$searchURL">
          <a class="{$style}" href="{$searchURL}" target="_top">
            <xsl:call-template name="searchWords"/>
          </a>
        </xsl:if>
        <xsl:if test="$feedbackURL">
          <br/>
          <xsl:text>&#10;</xsl:text>
          <br/>
          <xsl:text>&#10;</xsl:text>
          <a class="{$style}" href="{$feedbackURL}" target="_top">
            <xsl:call-template name="feedbackWords"/>
          </a>
        </xsl:if>
      </div>
    </xsl:if>
    <xsl:call-template name="preAddressHook"/>
    <address>
      <xsl:comment><xsl:text>
	Generated using an XSLT version </xsl:text><xsl:value-of select="system-property('xsl:version')"/> stylesheet
	based on <xsl:value-of select="$teixslHome"/>tei.xsl
	processed using: <xsl:value-of select="system-property('xsl:vendor')"/></xsl:comment>
    </address>
  </xsl:template>
  <xd:doc>
    <xd:short>[html] </xd:short>
    <xd:param name="title">title</xd:param>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template name="stdheader">
    <xsl:param name="title" select="'(no title)'"/>
    <xsl:choose>
      <xsl:when test="$pageLayout='Simple'">
        <h2 class="institution">
          <xsl:value-of select="$institution"/>
        </h2>
        <h2 class="department">
          <xsl:value-of select="$department"/>
        </h2>

	<xsl:call-template name="makeHTMLHeading">
	  <xsl:with-param name="class">maintitle</xsl:with-param>
	  <xsl:with-param name="text">
	    <xsl:value-of select="$title"/>
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
	

        <xsl:if test="$showTitleAuthor='true'">
          <xsl:if test="$verbose='true'">
            <xsl:message>displaying author and date</xsl:message>
          </xsl:if>
          <xsl:call-template name="generateAuthorList"/>
          <xsl:text> </xsl:text>
          <xsl:call-template name="generateDate"/>
        </xsl:if>
      </xsl:when>
      <xsl:otherwise>
	<xsl:call-template name="makeHTMLHeading">
	  <xsl:with-param name="class">maintitle</xsl:with-param>
	  <xsl:with-param name="text">
	    <xsl:value-of select="$title"/>
	  </xsl:with-param>
	  <xsl:with-param name="level">1</xsl:with-param>
	</xsl:call-template>

	<xsl:call-template name="makeHTMLHeading">
	  <xsl:with-param name="class">subtitle</xsl:with-param>
	  <xsl:with-param name="text">
          <xsl:call-template name="generateTitle"/>
	  </xsl:with-param>
	  <xsl:with-param name="level">2</xsl:with-param>
	</xsl:call-template>
	
        <xsl:if test="$showTitleAuthor='true'">
          <xsl:if test="$verbose='true'">
            <xsl:message>displaying author and date</xsl:message>
          </xsl:if>
          <xsl:call-template name="generateAuthorList"/>
          <xsl:text> </xsl:text>
          <xsl:call-template name="generateDate"/>
        </xsl:if>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  <xd:doc>
    <xd:short>[html] </xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template name="subtoc">
    <xsl:if
      test="child::tei:div|tei:div1|tei:div2|tei:div3|tei:div4|tei:div5|tei:div6">
      <xsl:variable name="parent">
        <xsl:choose>
          <xsl:when test="ancestor::tei:div">
            <xsl:apply-templates mode="ident" select="ancestor::tei:div[last()]"
            />
          </xsl:when>
          <xsl:otherwise>
            <xsl:apply-templates mode="ident" select="."/>
          </xsl:otherwise>
        </xsl:choose>
      </xsl:variable>
      <xsl:variable name="depth">
        <xsl:apply-templates mode="depth" select="."/>
      </xsl:variable>
      <p>
        <span class="subtochead">
          <xsl:call-template name="i18n">
            <xsl:with-param name="word">tocWords</xsl:with-param>
          </xsl:call-template>
        </span>
      </p>
      <div class="subtoc">
        <ul class="subtoc">
          <xsl:for-each
            select="tei:div|tei:div1|tei:div2|tei:div3|tei:div4|tei:div5|tei:div6">
            <xsl:variable name="innerdent">
              <xsl:apply-templates mode="generateLink" select="."/>
            </xsl:variable>
            <li class="subtoc">
              <xsl:call-template name="makeInternalLink">
                <xsl:with-param name="dest">
                  <xsl:value-of select="$innerdent"/>
                </xsl:with-param>
                <xsl:with-param name="class">
                  <xsl:value-of select="$class_subtoc"/>
                </xsl:with-param>
                <xsl:with-param name="body">
                  <xsl:call-template name="header">
		    <xsl:with-param name="display">simple</xsl:with-param>
		  </xsl:call-template>
                </xsl:with-param>
              </xsl:call-template>
            </li>
          </xsl:for-each>
        </ul>
      </div>
    </xsl:if>
  </xsl:template>
  <xd:doc>
    <xd:short>[html] </xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template name="summaryToc">
    <div class="teidiv">
      <p>Select headings on the left-hand side to see more explanation of the
        links on the right.</p>
      <table cellspacing="7">
        <thead>
          <tr>
            <th nowrap="nowrap"/>
            <th/>
          </tr>
        </thead>
        <xsl:for-each select="//tei:body/tei:div">
          <xsl:text>&#10;</xsl:text>
          <tr class="summaryline">
            <td align="right" class="summarycell" valign="top">
              <b>
                <a class="nolink" target="_top">
                  <xsl:attribute name="href">
                    <xsl:apply-templates mode="generateLink" select="."/>
                  </xsl:attribute>
                  <xsl:value-of select="tei:head"/>
                </a>
              </b>
            </td>
            <td class="link" valign="top">
              <xsl:for-each select=".//xref|.//xptr">
                <xsl:if test="position() &gt; 1">
                  <xsl:text> </xsl:text>
                  <img alt="*" src="/images/dbluball.gif"/>
                  <xsl:text> </xsl:text>
                </xsl:if>
                <span class="nowrap">
                  <xsl:apply-templates select="."/>
                </span>
              </xsl:for-each>
            </td>
          </tr>
        </xsl:for-each>
      </table>
    </div>
  </xsl:template>
  <xd:doc>
    <xd:short>[html] Make a TOC section </xd:short>
    <xd:param name="style">CSS style to use</xd:param>
    <xd:param name="id">ID to link to</xd:param>
    <xd:param name="force">whether to force a TOC entry even if other rules
      would normally prevent it</xd:param>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template name="tocSection">
    <xsl:param name="style"/>
    <xsl:param name="id"/>
    <xsl:param name="force">false</xsl:param>
    <xsl:choose>
      <xsl:when test="tei:div0">
        <xsl:for-each select="tei:div0[tei:head or $autoHead='true']">
          <xsl:call-template name="tocEntry">
            <xsl:with-param name="style" select="$style"/>
            <xsl:with-param name="id" select="$id"/>
          </xsl:call-template>
        </xsl:for-each>
      </xsl:when>
      <xsl:when test="tei:div1">
        <xsl:for-each select="tei:div1[tei:head or $autoHead='true']">
          <xsl:call-template name="tocEntry">
            <xsl:with-param name="style" select="$style"/>
            <xsl:with-param name="id" select="$id"/>
          </xsl:call-template>
        </xsl:for-each>
      </xsl:when>
      <xsl:when test="tei:div2 and ($splitLevel &gt;=1 or $force='true')">
        <xsl:for-each select="tei:div2[tei:head or $autoHead='true']">
          <xsl:call-template name="tocEntry">
            <xsl:with-param name="style" select="$style"/>
            <xsl:with-param name="id" select="$id"/>
          </xsl:call-template>
        </xsl:for-each>
      </xsl:when>
      <xsl:when test="tei:div3 and ($splitLevel &gt;=2 or $force='true')">
        <xsl:for-each select="tei:div3[tei:head or $autoHead='true']">
          <xsl:call-template name="tocEntry">
            <xsl:with-param name="style" select="$style"/>
            <xsl:with-param name="id" select="$id"/>
          </xsl:call-template>
        </xsl:for-each>
      </xsl:when>
      <xsl:when test="self::tei:div">
        <xsl:variable name="depth">
          <xsl:apply-templates mode="depth" select="."/>
        </xsl:variable>
        <xsl:if test="($splitLevel&gt;$depth  or $force='true')">
          <xsl:for-each select="tei:div[tei:head or $autoHead='true']">
            <xsl:call-template name="tocEntry">
              <xsl:with-param name="style" select="$style"/>
              <xsl:with-param name="id" select="$id"/>
            </xsl:call-template>
          </xsl:for-each>
        </xsl:if>
      </xsl:when>
      <xsl:otherwise>
        <xsl:for-each select="tei:div[tei:head or $autoHead='true']">
          <xsl:call-template name="tocEntry">
            <xsl:with-param name="style" select="$style"/>
            <xsl:with-param name="id" select="$id"/>
          </xsl:call-template>
        </xsl:for-each>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  <xd:doc>
    <xd:short>[html] Make a TOC entry </xd:short>
    <xd:param name="style">style</xd:param>
    <xd:param name="id">id</xd:param>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template name="tocEntry">
    <xsl:param name="style"/>
    <xsl:param name="id"/>
    <xsl:element name="{$tocElement}">
      <xsl:attribute name="class">
        <xsl:value-of select="$style"/>
        <xsl:apply-templates mode="depth" select="."/>
      </xsl:attribute>
      <xsl:choose>
        <xsl:when test="generate-id(.)=$id">
          <span class="toclist-this">
            <xsl:call-template name="header">
	      <xsl:with-param name="display">simple</xsl:with-param>
	    </xsl:call-template>
          </span>
        </xsl:when>
        <xsl:otherwise>
          <a>
            <xsl:attribute name="class">
              <xsl:value-of select="$style"/>
            </xsl:attribute>
            <xsl:if test="$virtualPages='true'">
              <xsl:attribute name="onclick">
                <xsl:text>switchDiv('</xsl:text>
                <xsl:apply-templates mode="ident" select="."/>
                <xsl:text>');return false</xsl:text>
              </xsl:attribute>
            </xsl:if>
            <xsl:attribute name="href">
              <xsl:apply-templates mode="generateLink" select="."/>
            </xsl:attribute>
            <xsl:call-template name="header">
	      <xsl:with-param name="display">simple</xsl:with-param>
	    </xsl:call-template>
          </a>
        </xsl:otherwise>
      </xsl:choose>
      <xsl:text>&#10;</xsl:text>
      <xsl:call-template name="tocSection">
        <xsl:with-param name="style" select="$style"/>
        <xsl:with-param name="id" select="$id"/>
        <xsl:with-param name="force">
          <xsl:if test="generate-id(.)=$id">true</xsl:if>
        </xsl:with-param>
      </xsl:call-template>
    </xsl:element>
  </xsl:template>
  <xd:doc>
    <xd:short>[html] </xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template name="topNavigation">
    <xsl:if test="ancestor::teiCorpus">
      <p class="{$alignNavigationPanel}">
        <xsl:call-template name="nextLink"/>
        <xsl:call-template name="previousLink"/>
        <xsl:call-template name="upLink">
          <xsl:with-param name="up" select="concat($masterFile,$standardSuffix)"/>
          <xsl:with-param name="title">
            <xsl:call-template name="contentsWord"/>
          </xsl:with-param>
        </xsl:call-template>
      </p>
    </xsl:if>
  </xsl:template>
  <xd:doc>
    <xd:short>[html] </xd:short>
    <xd:param name="up">the link to which "Up" goes</xd:param>
    <xd:param name="title">the text of the link</xd:param>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template name="upLink">
    <xsl:param name="up"/>
    <xsl:param name="title"/>
    <xsl:if test="$up">
      <span class="upLink"><xsl:text> </xsl:text><xsl:call-template name="i18n">
          <xsl:with-param name="word">upWord</xsl:with-param>
        </xsl:call-template>: </span>
      <a class="navigation">
        <xsl:choose>
          <xsl:when test="$title">
            <xsl:attribute name="href">
              <xsl:value-of select="$up"/>
            </xsl:attribute>
            <xsl:value-of select="$title"/>
          </xsl:when>
          <xsl:otherwise>
            <xsl:attribute name="href">
              <xsl:apply-templates mode="generateLink" select="$up"/>
            </xsl:attribute>
            <xsl:for-each select="$up">
              <xsl:call-template name="headerLink">
                <xsl:with-param name="minimal" select="$minimalCrossRef"/>
              </xsl:call-template>
            </xsl:for-each>
          </xsl:otherwise>
        </xsl:choose>
      </a>
    </xsl:if>
  </xsl:template>
  <xd:doc>
    <xd:short>[html] </xd:short>
    <xd:param name="path">path</xd:param>
    <xd:param name="class">class</xd:param>
    <xd:param name="whole">whole</xd:param>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template name="walkTree">
    <xsl:param name="path"/>
    <xsl:param name="class"/>
    <xsl:param name="whole" select="''"/>
    <xsl:choose>
      <xsl:when test="contains($path,'/')">
        <xsl:variable name="current">
          <xsl:value-of select="substring-before($path,'/')"/>
        </xsl:variable>
        <xsl:variable name="rest">
          <xsl:value-of select="substring-after($path,'/')"/>
        </xsl:variable>
        <xsl:call-template name="aCrumb">
          <xsl:with-param name="crumbBody">
            <xsl:choose>
              <xsl:when test="$rest='index.xsp' and $ID=''">
                <xsl:value-of select="$current"/>
              </xsl:when>
              <xsl:when test="$rest='index.xml' and $ID=''">
                <xsl:value-of select="$current"/>
              </xsl:when>
              <xsl:otherwise>
                <a class="{$class}" target="_top">
                  <xsl:attribute name="href"><xsl:value-of select="$whole"
                      />/<xsl:value-of select="$current"/><xsl:text>/</xsl:text></xsl:attribute>
                  <xsl:value-of select="$current"/>
                </a>
              </xsl:otherwise>
            </xsl:choose>
          </xsl:with-param>
        </xsl:call-template>
        <xsl:call-template name="walkTree">
          <xsl:with-param name="class">
            <xsl:value-of select="$class"/>
          </xsl:with-param>
          <xsl:with-param name="path" select="$rest"/>
          <xsl:with-param name="whole"><xsl:value-of select="$whole"
              />/<xsl:value-of select="$current"/></xsl:with-param>
        </xsl:call-template>
      </xsl:when>
      <xsl:otherwise>
        <xsl:if test="not($path='index.xsp' or $path='index.xml')">
          <xsl:call-template name="aCrumb">
            <xsl:with-param name="crumbBody">
              <a class="{$class}" target="_top">
                <xsl:attribute name="href"><xsl:value-of select="$whole"
                    />/<xsl:value-of select="$path"/></xsl:attribute>
                <xsl:value-of select="$path"/>
              </a>
            </xsl:with-param>
          </xsl:call-template>
        </xsl:if>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  <xd:doc>
    <xd:short>[html] </xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template name="writeDiv">
    <xsl:variable name="BaseFile">
      <xsl:value-of select="$masterFile"/>
      <xsl:call-template name="addCorpusID"/>
    </xsl:variable>
    <html>
      <xsl:call-template name="addLangAtt"/>
      <xsl:comment>THIS IS A GENERATED FILE. DO NOT EDIT (2)</xsl:comment>
      <head>
        <xsl:variable name="pagetitle">
          <xsl:call-template name="generateDivtitle"/>
        </xsl:variable>
        <title>
          <xsl:value-of select="$pagetitle"/>
        </title>
        <xsl:call-template name="headHook"/>
        <xsl:call-template name="metaHTML">
          <xsl:with-param name="title" select="$pagetitle"/>
        </xsl:call-template>
        <xsl:call-template name="includeCSS"/>
        <xsl:call-template name="cssHook"/>
        <xsl:call-template name="includeJavascript"/>
        <xsl:call-template name="javascriptHook"/>
      </head>
      <body id="TOP">
        <xsl:attribute name="onload">
          <xsl:text>startUp()</xsl:text>
        </xsl:attribute>
        <xsl:call-template name="bodyHook"/>
        <xsl:call-template name="bodyJavascriptHook"/>
        <div class="teidiv">
          <xsl:call-template name="divClassAttribute">
            <xsl:with-param name="depth">0</xsl:with-param>
          </xsl:call-template>
          <xsl:call-template name="stdheader">
            <xsl:with-param name="title">
              <xsl:call-template name="generateDivheading"/>
            </xsl:with-param>
          </xsl:call-template>
          <xsl:if test="$topNavigationPanel='true'">
            <xsl:call-template name="xrefpanel">
              <xsl:with-param name="homepage"
                select="concat($BaseFile,$standardSuffix)"/>
              <xsl:with-param name="mode" select="local-name(.)"/>
            </xsl:call-template>
          </xsl:if>
          <xsl:if test="$subTocDepth &gt;= 0">
            <xsl:call-template name="subtoc"/>
          </xsl:if>
          <xsl:call-template name="startHook"/>
          <xsl:call-template name="doDivBody"/>
          <xsl:call-template name="printNotes"/>
          <xsl:if test="$bottomNavigationPanel='true'">
            <xsl:call-template name="xrefpanel">
              <xsl:with-param name="homepage"
                select="concat($BaseFile,$standardSuffix)"/>
              <xsl:with-param name="mode" select="local-name(.)"/>
            </xsl:call-template>
          </xsl:if>
          <xsl:call-template name="stdfooter"/>
        </div>
      </body>
    </html>
  </xsl:template>
  <xd:doc>
    <xd:short>[html]provide a class attribute and/or ID for each div </xd:short>
    <xd:detail> </xd:detail>
    <xd:param name="depth">depth of section (1, 2 3, 4 etc)</xd:param>
  </xd:doc>
  <xsl:template name="divClassAttribute">
    <xsl:param name="depth"/>
    <xsl:choose>
      <xsl:when test="@type">
        <xsl:attribute name="class">
          <xsl:value-of select="@type"/>
        </xsl:attribute>
      </xsl:when>
      <xsl:otherwise>
        <xsl:attribute name="class">
          <xsl:text>teidiv</xsl:text>
          <xsl:value-of select="$depth"/>
        </xsl:attribute>
      </xsl:otherwise>
    </xsl:choose>
    <xsl:variable name="ident">
      <xsl:apply-templates mode="ident" select="."/>
    </xsl:variable>
    <xsl:attribute name="id">
      <xsl:value-of select="$ident"/>
    </xsl:attribute>
  </xsl:template>
  <xd:doc>
    <xd:short>[html] </xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template name="writeFrameToc">
    <html>
      <xsl:call-template name="addLangAtt"/>
      <xsl:comment>THIS FILE IS GENERATED FROM AN XML MASTER. DO NOT EDIT (3)</xsl:comment>
      <head>
        <title>
          <xsl:call-template name="generateTitle"/>
        </title>
        <xsl:call-template name="includeCSS"/>
        <xsl:call-template name="cssHook"/>
        <base target="framemain"/>
      </head>
      <body class="framemenu">
        <xsl:call-template name="logoPicture"/>
        <br/>
        <xsl:text>&#10;</xsl:text>
        <xsl:call-template name="linkListContents">
          <xsl:with-param name="style" select="'toclist'"/>
        </xsl:call-template>
        <xsl:call-template name="stdfooterFrame"/>
      </body>
    </html>
  </xsl:template>
  <xd:doc>
    <xd:short>[html] </xd:short>
    <xd:param name="homepage">homepage</xd:param>
    <xd:param name="mode">mode</xd:param>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template name="xrefpanel">
    <xsl:param name="homepage"/>
    <xsl:param name="mode"/>
    <p class="{$alignNavigationPanel}">
      <xsl:variable name="Parent">
        <xsl:call-template name="locateParent"/>
        <xsl:value-of select="$standardSuffix"/>
      </xsl:variable>
      <xsl:choose>
        <xsl:when test="$Parent = $standardSuffix">
          <xsl:call-template name="upLink">
            <xsl:with-param name="up" select="$homepage"/>
            <xsl:with-param name="title">
              <xsl:call-template name="contentsWord"/>
            </xsl:with-param>
          </xsl:call-template>
        </xsl:when>
        <xsl:otherwise>
          <xsl:call-template name="generateUpLink"/>
        </xsl:otherwise>
      </xsl:choose>
      <xsl:if test="not(ancestor-or-self::tei:TEI[@rend='nomenu'])">
        <xsl:call-template name="previousLink"/>
        <xsl:call-template name="nextLink"/>
      </xsl:if>
    </p>
  </xsl:template>
</xsl:stylesheet>
