<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet 
    xmlns:xd="http://www.pnp-software.com/XSLTdoc" 
    xmlns:tei="http://www.tei-c.org/ns/1.0" 
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform" 
    exclude-result-prefixes="tei xd" 
    version="1.0">
  <xd:doc type="stylesheet">
    <xd:short>
    TEI stylesheet customization module for HTML output.</xd:short>
    <xd:detail>
    This library is free software; you can redistribute it and/or
    modify it under the terms of the GNU Lesser General Public
    License as published by the Free Software Foundation; either
    version 2.1 of the License, or (at your option) any later version.

    This library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    Lesser General Public License for more details.

    You should have received a copy of the GNU Lesser General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

   
   
      </xd:detail>
    <xd:author>See AUTHORS</xd:author>
    <xd:cvsId>$Id$</xd:cvsId>
    <xd:copyright>2008, TEI Consortium</xd:copyright>
  </xd:doc>
  <xd:doc type="string" class="CSS">
CSS class for links derived from &lt;ptr&gt;
</xd:doc>
  <xsl:param name="class_ptr">ptr</xsl:param>
  <xd:doc type="string" class="CSS">
CSS class for links derived from &lt;ref&gt;
</xd:doc>
  <xsl:param name="class_ref">ref</xsl:param>
  <xd:doc type="string" class="CSS">
CSS class for links derived from &lt;xptr&gt;
</xd:doc>
  <xsl:param name="class_xptr">xptr</xsl:param>
  <xd:doc type="string" class="CSS">
CSS class for links derived from &lt;xref&gt;
</xd:doc>
  <xsl:param name="class_xref">xref</xsl:param>

  <xd:doc type="boolean" class="layout">
Number footnotes consecutively
</xd:doc>
  <xsl:param name="consecutiveFNs">false</xsl:param>

  <xd:doc type="boolean" class="layout">
Link back from footnotes to reference
</xd:doc>
  <xsl:param name="footnoteBackLink">false</xsl:param>

  <xd:doc type="boolean" class="cssFileInclude">
Whether to include CSS by reference or by XInclusion
</xd:doc>
  <xsl:param name="cssFileInclude">false</xsl:param>

  <xd:doc type="anyURI" class="CSS">
CSS style file to be associated with output file(s)
</xd:doc>
  <xsl:param name="cssFile">http://www.tei-c.org/release/xml/tei/stylesheet/tei.css</xsl:param>
  <xd:doc type="anyURI" class="CSS">
CSS style file for print; this will be given a media=print attribute.
    </xd:doc>
  <xsl:param name="cssPrintFile">http://www.tei-c.org/release/xml/tei/stylesheet/tei-print.css</xsl:param>
  <xd:doc type="anyURI" class="CSS">
Secondary CSS style file; this will be given a media=screen attribute,
so that it does not affect printing. It should be used for screen layout.
  </xd:doc>
    <xsl:param name="cssSecondaryFile"/>
  <xd:doc type="integer" class="figures">
Resolution of images. This is needed to calculate
HTML width and height (in pixels) from supplied dimensions.
</xd:doc>
  <xsl:param name="dpi">96</xsl:param>
  <xd:doc type="boolean" class="figures">
Display figures.
</xd:doc>
  <xsl:param name="showFigures">true</xsl:param>
  <xd:doc class="hook">
    <xd:short>[html] Hook where HTML can be inserted just after &lt;body&gt;</xd:short>
    <xd:detail>  </xd:detail>
  </xd:doc>
  <xsl:template name="bodyHook"/>
  <xd:doc class="hook">
    <xd:short>[html] Hook where HTML can be inserted just before the
    &lt;body&gt; ends.</xd:short>
    <xd:detail>This can be used to add a page-wide footer block.</xd:detail>
  </xd:doc>
  <xsl:template name="bodyEndHook"/>
  <xd:doc class="hook">
    <xd:short>[html] Hook where Javascript calls can be inserted  just after &lt;body&gt;</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template name="bodyJavascriptHook"/>
  <xd:doc class="hook">
    <xd:short>[html] Hook where extra CSS can be inserted</xd:short>
    <xd:detail>  </xd:detail>
  </xd:doc>
  <xsl:template name="cssHook"/>
  <xd:doc class="hook">
    <xd:short>[html] Hook where code can be added to the HTML &lt;head&gt;.</xd:short>
    <xd:detail>This would be used insert &lt;meta&gt; tags.</xd:detail>
  </xd:doc>
  <xsl:template name="headHook"/>
  <xd:doc class="hook">
    <xd:short>[html] Hook where HTML can be inserted when creating an &lt;img&gt;</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template name="imgHook"/>
  <xd:doc class="hook">
    <xd:short>[html] Hook where HTML can be inserted when processing a
    figure</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template name="figureHook"/>
  <xd:doc class="hook">
    <xd:short>[html] Hook where extra Javascript functions can be defined</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template name="javascriptHook"/>
  <xd:doc class="hook">
    <xd:short>[html] Hook where HTML can be inserted just before the &lt;address&gt;</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template name="preAddressHook"/>
  <xd:doc class="hook">
    <xd:short>[html] Hook where HTML can be inserted at the start of
    processing each section</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template name="startDivHook"/>
  <xd:doc class="hook">
    <xd:short>[html] Hook where HTML can be inserted at the beginning
    of the main text, after the header</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template name="startHook"/>
  <xd:doc class="hook">
    <xd:short>[html] Hook where HTML can be inserted after processing &lt;TEI&gt;</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template name="teiEndHook"/>
  <xd:doc class="hook">
    <xd:short>[html] Hook where HTML can be inserted before processing &lt;TEI&gt;</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template name="teiStartHook"/>
  <xd:doc class="hook">
    <xd:short>[html] Hook where HTML can be inserted when creating an
    &lt;a&gt; element</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template name="xrefHook"/>

  <xd:doc class="i18n">
    <xd:short>[html] Make a copyright claim</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template name="copyrightStatement">This page is copyrighted</xsl:template>
  <xd:doc class="layout">
    <xd:short>[html] Banner for top of column</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template name="columnHeader">
</xsl:template>
  <xd:doc type="string" class="layout">
    <xd:short>How to use the front/body/back matter in creating
columns.</xd:short>
    <xd:detail>
The choice is between
<dl><dt><b>all</b></dt><dd>use &lt;front&gt; for left-hand column,
use &lt;body&gt; for centre column, and use &lt;back&gt; for right-hand column</dd><dt><b>body</b></dt><dd>use &lt;body&gt; for right-hand column,
 generate left-hand with a TOC or whatever</dd></dl>
</xd:detail>
  </xd:doc>
  <xsl:param name="contentStructure">body</xsl:param>
  <xd:doc type="integer" class="layout">
    <xd:short>The difference between TEI div levels and HTML.
headings.</xd:short>
    <xd:detail>TEI &lt;div&gt;s are implicitly or explicitly numbered from 0
upwards; this offset is added to that number to produce an HTML
&lt;Hn&gt; element. So a value of 2 here means that a &lt;div0&gt;
will generate an &lt;h2&gt;</xd:detail>
  </xd:doc>
  <xsl:param name="divOffset">2</xsl:param>
  <xd:doc type="boolean" class="layout">
Make a separate file for footnotes
</xd:doc>
  <xsl:param name="footnoteFile">false</xsl:param>
  <xd:doc class="layout">
    <xd:short>[html] Header section across top of page </xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template name="hdr">
    <xsl:call-template name="pageHeader">
      <xsl:with-param name="mode"/>
    </xsl:call-template>
  </xsl:template>
  <xd:doc class="layout">
    <xd:short>[html] Navigation bar </xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template name="hdr2">
    <xsl:call-template name="navbar"/>
  </xsl:template>
  <xd:doc class="layout">
    <xd:short>[html] Text or action to take at the start of the
    breadcrumb trail </xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template name="preBreadCrumbPath"/>
  <xd:doc class="layout">
    <xd:short>[html] Breadcrumb trail </xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template name="hdr3">
    <a href="#rh-column" title="Go to main page content" class="skiplinks">Skip links</a>
    <a class="hide">|</a>
    <xsl:call-template name="crumbPath"/>
    <a class="hide">|</a>
    <a class="bannerright" href="{$parentURL}" title="Go to home page">
      <xsl:value-of select="$parentWords"/>
    </a>
  </xsl:template>
  <xd:doc class="layout">
    <xd:short>[html]Bottom of left-hand column</xd:short>
    <xd:param name="currentID">ID of selected section</xd:param>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template name="lh-col-bottom">
    <xsl:param name="currentID"/>
    <xsl:call-template name="leftHandFrame">
      <xsl:with-param name="currentID" select="$currentID"/>
    </xsl:call-template>
  </xsl:template>
  <xd:doc class="layout">
    <xd:short>[html]Top of left-hand column </xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template name="lh-col-top">
    <xsl:call-template name="searchbox"/>
    <xsl:call-template name="printLink"/>
  </xsl:template>
  <xd:doc type="string" class="layout">
Width of left-hand column when $pageLayout is "Table"
</xd:doc>
  <xsl:param name="linksWidth">15%</xsl:param>
  <xd:doc class="layout">
    <xd:short>[html] Logo</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template name="logoPicture">
    <a class="framelogo" href="http://www.tei-c.org/Stylesheets/teic/">
      <img src="http://www.tei-c.org/Stylesheets/teic/teixsl.png"
	   vspace="5" width="124" height="161" border="0" 
	   alt="created by TEI XSL Stylesheets"/>
    </a>
  </xsl:template>
  <xd:doc class="layout">
    <xd:short>[html] Making elements in HTML &lt;head&gt;</xd:short>
    <xd:param name="title">The text used to create the DC.Title field
    in the HTML header</xd:param>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template name="metaHTML">
    <xsl:param name="title"/>
    <meta name="author">
      <xsl:attribute name="content">
        <xsl:call-template name="generateAuthor"/>
      </xsl:attribute>
    </meta>
    <meta name="generator" content="Text Encoding Initiative Consortium XSLT stylesheets"/>
    <meta name="DC.Title" content="{$title}"/>
    <meta name="DC.Type" content="Text"/>
    <meta name="DC.Format" content="text/html"/>
    <meta http-equiv="Content-Type" 
      content="text/html; charset={$outputEncoding}"/>
  </xsl:template>
  <xd:doc class="layout">
    <xd:short>[html] Construction of navigation bar </xd:short>
    <xd:detail>A file is looked for relative to the <i>stylesheet</i> (the
    second parameter of the document function), which is expected to
    contain a TEI &lt;list&gt; where each &lt;item&gt; has an embedded
    &lt;xref&gt;</xd:detail>
  </xd:doc>
  <xsl:template name="navbar">
    <xsl:if test="not($navbarFile='')">
      <xsl:for-each select="document($navbarFile,document(''))">
        <xsl:for-each select="tei:list/tei:item">
          <span class="navbar">
            <a href="{$URLPREFIX}{tei:xref/@url}" class="navbar">
              <xsl:apply-templates select="tei:xref/text()"/>
            </a>
          </span>
          <xsl:if test="following-sibling::tei:item"> | </xsl:if>
        </xsl:for-each>
      </xsl:for-each>
    </xsl:if>
  </xsl:template>
  <xd:doc type="anyURI" class="layout">
    <xd:short>XML resource defining a navigation bar.</xd:short>
    <xd:detail>The XML should provide a &lt;list&gt; containing a series
of &lt;item&gt; elements, each containing an &lt;xref&gt; link.</xd:detail>
  </xd:doc>
  <xsl:param name="navbarFile"/>
  <xd:doc class="layout">
    <xd:short>[html] Banner for top of page</xd:short>
    <xd:param name="mode">layout mode</xd:param>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template name="pageHeader">
    <xsl:param name="mode"/>
    <xsl:choose>
      <xsl:when test="$mode='table'">
        <table width="100%" border="0">
          <tr>
            <td height="98" class="bgimage"
		onclick="window.location='{$homeURL}'"
		cellpadding="0">

	      <xsl:call-template name="makeHTMLHeading">
		<xsl:with-param name="class">subtitle</xsl:with-param>
		<xsl:with-param name="text">
		  <xsl:call-template name="generateSubTitle"/>
		</xsl:with-param>
		<xsl:with-param name="level">2</xsl:with-param>
	      </xsl:call-template>

	      <xsl:call-template name="makeHTMLHeading">
		<xsl:with-param name="class">title</xsl:with-param>
		<xsl:with-param name="text">
		  <xsl:call-template name="generateTitle"/>
		</xsl:with-param>
		<xsl:with-param name="level">1</xsl:with-param>
	      </xsl:call-template>

            </td>
            <td valign="top"/>
          </tr>
        </table>
      </xsl:when>
      <xsl:otherwise>
	<xsl:call-template name="makeHTMLHeading">
	  <xsl:with-param name="class">subtitle</xsl:with-param>
	  <xsl:with-param name="text">
	    <xsl:call-template name="generateSubTitle"/>
	  </xsl:with-param>
	  <xsl:with-param name="level">2</xsl:with-param>
	</xsl:call-template>
	
	<xsl:call-template name="makeHTMLHeading">
	  <xsl:with-param name="class">title</xsl:with-param>
	  <xsl:with-param name="text">
	    <xsl:call-template name="generateTitle"/>
	  </xsl:with-param>
	  <xsl:with-param name="level">1</xsl:with-param>
	</xsl:call-template>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>

  <xd:doc>
    <xd:short>[html] Make a heading, if there some text to display</xd:short>
    <xd:param name="text">Heading title</xd:param>
    <xd:param name="class">CSS class</xd:param>
    <xd:param name="level">Heading level</xd:param>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template name="makeHTMLHeading">
    <xsl:param name="text"/>
    <xsl:param name="class">title</xsl:param>
    <xsl:param name="level">1</xsl:param>
    <xsl:if test="not($text='')">
      <xsl:element name="h{$level}">
	<xsl:attribute name="class">
	  <xsl:value-of select="$class"/>
	</xsl:attribute>
	<xsl:copy-of select="$text"/>
      </xsl:element>
    </xsl:if>
  </xsl:template>

  <xd:doc>
    <xd:short>[html] Make a link saying how to get printable version
    of file</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template name="printLink"/>
  <xd:doc class="layout">
    <xd:short>[html] Bottom of right-hand column</xd:short>
    <xd:param name="currentID">ID of selected section</xd:param>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template name="rh-col-bottom">
    <xsl:param name="currentID"/>
    <xsl:call-template name="mainFrame">
      <xsl:with-param name="currentID" select="$currentID"/>
    </xsl:call-template>
  </xsl:template>
  <xd:doc class="layout">
    <xd:short>[html] Top of right-hand column</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template name="rh-col-top">
    <xsl:call-template name="columnHeader"/>
  </xsl:template>
  <xd:doc class="layout">
    <xd:short>[html] Make a search box</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template name="searchbox"/>
  <xd:doc class="layout">
    <xd:short>[html] Construct a label for the link which makes a
    printable version of the document.</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template name="singleFileLabel">For Printing</xsl:template>
  <xd:doc type="string" class="links">
How to align the navigation panel at the bottom of the page
</xd:doc>
  <xsl:param name="alignNavigationPanel">right</xsl:param>
  <xd:doc type="boolean" class="links">
Display navigation panel at bottom of pages
</xd:doc>
  <xsl:param name="bottomNavigationPanel">true</xsl:param>
  <xd:doc type="anyURI" class="links">
Link for feedback
</xd:doc>
  <xsl:param name="feedbackURL">mailto:feedback</xsl:param>
  <xd:doc type="string" class="links">
Fixed string to insert before normal page title in HTML meta
&lt;title&gt; element
</xd:doc>
  <xsl:param name="htmlTitlePrefix"/>
  <xd:doc type="boolean" class="links">
Make a panel with next page/previous page links.
</xd:doc>
  <xsl:param name="linkPanel">true</xsl:param>
  <xd:doc type="boolean" class="misc">
Generate a unique ID for all paragraphs
</xd:doc>
  <xsl:param name="generateParagraphIDs">true</xsl:param>
  <xd:doc type="string" class="misc">
    <xd:short>Character separating values in a rend attribute.</xd:short>
    <xd:detail>Some projects use multiple values in <tt>rend</tt>
attributes. These are handled, but the separator character must
be specified.</xd:detail>
  </xd:doc>
  <xsl:param name="rendSeparator">;</xsl:param>
  <xd:doc type="boolean" class="misc">
Show a title and author at start of document
</xd:doc>
  <xsl:param name="showTitleAuthor">false</xsl:param>
  <xd:doc type="boolean" class="misc">
Be talkative while working.
</xd:doc>

  <xsl:param name="verbose">false</xsl:param>
  <xd:doc type="boolean" class="layout">
    Make all notes into endnotes
  </xd:doc>
  <xsl:param name="autoEndNotes">false</xsl:param>


  <xd:doc type="string" class="output">
An ID passed to the stylesheet to indicate which section to display
</xd:doc>
  <xsl:param name="ID"/>

  <xd:doc type="string" class="output">
A wrapper around the ID, to allow for other ways of getting it
</xd:doc>
  <xsl:param name="requestedID">
    <xsl:value-of select="$ID"/>
  </xsl:param>

  <xd:doc type="string" class="output">
A path fragment to put before all internal URLs 
</xd:doc>
  <xsl:param name="URLPREFIX"/>
  <xd:doc type="string" class="output">
The name of the output file
</xd:doc>
  <xsl:param name="outputName"/>
  <xd:doc type="string" class="output">
Directory in which to place generated files.
</xd:doc>
  <xsl:param name="outputDir"/>
  <xd:doc type="string" class="output">
Encoding of output file(s).
</xd:doc>
  <xsl:param name="outputEncoding">iso-8859-1</xsl:param>
  <xd:doc type="string" class="output">
Output method for output file(s).
</xd:doc>
  <xsl:param name="outputMethod">html</xsl:param>
  <xd:doc type="string" class="output">
Suffix of output file(s).
</xd:doc>
  <xsl:param name="outputSuffix">.html</xsl:param>
  <xd:doc type="string" class="output">
Public Doctype of output file(s).
</xd:doc>
  <xsl:param name="doctypePublic">-//W3C//DTD HTML 4.0 Transitional//EN</xsl:param>
  <xd:doc type="string" class="output">
System Doctype of output file(s).
</xd:doc>
  <xsl:param name="doctypeSystem">http://www.w3.org/TR/html4/loose.dtd</xsl:param>
  <xd:doc type="string" class="output">
    <xd:short>The style of HTML (Simple, CSS or Table) which creates the layout for generated pages.</xd:short>
    <xd:detail>The choice is between
<dl><dt><b>Simple</b></dt><dd>A linear presentation is created</dd><dt><b>CSS</b></dt><dd>The page is created as a series of nested
 &lt;div&gt;s which can be arranged using CSS into a multicolumn layout</dd><dt><b>Table</b></dt><dd>The page is created as an HTML table</dd></dl>
</xd:detail>
  </xd:doc>
  <xsl:param name="pageLayout">Simple</xsl:param>
  <xd:doc type="boolean" class="output">
Pass through input essentially unchanged
</xd:doc>
  <xsl:param name="rawXML">false</xsl:param>
  <xd:doc type="boolean" class="output">
Break back matter into separate HTML pages (if splitting enabled).
</xd:doc>
  <xsl:param name="splitBackmatter">true</xsl:param>
  <xd:doc type="boolean" class="output">
Break front matter into separate HTML pages (if splitting enabled).
</xd:doc>
  <xsl:param name="splitFrontmatter">true</xsl:param>
  <xd:doc type="integer" class="output">
    <xd:short>Level at which to split sections.</xd:short>
    <xd:detail>When processing a &lt;div&gt; or &lt;div[0-5]&gt;, compare
the nesting depth and see whether to start a new HTML page. Since the
TEI starts with &lt;div0&gt;, setting this parameter to 0 will cause
top-level sections to be split apart. The default is not to split at
all.
</xd:detail>
  </xd:doc>
  <xsl:param name="splitLevel">-1</xsl:param>
  <xd:doc type="string" class="output">
Suffix for generated output files.
</xd:doc>
  <xsl:param name="standardSuffix">
    <xsl:choose>
      <xsl:when test="$rawXML='true'">.xml</xsl:when>
      <xsl:when test="tei:teiCorpus">.html</xsl:when>
      <xsl:when test="$STDOUT='true'"/>
      <xsl:otherwise>
	<xsl:value-of select="$outputSuffix"/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:param>
  <xd:doc type="boolean" class="output">
Display navigation panel at top of pages.
</xd:doc>
  <xsl:param name="topNavigationPanel">true</xsl:param>
  <xd:doc type="string" class="output">
How to specify infra-document links. When a document is split,
links need to be constructed between parts of the document. 
The default is to use a query parameter on the URL.
</xd:doc>
  <xsl:param name="urlChunkPrefix">?ID=</xsl:param>
  <xd:doc type="boolean" class="output">
    <xd:short>Construct links using existing ID values.</xd:short>
    <xd:detail>It is often nice if, when making separate files, their names
correspond to the ID attribute of the &gt;div&lt;. Alternatively, you
	  can let the system choose names.</xd:detail>
  </xd:doc>
  <xsl:param name="useIDs">true</xsl:param>
  <xd:doc type="string" class="style">
HTML element to put around visible text of display URLs
</xd:doc>
  <xsl:param name="urlMarkup">span</xsl:param>
  <xd:doc type="boolean" class="output">
Whether to make split pages appear virtually
as layers in HTML, or physically as separate
pages or server requests.
</xd:doc>
  <xsl:param name="virtualPages">false</xsl:param>
  <xd:doc type="boolean" class="output">
Make XHTML-compatible markup
</xd:doc>
  <xsl:param name="xhtml">false</xsl:param>
  <xd:doc type="boolean" class="toc">
Make an automatic table of contents
</xd:doc>
  <xsl:param name="autoToc">true</xsl:param>
  <xd:doc type="string" class="toc">
CSS class for second-level TOC entries
</xd:doc>
  <xsl:param name="class_subtoc">subtoc</xsl:param>
  <xd:doc type="integer" class="toc">
    <xd:short>Depth at which to stop doing a recursive table of
contents.</xd:short>
    <xd:detail>You can have a mini table of contents at the start
of each section. The default is only to construct a TOC at the
top level; a value of -1 here means no subtoc at all. </xd:detail>
  </xd:doc>
  <xsl:param name="subTocDepth">-1</xsl:param>
  <xd:doc type="boolean" class="toc">
Include the back matter in the table of contents.
</xd:doc>
  <xsl:param name="tocBack">true</xsl:param>
  <xd:doc type="string" class="toc">
Depth to which table of contents is constructed.
</xd:doc>
  <xsl:param name="tocDepth">5</xsl:param>
  <xd:doc type="boolean" class="toc">
Include the front matter in the table of contents.
</xd:doc>
  <xsl:param name="tocFront">true</xsl:param>
  <xd:doc type="string" class="toc">
Which HTML element to wrap each TOCs entry in.
</xd:doc>
  <xsl:param name="tocElement">p</xsl:param>
  <xd:doc type="string" class="toc">
Which HTML element to wrap each TOC sections in.
</xd:doc>
  <xsl:param name="tocContainerElement">div</xsl:param>


  <xd:doc type="string" class="toc">
Text to link back to from foot of ODD reference pages
  </xd:doc>
<xsl:param name="refDocFooterText">TEI Guidelines</xsl:param>

  <xd:doc type="anyURI" class="toc">
URL to link back to from foot of ODD reference pages
  </xd:doc>
<xsl:param name="refDocFooterURL">index.html</xsl:param>



<xsl:template name="navInterSep">
  <xsl:text>: </xsl:text>
</xsl:template>

  <xd:doc class="hook">
    <xd:short>[html] Hooks where HTML can be inserted when processing
    &lt;egXML&gt; element</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
<xsl:template name="egXMLStartHook"/>
<xsl:template name="egXMLEndHook"/>

<xsl:template name="emphasize">
  <xsl:param name="class"/>
  <xsl:param name="content"/>
  <xsl:choose>
    <xsl:when test="$class='titlea'">
      <q class="titlea">
	<xsl:copy-of select="$content"/>
      </q>
    </xsl:when>
    <xsl:otherwise>
      <span class="{$class}">
	<xsl:copy-of select="$content"/>
      </span>
    </xsl:otherwise>
  </xsl:choose>
</xsl:template>


</xsl:stylesheet>
