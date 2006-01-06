<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet xmlns:xd="http://www.pnp-software.com/XSLTdoc" xmlns:m="http://www.w3.org/1998/Math/MathML" xmlns:tei="http://www.tei-c.org/ns/1.0" xmlns:edate="http://exslt.org/dates-and-times" xmlns:xsl="http://www.w3.org/1999/XSL/Transform" extension-element-prefixes="edate" exclude-result-prefixes="edate xd" version="1.0">

<xd:doc type="stylesheet">
    <xd:short>TEI stylesheet customization module, common for all
    output formats.</xd:short>
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
    <xd:author>Sebastian Rahtz sebastian.rahtz@oucs.ox.ac.uk</xd:author>
    <xd:cvsId>$Id$</xd:cvsId>
    <xd:copyright>2005, TEI Consortium</xd:copyright>
</xd:doc>
  <xsl:key name="IDS" match="tei:*[@xml:id]" use="@xml:id"/>

<xd:doc type="string" class="CSS">
CSS class for TOC entries
</xd:doc>
<xsl:param name="class_toc">toc</xsl:param>

<xd:doc type="string" class="figures">
<xd:short>Directory specification to put before names of graphics files, unless
they start with "./"</xd:short>
<xd:detail> </xd:detail>
</xd:doc>
<xsl:param name="graphicsPrefix"/>

<xd:doc type="string" class="figures">
Default file suffix for graphics files, if not directly specified
</xd:doc>
<xsl:param name="graphicsSuffix">.png</xsl:param>

<xd:doc type="decimal" class="figures">
Scaling of imported graphics
</xd:doc>
<xsl:param name="standardScale">1</xsl:param>


<xd:doc type="boolean" class="headings">
Construct a heading (eg "Heading") for &lt;div&gt; elements with no  &lt;head&gt;
</xd:doc>
<xsl:param name="autoHead"/>

<xd:doc class="headings">
    <xd:short>[common] How to make a heading for section if there is
    no &lt;head&gt;</xd:short>
    <xd:detail> </xd:detail>
</xd:doc>
<xsl:template name="autoMakeHead">
<xsl:choose>
  <xsl:when test="head">
    <xsl:apply-templates mode="plain" select="head"/>
  </xsl:when>
  <xsl:when test="@type"><xsl:value-of select="@type"/></xsl:when>
  <xsl:otherwise>Heading</xsl:otherwise>
</xsl:choose>
</xsl:template>

<xd:doc type="string" class="headings">
Punctuation to insert after a section number
</xd:doc>
<xsl:param name="headingNumberSuffix">
  <xsl:text>.</xsl:text>
  <xsl:value-of select="$numberSpacer"/>
</xsl:param>

<xd:doc type="string" class="headings">
Character to put after number of section header
</xd:doc>
<xsl:param name="numberSpacer">
    <xsl:text> </xsl:text>
</xsl:param>

<xd:doc type="string" class="localisation">
The language to use when generating text (use ISO 2-letter codes)
</xd:doc>
<xsl:param name="lang">en</xsl:param>

<xd:doc type="string" class="localisation">
The word for "Appendix"; by default uses language-specific lookup table.
</xd:doc>
<xsl:param name="appendixWords"></xsl:param>

<xd:doc type="string" class="localisation">
The word for "Author"; by default uses language-specific lookup table.
</xd:doc>
<xsl:param name="authorWord"></xsl:param>

<xd:doc type="string" class="localisation">
The word for "Bibliography"; by default uses language-specific lookup table.
</xd:doc>
<xsl:param name="biblioWords"></xsl:param>

<xd:doc class="localisation">
    <xd:short>[common] Prefix text before an auto-generated table of contents </xd:short>
    <xd:detail> </xd:detail>
</xd:doc>
<xsl:template name="contentsHeading">
<xsl:call-template name ="i18n">
  <xsl:with-param name="word">contentsHeading</xsl:with-param>
</xsl:call-template>
</xsl:template>

<xd:doc class="localisation">
    <xd:short>[common] Title for "Contents"; by default uses language-specific lookup table.</xd:short>
    <xd:detail> </xd:detail>
</xd:doc>
<xsl:template name="contentsWord">
<xsl:call-template name ="i18n">
  <xsl:with-param name="word">contentsWord</xsl:with-param>
</xsl:call-template>
</xsl:template>

<xd:doc type="string" class="localisation">
The word for "Date"; by default uses language-specific lookup table.
</xd:doc>
<xsl:param name="dateWord"></xsl:param>

<xd:doc class="localisation">
    <xd:short>[common] Title for "Feedback"</xd:short>
    <xd:detail> </xd:detail>
</xd:doc>
<xsl:template name="feedbackWords">
<xsl:call-template name ="i18n">
  <xsl:with-param name="word">feedbackWords</xsl:with-param>
</xsl:call-template>
</xsl:template>

<xd:doc type="string" class="localisation">
The word for "Figure"; by default uses language-specific lookup table.
</xd:doc>
<xsl:param name="figureWord"></xsl:param>

<xd:doc type="string" class="localisation">
The word for "Figures"; by default uses language-specific lookup table.
</xd:doc>
<xsl:param name="figureWords"></xsl:param>

<xd:doc type="string" class="localisation">
The word for "Next"; by default uses language-specific lookup table.
</xd:doc>
<xsl:param name="nextWord"></xsl:param>

<xd:doc type="string" class="localisation">
The word for "Notes"; by default uses language-specific lookup table.
</xd:doc>
<xsl:param name="noteHeading">
</xsl:param>

<xd:doc type="string" class="localisation">
The word for "Previous"; by default uses language-specific lookup table.
</xd:doc>
<xsl:param name="previousWord"></xsl:param>

<xd:doc type="string" class="localisation">
The word for "revised"; by default uses language-specific lookup table.
</xd:doc>
<xsl:param name="revisedWord"></xsl:param>

<xd:doc class="localisation">
    <xd:short>[common] Title for "Search"; by default uses language-specific lookup table. </xd:short>
    <xd:detail> </xd:detail>
</xd:doc>
<xsl:template name="searchWords">
<xsl:call-template name ="i18n">
  <xsl:with-param name="word">searchWords</xsl:with-param>
</xsl:call-template>
</xsl:template>

<xd:doc type="string" class="localisation">
The word for "Table"; by default uses language-specific lookup table.
</xd:doc>
<xsl:param name="tableWord"></xsl:param>

<xd:doc type="string" class="localisation">
The word for "Tables"; by default uses language-specific lookup table.
</xd:doc>
<xsl:param name="tableWords"></xsl:param>

<xd:doc type="string" class="localisation">
The word for "Contents"; by default uses language-specific lookup table.
</xd:doc>
<xsl:param name="tocWords"></xsl:param>

<xd:doc type="string" class="localisation">
The word for "Up"; by default uses language-specific lookup table.
</xd:doc>
<xsl:param name="upWord"></xsl:param>

<xd:doc type="string" class="links">
Name of department within institution
</xd:doc>
<xsl:param name="department"/>

<xd:doc type="string" class="links">
Name of link to home page of application
</xd:doc>
<xsl:param name="homeLabel">Home</xsl:param>

<xd:doc type="anyURI" class="links">
Home URL of project
</xd:doc>
<xsl:param name="homeURL">http://www.tei-c.org/</xsl:param>

<xd:doc type="string" class="links">
Label for link to project
</xd:doc>
<xsl:param name="homeWords">TEI</xsl:param>

<xd:doc type="string" class="links">
Name of institution
</xd:doc>
<xsl:param name="institution">TEI XSL Stylesheets</xsl:param>

<xd:doc type="anyURI" class="links">
Link to overall institution
</xd:doc>
<xsl:param name="parentURL">http://www.tei-c.org/</xsl:param>

<xd:doc type="string" class="links">
Name of overall institution
</xd:doc>
<xsl:param name="parentWords">My Parent Institution</xsl:param>

<xd:doc type="anyURI" class="links">
Link to search application
</xd:doc>
<xsl:param name="searchURL">http://www.google.com</xsl:param>

<xd:doc type="anyURI" class="misc">
The home page for these stylesheets
</xd:doc>
<xsl:param name="teixslHome">http://www.tei-c.org/Stylesheets/teic/</xsl:param>

<xd:doc type="boolean" class="misc">
Process elements according to assumptions of TEI P4
</xd:doc>
<xsl:param name="teiP4Compat">false</xsl:param>

<xd:doc type="boolean" class="misc">
Title, author and date is taken from the &lt;teiHeader&gt;
     rather than looked for in the front matter
</xd:doc>
<xsl:param name="useHeaderFrontMatter">false</xsl:param>

<xd:doc class="numbering">
    <xd:short>[common] How to number sections in back matter</xd:short>
    <xd:detail> </xd:detail>
</xd:doc>
<xsl:template name="numberBackDiv">
 <xsl:if test="not($numberBackHeadings='')">
   <xsl:number format="A.1.1.1.1.1" level="multiple" count="tei:div|tei:div0|tei:div1|tei:div2|tei:div3|tei:div4|tei:div5|tei:div6"/>
</xsl:if>
</xsl:template>

<xd:doc type="boolean" class="numbering">
Automatically number figures in back matter
</xd:doc>
<xsl:param name="numberBackFigures">false</xsl:param>

<xd:doc type="string" class="numbering">
How to construct heading numbering in back matter
</xd:doc>
<xsl:param name="numberBackHeadings">A.1</xsl:param>

<xd:doc type="boolean" class="numbering">
Automatically number tables in back matter
</xd:doc>
<xsl:param name="numberBackTables">true</xsl:param>

<xd:doc class="numbering">
    <xd:short>[common] How to number sections in main matter</xd:short>
    <xd:detail> </xd:detail>
</xd:doc>
<xsl:template name="numberBodyDiv">
 <xsl:if test="$numberHeadings='true'">
   <xsl:number level="multiple" count="tei:div|tei:div0|tei:div1|tei:div2|tei:div3|tei:div4|tei:div5|tei:div6"/>
</xsl:if>
</xsl:template>

<xd:doc type="string" class="numbering">
How to construct heading numbering in main matter
</xd:doc>
<xsl:param name="numberBodyHeadings">1.1.1.1</xsl:param>

<xd:doc type="boolean" class="numbering">
Automatically number figures
</xd:doc>
<xsl:param name="numberFigures">true</xsl:param>

<xd:doc class="numbering">
    <xd:short>[common] How to number sections in front matter</xd:short>
    <xd:detail> </xd:detail>
</xd:doc>
<xsl:template name="numberFrontDiv">
 <xsl:if test="not($numberFrontHeadings='')">
   <xsl:number level="multiple" count="tei:div|tei:div0|tei:div1|tei:div2|tei:div3|tei:div4|tei:div5|tei:div6"/>
 </xsl:if>
</xsl:template>

<xd:doc type="boolean" class="numbering">
Automatically number figures in front matter
</xd:doc>
<xsl:param name="numberFrontFigures">false</xsl:param>

<xd:doc type="string" class="numbering">
How to construct heading numbering in front matter
</xd:doc>
<xsl:param name="numberFrontHeadings"/>

<xd:doc type="boolean" class="numbering">
Automatically number tables in front matter
</xd:doc>
<xsl:param name="numberFrontTables">true</xsl:param>

<xd:doc type="boolean" class="numbering">
Automatically number sections
</xd:doc>
<xsl:param name="numberHeadings">true</xsl:param>

<xd:doc type="integer" class="numbering">
Depth to which sections should be numbered
</xd:doc>
<xsl:param name="numberHeadingsDepth">9</xsl:param>

<xd:doc type="boolean" class="numbering">
Automatically number tables
</xd:doc>
<xsl:param name="numberTables">true</xsl:param>

<xd:doc type="boolean" class="numbering">
Use value of "n" attribute to number sections
</xd:doc>
<xsl:param name="prenumberedHeadings">false</xsl:param>

<xd:doc type="string" class="output">
The complete URL when the document is being delivered from a web
server (normally set by Apache or Cocoon)
</xd:doc>
<xsl:param name="REQUEST"/>

<xd:doc type="boolean" class="output">
Write to standard output channel
</xd:doc>
<xsl:param name="STDOUT">true</xsl:param>

<xd:doc type="string" class="style">
Display of &lt;pb&gt; element. Choices are "visible", "active" and "none".
</xd:doc>
<xsl:param name="pagebreakStyle">none</xsl:param>


<xd:doc type="string" class="style">
How to display Relax NG schema fragments (rnc or rng)
</xd:doc>
<xsl:param name="displayMode">rnc</xsl:param>

<xd:doc type="boolean" class="style">
Provide minimal context for a link
</xd:doc>
<xsl:param name="minimalCrossRef">false</xsl:param>

<xd:doc type="string" class="style">
Character to insert at end of quote.
</xd:doc>
<xsl:param name="postQuote">&#8217;</xsl:param>

<xd:doc type="string" class="style">
Character to insert at start of quote
</xd:doc>
<xsl:param name="preQuote">&#8216;</xsl:param>

<xd:doc type="string" class="tables">
Default alignment of table cells
</xd:doc>
<xsl:param name="cellAlign">left</xsl:param>

<xd:doc type="string" class="tables">
Default alignment of tables
</xd:doc>
<xsl:param name="tableAlign">left</xsl:param>
</xsl:stylesheet>
