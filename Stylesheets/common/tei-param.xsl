<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet exclude-result-prefixes="edate xd"
  extension-element-prefixes="edate" version="1.0"
  xmlns:edate="http://exslt.org/dates-and-times"
  xmlns:m="http://www.w3.org/1998/Math/MathML"
  xmlns:tei="http://www.tei-c.org/ns/1.0"
  xmlns:xd="http://www.pnp-software.com/XSLTdoc"
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
  <xd:doc type="stylesheet">
    <xd:short>TEI stylesheet customization module, common for all output
      formats.</xd:short>
    <xd:detail> This library is free software; you can redistribute it and/or
      modify it under the terms of the GNU Lesser General Public License as
      published by the Free Software Foundation; either version 2.1 of the
      License, or (at your option) any later version. This library is
      distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY;
      without even the implied warranty of MERCHANTABILITY or FITNESS FOR A
      PARTICULAR PURPOSE. See the GNU Lesser General Public License for more
      details. You should have received a copy of the GNU Lesser General Public
      License along with this library; if not, write to the Free Software
      Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA </xd:detail>
    <xd:author>See AUTHORS</xd:author>
    <xd:cvsId>$Id$</xd:cvsId>
    <xd:copyright>2007, TEI Consortium</xd:copyright>
  </xd:doc>
  <xsl:key match="tei:*[@xml:id]" name="IDS" use="@xml:id"/>
  <xd:doc class="CSS" type="string">CSS class for TOC entries</xd:doc>
  <xsl:param name="class_toc">toc</xsl:param>

  <xd:doc class="figures" type="string">
    <xd:short>Directory specification to put before names of graphics files,
      unless they start with "./"</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:param name="graphicsPrefix"/>
  <xd:doc class="figures" type="string"> Default file suffix for graphics files,
    if not directly specified</xd:doc>
  <xsl:param name="graphicsSuffix">.png</xsl:param>
  <xd:doc class="figures" type="decimal"> Scaling of imported graphics</xd:doc>
  <xsl:param name="standardScale">1</xsl:param>
  <xd:doc class="headings" type="boolean"> Construct a heading 
    for &lt;div&gt; elements with no &lt;head&gt;</xd:doc>
  <xsl:param name="autoHead"/>
  <xd:doc class="headings">
    <xd:short>[common] How to make a heading for section if there is no
      &lt;head&gt;</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template name="autoMakeHead">
    <xsl:choose>
      <xsl:when test="tei:head">
        <xsl:apply-templates mode="plain" select="tei:head"/>
      </xsl:when>
      <xsl:when test="@type">
        <xsl:value-of select="@type"/>
      </xsl:when>
      <xsl:otherwise>
	<xsl:text>&#160;</xsl:text>
     </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  <xd:doc class="headings" type="string"> Punctuation to insert after a section
    number</xd:doc>
  <xsl:param name="headingNumberSuffix">
    <xsl:text>.</xsl:text>
    <xsl:value-of select="$numberSpacer"/>
  </xsl:param>
  <xd:doc class="headings" type="string"> Character to put after number of
    section header</xd:doc>
  <xsl:param name="numberSpacer">
    <xsl:text> </xsl:text>
  </xsl:param>
  <xd:doc class="links" type="string"> Name of department within institution</xd:doc>
  <xsl:param name="department"/>
  <xd:doc class="links" type="string"> Name of link to home page of application</xd:doc>
  <xsl:param name="homeLabel">Home</xsl:param>
  <xd:doc class="links" type="anyURI">Project Home</xd:doc>
  <xsl:param name="homeURL">http://www.tei-c.org/</xsl:param>
  <xd:doc class="links" type="string">Project</xd:doc>
  <xsl:param name="homeWords">TEI</xsl:param>
  <xd:doc class="links" type="string"> Institution</xd:doc>
  <xsl:param name="institution">A TEI Project</xsl:param>
  <xd:doc class="links" type="anyURI"> Institution link</xd:doc>
  <xsl:param name="parentURL">http://www.tei-c.org/</xsl:param>
  <xd:doc class="links" type="string"> Name of overall institution</xd:doc>
  <xsl:param name="parentWords">Parent Institution</xsl:param>
  <xd:doc class="links" type="anyURI"> Link to search application</xd:doc>
  <xsl:param name="searchURL">http://www.google.com</xsl:param>
  <xd:doc class="misc" type="anyURI"> The home page for these stylesheets</xd:doc>
  <xsl:param name="teixslHome">http://www.tei-c.org/Stylesheets/teic/</xsl:param>
  <xd:doc class="misc" type="boolean"> Process elements according to assumptions
    of TEI P4</xd:doc>
  <xsl:param name="teiP4Compat">false</xsl:param>
  <xd:doc class="misc" type="boolean"> Title, author and date is taken from the
    &lt;teiHeader&gt; rather than looked for in the front matter</xd:doc>
  <xsl:param name="useHeaderFrontMatter">false</xsl:param>
  <xd:doc class="numbering">
    <xd:short>[common] How to number sections in back matter</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template name="numberBackDiv">
    <xsl:if test="not($numberBackHeadings='')">
      <xsl:number
        count="tei:div|tei:div0|tei:div1|tei:div2|tei:div3|tei:div4|tei:div5|tei:div6"
        format="A.1.1.1.1.1" level="multiple"/>
    </xsl:if>
  </xsl:template>
  <xd:doc class="numbering" type="boolean"> Automatically number figures in back
    matter</xd:doc>
  <xsl:param name="numberBackFigures">false</xsl:param>
  <xd:doc class="numbering" type="string"> How to construct heading numbering in
    back matter</xd:doc>
  <xsl:param name="numberBackHeadings">A.1</xsl:param>
  <xd:doc class="numbering" type="boolean"> Automatically number tables in back
    matter</xd:doc>
  <xsl:param name="numberBackTables">true</xsl:param>
  <xd:doc class="numbering">
    <xd:short>[common] How to number sections in main matter</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template name="numberBodyDiv">
    <xsl:if test="$numberHeadings='true'">
      <xsl:number
        count="tei:div|tei:div0|tei:div1|tei:div2|tei:div3|tei:div4|tei:div5|tei:div6"
        level="multiple"/>
    </xsl:if>
  </xsl:template>
  <xd:doc class="numbering" type="string"> How to construct heading numbering in
    main matter</xd:doc>
  <xsl:param name="numberBodyHeadings">1.1.1.1</xsl:param>
  <xd:doc class="numbering" type="boolean"> Automatically number figures</xd:doc>
  <xsl:param name="numberFigures">true</xsl:param>
  <xd:doc class="numbering">
    <xd:short>[common] How to number sections in front matter</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template name="numberFrontDiv">
    <xsl:param name="minimal"/>
      <xsl:number
        count="tei:div|tei:div0|tei:div1|tei:div2|tei:div3|tei:div4|tei:div5|tei:div6"
        level="multiple"/>
      <xsl:if test="$minimal='false'">
	<xsl:value-of select="$numberSpacer"/>
      </xsl:if>
  </xsl:template>
  <xd:doc class="numbering" type="boolean"> Automatically number figures in
    front matter</xd:doc>
  <xsl:param name="numberFrontFigures">false</xsl:param>
  <xd:doc class="numbering" type="string"> How to construct heading numbering in
    front matter</xd:doc>
  <xsl:param name="numberFrontHeadings"/>
  <xd:doc class="numbering" type="boolean"> Automatically number tables in front
    matter</xd:doc>
  <xsl:param name="numberFrontTables">true</xsl:param>
  <xd:doc class="numbering" type="boolean"> Automatically number sections</xd:doc>
  <xsl:param name="numberHeadings">true</xsl:param>
  <xd:doc class="numbering" type="integer"> Depth to which sections should be
    numbered</xd:doc>
  <xsl:param name="numberHeadingsDepth">9</xsl:param>
  <xd:doc class="numbering" type="boolean"> Automatically number tables</xd:doc>
  <xsl:param name="numberTables">true</xsl:param>
  <xd:doc class="numbering" type="boolean"> Use value of "n" attribute to number
    sections</xd:doc>
  <xsl:param name="prenumberedHeadings">false</xsl:param>
  <xd:doc class="output" type="string"> The complete URL when the document is
    being delivered from a web server (normally set by Apache or Cocoon)</xd:doc>
  <xsl:param name="REQUEST"/>
  <xd:doc class="output" type="boolean"> Write to standard output channel</xd:doc>
  <xsl:param name="STDOUT">true</xsl:param>
  <xd:doc class="style" type="string"> Display of &lt;pb&gt; element.
    Choices are "visible", "active" and "none".</xd:doc>
  <xsl:param name="pagebreakStyle">none</xsl:param>
  <xd:doc class="style" type="string"> How to display Relax NG schema fragments
    (rnc or rng)</xd:doc>
  <xsl:param name="displayMode">rnc</xsl:param>
  <xd:doc class="style" type="boolean"> Provide minimal context for a link</xd:doc>
  <xsl:param name="minimalCrossRef">false</xsl:param>
  <xd:doc class="style" type="string"> Character to insert at end of quote.</xd:doc>
  <xsl:param name="postQuote">’</xsl:param>
  <xd:doc class="style" type="string"> Character to insert at start of quote</xd:doc>
  <xsl:param name="preQuote">‘</xsl:param>
  <xd:doc class="tables" type="string"> Default alignment of table cells</xd:doc>
  <xsl:param name="cellAlign">left</xsl:param>
  <xd:doc class="tables" type="string"> Default alignment of tables</xd:doc>
  <xsl:param name="tableAlign">table</xsl:param>

  <xd:doc class="hook">
    <xd:short>[common] Hook where actions can be inserted when making
     a heading</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template name="sectionHeadHook"/>

</xsl:stylesheet>
