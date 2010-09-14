<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:sch="http://purl.oclc.org/dsdl/schematron" xmlns:m="http://www.w3.org/1998/Math/MathML" xmlns:tei="http://www.tei-c.org/ns/1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform" exclude-result-prefixes="m tei xsi sch" version="2.0">
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" scope="stylesheet" type="stylesheet">
    <desc>
      <p>TEI stylesheet customization module, common for all output
      formats.</p>
      <p> This library is free software; you can redistribute it and/or
      modify it under the terms of the GNU Lesser General Public License as
      published by the Free Software Foundation; either version 2.1 of the
      License, or (at your option) any later version. This library is
      distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY;
      without even the implied warranty of MERCHANTABILITY or FITNESS FOR A
      PARTICULAR PURPOSE. See the GNU Lesser General Public License for more
      details. You should have received a copy of the GNU Lesser General Public
      License along with this library; if not, write to the Free Software
      Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA </p>
      <p>Author: See AUTHORS</p>
      <p>Id: $Id$</p>
      <p>Copyright: 2008, TEI Consortium</p>
    </desc>
  </doc>
  <xsl:key match="tei:*[@xml:id]" name="IDS" use="@xml:id"/>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" class="CSS" type="string">
    <desc>CSS class for TOC entries</desc>
  </doc>
  <xsl:param name="class_toc">toc</xsl:param>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" class="figures" type="string">
    <desc>Directory specification to put before names of graphics files,
      unless they start with "./"</desc>
  </doc>
  <xsl:param name="graphicsPrefix"/>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" class="figures" type="string">
    <desc> Default file suffix for graphics files,
    if not directly specified</desc>
  </doc>
  <xsl:param name="graphicsSuffix">.png</xsl:param>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" class="figures" type="decimal">
    <desc> Scaling of imported graphics</desc>
  </doc>
  <xsl:param name="standardScale">1</xsl:param>

  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" class="headings" type="boolean">
    <desc> Construct a heading 
    for &lt;div&gt; elements with no &lt;head&gt;</desc>
  </doc>
  <xsl:param name="autoHead"/>


  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" class="headings">
    <desc>[common] How to make a heading for section if there is no
      &lt;head&gt;</desc>
  </doc>
  <xsl:template name="autoMakeHead">
    <xsl:param name="display"/>
    <xsl:choose>
      <xsl:when test="tei:head and $display='full'">
        <xsl:apply-templates select="tei:head" mode="makeheading"/>
      </xsl:when>
      <xsl:when test="tei:head">
        <xsl:apply-templates select="tei:head" mode="plain"/>
      </xsl:when>
      <xsl:when test="@n">
	<xsl:if test="@type">
	  <xsl:value-of select="@type"/>
	  <xsl:text> </xsl:text>
	</xsl:if>
	<xsl:value-of select="@n"/>
      </xsl:when>
      <xsl:when test="@type">
        <xsl:value-of select="@type"/>
      </xsl:when>
      <xsl:otherwise>
        <xsl:text>➤</xsl:text>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" class="headings" type="string">
    <desc> Punctuation to insert after a section
    number</desc>
  </doc>
  <xsl:template name="headingNumberSuffix">
    <xsl:text>.</xsl:text>
    <xsl:value-of select="$numberSpacer"/>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" class="headings" type="string">
    <desc> Character to put after number of
    section header</desc>
  </doc>
  <xsl:param name="numberSpacer">
    <xsl:text> </xsl:text>
  </xsl:param>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" class="links" type="string">
    <desc> Name of department within institution</desc>
  </doc>
  <xsl:param name="department"/>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" class="links" type="string">
    <desc> Name of link to home page of application</desc>
  </doc>
  <xsl:param name="homeLabel">Home</xsl:param>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" class="links" type="anyURI">
    <desc>Project Home</desc>
  </doc>
  <xsl:param name="homeURL">http://www.tei-c.org/</xsl:param>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" class="links" type="string">
    <desc>Project</desc>
  </doc>
  <xsl:param name="homeWords">TEI</xsl:param>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" class="links" type="string">
    <desc> Institution</desc>
  </doc>
  <xsl:param name="institution">A TEI Project</xsl:param>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" class="links" type="anyURI">
    <desc> Institution link</desc>
  </doc>
  <xsl:param name="parentURL">http://www.tei-c.org/</xsl:param>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" class="links" type="string">
    <desc> Name of overall institution</desc>
  </doc>
  <xsl:param name="parentWords">Parent Institution</xsl:param>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" class="links" type="anyURI">
    <desc> Link to search application</desc>
  </doc>
  <xsl:param name="searchURL">http://www.google.com</xsl:param>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" class="misc" type="anyURI">
    <desc> The home page for these stylesheets</desc>
  </doc>
  <xsl:param name="teixslHome">http://www.tei-c.org/Stylesheets/teic/</xsl:param>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" class="misc" type="boolean">
    <desc> Process elements according to assumptions
    of TEI P4</desc>
  </doc>
  <xsl:param name="teiP4Compat">false</xsl:param>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" class="misc" type="boolean">
    <desc> Title, author and date is taken from the
    &lt;teiHeader&gt; rather than looked for in the front matter</desc>
  </doc>
  <xsl:param name="useHeaderFrontMatter">false</xsl:param>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" class="numbering">
    <desc>[common] How to number sections in back matter</desc>
  </doc>
  <xsl:template name="numberBackDiv">
    <xsl:if test="not($numberBackHeadings='')">
      <xsl:number count="tei:div|tei:div1|tei:div2|tei:div3|tei:div4|tei:div5|tei:div6" format="A.1.1.1.1.1" level="multiple"/>
    </xsl:if>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" class="numbering" type="boolean">
    <desc> Automatically number figures in back
    matter</desc>
  </doc>
  <xsl:param name="numberBackFigures">false</xsl:param>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" class="numbering" type="string">
    <desc> How to construct heading numbering in
    back matter</desc>
  </doc>
  <xsl:param name="numberBackHeadings">A.1</xsl:param>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" class="numbering" type="boolean">
    <desc> Automatically number tables in back
    matter</desc>
  </doc>
  <xsl:param name="numberBackTables">true</xsl:param>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" class="numbering">
    <desc>[common] How to number sections in main matter</desc>
  </doc>
  <xsl:template name="numberBodyDiv">
    <xsl:if test="$numberHeadings='true'">
      <xsl:number count="tei:div|tei:div1|tei:div2|tei:div3|tei:div4|tei:div5|tei:div6" level="multiple"/>
    </xsl:if>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" class="numbering" type="string">
    <desc> How to construct heading numbering in
    main matter</desc>
  </doc>
  <xsl:param name="numberBodyHeadings">1.1.1.1</xsl:param>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" class="numbering" type="boolean">
    <desc> Automatically number figures</desc>
  </doc>
  <xsl:param name="numberFigures">true</xsl:param>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" class="numbering">
    <desc>[common] How to number sections in front matter</desc>
  </doc>
  <xsl:template name="numberFrontDiv">
    <xsl:param name="minimal"/>
    <xsl:number count="tei:div|tei:div1|tei:div2|tei:div3|tei:div4|tei:div5|tei:div6" level="multiple"/>
    <xsl:if test="$minimal='false'">
      <xsl:value-of select="$numberSpacer"/>
    </xsl:if>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" class="numbering" type="boolean">
    <desc> Automatically number figures in
    front matter</desc>
  </doc>
  <xsl:param name="numberFrontFigures">false</xsl:param>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" class="numbering" type="string">
    <desc> How to construct heading numbering in
    front matter</desc>
  </doc>
  <xsl:param name="numberFrontHeadings"/>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" class="numbering" type="boolean">
    <desc> Automatically number tables in front
    matter</desc>
  </doc>
  <xsl:param name="numberFrontTables">true</xsl:param>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" class="numbering" type="boolean">
    <desc> Automatically number sections</desc>
  </doc>
  <xsl:param name="numberHeadings">true</xsl:param>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" class="numbering" type="integer">
    <desc> Depth to which sections should be
    numbered</desc>
  </doc>
  <xsl:param name="numberHeadingsDepth">9</xsl:param>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" class="numbering" type="boolean">
    <desc> Automatically number tables</desc>
  </doc>
  <xsl:param name="numberTables">true</xsl:param>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" class="numbering" type="boolean">
    <desc> Use value of "n" attribute to number
    sections</desc>
  </doc>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" class="numbering" type="boolean">
    <desc>
    Automatically number paragraphs.
  </desc>
  </doc>
  <xsl:param name="numberParagraphs">false</xsl:param>
  <xsl:param name="prenumberedHeadings">false</xsl:param>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" class="output" type="string">
    <desc> The complete URL when the document is
    being delivered from a web server (normally set by Apache or Cocoon)</desc>
  </doc>
  <xsl:param name="REQUEST"/>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" class="output" type="boolean">
    <desc> Write to standard output channel</desc>
  </doc>
  <xsl:param name="STDOUT">true</xsl:param>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" class="style" type="string">
    <desc> Display of &lt;pb&gt; element.
    Choices are "visible", "active" and "none".</desc>
  </doc>
  <xsl:param name="pagebreakStyle">visible</xsl:param>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" class="style" type="string">
    <desc> How to display Relax NG schema fragments
    (rnc or rng)</desc>
  </doc>
  <xsl:param name="displayMode">rnc</xsl:param>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" class="style" type="boolean">
    <desc> Provide minimal context for a link</desc>
  </doc>
  <xsl:param name="minimalCrossRef">false</xsl:param>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" class="style" type="string">
    <desc> Character to insert at end of quote.</desc>
  </doc>
  <xsl:param name="postQuote">’</xsl:param>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" class="style" type="string">
    <desc> Character to insert at start of quote</desc>
  </doc>
  <xsl:param name="preQuote">‘</xsl:param>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" class="tables" type="string">
    <desc> Default alignment of table cells</desc>
  </doc>
  <xsl:param name="cellAlign">left</xsl:param>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" class="tables" type="string">
    <desc> Default alignment of tables</desc>
  </doc>
  <xsl:param name="tableAlign">left</xsl:param>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" class="layout" type="boolean">
    <desc>
Whether to make simplified display of ODD
</desc>
  </doc>
  <xsl:param name="oddWeaveLite">false</xsl:param>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" class="layout" type="string">
    <desc>
Paragraph indentation
</desc>
  </doc>
  <xsl:param name="parIndent">1em</xsl:param>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" class="layout" type="string">
    <desc>
Default spacing between paragraphs
</desc>
  </doc>
  <xsl:param name="parSkip">0pt</xsl:param>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" class="hook">
    <desc>[common] Hook where actions can be inserted when making
     a heading</desc>
  </doc>
  <xsl:template name="sectionHeadHook"/>
</xsl:stylesheet>
