<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet xmlns:xd="http://www.pnp-software.com/XSLTdoc" xmlns:fo="http://www.w3.org/1999/XSL/Format" xmlns:tei="http://www.tei-c.org/ns/1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform" exclude-result-prefixes="fo tei xd" version="1.0">
  <xd:doc type="stylesheet">
    <xd:short>
    TEI stylesheet
    customization module for 
      fo output.</xd:short>
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
    <xd:copyright>2007, TEI Consortium</xd:copyright>
  </xd:doc>
  <xd:doc type="string" class="figures">
How to scale figures if no width and height specified (pass to XSL FO content-width)
</xd:doc>
  <xsl:param name="autoScaleFigures"/>
  <xd:doc type="boolean" class="figures">
Put captions on inline figures
</xd:doc>
  <xsl:param name="captionInlineFigures">false</xsl:param>
  <xd:doc class="figures">
    <xd:short>[fo] Set attributes for display of figures</xd:short>
    <xd:detail>
      <p/>
    </xd:detail>
  </xd:doc>
  <xsl:template name="figureCaptionstyle">
    <xsl:attribute name="text-align">center</xsl:attribute>
    <xsl:attribute name="font-style">italic</xsl:attribute>
    <xsl:attribute name="end-indent">
      <xsl:value-of select="$exampleMargin"/>
    </xsl:attribute>
    <xsl:attribute name="start-indent">
      <xsl:value-of select="$exampleMargin"/>
    </xsl:attribute>
  </xsl:template>
  <xd:doc type="boolean" class="figures">
Show the contents of &lt;head&gt; in a cross-reference to table or figure
</xd:doc>
  <xsl:param name="showFloatHead">false</xsl:param>
  <xd:doc type="boolean" class="figures">
Show a title for figures or tables (eg Table or Figure) in a cross-reference
</xd:doc>
  <xsl:param name="showFloatLabel">false</xsl:param>
  <xd:doc type="boolean" class="figures">
Show the page number in a cross-reference to table or figure
</xd:doc>
  <xsl:param name="xrefShowPage">false</xsl:param>
  <xd:doc class="hook">
    <xd:short>[fo] Hook where extra material can be inserted after the
    &lt;body&gt; has been processed</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template name="afterBodyHook"/>
  <xd:doc class="hook">
    <xd:short>[fo] Hook where work can be done at the start of each block</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template name="blockStartHook"/>
  <xd:doc class="hook">
    <xd:short>[fo] Hook where extra page masters can be defined</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template name="pageMasterHook"/>
  <xd:doc type="boolean" class="layout">
Put back matter in multiple columns
</xd:doc>
  <xsl:param name="backMulticolumns">false</xsl:param>
  <xd:doc type="string" class="layout">
Margin at bottom of text body
</xd:doc>
  <xsl:param name="bodyMarginBottom">24pt</xsl:param>
  <xd:doc type="string" class="layout">
Margin at top of text body
</xd:doc>
  <xsl:param name="bodyMarginTop">24pt</xsl:param>
  <xd:doc type="boolean" class="layout">
Put body matter in multiple columns
</xd:doc>
  <xsl:param name="bodyMulticolumns">false</xsl:param>
  <xd:doc type="string" class="layout">
Symbol for 4th level itemized list
</xd:doc>
  <xsl:param name="bulletFour">+</xsl:param>
  <xd:doc type="string" class="layout">
Symbol for top-level itemized list
</xd:doc>
  <xsl:param name="bulletOne">•</xsl:param>
  <xd:doc type="string" class="layout">
Symbol for 3rd level itemized list
</xd:doc>
  <xsl:param name="bulletThree">*</xsl:param>
  <xd:doc type="string" class="layout">
Symbol for 2nd level itemized list
</xd:doc>
  <xsl:param name="bulletTwo">–</xsl:param>
  <xd:doc type="integer" class="layout">
Number of columns, when multiple-column work is requested
</xd:doc>
  <xsl:param name="columnCount">1</xsl:param>
  <xd:doc type="string" class="layout">
XSL FO "provisional-distance-between starts"
</xd:doc>
  <xsl:param name="distanceBetweenStarts">18pt</xsl:param>
  <xd:doc type="string" class="layout">
XSL FO "provisional-distance-between starts" for gloss lists
</xd:doc>
  <xsl:param name="distanceBetweenGlossStarts">42pt</xsl:param>
  <xd:doc type="boolean" class="layout">
Display section headings in running heads
</xd:doc>
  <xsl:param name="divRunningheads">false</xsl:param>
  <xd:doc type="string" class="layout">
Space below examples
</xd:doc>
  <xsl:param name="exampleAfter">4pt</xsl:param>
  <xd:doc type="string" class="layout">
Space above examples
</xd:doc>
  <xsl:param name="exampleBefore">4pt</xsl:param>
  <xd:doc type="string" class="layout">
Left margin for examples
</xd:doc>
  <xsl:param name="exampleMargin">12pt</xsl:param>
  <xd:doc type="string" class="layout">
Left margin of flow
</xd:doc>
  <xsl:param name="flowMarginLeft"/>
  <xd:doc type="string" class="layout">
Which named page master name to use
</xd:doc>
  <xsl:param name="forcePageMaster"/>
  <xd:doc type="string" class="layout">
How to format page numbers in back matter (use XSLT number format)
</xd:doc>
  <xsl:param name="formatBackpage">1</xsl:param>
  <xd:doc type="string" class="layout">
How to format page numbers in main matter (use XSLT number format)
</xd:doc>
  <xsl:param name="formatBodypage">1</xsl:param>
  <xd:doc type="string" class="layout">
How to format page numbers in front matter (use XSLT number format)
</xd:doc>
  <xsl:param name="formatFrontpage">i</xsl:param>
  <xd:doc type="boolean" class="layout">
Put front matter in multiple columns
</xd:doc>
  <xsl:param name="frontMulticolumns">false</xsl:param>
  <xd:doc type="string" class="layout">
Indentation for bibliography entries
</xd:doc>
  <xsl:param name="indentBibl">1em</xsl:param>
  <xd:doc type="string" class="layout">
XSL FO "provisional-label-separation"
</xd:doc>
  <xsl:param name="labelSeparation">6pt</xsl:param>
  <xd:doc type="string" class="layout">
Space above lists at top level
</xd:doc>
  <xsl:param name="listAbove-1">6pt</xsl:param>
  <xd:doc type="string" class="layout">
Space above lists at 2nd level
</xd:doc>
  <xsl:param name="listAbove-2">4pt</xsl:param>
  <xd:doc type="string" class="layout">
Space above lists at 3rd level
</xd:doc>
  <xsl:param name="listAbove-3">0pt</xsl:param>
  <xd:doc type="string" class="layout">
Space above lists at 4th level
</xd:doc>
  <xsl:param name="listAbove-4">0pt</xsl:param>
  <xd:doc type="string" class="layout">
Space below lists at top level
</xd:doc>
  <xsl:param name="listBelow-1">6pt</xsl:param>
  <xd:doc type="string" class="layout">
Space below lists at 2nd level
</xd:doc>
  <xsl:param name="listBelow-2">4pt</xsl:param>
  <xd:doc type="string" class="layout">
Space below lists at 3rd level
</xd:doc>
  <xsl:param name="listBelow-3">0pt</xsl:param>
  <xd:doc type="string" class="layout">
Space below lists at 4th level
</xd:doc>
  <xsl:param name="listBelow-4">0pt</xsl:param>
  <xd:doc type="string" class="layout">
Spacing between list items
</xd:doc>
  <xsl:param name="listItemsep">4pt</xsl:param>
  <xd:doc type="string" class="layout">
Left margin for gloss lists
</xd:doc>
  <xsl:param name="listLeftGlossIndent">0.5in</xsl:param>
  <xd:doc type="string" class="layout">
Left margin for nested gloss lists
</xd:doc>
  <xsl:param name="listLeftGlossInnerIndent">0.25in</xsl:param>
  <xd:doc type="string" class="layout">
Indentation for lists
</xd:doc>
  <xsl:param name="listLeftIndent">0pt</xsl:param>
  <xd:doc type="string" class="layout">
Right margin for lists
</xd:doc>
  <xsl:param name="listRightMargin">10pt</xsl:param>
  <xd:doc type="string" class="layout">
Paper height
</xd:doc>
  <xsl:param name="pageHeight">297mm</xsl:param>
  <xd:doc type="string" class="layout">
Margin at bottom of text area
</xd:doc>
  <xsl:param name="pageMarginBottom">100pt</xsl:param>
  <xd:doc type="string" class="layout">
Left margin
</xd:doc>
  <xsl:param name="pageMarginLeft">80pt</xsl:param>
  <xd:doc type="string" class="layout">
Right margin
</xd:doc>
  <xsl:param name="pageMarginRight">150pt</xsl:param>
  <xd:doc type="string" class="layout">
Margin at top of text area
</xd:doc>
  <xsl:param name="pageMarginTop">75pt</xsl:param>
  <xd:doc type="string" class="layout">
Paper width
</xd:doc>
  <xsl:param name="pageWidth">211mm</xsl:param>
  <xd:doc type="string" class="layout">
Paragraph indentation
</xd:doc>
  <xsl:param name="parIndent">1em</xsl:param>
  <xd:doc type="string" class="layout">
Default spacing between paragraphs
</xd:doc>
  <xsl:param name="parSkip">0pt</xsl:param>
  <xd:doc type="string" class="layout">
Maximum space allowed between paragraphs
</xd:doc>
  <xsl:param name="parSkipmax">12pt</xsl:param>
  <xd:doc type="anyURI" class="layout">
External XML file containing specifications for column sizes for
tables in document
</xd:doc>
  <xsl:param name="readColSpecFile"/>
  <xd:doc type="string" class="layout">
Region after
</xd:doc>
  <xsl:param name="regionAfterExtent">14pt</xsl:param>
  <xd:doc type="string" class="layout">
Region before
</xd:doc>
  <xsl:param name="regionBeforeExtent">14pt</xsl:param>
  <xd:doc type="boolean" class="layout">
Construct running headers from page number and section headings
</xd:doc>
  <xsl:param name="sectionHeaders">true</xsl:param>
  <xd:doc type="string" class="layout">
Space after bibliography
</xd:doc>
  <xsl:param name="spaceAfterBibl">0pt</xsl:param>
  <xd:doc type="string" class="layout">
Space above and below a table
</xd:doc>
  <xsl:param name="spaceAroundTable">8pt</xsl:param>
  <xd:doc type="string" class="layout">
Space above bibliography
</xd:doc>
  <xsl:param name="spaceBeforeBibl">4pt</xsl:param>
  <xd:doc type="string" class="layout">
Space below caption of figure or table
</xd:doc>
  <xsl:param name="spaceBelowCaption">4pt</xsl:param>
  <xd:doc type="boolean" class="layout">
Make title page
</xd:doc>
  <xsl:param name="titlePage">true</xsl:param>
  <xd:doc type="boolean" class="layout">
Make 2-page spreads
</xd:doc>
  <xsl:param name="twoSided">true</xsl:param>
  <xd:doc type="string" class="output">
Language (for hyphenation)
</xd:doc>
  <xsl:param name="language">en_US</xsl:param>
  <xd:doc type="string" class="output">
    <xd:short>Name of intended XSL FO engine</xd:short>
    <xd:detail>This is used to tailor the result for different XSL FO processors.
By default, no special measures are taken, so
there are no bookmarks or other such features. Possible values are
<ul><li>passivetex (the TeX-based PassiveTeX processor</li><li>xep   (XEP)</li><li>fop (FOP)</li><li>antenna  (Antenna House)</li></ul>
</xd:detail>
  </xd:doc>
  <xsl:param name="foEngine"/>
  <xd:doc type="boolean" class="style">
Make &lt;lb&gt; active (ie cause a line break)
</xd:doc>
  <xsl:param name="activeLinebreaks">true</xsl:param>
  <xd:doc type="string" class="style">
Alignment of text (ie justified or ragged)
</xd:doc>
  <xsl:param name="alignment">justify</xsl:param>
  <xd:doc type="string" class="style">
Font size for display of author name 
</xd:doc>
  <xsl:param name="authorSize">14pt</xsl:param>
  <xd:doc type="string" class="style">
Font size for bibliography
</xd:doc>
  <xsl:param name="biblSize">16pt</xsl:param>
  <xd:doc type="string" class="style">
Default font for body
</xd:doc>
  <xsl:param name="bodyFont">Times</xsl:param>
  <xd:doc type="string" class="style">
Default font size for body (without dimension)
</xd:doc>
  <xsl:param name="bodyMaster">10</xsl:param>
  <xd:doc type="string" class="style">
Calculation of normal body font size (add dimension)
</xd:doc>
  <xsl:param name="bodySize">
    <xsl:value-of select="$bodyMaster"/>
    <xsl:text>pt</xsl:text>
  </xsl:param>
  <xd:doc type="string" class="style">
Font size for display of date
</xd:doc>
  <xsl:param name="dateSize">14pt</xsl:param>
  <xd:doc type="string" class="style">
Font for section headings
</xd:doc>
  <xsl:param name="divFont">Times</xsl:param>
  <xd:doc class="style">
    <xd:short>[fo] How to display section headings in a cross-reference </xd:short>
    <xd:param name="head">section title</xd:param>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template name="divXRefHeading">
    <xsl:param name="head">
      <xsl:apply-templates mode="section" select="tei:head"/>
    </xsl:param>
    <xsl:text> (</xsl:text>
    <xsl:value-of select="normalize-space($head)"/>
    <xsl:text>)</xsl:text>
  </xsl:template>
  <xd:doc type="string" class="style">
Colour for display of &lt;eg&gt; blocks.
</xd:doc>
  <xsl:param name="exampleColor">black</xsl:param>
  <xd:doc type="string" class="style">
Colour for background display of &lt;eg&gt; blocks.
</xd:doc>
  <xsl:param name="exampleBackgroundColor">yellow</xsl:param>
  <xd:doc type="string" class="style">
Calculation of font size for examples (add dimension)
</xd:doc>
  <xsl:param name="exampleSize">
    <xsl:value-of select="$bodyMaster * 0.8"/>
    <xsl:text>pt</xsl:text>
  </xsl:param>
  <xd:doc type="string" class="style">
Font size for footnotes
</xd:doc>
  <xsl:param name="footnoteSize">8pt</xsl:param>
  <xd:doc type="string" class="style">
Font size for footnote numbers
</xd:doc>
  <xsl:param name="footnotenumSize">7pt</xsl:param>
  <xd:doc type="string" class="style">
Colour for display of element names
</xd:doc>
  <xsl:param name="giColor">black</xsl:param>
  <xd:doc type="string" class="style">
Indentation of headings
</xd:doc>
  <xsl:param name="headingOutdent">-3em</xsl:param>
  <xd:doc type="boolean" class="style">
Hyphenate text
</xd:doc>
  <xsl:param name="hyphenate">true</xsl:param>
  <xd:doc type="string" class="style">
Colour for display of &lt;ident&gt; values
    Customization parameter.
</xd:doc>
  <xsl:param name="identColor">black</xsl:param>
  <xd:doc class="style">
    <xd:short>[fo] Set attributes for display of links</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template name="linkStyle">
    <xsl:attribute name="text-decoration">underline</xsl:attribute>
  </xsl:template>
  <xd:doc type="string" class="style">
Font family for running header and footer
</xd:doc>
  <xsl:param name="runFont">sans-serif</xsl:param>
  <xd:doc type="string" class="style">
Font size for running header and footer
</xd:doc>
  <xsl:param name="runSize">9pt</xsl:param>
  <xd:doc type="string" class="style">
Sans-serif font
</xd:doc>
  <xsl:param name="sansFont">Helvetica</xsl:param>
  <xd:doc class="style">
    <xd:short>[fo] Set attributes for display of heading for chapters (level 0)</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template name="setupDiv0">
    <xsl:attribute name="font-size">18pt</xsl:attribute>
    <xsl:attribute name="text-align">left</xsl:attribute>
    <xsl:attribute name="font-weight">bold</xsl:attribute>
    <xsl:attribute name="space-after">6pt</xsl:attribute>
    <xsl:attribute name="space-before.optimum">12pt</xsl:attribute>
    <xsl:attribute name="text-indent">
      <xsl:value-of select="$headingOutdent"/>
    </xsl:attribute>
  </xsl:template>
  <xd:doc class="style">
    <xd:short>[fo] Set attributes for display of heading for 1st level sections</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template name="setupDiv1">
    <xsl:attribute name="font-size">14pt</xsl:attribute>
    <xsl:attribute name="text-align">left</xsl:attribute>
    <xsl:attribute name="font-weight">bold</xsl:attribute>
    <xsl:attribute name="space-after">3pt</xsl:attribute>
    <xsl:attribute name="space-before.optimum">9pt</xsl:attribute>
    <xsl:attribute name="text-indent">
      <xsl:value-of select="$headingOutdent"/>
    </xsl:attribute>
  </xsl:template>
  <xd:doc class="style">
    <xd:short>[fo] Set attributes for display of heading for 2nd level sections </xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template name="setupDiv2">
    <xsl:attribute name="font-size">12pt</xsl:attribute>
    <xsl:attribute name="text-align">left</xsl:attribute>
    <xsl:attribute name="font-weight">bold</xsl:attribute>
    <xsl:attribute name="font-style">italic</xsl:attribute>
    <xsl:attribute name="space-after">2pt</xsl:attribute>
    <xsl:attribute name="space-before.optimum">4pt</xsl:attribute>
    <xsl:attribute name="text-indent">
      <xsl:value-of select="$headingOutdent"/>
    </xsl:attribute>
  </xsl:template>
  <xd:doc class="style">
    <xd:short>[fo]Set attributes for display of heading for 3rd level sections </xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template name="setupDiv3">
    <xsl:attribute name="font-size">10pt</xsl:attribute>
    <xsl:attribute name="text-align">left</xsl:attribute>
    <xsl:attribute name="font-style">italic</xsl:attribute>
    <xsl:attribute name="space-after">0pt</xsl:attribute>
    <xsl:attribute name="space-before.optimum">4pt</xsl:attribute>
    <xsl:attribute name="text-indent">
      <xsl:value-of select="$headingOutdent"/>
    </xsl:attribute>
  </xsl:template>
  <xd:doc class="style">
    <xd:short>[fo] Set attributes for display of heading for 4th level sections </xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template name="setupDiv4">
    <xsl:attribute name="font-size">10pt</xsl:attribute>
    <xsl:attribute name="space-before.optimum">4pt</xsl:attribute>
    <xsl:attribute name="text-indent">
      <xsl:value-of select="$headingOutdent"/>
    </xsl:attribute>
  </xsl:template>
  <xd:doc class="style">
    <xd:short>[fo] How to display the link text of a &lt;ptr&gt;</xd:short>
    <xd:param name="dest">the URL being linked to</xd:param>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template name="showXrefURL">
    <xsl:param name="dest"/>
    <xsl:value-of select="$dest"/>
  </xsl:template>
  <xd:doc type="string" class="style">
Calculation of small font size (add dimension)
</xd:doc>
  <xsl:param name="smallSize">
    <xsl:value-of select="$bodyMaster * 0.9"/>
    <xsl:text>pt</xsl:text>
  </xsl:param>
  <xd:doc type="string" class="style">
Create font size for tables, by reference to $bodyMaster
</xd:doc>
  <xsl:param name="tableSize">
    <xsl:value-of select="$bodyMaster * 0.9"/>
    <xsl:text>pt</xsl:text>
  </xsl:param>
  <xd:doc type="string" class="style">
Font size for display of title
</xd:doc>
  <xsl:param name="titleSize">16pt</xsl:param>
  <xd:doc type="string" class="style">
Font size for TOC heading
</xd:doc>
  <xsl:param name="tocSize">16pt</xsl:param>
  <xd:doc type="string" class="style">
Font for literal code
</xd:doc>
  <xsl:param name="typewriterFont">Courier</xsl:param>
  <xd:doc type="string" class="tables">
Default colour for background of table cells which are labelling rows
or columns</xd:doc>
  <xsl:param name="defaultCellLabelBackground">silver</xsl:param>
  <xd:doc type="boolean" class="tables">
Force tables to appear inline
</xd:doc>
  <xsl:param name="inlineTables">false</xsl:param>
  <xd:doc type="boolean" class="tables">
Put a caption on tables  
</xd:doc>
  <xsl:param name="makeTableCaption">true</xsl:param>
  <xd:doc type="string" class="tables">
Alignment of table captions
</xd:doc>
  <xsl:param name="tableCaptionAlign">center</xsl:param>
  <xd:doc class="tables">
    <xd:short>[fo] Set attributes for display of table </xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template name="tableCaptionstyle">
    <xsl:attribute name="text-align">center</xsl:attribute>
    <xsl:attribute name="font-style">italic</xsl:attribute>
    <xsl:attribute name="end-indent">
      <xsl:value-of select="$exampleMargin"/>
    </xsl:attribute>
    <xsl:attribute name="start-indent">
      <xsl:value-of select="$exampleMargin"/>
    </xsl:attribute>
    <xsl:attribute name="space-before">
      <xsl:value-of select="$spaceAroundTable"/>
    </xsl:attribute>
    <xsl:attribute name="space-after">
      <xsl:value-of select="$spaceBelowCaption"/>
    </xsl:attribute>
    <xsl:attribute name="keep-with-next">always</xsl:attribute>
  </xsl:template>
  <xd:doc type="string" class="tables">
Default padding on table cells
</xd:doc>
  <xsl:param name="tableCellPadding">2pt</xsl:param>
  <xd:doc type="string" class="toc">
Indentation for level 0 TOC entries
</xd:doc>
  <xsl:param name="div0Tocindent">0in</xsl:param>
  <xd:doc type="string" class="toc">
Indentation for level 1 TOC entries
</xd:doc>
  <xsl:param name="div1Tocindent">0.25in</xsl:param>
  <xd:doc type="string" class="toc">
Indentation for level 2 TOC entries
</xd:doc>
  <xsl:param name="div2Tocindent">0.5in</xsl:param>
  <xd:doc type="string" class="toc">
Indentation for level 3 TOC entries
</xd:doc>
  <xsl:param name="div3Tocindent">0.75in</xsl:param>
  <xd:doc type="string" class="toc">
Indentation for level 4 TOC entries
</xd:doc>
  <xsl:param name="div4Tocindent">1in</xsl:param>
  <xd:doc type="boolean" class="toc">
Make TOC for sections in &lt;back&gt;
</xd:doc>
  <xsl:param name="tocBack">true</xsl:param>
  <xd:doc type="boolean" class="toc">
Make TOC for sections in &lt;front&gt;
</xd:doc>
  <xsl:param name="tocFront">true</xsl:param>
  <xd:doc type="string" class="toc">
Punctuation to insert after a section number in a TOC
</xd:doc>
  <xsl:param name="tocNumberSuffix">. </xsl:param>
  <xd:doc type="integer" class="toc">
Page number on which TOC should start
</xd:doc>
  <xsl:param name="tocStartPage">1</xsl:param>
</xsl:stylesheet>
