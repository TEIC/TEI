<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet 
                xmlns="http://www.w3.org/1999/XSL/Format"
                xmlns:tei="http://www.tei-c.org/ns/1.0"
                xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                exclude-result-prefixes="tei"
                version="2.0">
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" scope="stylesheet" type="stylesheet">
      <desc>
         <p>
    TEI stylesheet
    customization module for 
      fo output.</p>
         <p>This software is dual-licensed:

1. Distributed under a Creative Commons Attribution-ShareAlike 3.0
Unported License http://creativecommons.org/licenses/by-sa/3.0/ 

2. http://www.opensource.org/licenses/BSD-2-Clause
		
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are
met:

* Redistributions of source code must retain the above copyright
notice, this list of conditions and the following disclaimer.

* Redistributions in binary form must reproduce the above copyright
notice, this list of conditions and the following disclaimer in the
documentation and/or other materials provided with the distribution.

This software is provided by the copyright holders and contributors
"as is" and any express or implied warranties, including, but not
limited to, the implied warranties of merchantability and fitness for
a particular purpose are disclaimed. In no event shall the copyright
holder or contributors be liable for any direct, indirect, incidental,
special, exemplary, or consequential damages (including, but not
limited to, procurement of substitute goods or services; loss of use,
data, or profits; or business interruption) however caused and on any
theory of liability, whether in contract, strict liability, or tort
(including negligence or otherwise) arising in any way out of the use
of this software, even if advised of the possibility of such damage.
</p>
         <p>Author: See AUTHORS</p>
         <p>Id: $Id$</p>
         <p>Copyright: 2011, TEI Consortium</p>
      </desc>
   </doc>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" class="figures" type="string">
      <desc>
How to scale figures if no width and height specified (pass to XSL FO content-width)
</desc>
   </doc>
  <xsl:param name="autoScaleFigures"/>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" class="figures" type="boolean">
      <desc>
Put captions on inline figures
</desc>
   </doc>
  <xsl:param name="captionInlineFigures">false</xsl:param>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" class="figures">
      <desc>[fo] Set attributes for display of figures</desc>
   </doc>
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
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" class="figures" type="boolean">
      <desc>
Show the contents of &lt;head&gt; in a cross-reference to table or figure
</desc>
   </doc>
  <xsl:param name="showFloatHead">false</xsl:param>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" class="figures" type="boolean">
      <desc>
Show a title for figures or tables (eg Table or Figure) in a cross-reference
</desc>
   </doc>
  <xsl:param name="showFloatLabel">false</xsl:param>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" class="figures" type="boolean">
      <desc>
Show the page number in a cross-reference to table or figure
</desc>
   </doc>
  <xsl:param name="xrefShowPage">false</xsl:param>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" class="hook">
      <desc>[fo] Hook where extra material can be inserted after the
    &lt;body&gt; has been processed</desc>
   </doc>
  <xsl:template name="afterBodyHook"/>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" class="hook">
      <desc>[fo] Hook where work can be done at the start of each block</desc>
   </doc>
  <xsl:template name="blockStartHook"/>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" class="hook">
      <desc>[fo] Hook where extra page masters can be defined</desc>
   </doc>
  <xsl:template name="pageMasterHook"/>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" class="layout" type="boolean">
      <desc>
Put back matter in multiple columns
</desc>
   </doc>
  <xsl:param name="backMulticolumns">false</xsl:param>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" class="layout" type="string">
      <desc>
Margin at bottom of text body
</desc>
   </doc>
  <xsl:param name="bodyMarginBottom">24pt</xsl:param>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" class="layout" type="string">
      <desc>
Margin at top of text body
</desc>
   </doc>
  <xsl:param name="bodyMarginTop">24pt</xsl:param>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" class="layout" type="boolean">
      <desc>
Put body matter in multiple columns
</desc>
   </doc>
  <xsl:param name="bodyMulticolumns">false</xsl:param>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" class="layout" type="string">
      <desc>
Symbol for 4th level itemized list
</desc>
   </doc>
  <xsl:param name="bulletFour">+</xsl:param>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" class="layout" type="string">
      <desc>
Symbol for top-level itemized list
</desc>
   </doc>
  <xsl:param name="bulletOne">•</xsl:param>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" class="layout" type="string">
      <desc>
Symbol for 3rd level itemized list
</desc>
   </doc>
  <xsl:param name="bulletThree">*</xsl:param>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" class="layout" type="string">
      <desc>
Symbol for 2nd level itemized list
</desc>
   </doc>
  <xsl:param name="bulletTwo">–</xsl:param>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" class="layout" type="integer">
      <desc>
Number of columns, when multiple-column work is requested
</desc>
   </doc>
  <xsl:param name="columnCount">1</xsl:param>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" class="layout" type="string">
      <desc>
XSL FO "provisional-distance-between starts"
</desc>
   </doc>
  <xsl:param name="betweenStarts">18pt</xsl:param>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" class="layout" type="string">
      <desc>
XSL FO "provisional-distance-between starts" for gloss lists
</desc>
   </doc>
  <xsl:param name="betweenGlossStarts">42pt</xsl:param>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" class="layout" type="string">
      <desc>
XSL FO "provisional-distance-between starts" for bibliographies
</desc>
   </doc>
  <xsl:param name="betweenBiblStarts">14pt</xsl:param>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" class="layout" type="boolean">
      <desc>
Display section headings in running heads
</desc>
   </doc>
  <xsl:param name="divRunningheads">false</xsl:param>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" class="layout" type="string">
      <desc>
Space below examples
</desc>
   </doc>
  <xsl:param name="exampleAfter">4pt</xsl:param>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" class="layout" type="string">
      <desc>
Space above examples
</desc>
   </doc>
  <xsl:param name="exampleBefore">4pt</xsl:param>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" class="layout" type="string">
      <desc>
Left margin for examples
</desc>
   </doc>
  <xsl:param name="exampleMargin">12pt</xsl:param>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" class="layout" type="string">
      <desc>
Left margin of flow
</desc>
   </doc>
  <xsl:param name="flowMarginLeft"/>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" class="layout" type="string">
      <desc>
Which named page master name to use
</desc>
   </doc>
  <xsl:param name="forcePageMaster"/>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" class="layout" type="string">
      <desc>
How to format page numbers in back matter (use XSLT number format)
</desc>
   </doc>
  <xsl:param name="formatBackpage">1</xsl:param>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" class="layout" type="string">
      <desc>
How to format page numbers in main matter (use XSLT number format)
</desc>
   </doc>
  <xsl:param name="formatBodypage">1</xsl:param>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" class="layout" type="string">
      <desc>
How to format page numbers in front matter (use XSLT number format)
</desc>
   </doc>
  <xsl:param name="formatFrontpage">i</xsl:param>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" class="layout" type="boolean">
      <desc>
Put front matter in multiple columns
</desc>
   </doc>
  <xsl:param name="frontMulticolumns">false</xsl:param>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" class="layout" type="string">
      <desc>
XSL FO "provisional-label-separation"
</desc>
   </doc>
  <xsl:param name="labelSeparation">6pt</xsl:param>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" class="layout" type="string">
      <desc>
Space above lists at top level
</desc>
   </doc>
  <xsl:param name="listAbove-1">6pt</xsl:param>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" class="layout" type="string">
      <desc>
Space above lists at 2nd level
</desc>
   </doc>
  <xsl:param name="listAbove-2">4pt</xsl:param>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" class="layout" type="string">
      <desc>
Space above lists at 3rd level
</desc>
   </doc>
  <xsl:param name="listAbove-3">0pt</xsl:param>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" class="layout" type="string">
      <desc>
Space above lists at 4th level
</desc>
   </doc>
  <xsl:param name="listAbove-4">0pt</xsl:param>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" class="layout" type="string">
      <desc>
Space below lists at top level
</desc>
   </doc>
  <xsl:param name="listBelow-1">6pt</xsl:param>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" class="layout" type="string">
      <desc>
Space below lists at 2nd level
</desc>
   </doc>
  <xsl:param name="listBelow-2">4pt</xsl:param>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" class="layout" type="string">
      <desc>
Space below lists at 3rd level
</desc>
   </doc>
  <xsl:param name="listBelow-3">0pt</xsl:param>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" class="layout" type="string">
      <desc>
Space below lists at 4th level
</desc>
   </doc>
  <xsl:param name="listBelow-4">0pt</xsl:param>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" class="layout" type="string">
      <desc>
Spacing between list items
</desc>
   </doc>
  <xsl:param name="listItemsep">4pt</xsl:param>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" class="layout" type="string">
      <desc>
Left margin for gloss lists
</desc>
   </doc>
  <xsl:param name="listLeftGlossIndent">0.5in</xsl:param>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" class="layout" type="string">
      <desc>
Left margin for nested gloss lists
</desc>
   </doc>
  <xsl:param name="listLeftGlossInnerIndent">0.25in</xsl:param>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" class="layout" type="string">
      <desc>
Indentation for lists
</desc>
   </doc>
  <xsl:param name="listLeftIndent">0pt</xsl:param>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" class="layout" type="string">
      <desc>
Right margin for lists
</desc>
   </doc>
  <xsl:param name="listRightMargin">10pt</xsl:param>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" class="layout" type="string">
      <desc>
Paper height
</desc>
   </doc>
  <xsl:param name="pageHeight">297mm</xsl:param>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" class="layout" type="string">
      <desc>
Margin at bottom of text area
</desc>
   </doc>
  <xsl:param name="pageMarginBottom">100pt</xsl:param>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" class="layout" type="string">
      <desc>
Left margin
</desc>
   </doc>
  <xsl:param name="pageMarginLeft">80pt</xsl:param>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" class="layout" type="string">
      <desc>
Right margin
</desc>
   </doc>
  <xsl:param name="pageMarginRight">150pt</xsl:param>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" class="layout" type="string">
      <desc>
Margin at top of text area
</desc>
   </doc>
  <xsl:param name="pageMarginTop">75pt</xsl:param>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" class="layout" type="string">
      <desc>
Paper width
</desc>
   </doc>
  <xsl:param name="pageWidth">211mm</xsl:param>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" class="layout" type="string">
      <desc>
Maximum space allowed between paragraphs
</desc>
   </doc>
  <xsl:param name="parSkipmax">12pt</xsl:param>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" class="layout" type="anyURI">
      <desc>
External XML file containing specifications for column sizes for
tables in document
</desc>
   </doc>
  <xsl:param name="readColSpecFile"/>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" class="layout" type="string">
      <desc>
Region after
</desc>
   </doc>
  <xsl:param name="regionAfterExtent">14pt</xsl:param>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" class="layout" type="string">
      <desc>
Region before
</desc>
   </doc>
  <xsl:param name="regionBeforeExtent">14pt</xsl:param>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" class="layout" type="boolean">
      <desc>
Construct running headers from page number and section headings
</desc>
   </doc>
  <xsl:param name="sectionHeaders">true</xsl:param>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" class="layout" type="string">
      <desc>
Space after bibliography
</desc>
   </doc>
  <xsl:param name="spaceAfterBibl">0pt</xsl:param>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" class="layout" type="string">
      <desc>
Space above and below a table
</desc>
   </doc>
  <xsl:param name="spaceAroundTable">8pt</xsl:param>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" class="layout" type="string">
      <desc>
Space above bibliography
</desc>
   </doc>
  <xsl:param name="spaceBeforeBibl">4pt</xsl:param>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" class="layout" type="string">
      <desc>
Space below caption of figure or table
</desc>
   </doc>
  <xsl:param name="spaceBelowCaption">4pt</xsl:param>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" class="layout" type="boolean">
      <desc>
Make title page
</desc>
   </doc>
  <xsl:param name="titlePage">true</xsl:param>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" class="layout" type="boolean">
      <desc>
Make 2-page spreads
</desc>
   </doc>
  <xsl:param name="twoSided">true</xsl:param>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" class="output" type="string">
      <desc>
Language (for hyphenation)
</desc>
   </doc>
  <xsl:param name="language">en_US</xsl:param>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" class="output" type="string">
      <desc>
         <p>Name of intended XSL FO engine</p>
         <p>This is used to tailor the result for different XSL FO processors.
By default, no special measures are taken, so
there are no bookmarks or other such features. Possible values are
<ul xmlns="http://www.w3.org/1999/XSL/Format">
               <li>passivetex (the TeX-based PassiveTeX processor)</li>
               <li>xep   (XEP)</li>
               <li>fop (FOP)</li>
               <li>antenna  (Antenna House)</li>
            </ul>
         </p>
      </desc>
   </doc>
  <xsl:param name="foEngine"/>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" class="style" type="boolean">
      <desc>
Make &lt;lb&gt; active (ie cause a line break)
</desc>
   </doc>
  <xsl:param name="activeLinebreaks">true</xsl:param>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" class="style" type="string">
      <desc>
Alignment of text (ie justified or ragged)
</desc>
   </doc>
  <xsl:param name="alignment">justify</xsl:param>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" class="style" type="string">
      <desc>
Font size for bibliography
</desc>
   </doc>
  <xsl:param name="biblSize">16pt</xsl:param>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" class="style" type="string">
      <desc>
Default font for body
</desc>
   </doc>
  <xsl:param name="bodyFont">Times</xsl:param>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" class="style" type="string">
      <desc>
Default font size for body (without dimension)
</desc>
   </doc>
  <xsl:param name="bodyMaster">10</xsl:param>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" class="style" type="string">
      <desc>
Calculation of normal body font size (add dimension)
</desc>
   </doc>
  <xsl:param name="bodySize">
      <xsl:value-of select="$bodyMaster"/>
      <xsl:text>pt</xsl:text>
  </xsl:param>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" class="style" type="string">
      <desc>
Font for section headings
</desc>
   </doc>
  <xsl:param name="divFont">Times</xsl:param>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" class="style">
      <desc>[fo] How to display section headings in a cross-reference <param name="head">section title</param>
      </desc>
   </doc>
  <xsl:template name="divXRefHeading">
      <xsl:param name="head">
         <xsl:apply-templates mode="section" select="tei:head"/>
      </xsl:param>
      <xsl:text> (</xsl:text>
      <xsl:value-of select="normalize-space($head)"/>
      <xsl:text>)</xsl:text>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" class="style" type="string">
      <desc>
Colour for display of &lt;eg&gt; blocks.
</desc>
   </doc>
  <xsl:param name="exampleColor">black</xsl:param>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" class="style" type="string">
      <desc>
Colour for background display of &lt;eg&gt; blocks.
</desc>
   </doc>
  <xsl:param name="exampleBackgroundColor">gray</xsl:param>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" class="style" type="string">
      <desc>
Calculation of font size for examples (add dimension)
</desc>
   </doc>
  <xsl:param name="exampleSize">
      <xsl:value-of select="$bodyMaster * 0.6"/>
      <xsl:text>pt</xsl:text>
  </xsl:param>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" class="style" type="string">
      <desc>
Calculation of font size for quotations
</desc>
   </doc>
  <xsl:param name="quoteSize">
      <xsl:value-of select="$bodyMaster * 0.9"/>
      <xsl:text>pt</xsl:text>
  </xsl:param>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" class="style" type="string">
      <desc>
Font size for footnotes
</desc>
   </doc>
  <xsl:param name="footnoteSize">
      <xsl:value-of select="$bodyMaster * 0.8"/>
  </xsl:param>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" class="style" type="string">
      <desc>
Font size for footnote numbers
</desc>
   </doc>
  <xsl:param name="footnotenumSize">
    <xsl:value-of select="$bodyMaster * 0.7"/>
  </xsl:param>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" class="style" type="string">
      <desc>
Colour for display of element names
</desc>
   </doc>
  <xsl:param name="giColor">black</xsl:param>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" class="style" type="string">
      <desc>
Indentation of headings
</desc>
   </doc>
  <xsl:param name="headingOutdent">0em</xsl:param>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" class="style" type="boolean">
      <desc>
Hyphenate text
</desc>
   </doc>
  <xsl:param name="hyphenate">true</xsl:param>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" class="style" type="string">
      <desc>
Colour for display of &lt;ident&gt; values
    Customization parameter.
</desc>
   </doc>
  <xsl:param name="identColor">black</xsl:param>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" class="style">
      <desc>[fo] Set attributes for display of links</desc>
   </doc>
  <xsl:template name="linkStyle">
      <xsl:attribute name="text-decoration">underline</xsl:attribute>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" class="style" type="string">
      <desc>
Font family for running header and footer
</desc>
   </doc>
  <xsl:param name="runFont">sans-serif</xsl:param>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" class="style" type="string">
      <desc>
Font size for running header and footer
</desc>
   </doc>
  <xsl:param name="runSize">9pt</xsl:param>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" class="style" type="string">
      <desc>
Sans-serif font
</desc>
   </doc>
  <xsl:param name="sansFont">Helvetica</xsl:param>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" class="style">
      <desc>[fo] Set attributes for display of heading for chapters (level 0)</desc>
   </doc>
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
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" class="style">
      <desc>[fo] Set attributes for display of heading for 1st level sections</desc>
   </doc>
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
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" class="style">
      <desc>[fo] Set attributes for display of heading for 2nd level sections </desc>
   </doc>
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
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" class="style">
      <desc>[fo]Set attributes for display of heading for 3rd level sections </desc>
   </doc>
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
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" class="style">
      <desc>[fo] Set attributes for display of heading for 4th level sections </desc>
   </doc>
  <xsl:template name="setupDiv4">
      <xsl:attribute name="font-size">10pt</xsl:attribute>
      <xsl:attribute name="text-align">left</xsl:attribute>
      <xsl:attribute name="font-style">italic</xsl:attribute>
      <xsl:attribute name="space-after">0pt</xsl:attribute>
      <xsl:attribute name="space-before.optimum">4pt</xsl:attribute>
      <xsl:attribute name="text-indent">
         <xsl:value-of select="$headingOutdent"/>
      </xsl:attribute>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" class="style">
      <desc>[fo] Set attributes for display of heading for 5th level sections </desc>
   </doc>
  <xsl:template name="setupDiv5">
      <xsl:attribute name="font-size">10pt</xsl:attribute>
      <xsl:attribute name="text-align">left</xsl:attribute>
      <xsl:attribute name="font-style">italic</xsl:attribute>
      <xsl:attribute name="space-after">0pt</xsl:attribute>
      <xsl:attribute name="space-before.optimum">4pt</xsl:attribute>
      <xsl:attribute name="text-indent">
         <xsl:value-of select="$headingOutdent"/>
      </xsl:attribute>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" class="style">
      <desc>[fo] Set attributes for display of heading for 6th level sections </desc>
   </doc>
  <xsl:template name="setupDiv6">
      <xsl:attribute name="font-size">10pt</xsl:attribute>
      <xsl:attribute name="text-align">left</xsl:attribute>
      <xsl:attribute name="font-style">italic</xsl:attribute>
      <xsl:attribute name="space-after">0pt</xsl:attribute>
      <xsl:attribute name="space-before.optimum">4pt</xsl:attribute>
      <xsl:attribute name="text-indent">
         <xsl:value-of select="$headingOutdent"/>
      </xsl:attribute>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" class="style">
      <desc>[fo] How to display the link text of a &lt;ptr&gt;<param name="dest">the URL being linked to</param>
      </desc>
   </doc>
  <xsl:template name="showXrefURL">
      <xsl:param name="dest"/>
      <xsl:value-of select="$dest"/>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" class="style" type="string">
      <desc>
Calculation of small font size (add dimension)
</desc>
   </doc>
  <xsl:param name="smallSize">
      <xsl:value-of select="$bodyMaster * 0.9"/>
      <xsl:text>pt</xsl:text>
  </xsl:param>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" class="style" type="string">
      <desc>
Create font size for tables, by reference to $bodyMaster
</desc>
   </doc>
  <xsl:param name="tableSize">
      <xsl:value-of select="$bodyMaster * 0.9"/>
      <xsl:text>pt</xsl:text>
  </xsl:param>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" class="style" type="string">
      <desc>
Font size for TOC heading
</desc>
   </doc>
  <xsl:param name="tocSize">16pt</xsl:param>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" class="style" type="string">
      <desc>
Font for literal code
</desc>
   </doc>
  <xsl:param name="typewriterFont">Courier</xsl:param>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" class="tables" type="string">
      <desc>
Default colour for background of table cells which are labelling rows
or columns</desc>
   </doc>
  <xsl:param name="defaultCellLabelBackground">silver</xsl:param>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" class="tables" type="boolean">
      <desc>
Force tables to appear inline
</desc>
   </doc>
  <xsl:param name="inlineTables">false</xsl:param>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" class="tables" type="boolean">
      <desc>
Put a caption on tables  
</desc>
   </doc>
  <xsl:param name="makeTableCaption">true</xsl:param>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" class="tables" type="string">
      <desc>
Alignment of table captions
</desc>
   </doc>
  <xsl:param name="tableCaptionAlign">center</xsl:param>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" class="tables">
      <desc>[fo] Set attributes for display of table </desc>
   </doc>
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
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" class="tables" type="string">
      <desc>
Default padding on table cells
</desc>
   </doc>
  <xsl:param name="tableCellPadding">2pt</xsl:param>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" class="toc" type="string">
      <desc>
Indentation for level 0 TOC entries
</desc>
   </doc>
  <xsl:param name="div0Tocindent">0in</xsl:param>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" class="toc" type="string">
      <desc>
Indentation for level 1 TOC entries
</desc>
   </doc>
  <xsl:param name="div1Tocindent">0.25in</xsl:param>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" class="toc" type="string">
      <desc>
Indentation for level 2 TOC entries
</desc>
   </doc>
  <xsl:param name="div2Tocindent">0.5in</xsl:param>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" class="toc" type="string">
      <desc>
Indentation for level 3 TOC entries
</desc>
   </doc>
  <xsl:param name="div3Tocindent">0.75in</xsl:param>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" class="toc" type="string">
      <desc>
Indentation for level 4 TOC entries
</desc>
   </doc>
  <xsl:param name="div4Tocindent">1in</xsl:param>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" class="toc" type="string">
      <desc>
    Indentation for level 5 TOC entries
  </desc>
   </doc>
  <xsl:param name="div5Tocindent">1.25in</xsl:param>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" class="toc" type="boolean">
      <desc>
Make TOC for sections in &lt;back&gt;
</desc>
   </doc>
  <xsl:param name="tocBack">true</xsl:param>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" class="toc" type="boolean">
      <desc>
Make TOC for sections in &lt;front&gt;
</desc>
   </doc>
  <xsl:param name="tocFront">true</xsl:param>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" class="toc" type="string">
      <desc>
Punctuation to insert after a section number in a TOC
</desc>
   </doc>
  <xsl:param name="tocNumberSuffix">. </xsl:param>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" class="toc" type="integer">
      <desc>
Page number on which TOC should start
</desc>
   </doc>
  <xsl:param name="tocStartPage">1</xsl:param>
</xsl:stylesheet>