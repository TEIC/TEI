<!-- 
TEI XSLT stylesheet family version 1.0
$Date$, $Revision$, $Author$

XSL FO stylesheet to format TEI XML documents 

##LICENSE
-->

<xsl:stylesheet
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0"
  xmlns:fo="http://www.w3.org/1999/XSL/Format"
  >

<!-- Parameterization. Lots more to do here -->
<xsl:param name="REQUEST"/>
<xsl:param name="STDOUT"/>

<!-- page setup -->
<xsl:param name="pageWidth">211mm</xsl:param>
<xsl:param name="pageHeight">297mm</xsl:param>
<xsl:param name="regionAfterExtent">14pt</xsl:param>
<xsl:param name="regionBeforeExtent">14pt</xsl:param>
<xsl:param name="bodyMarginBottom">24pt</xsl:param>
<xsl:param name="bodyMarginTop">24pt</xsl:param>
<xsl:param name="pageMarginTop">75pt</xsl:param>
<xsl:param name="pageMarginBottom">100pt</xsl:param>
<xsl:param name="pageMarginLeft">80pt</xsl:param>
<xsl:param name="pageMarginRight">150pt</xsl:param>
<xsl:param name="bodyFont">Times Roman</xsl:param>
<xsl:param name="divFont">Times Roman</xsl:param>
<xsl:param name="typewriterFont">Computer-Modern-Typewriter</xsl:param>
<xsl:param name="sansFont">Helvetica</xsl:param>
<xsl:param name="bodyMaster">10</xsl:param>
<xsl:param name="bodySize">
 <xsl:value-of select="$bodyMaster"/><xsl:text>pt</xsl:text>
</xsl:param>
<xsl:param name="smallSize">
 <xsl:value-of select="$bodyMaster * 0.9"/><xsl:text>pt</xsl:text>
</xsl:param>
<xsl:param name="flowMarginLeft"></xsl:param>
<xsl:param name="runSize">9pt</xsl:param>
<xsl:param name="runFont">sans-serif</xsl:param>

<xsl:param name="useHeaderFrontMatter"/>


<!-- general H&J setup -->
<xsl:param name="hyphenate">true</xsl:param>
<xsl:param name="alignment">justify</xsl:param>


<xsl:param name="footnoteSize">8pt</xsl:param>
<xsl:param name="footnotenumSize">7pt</xsl:param>
<xsl:param name="authorSize">14pt</xsl:param>
<xsl:param name="dateSize">14pt</xsl:param>
<xsl:param name="titleSize">16pt</xsl:param>

<xsl:param name="headingOutdent">-3em</xsl:param>
<xsl:template name="linkStyle">
  <xsl:attribute name="text-decoration">underline</xsl:attribute>
</xsl:template>
<xsl:param name="giColor">black</xsl:param>
<xsl:param name="identColor">black</xsl:param>
<xsl:param name="parIndent">1em</xsl:param>
<xsl:param name="parSkip">0pt</xsl:param>
<xsl:param name="parSkipmax">12pt</xsl:param>
<xsl:param name="activeLinebreaks"></xsl:param>
<xsl:param name="activePagebreaks"></xsl:param>

<!-- table of contents customization -->
<xsl:param name="tocSize">16pt</xsl:param>
<xsl:param name="div0Tocindent">0in</xsl:param>
<xsl:param name="div1Tocindent">0.25in</xsl:param>
<xsl:param name="div2Tocindent">0.5in</xsl:param>
<xsl:param name="div3Tocindent">0.75in</xsl:param>
<xsl:param name="div4Tocindent">1in</xsl:param>
<xsl:param name="tocFront">true</xsl:param>
<xsl:param name="tocBack">true</xsl:param>
<xsl:param name="headingNumberSuffix">. </xsl:param>
<xsl:param name="tocNumberSuffix">. </xsl:param>
<xsl:param name="tocStartPage">1</xsl:param>

<!-- list customization -->
<xsl:param name="listRightMargin">10pt</xsl:param>
<xsl:param name="listNormalIndent">15pt</xsl:param>
<xsl:param name="listLeftGlossIndent">0.5in</xsl:param>
<xsl:param name="listLeftGlossInnerIndent">0.25in</xsl:param>
<xsl:param name="listLeftIndent">15pt</xsl:param>
<xsl:param name="listItemsep">4pt</xsl:param>
<xsl:param name="listAbove-1">6pt</xsl:param>
<xsl:param name="listBelow-1">6pt</xsl:param>
<xsl:param name="listAbove-2">4pt</xsl:param>
<xsl:param name="listBelow-2">4pt</xsl:param>
<xsl:param name="listAbove-3">0pt</xsl:param>
<xsl:param name="listBelow-3">0pt</xsl:param>
<xsl:param name="listAbove-4">0pt</xsl:param>
<xsl:param name="listBelow-4">0pt</xsl:param>
<!--<xsl:param name="bulletOne">&#x2022;</xsl:param> -->
<xsl:param name="bulletOne">&#x2219;</xsl:param>
<xsl:param name="bulletTwo">&#x2013;</xsl:param>
<xsl:param name="bulletThree">&#x002A;</xsl:param>
<xsl:param name="bulletFour">&#x002B;</xsl:param>


<!-- example/tei:quotation setup -->
<xsl:param name="exampleSize">
 <xsl:value-of select="$bodyMaster * 0.8"/><xsl:text>pt</xsl:text>
</xsl:param>
<xsl:param name="exampleMargin">12pt</xsl:param>
<xsl:param name="exampleBefore">4pt</xsl:param>
<xsl:param name="exampleAfter">4pt</xsl:param>

<!-- do we want a separate title sheet -->
<xsl:param name="titlePage"></xsl:param>

<!-- style for OUCS -->
<xsl:param name="OUCS"></xsl:param>

<!-- use bookmarks option in hyperref -->
<xsl:param name="pdfBookmarks"></xsl:param>
<xsl:param name="divRunningheads"></xsl:param>

<!-- default language -->
<xsl:param name="language">en_US</xsl:param>

<!-- page layout -->
<xsl:param name="forcePageMaster"></xsl:param>
<xsl:param name="twoSided">true</xsl:param>
<xsl:param name="frontMulticolumns"></xsl:param>
<xsl:param name="bodyMulticolumns"></xsl:param>
<xsl:param name="backMulticolumns"></xsl:param>
<xsl:param name="columnCount">1</xsl:param>
<xsl:template name="hookDefinepagemasters"/>

<!-- running heads -->
<xsl:param name="sectionHeaders">true</xsl:param>

<!-- page numbering -->
<xsl:param name="formatFrontpage">i</xsl:param>
<xsl:param name="formatBodypage">1</xsl:param>
<xsl:param name="formatBackpage">1</xsl:param>
<xsl:param name="formatAppendix">A.1.</xsl:param>

<!-- section headings -->
<xsl:template name="divXRefHeading">
<xsl:param name="x">
      <xsl:apply-templates mode="section" select="tei:head"/>
</xsl:param>
  <xsl:text> (</xsl:text>
    <xsl:value-of select="normalize-space($x)"/>
  <xsl:text>)</xsl:text>
</xsl:template>

<xsl:template name="setupDiv0">
 <xsl:attribute name="font-size">18pt</xsl:attribute>
 <xsl:attribute name="text-align">left</xsl:attribute>
 <xsl:attribute name="font-weight">bold</xsl:attribute>
 <xsl:attribute name="space-after">6pt</xsl:attribute>
 <xsl:attribute name="space-before.optimum">12pt</xsl:attribute>
 <xsl:attribute name="text-indent"><xsl:value-of select="$headingOutdent"/></xsl:attribute>
</xsl:template>

<xsl:template name="setupDiv1">
 <xsl:attribute name="font-size">14pt</xsl:attribute>
 <xsl:attribute name="text-align">left</xsl:attribute>
 <xsl:attribute name="font-weight">bold</xsl:attribute>
 <xsl:attribute name="space-after">3pt</xsl:attribute>
 <xsl:attribute name="space-before.optimum">9pt</xsl:attribute>
 <xsl:attribute name="text-indent"><xsl:value-of select="$headingOutdent"/></xsl:attribute>
</xsl:template>

<xsl:template name="setupDiv2">
 <xsl:attribute name="font-size">12pt</xsl:attribute>
 <xsl:attribute name="text-align">left</xsl:attribute>
 <xsl:attribute name="font-weight">bold</xsl:attribute>
 <xsl:attribute name="font-style">italic</xsl:attribute>
 <xsl:attribute name="space-after">2pt</xsl:attribute>
 <xsl:attribute name="space-before.optimum">4pt</xsl:attribute>
 <xsl:attribute name="text-indent"><xsl:value-of select="$headingOutdent"/></xsl:attribute>
</xsl:template>

<xsl:template name="setupDiv3">
 <xsl:attribute name="font-size">10pt</xsl:attribute>
 <xsl:attribute name="text-align">left</xsl:attribute>
 <xsl:attribute name="font-style">italic</xsl:attribute>
 <xsl:attribute name="space-after">0pt</xsl:attribute>
 <xsl:attribute name="space-before.optimum">4pt</xsl:attribute>
 <xsl:attribute name="text-indent"><xsl:value-of select="$headingOutdent"/></xsl:attribute>
</xsl:template>

<xsl:template name="setupDiv4">
 <xsl:attribute name="font-size">10pt</xsl:attribute>
 <xsl:attribute name="space-before.optimum">4pt</xsl:attribute>
 <xsl:attribute name="text-indent"><xsl:value-of select="$headingOutdent"/></xsl:attribute>
</xsl:template>

<xsl:template name="blockStartHook"/>

<!-- pictures and graphics -->
<!-- allows for pictures in a subdirectory -->
<xsl:param name="graphicsPrefix"></xsl:param>
<xsl:param name="graphicsSuffix">.png</xsl:param>
<xsl:param name="autoScaleFigures"></xsl:param>
<xsl:param name="captionInlinefigures"></xsl:param>
<xsl:param name="xrefShowTitle"></xsl:param>
<xsl:param name="xrefShowHead"></xsl:param>
<xsl:param name="xrefShowPage"></xsl:param>

<xsl:template name="showXrefURL">
  <xsl:param name="dest"/>
</xsl:template>

<!-- alternate
<xsl:template name="showXrefURL">
  <xsl:param name="dest"/>
  <fo:inline font-family="{$typewriterFont}">
    <xsl:text> [</xsl:text>
    <xsl:value-of select="$dest"/>
    <xsl:text>]</xsl:text>
  </fo:inline>
</xsl:template>
-->

<xsl:param name="readColSpecFile"></xsl:param>

<!-- figures -->
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

<!-- bibliography customization -->
<xsl:param name="biblSize">16pt</xsl:param>
<xsl:param name="indentBibl">1em</xsl:param>
<xsl:param name="spaceBeforeBibl">4pt</xsl:param>
<xsl:param name="spaceAfterBibl">0pt</xsl:param>

<!-- table customization -->
<xsl:param name="inlineTables"></xsl:param>
<xsl:param name="tableCaptionAlign">center</xsl:param>
<xsl:param name="tableAlign">center</xsl:param>
<xsl:param name="rowAlign">left</xsl:param>
<xsl:param name="spaceBelowCaption">4pt</xsl:param>
<xsl:param name="spaceAroundTable">8pt</xsl:param>
<xsl:param name="tableCellPadding">2pt</xsl:param>
<xsl:param name="makeTableCaption">true</xsl:param>
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
<xsl:param name="tableSize">
 <xsl:value-of select="$bodyMaster * 0.9"/><xsl:text>pt</xsl:text>
</xsl:param>

<xsl:template name="backHook"/>

<xsl:template name="copyrightstatement">
      <fo:block  padding-start="1in" padding-end="1in"
        text-align="justify">This material
is copyrighted by the University of Oxford.</fo:block>
</xsl:template>

</xsl:stylesheet>
