<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet xmlns:a="http://schemas.openxmlformats.org/drawingml/2006/main" xmlns:cals="http://www.oasis-open.org/specs/tm9901" xmlns:contypes="http://schemas.openxmlformats.org/package/2006/content-types" xmlns:cp="http://schemas.openxmlformats.org/package/2006/metadata/core-properties" xmlns:dc="http://purl.org/dc/elements/1.1/" xmlns:dcmitype="http://purl.org/dc/dcmitype/" xmlns:dcterms="http://purl.org/dc/terms/" xmlns:html="http://www.w3.org/1999/xhtml" xmlns:iso="http://www.iso.org/ns/1.0" xmlns:m="http://schemas.openxmlformats.org/officeDocument/2006/math" xmlns:mml="http://www.w3.org/1998/Math/MathML" xmlns:o="urn:schemas-microsoft-com:office:office" xmlns:pic="http://schemas.openxmlformats.org/drawingml/2006/picture" xmlns:r="http://schemas.openxmlformats.org/officeDocument/2006/relationships" xmlns:tbx="http://www.lisa.org/TBX-Specification.33.0.html" xmlns:tei="http://www.tei-c.org/ns/1.0" xmlns:teidocx="http://www.tei-c.org/ns/teidocx/1.0" xmlns:teix="http://www.tei-c.org/ns/Examples" xmlns:v="urn:schemas-microsoft-com:vml" xmlns:fn="http://www.w3.org/2005/02/xpath-functions" xmlns:ve="http://schemas.openxmlformats.org/markup-compatibility/2006" xmlns:w10="urn:schemas-microsoft-com:office:word" xmlns:w="http://schemas.openxmlformats.org/wordprocessingml/2006/main" xmlns:wne="http://schemas.microsoft.com/office/word/2006/wordml" xmlns:wp="http://schemas.openxmlformats.org/drawingml/2006/wordprocessingDrawing" xmlns:its="http://www.w3.org/2005/11/its" xmlns:xs="http://www.w3.org/2001/XMLSchema" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="2.0" exclude-result-prefixes="cp ve o r m v wp w10 w wne mml tbx iso its tei a xs pic fn xsi dc dcterms dcmitype contypes teidocx teix html cals">
  <xsl:import href="placeholders.xsl"/>
  <xsl:import href="../../common2/functions.xsl"/>
  <xsl:import href="../utils/functions.xsl"/>
  <xsl:import href="../utils/variables.xsl"/>
  <xsl:import href="../utils/identity/identity.xsl"/>
  <xsl:import href="../utils/verbatim/tei-docx-verbatim.xsl"/>
  <xsl:import href="../utils/maths/mml2omml.xsl"/>
  <xsl:import href="../../common2/core.xsl"/>
  <xsl:import href="../../common2/msdescription.xsl"/>
  <!-- Deals with dynamic text creation such as toc -->
  <xsl:include href="dynamic/dynamic.xsl"/>
  <!-- Templates transforming graphic elements -->
  <xsl:include href="graphics/graphics.xsl"/>
  <!-- Templates transforming elements from drama -->
  <xsl:include href="drama/drama.xsl"/>
  <!-- Templates transforming lists -->
  <xsl:include href="lists/lists.xsl"/>
  <!-- Templates transforming math elements -->
  <xsl:include href="maths/maths.xsl"/>
  <!-- Templates transforming ODD-related elements -->
  <xsl:include href="odds/tagdocs.xsl"/>
  <!-- Templates transforming tei milestones into sectPr -->
  <xsl:include href="wordsections/wordsections.xsl"/>
  <!-- Load stylesheets helping with the creation of special files -->
  <xsl:include href="docxfiles/application.xsl"/>
  <xsl:include href="docxfiles/content-types.xsl"/>
  <xsl:include href="docxfiles/endnotes.xsl"/>
  <xsl:include href="docxfiles/footers.xsl"/>
  <xsl:include href="docxfiles/comments.xsl"/>
  <xsl:include href="docxfiles/footnotes.xsl"/>
  <xsl:include href="docxfiles/headers.xsl"/>
  <xsl:include href="docxfiles/numbering-definition.xsl"/>
  <xsl:include href="docxfiles/relationships.xsl"/>
  <xsl:include href="docxfiles/settings.xsl"/>
  <xsl:param name="createanttask">false</xsl:param>
  <xsl:param name="useHeaderFrontMatter">false</xsl:param>
  <xsl:param name="useFixedDate">false</xsl:param>
  <!--
        A4 is 210mm x 297mm; leaving 1in margin (25mm),
        gives 160 x 247 approx useable area. For figures,
	Microsoft use English metrical units (emu), in which
        1mm = 3600 units. So page size is 57600 * 889200
        Divide by 100 to avoid overflow in calculations.

        For other measurements in Word, see useful discussion at
	http://startbigthinksmall.wordpress.com/2010/01/04/points-inches-and-emus-measuring-units-in-office-open-xml/
    -->
  <xsl:param name="pageWidth">576</xsl:param>
  <xsl:param name="pageHeight">890</xsl:param>
  <xsl:param name="tableWidthPercentage"></xsl:param>
  <xsl:param name="defaultHeaderFooterFile">templates/default.xml</xsl:param>
  <xsl:param name="postQuote">’</xsl:param>
  <xsl:param name="preQuote">‘</xsl:param>
  <xsl:param name="bulletOne"></xsl:param>
  <xsl:param name="bulletTwo">•</xsl:param>
  <xsl:param name="bulletThree">*</xsl:param>
  <xsl:param name="bulletFour">+</xsl:param>
  <xsl:param name="bulletFive">•</xsl:param>
  <xsl:param name="bulletSix">•</xsl:param>
  <xsl:param name="bulletSeven">•</xsl:param>
  <xsl:param name="bulletEight">•</xsl:param>
  <xsl:param name="word-directory">..</xsl:param>
  <xsl:param name="inputDir">.</xsl:param>
  <xsl:param name="debug">false</xsl:param>
  <xsl:param name="styleDoc">
    <xsl:value-of select="concat($wordDirectory, '/word/styles.xml')"/>
  </xsl:param>
  <xsl:param name="docDoc">
    <xsl:value-of select="concat($wordDirectory, '/word/document.xml')"/>
  </xsl:param>
  <xsl:param name="shadowGraphics">false</xsl:param>
  <xsl:param name="alignFigures">center</xsl:param>
  <xsl:param name="renderAddDel">true</xsl:param>
  <xsl:param name="addColour">red</xsl:param>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" scope="stylesheet" type="stylesheet">
    <desc>
      <p> TEI stylesheet for making Word docx files from TEI XML </p>
      <p><h1 xmlns="">Stylesheet documentation</h1><h2 xmlns="">template modes</h2><h3 xmlns="">get-style</h3><h1 xmlns="">License</h1>This software is dual-licensed:

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
      <p>Copyright: 2008, TEI Consortium</p>
    </desc>
  </doc>
  <xsl:strip-space elements="*"/>
  <xsl:preserve-space elements="tei:text"/>
  <xsl:output method="xml" version="1.0" encoding="UTF-8"/>
  <xsl:param name="isofreestanding">true</xsl:param>
  <xsl:key name="FOOTERS" match="tei:fw[@type='footer']" use="@xml:id"/>
  <xsl:key name="HEADERS" match="tei:fw[@type='header']" use="@xml:id"/>
  <xsl:key name="ALLFOOTERS" match="tei:fw[@type='footer']" use="1"/>
  <xsl:key name="ALLHEADERS" match="tei:fw[@type='header']" use="1"/>
  <xsl:key name="ENDNOTES" match="tei:note[@place='end']" use="1"/>
  <xsl:key name="FOOTNOTES" match="tei:note[@place='foot' or @place='bottom' ]" use="1"/>
  <xsl:key name="OL" match="tei:list[@type='ordered']" use="1"/>
  <xsl:key name="BLIP" match="a:blip" use="1"/>
  <xsl:key name="Styles" match="w:style/w:name" use="@w:val"/>
  <xsl:variable name="align">right</xsl:variable>
  <xsl:variable name="wordDirectory">
    <xsl:value-of select="translate($word-directory,'\\','/')"/>
  </xsl:variable>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc>
        Generic template from msDescription for high-level section
    </desc>
  </doc>
  <xsl:template name="msSection">
    <xsl:param name="level"/>
    <xsl:param name="implicitBlock"/>
    <xsl:param name="heading"/>
    <w:p>
      <w:pPr>
        <w:pStyle w:val="tei{local-name()}"/>
      </w:pPr>
      <w:r>
        <w:t>
          <xsl:value-of select="$heading"/>
        </w:t>
      </w:r>
    </w:p>
    <xsl:call-template name="block-element"/>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc>
        Generic template from msDescription for inline objects
    </desc>
  </doc>
  <xsl:template name="msInline">
    <xsl:param name="before"/>
    <xsl:param name="after"/>
    <xsl:param name="style"/>
    <w:r>
      <w:rPr>
        <w:rStyle w:val="tei{local-name()}"/>
        <xsl:choose>
          <xsl:when test="$style='italic'">
            <w:i/>
          </xsl:when>
          <xsl:when test="$style='bold'">
            <w:b/>
          </xsl:when>
        </xsl:choose>
        <xsl:if test="$renderAddDel='true' and ancestor-or-self::tei:del">
          <w:strike/>
        </xsl:if>
      </w:rPr>
      <w:t>
        <xsl:value-of select="$before"/>
        <xsl:value-of select="."/>
        <xsl:value-of select="$after"/>
      </w:t>
    </w:r>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc>
        Generic template from msDescription for mid-level block
    </desc>
  </doc>
  <xsl:template name="msBlock">
    <xsl:param name="style"/>
    <xsl:call-template name="block-element">
      <xsl:with-param name="style">
        <xsl:value-of select="$style"/>
      </xsl:with-param>
    </xsl:call-template>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc>
        Generic template from msDescription for labelled object
    </desc>
  </doc>
  <xsl:template name="msLabelled">
    <xsl:param name="before"/>
    <w:r>
      <w:rPr>
        <w:i/>
      </w:rPr>
      <w:t>
        <xsl:attribute name="xml:space">preserve</xsl:attribute>
        <xsl:value-of select="$before"/>
        <xsl:text>: </xsl:text>
      </w:t>
    </w:r>
    <w:r>
      <w:rPr>
        <w:rStyle w:val="tei{local-name()}"/>
      </w:rPr>
      <w:t>
        <xsl:value-of select="."/>
      </w:t>
    </w:r>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc>
        Generic template from msDescription for literal text
    </desc>
  </doc>
  <xsl:template name="msLiteral">
    <xsl:param name="text"/>
    <w:r>
      <w:rPr/>
      <w:t>
        <xsl:attribute name="xml:space">preserve</xsl:attribute>
        <xsl:value-of select="$text"/>
      </w:t>
    </w:r>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc>
        The starting point in the conversion to docx.
    </desc>
  </doc>
  <xsl:template match="/tei:TEI">
    <xsl:call-template name="write-docxfiles"/>
    <xsl:call-template name="create-document-dot-xml"/>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc>
        Calls templates that are responsible for creating all necessary files besides the
            main document.xml
    </desc>
  </doc>
  <xsl:template name="write-docxfiles">
    <!-- header and footers -->
    <xsl:call-template name="write-docxfile-header-files"/>
    <!-- footer files -->
    <xsl:call-template name="write-docxfile-footer-files"/>
    <!-- numbering file -->
    <xsl:call-template name="write-docxfile-numbering-definition"/>
    <!-- footnotes file -->
    <xsl:call-template name="write-docxfile-footnotes-file"/>
    <!-- endnotes file -->
    <xsl:call-template name="write-docxfile-endnotes-file"/>
    <!-- comments file -->
    <xsl:call-template name="write-docxfile-comments-file"/>
    <!-- main relationships -->
    <xsl:call-template name="write-docxfile-main-relationships"/>
    <!-- relationships -->
    <xsl:call-template name="write-docxfile-relationships"/>
    <!-- write Content Types -->
    <xsl:call-template name="write-docxfile-content-types"/>
    <!-- settings -->
    <xsl:call-template name="write-docxfile-settings"/>
    <!-- app files -->
    <xsl:call-template name="write-docxfile-docprops-core"/>
    <xsl:call-template name="write-docxfile-docprops-app"/>
    <xsl:call-template name="write-docxfile-docprops-custom"/>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc>
        Create the contents of the main document.xml file, that contains all "real" content.
    </desc>
  </doc>
  <xsl:template name="create-document-dot-xml">
    <w:document>
      <w:body>
        <!-- Front -->
        <xsl:call-template name="write-document-dot-xml-frontmatter"/>
        <!-- Main -->
        <xsl:call-template name="write-document-dot-xml-maincontent"/>
        <!-- Back -->
        <xsl:call-template name="write-document-dot-xml-backmatter"/>
        <!-- Clearing up at the end -->
        <xsl:call-template name="write-document-dot-xml-postclearing"/>
      </w:body>
    </w:document>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc>
        Handles the front matter
    </desc>
  </doc>
  <xsl:template name="write-document-dot-xml-frontmatter">
    <!-- are there specific titlepages -->
    <xsl:call-template name="titlepages"/>
    <!-- header components -->
    <xsl:call-template name="headerParts"/>
    <!-- The front matter -->
    <xsl:apply-templates select="tei:text/tei:front"/>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc>
        Handles the main content
    </desc>
  </doc>
  <xsl:template name="write-document-dot-xml-maincontent">
    <!-- document title -->
    <xsl:call-template name="document-title"/>
    <!-- Describes the main part of the document -->
    <xsl:apply-templates select="tei:text/tei:body"/>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc>
        Handles the back matter
    </desc>
  </doc>
  <xsl:template name="write-document-dot-xml-backmatter">
    <!-- Describes the back matter of the document -->
    <xsl:apply-templates select="tei:text/tei:back"/>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc>
      <p>Inserts final word specific sections</p>
      <p> Inserts a final sectPr section if we need one </p>
    </desc>
  </doc>
  <xsl:template name="write-document-dot-xml-postclearing">
    <!-- write out final sectPr .. if exists -->
    <xsl:choose>
      <xsl:when test="tei:text/tei:milestone">
        <xsl:apply-templates select="tei:text/tei:milestone[1]">
          <xsl:with-param name="final-section">true</xsl:with-param>
        </xsl:apply-templates>
      </xsl:when>
      <xsl:otherwise>
        <xsl:apply-templates select="document($defaultHeaderFooterFile)/tei:TEI/tei:text/tei:milestone">
          <xsl:with-param name="final-section">true</xsl:with-param>
        </xsl:apply-templates>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc>
      <p>Template used to process block elements:</p>
      <ul>
        <li><b>style</b>: A style for all the <gi>w:p</gi>s</li>
        <li><b>pPr</b>: An entire <gi>pPr</gi> element to use </li>
        <li><b>nop</b>: a w:p has already been created and no new w:p is to be created </li>
        <li><b>bookmark-id</b>: if present in conjunction with bookmark-name, a bookmark is created around the current element </li>
        <li><b>bookmark-name</b>: see bookmark-id </li>
      </ul>
    </desc>
  </doc>
  <xsl:template name="block-element">
    <xsl:param name="style"/>
    <xsl:param name="select" select="."/>
    <xsl:param name="pPr"/>
    <xsl:param name="nop">false</xsl:param>
    <xsl:param name="bookmark-id"/>
    <xsl:param name="bookmark-name"/>
    <xsl:for-each select="$select">
      <xsl:for-each-group select="*|processing-instruction()|text()" group-adjacent="1">
        <xsl:call-template name="_process-blockelement">
          <xsl:with-param name="style" select="$style"/>
          <xsl:with-param name="pPr" select="$pPr"/>
          <xsl:with-param name="nop" select="$nop"/>
          <xsl:with-param name="bookmark-id" select="$bookmark-id"/>
          <xsl:with-param name="bookmark-name" select="$bookmark-name"/>
        </xsl:call-template>
      </xsl:for-each-group>
    </xsl:for-each>
  </xsl:template>
  <!-- end template block-element -->
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc>
      <p>
            This template processes block elements (or better to say the children of a block element)
            and should never be called directly 
            (call block-element instead). The function processes all children and puts
            all inline elements into one w:p. If it encounters a nested block element
            (e.g. a note inside a p) then it closes the previous w:p processes that block
            element and then again starts putting all following inline elements into another
            w:p element.
            
            @see block-element
        </p>
    </desc>
  </doc>
  <xsl:template name="_process-blockelement">
    <xsl:param name="style"/>
    <xsl:param name="pPr"/>
    <xsl:param name="nop"/>
    <xsl:param name="bookmark-id"/>
    <xsl:param name="bookmark-name"/>
    <!-- Process Child elements -->
    <xsl:for-each-group select="current-group()" group-starting-with="*[not(tei:is-inline(.))]">
      <xsl:choose>
        <!-- if the current item is a block element, we process that one,
                     and then take call this function recursively was all the other
                     elements -->
        <xsl:when test="self::*[not(tei:is-inline(.))]">
          <!-- process block element -->
          <xsl:apply-templates select=".">
            <xsl:with-param name="style" select="$style"/>
            <xsl:with-param name="pPr" select="$pPr"/>
            <xsl:with-param name="nop" select="$nop"/>
          </xsl:apply-templates>
          <!-- process all the other elements in the current group -->
          <xsl:for-each-group select="current-group() except ." group-adjacent="1">
            <xsl:call-template name="_process-blockelement">
              <xsl:with-param name="style" select="$style"/>
              <xsl:with-param name="pPr" select="$pPr"/>
              <xsl:with-param name="nop" select="$nop"/>
            </xsl:call-template>
          </xsl:for-each-group>
        </xsl:when>
        <!-- we encountered an inline element. This means that the current group only
                     contains inline elements -->
        <xsl:otherwise>
          <!--
	  <xsl:message>+@@ <xsl:value-of select="name()"/>: pPr:	  <xsl:if test="not(empty($pPr))"><xsl:copy-of
	  select="$pPr"/></xsl:if>; style: <xsl:if
	  test="not(empty($style))"><xsl:copy-of
	  select="$style"/></xsl:if></xsl:message>
-->
          <!-- create all text runs for each item in the current group. we will later
                         on decide whether we are grouping them together in a w:p or not. -->
          <xsl:variable name="innerRuns">
            <!-- add paragraph properties (if nobody else created a w:p ($nop)) -->
            <xsl:if test="$nop!='true'">
              <xsl:choose>
                <xsl:when test="not($style='')">
                  <w:pPr>
                    <w:pStyle>
                      <xsl:attribute name="w:val" select="$style"/>
                    </w:pStyle>
                  </w:pPr>
                </xsl:when>
                <xsl:when test="not(empty($pPr))">
                  <xsl:copy-of select="$pPr"/>
                </xsl:when>
              </xsl:choose>
            </xsl:if>
            <!-- bookmark start -->
            <xsl:if test="string-length($bookmark-name) &gt; 0 and string-length($bookmark-id) &gt; 0">
              <w:bookmarkStart w:id="{$bookmark-id}" w:name="{$bookmark-name}"/>
            </xsl:if>
            <!-- Create text runs -->
            <xsl:for-each select="current-group()">
              <xsl:apply-templates select=".">
                <xsl:with-param name="style" select="$style"/>
                <xsl:with-param name="pPr" select="$pPr"/>
              </xsl:apply-templates>
            </xsl:for-each>
            <!-- bookmark end-->
            <xsl:if test="string-length($bookmark-name) &gt; 0 and string-length($bookmark-id) &gt; 0">
              <w:bookmarkEnd w:id="{$bookmark-id}"/>
            </xsl:if>
          </xsl:variable>
          <!-- write out text runs.
                         if someone has already created a w:p ($nop) we may not create another one. -->
          <xsl:choose>
            <xsl:when test="$nop='true'">
              <xsl:copy-of select="$innerRuns"/>
            </xsl:when>
            <xsl:otherwise>
              <w:p>
                <xsl:copy-of select="$innerRuns"/>
              </w:p>
            </xsl:otherwise>
          </xsl:choose>
        </xsl:otherwise>
      </xsl:choose>
    </xsl:for-each-group>
    <!-- end process children -->
  </xsl:template>
  <!-- end template _process-blockelement -->
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc>
        Anchors
    </desc>
  </doc>
  <xsl:template match="tei:anchor">
    <xsl:variable name="N">
      <xsl:number level="any"/>
    </xsl:variable>
    <w:bookmarkStart w:id="{number($N) + 20000}" w:name="{@xml:id}"/>
    <w:bookmarkEnd w:id="{number($N) + 20000}"/>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc>
        Handles text sections. Adds a bookmark if they are the
	first text in this object. 
    </desc>
  </doc>
  <xsl:template match="text()">
    <xsl:param name="character-style"/>
    <xsl:choose>
      <xsl:when test="ancestor::tei:head"/>
      <xsl:when test="ancestor::tei:bibl"/>
      <xsl:when test="preceding-sibling::text()"/>
      <xsl:when test="parent::tei:head/parent::tei:*/@xml:id">
        <xsl:for-each select="parent::tei:head/parent::tei:*">
          <xsl:variable name="N">
            <xsl:number level="any"/>
          </xsl:variable>
          <w:bookmarkStart w:id="{number($N) + 10000}" w:name="{@xml:id}"/>
        </xsl:for-each>
      </xsl:when>
      <xsl:when test="../@xml:id">
        <xsl:for-each select="..">
          <xsl:variable name="N">
            <xsl:number level="any"/>
          </xsl:variable>
          <w:bookmarkStart w:id="{number($N) + 10000}" w:name="{@xml:id}"/>
        </xsl:for-each>
      </xsl:when>
    </xsl:choose>
    <xsl:if test="parent::tei:head/parent::tei:div[@iso:status]">
      <w:r>
        <w:t>
          <xsl:attribute name="xml:space">preserve</xsl:attribute>
          <xsl:text> (</xsl:text>
          <xsl:value-of select="../../@iso:status"/>
          <xsl:text>) </xsl:text>
        </w:t>
      </w:r>
    </xsl:if>
    <!-- if no specific style is assigned we might check for any other indication to assign 
	   some style ... -->
    <xsl:variable name="renderingProperties">
      <xsl:for-each select="..">
        <xsl:call-template name="applyRend"/>
      </xsl:for-each>
    </xsl:variable>
    <xsl:variable name="rProps">
      <xsl:if test="string-length($character-style) &gt; 0 or not(empty($renderingProperties))">
        <w:rPr>
          <xsl:if test="string-length($character-style) &gt; 0">
            <w:rStyle>
              <xsl:choose>
                <!-- this is a rogue - trap it and kill it -->
                <xsl:when test="$character-style='footnote reference'">
                  <xsl:attribute name="w:val" select="'FootnoteReference'"/>
                </xsl:when>
                <xsl:otherwise>
                  <xsl:attribute name="w:val" select="$character-style"/>
                </xsl:otherwise>
              </xsl:choose>
            </w:rStyle>
          </xsl:if>
          <xsl:copy-of select="$renderingProperties"/>
          <xsl:if test="ancestor::*[@xml:lang]">
            <w:lang w:val="{ancestor::*[@xml:lang][1]/@xml:lang}"/>
          </xsl:if>
        </w:rPr>
      </xsl:if>
    </xsl:variable>
    <xsl:choose>
      <xsl:when test="$renderAddDel='true'">
        <w:r>
          <xsl:copy-of select="$rProps"/>
          <w:t>
            <xsl:call-template name="Text"/>
          </w:t>
        </w:r>
      </xsl:when>
      <xsl:when test="parent::tei:del">
        <w:del>
          <xsl:variable name="N">
            <xsl:for-each select="..">
              <xsl:number level="any"/>
            </xsl:for-each>
          </xsl:variable>
          <xsl:attribute name="w:id" select="number($N) + 50000"/>
          <xsl:if test="../@when">
            <xsl:attribute name="w:date">
              <xsl:value-of select="../@when"/>
            </xsl:attribute>
          </xsl:if>
          <w:r>
            <xsl:copy-of select="$rProps"/>
            <w:delText>
              <xsl:call-template name="Text"/>
            </w:delText>
          </w:r>
        </w:del>
      </xsl:when>
      <xsl:when test="parent::tei:add">
        <w:ins>
          <xsl:variable name="N">
            <xsl:for-each select="..">
              <xsl:number level="any"/>
            </xsl:for-each>
          </xsl:variable>
          <xsl:attribute name="w:id" select="number($N) + 50000"/>
          <xsl:if test="../@when">
            <xsl:attribute name="w:date">
              <xsl:value-of select="../@when"/>
            </xsl:attribute>
          </xsl:if>
          <w:r>
            <xsl:copy-of select="$rProps"/>
            <w:t>
              <xsl:call-template name="Text"/>
            </w:t>
          </w:r>
        </w:ins>
      </xsl:when>
      <xsl:otherwise>
        <w:r>
          <xsl:copy-of select="$rProps"/>
          <w:t>
            <xsl:call-template name="Text"/>
          </w:t>
        </w:r>
      </xsl:otherwise>
    </xsl:choose>
    <xsl:choose>
      <xsl:when test="ancestor::tei:head"/>
      <xsl:when test="ancestor::tei:bibl"/>
      <xsl:when test="following-sibling::text()"/>
      <xsl:when test="../@xml:id">
        <xsl:for-each select="..">
          <xsl:variable name="N">
            <xsl:number level="any"/>
          </xsl:variable>
          <w:bookmarkEnd w:id="{number($N) + 10000}"/>
        </xsl:for-each>
      </xsl:when>
      <xsl:when test="parent::tei:head/parent::tei:*/@xml:id">
        <xsl:for-each select="parent::tei:head/parent::tei:*">
          <xsl:variable name="N">
            <xsl:number level="any"/>
          </xsl:variable>
          <w:bookmarkEnd w:id="{number($N) + 10000}"/>
        </xsl:for-each>
      </xsl:when>
    </xsl:choose>
  </xsl:template>
  <xsl:template name="Text">
    <xsl:choose>
      <xsl:when test="parent::w:body">
        <xsl:message terminate="yes">CDATA found in body! [<xsl:value-of select="."/>]</xsl:message>
      </xsl:when>
      <xsl:otherwise>
        <xsl:variable name="out">
          <xsl:choose>
            <xsl:when test=".=' ' or ../@xml:space='preserve'">
              <xsl:value-of select="."/>
            </xsl:when>
            <xsl:otherwise>
              <xsl:if test="starts-with(.,' ') or starts-with(.,'&#10;')">
                <xsl:text> </xsl:text>
              </xsl:if>
              <xsl:value-of select="normalize-space(.)"/>
              <xsl:choose>
                <xsl:when test="substring(.,string-length(.),1)=' '">
                  <xsl:text> </xsl:text>
                </xsl:when>
                <xsl:when test="substring(.,string-length(.),1)='&#9;'">
                  <xsl:text> </xsl:text>
                </xsl:when>
                <xsl:when test="substring(.,string-length(.),1)='&#10;'">
                  <xsl:text> </xsl:text>
                </xsl:when>
              </xsl:choose>
              <xsl:if test="substring(.,string-length(.),1)='&#10;'">
                <xsl:text> </xsl:text>
              </xsl:if>
            </xsl:otherwise>
          </xsl:choose>
        </xsl:variable>
        <xsl:choose>
          <xsl:when test="contains($out,'ߛ')">
            <xsl:attribute name="xml:space">preserve</xsl:attribute>
            <xsl:value-of select="substring-before($out,'ߛ')"/>
            <w:noBreakHyphen/>
            <xsl:attribute name="xml:space">preserve</xsl:attribute>
            <xsl:value-of select="substring-after($out,'ߛ')"/>
          </xsl:when>
          <xsl:otherwise>
            <xsl:attribute name="xml:space">preserve</xsl:attribute>
            <xsl:value-of select="$out"/>
          </xsl:otherwise>
        </xsl:choose>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc>
        
            Style definition templates: 
            No default Style for any block or inline element
        
    </desc>
  </doc>
  <xsl:template match="*" mode="get-style"/>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc> 
        to a given style name, this template returns the correct style id
        looking it up in styles.xml 
    </desc>
  </doc>
  <xsl:template name="getStyleName">
    <xsl:param name="in"/>
    <xsl:for-each select="document($styleDoc,/)">
      <xsl:for-each select="key('Styles',$in)">
        <xsl:value-of select="parent::w:style/@w:styleId"/>
      </xsl:for-each>
    </xsl:for-each>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc>
        Tests whether to add rendering elements to a run.
        Word styles cannot not be added in applyRend. If you want to add
        a style go for a get-style template. The order of these
	elements in Word does matter, by the way. 
     </desc>
  </doc>
  <xsl:template name="applyRend">
    <!-- use a custom font -->
    <xsl:choose>
      <xsl:when test="@iso:font">
        <w:rFonts w:ascii="{@iso:font}" w:hAnsi="{@iso:font}"/>
      </xsl:when>
      <!-- typewriter font -->
      <xsl:when test="contains(@rend,'typewriter') or tei:render-typewriter(.)">
        <w:rFonts w:ascii="Courier" w:hAnsi="Courier"/>
      </xsl:when>
      <xsl:when test="contains(@rend, 'Special') or matches(@iso:style,'font-family')">
        <xsl:call-template name="getStyleFonts">
          <xsl:with-param name="css" select="@iso:style"/>
        </xsl:call-template>
      </xsl:when>
      <xsl:when test="matches(parent::*/@iso:style,'font-family')">
        <xsl:call-template name="getStyleFonts">
          <xsl:with-param name="css" select="parent::*/@iso:style"/>
        </xsl:call-template>
      </xsl:when>
    </xsl:choose>
    <!-- bold -->
    <xsl:choose>
      <xsl:when test="tei:render-bold(.)">
        <w:b/>
      </xsl:when>
      <xsl:when test="self::tei:hi[not(@rend)]">
        <w:b/>
      </xsl:when>
      <xsl:when test="self::tbx:hi[@style='bold']">
        <w:i/>
      </xsl:when>
      <xsl:when test="contains(@rend,'bold')">
        <w:b/>
      </xsl:when>
      <xsl:when test="contains(@rend,'normalweight')">
        <w:b w:val="0"/>
      </xsl:when>
    </xsl:choose>
    <!-- italic -->
    <xsl:choose>
      <xsl:when test="tei:render-italic(.)">
        <w:i/>
      </xsl:when>
      <xsl:when test="self::tei:emph">
        <w:i/>
      </xsl:when>
      <xsl:when test="self::tbx:hi[@style='italics']">
        <w:i/>
      </xsl:when>
      <xsl:when test="self::tbx:hi[@style='it']">
        <w:i/>
      </xsl:when>
    </xsl:choose>
    <!-- small caps -->
    <xsl:if test="tei:render-smallcaps(.)">
      <w:smallCaps/>
    </xsl:if>
    <!-- all caps -->
    <xsl:if test="contains(@rend,'capsall')">
      <w:caps/>
    </xsl:if>
    <!-- strikethrough -->
    <xsl:choose>
      <xsl:when test="$renderAddDel='true' and ancestor-or-self::tei:del">
        <w:strike/>
      </xsl:when>
      <xsl:when test="contains(@rend,'strikethrough')">
        <w:strike/>
      </xsl:when>
      <xsl:when test="contains(@rend,'strikedoublethrough')">
        <w:dstrike/>
      </xsl:when>
    </xsl:choose>
    <!-- colour -->
    <xsl:choose>
      <xsl:when test="$renderAddDel='true' and ancestor-or-self::tei:add">
        <w:color w:val="{$addColour}"/>
      </xsl:when>
      <xsl:when test="contains(@rend,'color(')">
        <w:color w:val="{substring-before(substring-after(@rend,'color('),')')}"/>
      </xsl:when>
    </xsl:choose>
    <!-- background color -->
    <xsl:if test="contains(@rend,'background(')">
      <w:highlight w:val="{substring-before(substring-after(@rend,'background('),')')}"/>
    </xsl:if>
    <!-- underline -->
    <xsl:choose>
      <xsl:when test="contains(@rend,'underline') ">
        <w:u w:val="single"/>
      </xsl:when>
      <xsl:when test="contains(@rend,'underwavyline') ">
        <w:u w:val="wave"/>
      </xsl:when>
      <xsl:when test="contains(@rend,'underdoubleline')">
        <w:u w:val="double"/>
      </xsl:when>
    </xsl:choose>
    <!-- sub- and superscript -->
    <xsl:choose>
      <xsl:when test="contains(@iso:style,'position')">
        <w:position>
          <xsl:attribute name="w:val">
            <xsl:value-of select="normalize-space(substring-before((substring-after(@iso:style,'position:')),';'))"/>
          </xsl:attribute>
        </w:position>
      </xsl:when>
      <xsl:when test="self::tbx:hi[@style='subscript']">
        <w:vertAlign w:val="subscript"/>
      </xsl:when>
      <xsl:when test="contains(@rend,'subscript')">
        <w:vertAlign w:val="subscript"/>
      </xsl:when>
      <xsl:when test="contains(@rend,'sub')">
        <w:vertAlign w:val="subscript"/>
      </xsl:when>
    </xsl:choose>
    <xsl:choose>
      <xsl:when test="self::tbx:hi[@style='superscript']">
        <w:vertAlign w:val="superscript"/>
      </xsl:when>
      <xsl:when test="contains(@rend,'superscript')">
        <w:vertAlign w:val="superscript"/>
      </xsl:when>
      <xsl:when test="contains(@rend,'sup')">
        <w:vertAlign w:val="sup"/>
      </xsl:when>
    </xsl:choose>
    <!-- text direction -->
    <xsl:if test="@its:dir!=''">
      <!-- only handling RTL at the moment -->
      <xsl:if test="matches(@its:dir,'rtl')">
        <w:rtl/>
      </xsl:if>
    </xsl:if>
  </xsl:template>
  <!-- 
        Footnotes
    -->
  <xsl:template name="create-footnote">
    <xsl:variable name="num">
      <xsl:number count="tei:note[@place='foot' or @place='bottom' ]" level="any"/>
    </xsl:variable>
    <xsl:variable name="id" select="number($num)+1"/>
    <w:r>
      <w:rPr>
        <w:rStyle w:val="FootnoteReference"/>
      </w:rPr>
      <w:footnoteReference w:id="{$id}"/>
    </w:r>
    <w:r>
      <w:t xml:space="preserve"> </w:t>
    </w:r>
  </xsl:template>
  <xsl:template name="create-comment">
    <w:r>
      <w:rPr>
        <w:rStyle w:val="CommentReference"/>
        <w:vanish/>
      </w:rPr>
      <xsl:variable name="n">
        <xsl:number level="any" count="tei:note[@place='comment']"/>
      </xsl:variable>
      <w:commentReference w:id="{$n - 1}"/>
    </w:r>
  </xsl:template>
  <!-- 
        Endnotes
    -->
  <xsl:template name="create-endnote">
    <xsl:variable name="num">
      <xsl:number count="tei:note[@place='end']" level="any"/>
    </xsl:variable>
    <xsl:variable name="id" select="$num+1"/>
    <w:r>
      <w:rPr>
        <w:rStyle w:val="EndnoteReference"/>
        <w:noProof/>
      </w:rPr>
      <w:endnoteReference w:id="{$id}"/>
    </w:r>
    <w:r>
      <w:t xml:space="preserve"> </w:t>
    </w:r>
  </xsl:template>
  <!--
        Headers and Sections  
    -->
  <xsl:template match="tei:head[parent::tei:div or parent::tei:div1 or parent::tei:div2 or parent::tei:div3 or parent::tei:div4 or parent::tei:div5 or parent::tei:div6 or parent::tei:div7]">
    <xsl:param name="pPr"/>
    <!-- find out what level we are at -->
    <xsl:variable name="level">
      <xsl:value-of select="count(ancestor-or-self::tei:div| ancestor-or-self::tei:div1| ancestor-or-self::tei:div2| ancestor-or-self::tei:div3| ancestor-or-self::tei:div4| ancestor-or-self::tei:div5| ancestor-or-self::tei:div6| ancestor-or-self::tei:div7)"/>
    </xsl:variable>
    <xsl:variable name="number">
      <xsl:number level="any"/>
    </xsl:variable>
    <xsl:variable name="getstyle" select="tei:get-headingstyle(.,$level)"/>
    <xsl:call-template name="block-element">
      <!-- we want a bookmark for referencing this section -->
      <xsl:with-param name="bookmark-id">
        <xsl:value-of select="1000+$number"/>
      </xsl:with-param>
      <xsl:with-param name="bookmark-name">
        <xsl:choose>
          <xsl:when test="parent::tei:div/@xml:id">
            <xsl:value-of select="parent::tei:div/@xml:id"/>
          </xsl:when>
          <xsl:otherwise>
            <xsl:text>_SECTION_</xsl:text>
            <xsl:value-of select="1000+$number"/>
          </xsl:otherwise>
        </xsl:choose>
      </xsl:with-param>
      <!-- find the correct header style -->
      <xsl:with-param name="style">
        <xsl:choose>
          <xsl:when test="not($pPr  instance of xs:string)">
            <xsl:value-of select="$pPr/w:pPr/w:pStyle/@w:val"/>
          </xsl:when>
          <xsl:when test="string-length($getstyle) &gt; 0">
            <xsl:call-template name="getStyleName">
              <xsl:with-param name="in" select="$getstyle"/>
            </xsl:call-template>
          </xsl:when>
          <xsl:when test="parent::tei:div/parent::tei:back">
            <xsl:text>ANNEX</xsl:text>
          </xsl:when>
          <xsl:when test="ancestor::tei:back">
            <xsl:value-of select="concat('a',$level)"/>
          </xsl:when>
          <xsl:otherwise>
            <xsl:call-template name="getStyleName">
              <xsl:with-param name="in">
                <xsl:text>heading </xsl:text>
                <xsl:value-of select="$level"/>
              </xsl:with-param>
            </xsl:call-template>
          </xsl:otherwise>
        </xsl:choose>
      </xsl:with-param>
    </xsl:call-template>
  </xsl:template>
  <!-- quoted text; if it surrounds egXML, just pass on -->
  <xsl:template match="tei:q|tei:said|tei:soCalled">
    <xsl:choose>
      <xsl:when test="*[not(tei:is-inline(.))] or parent::tei:cit|parent::tei:div">
        <xsl:call-template name="block-element"/>
      </xsl:when>
      <xsl:when test="tei:l">
        <xsl:apply-templates/>
      </xsl:when>
      <xsl:otherwise>
        <w:r>
          <w:t>
            <xsl:value-of select="$preQuote"/>
          </w:t>
        </w:r>
        <xsl:apply-templates/>
        <w:r>
          <w:t>
            <xsl:value-of select="$postQuote"/>
          </w:t>
        </w:r>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  <!-- 
        GI
    -->
  <xsl:template match="tei:hi[@rend='specList-elementSpec']">
    <xsl:apply-templates/>
  </xsl:template>
  <xsl:template match="tei:gi">
    <w:r>
      <w:t>&lt;</w:t>
    </w:r>
    <xsl:apply-templates/>
    <w:r>
      <w:t>&gt;</w:t>
    </w:r>
  </xsl:template>
  <xsl:template match="tei:caesura">
    <w:r>
      <w:t>   </w:t>
    </w:r>
  </xsl:template>
  <!-- 
        Handle examples
    -->
  <xsl:template match="teix:egXML|tei:p[@rend='eg']">
    <xsl:param name="simple">false</xsl:param>
    <xsl:param name="highlight"/>
    <xsl:call-template name="block-element">
      <xsl:with-param name="select">
        <tei:p rend="Special" iso:style="font-family:DejaVu Sans Mono; font-size:18; text-align:left;">
          <xsl:call-template name="create-egXML-section"/>
        </tei:p>
      </xsl:with-param>
    </xsl:call-template>
  </xsl:template>
  <xsl:template match="tei:eg">
    <xsl:variable name="content">
      <xsl:apply-templates select="*|processing-instruction()|comment()|text()" mode="egcopy"/>
    </xsl:variable>
    <xsl:for-each select="$content">
      <xsl:call-template name="block-element">
        <xsl:with-param name="style">Special</xsl:with-param>
        <xsl:with-param name="select">
          <tei:p rend="Special" iso:style="font-family:DejaVu Sans Mono; font-size:18;text-align:left;">
            <xsl:copy-of select="*|processing-instruction()|comment()|text()"/>
          </tei:p>
        </xsl:with-param>
      </xsl:call-template>
    </xsl:for-each>
  </xsl:template>
  <xsl:template match="text()" mode="egcopy">
    <xsl:for-each select="tokenize(.,'\n')">
      <xsl:choose>
        <xsl:when test="position()=last()">
          <xsl:value-of select="."/>
        </xsl:when>
        <xsl:when test=".='' and position()=1">
          <xsl:value-of select="."/>
        </xsl:when>
        <xsl:otherwise>
          <xsl:value-of select="."/>
          <tei:lb/>
        </xsl:otherwise>
      </xsl:choose>
    </xsl:for-each>
  </xsl:template>
  <xsl:template match="@*|comment()|processing-instruction()" mode="egcopy">
    <xsl:copy-of select="."/>
  </xsl:template>
  <xsl:template match="*" mode="egcopy">
    <xsl:copy>
      <xsl:apply-templates select="*|@*|processing-instruction()|comment()|text()" mode="egcopy"/>
    </xsl:copy>
  </xsl:template>
  <!-- 
        Handle TEI tables 
    -->
  <xsl:template match="tei:table">
    <xsl:call-template name="table-body"/>
    <xsl:call-template name="table-header"/>
  </xsl:template>
  <xsl:template name="table-body">
    <w:tbl>
      <xsl:choose>
        <xsl:when test="w:tblPr">
          <xsl:copy-of select="w:tblPr"/>
        </xsl:when>
        <xsl:when test="@rend='attDef'">
          <w:tblPr>
            <w:tblW w:w="0" w:type="auto"/>
            <w:jc w:val="left"/>
            <w:tblBorders>
              <w:top w:val="nil" w:space="0" w:color="auto"/>
              <w:left w:val="nil" w:space="0" w:color="auto"/>
              <w:bottom w:val="nil" w:space="0" w:color="auto"/>
              <w:right w:val="nil" w:space="0" w:color="auto"/>
              <w:insideH w:val="nil" w:space="0" w:color="auto"/>
              <w:insideV w:val="nil" w:space="0" w:color="auto"/>
            </w:tblBorders>
          </w:tblPr>
        </xsl:when>
        <xsl:when test="@rend='attList'">
          <w:tblPr>
            <w:tblW w:w="0" w:type="auto"/>
            <w:jc w:val="left"/>
            <w:tblBorders>
              <w:top w:val="nil" w:space="0" w:color="auto"/>
              <w:left w:val="nil" w:space="0" w:color="auto"/>
              <w:bottom w:val="nil" w:space="0" w:color="auto"/>
              <w:right w:val="nil" w:space="0" w:color="auto"/>
              <w:insideH w:val="nil" w:space="0" w:color="auto"/>
              <w:insideV w:val="nil" w:space="0" w:color="auto"/>
            </w:tblBorders>
          </w:tblPr>
        </xsl:when>
        <xsl:when test="@rend='norules'">
          <w:tblPr>
            <w:tblW w:w="0" w:type="auto"/>
            <w:jc w:val="left"/>
            <w:tblBorders>
              <w:top w:val="nil" w:space="0" w:color="auto"/>
              <w:left w:val="nil" w:space="0" w:color="auto"/>
              <w:bottom w:val="nil" w:space="0" w:color="auto"/>
              <w:right w:val="nil" w:space="0" w:color="auto"/>
              <w:insideH w:val="nil" w:space="0" w:color="auto"/>
              <w:insideV w:val="nil" w:space="0" w:color="auto"/>
            </w:tblBorders>
          </w:tblPr>
        </xsl:when>
        <xsl:when test="@rend='wovenodd'">
          <w:tblPr>
            <w:tblW w:w="0" w:type="auto"/>
            <w:jc w:val="left"/>
            <w:tblBorders>
              <w:top w:val="single" w:sz="6" w:space="0" w:color="auto"/>
              <w:left w:val="single" w:sz="6" w:space="0" w:color="auto"/>
              <w:bottom w:val="single" w:sz="6" w:space="0" w:color="auto"/>
              <w:right w:val="single" w:sz="6" w:space="0" w:color="auto"/>
              <w:insideH w:val="single" w:sz="6" w:space="0" w:color="auto"/>
              <w:insideV w:val="single" w:sz="6" w:space="0" w:color="auto"/>
            </w:tblBorders>
          </w:tblPr>
        </xsl:when>
        <xsl:otherwise>
          <w:tblPr>
            <w:tblW w:w="0" w:type="auto"/>
            <w:jc w:val="center"/>
            <w:tblBorders>
              <w:top w:val="single" w:sz="6" w:space="0" w:color="auto"/>
              <w:left w:val="single" w:sz="6" w:space="0" w:color="auto"/>
              <w:bottom w:val="single" w:sz="6" w:space="0" w:color="auto"/>
              <w:right w:val="single" w:sz="6" w:space="0" w:color="auto"/>
              <w:insideH w:val="single" w:sz="6" w:space="0" w:color="auto"/>
              <w:insideV w:val="single" w:sz="6" w:space="0" w:color="auto"/>
            </w:tblBorders>
          </w:tblPr>
        </xsl:otherwise>
      </xsl:choose>
      <xsl:choose>
        <xsl:when test="html:colgroup">
          <w:tblGrid>
            <xsl:for-each select="html:colgroup/html:col">
              <w:gridCol w:w="{tei:convert-dim-pt20(@width)}"/>
            </xsl:for-each>
          </w:tblGrid>
        </xsl:when>
        <!-- if it is defined in word's namespace -->
        <xsl:when test="w:tblGrid">
          <xsl:copy-of select="w:tblGrid"/>
        </xsl:when>
        <xsl:otherwise>
	  <xsl:variable name="maxcols" select="max(.//tei:row/count(tei:cell))"/>
          <w:tblGrid>
            <xsl:for-each select="1 to $maxcols">
              <w:gridCol w:type="pct" w:w="{round((100 div $maxcols) *  50)}"/>
            </xsl:for-each>
          </w:tblGrid>
        </xsl:otherwise>
      </xsl:choose>
      <xsl:apply-templates select="tei:row"/>
    </w:tbl>
    <w:p/>
  </xsl:template>
  <xsl:template name="table-header">
    <xsl:if test="tei:head">
      <xsl:variable name="number">
        <xsl:number level="any"/>
      </xsl:variable>
      <xsl:for-each select="tei:head[1]">
        <xsl:choose>
          <xsl:when test="../@xml:id">
            <xsl:call-template name="block-element">
              <xsl:with-param name="style">Tabletitle</xsl:with-param>
              <!-- we want a bookmark for referencing this table -->
              <xsl:with-param name="bookmark-id">
                <xsl:value-of select="1000+$number"/>
              </xsl:with-param>
              <xsl:with-param name="bookmark-name">
                <xsl:value-of select="../@xml:id"/>
              </xsl:with-param>
            </xsl:call-template>
          </xsl:when>
          <xsl:otherwise>
            <xsl:call-template name="block-element">
              <xsl:with-param name="style">Tabletitle</xsl:with-param>
            </xsl:call-template>
          </xsl:otherwise>
        </xsl:choose>
      </xsl:for-each>
    </xsl:if>
  </xsl:template>
  <xsl:template match="tei:row">
    <w:tr>
      <xsl:choose>
        <xsl:when test="w:trPr">
          <xsl:copy-of select="w:trPr"/>
        </xsl:when>
        <xsl:otherwise> </xsl:otherwise>
      </xsl:choose>
      <w:tblPrEx>
        <w:tblLayout w:type="autofit"/>
      </w:tblPrEx>
      <xsl:apply-templates select="tei:cell"/>
    </w:tr>
  </xsl:template>
  <xsl:template match="tei:cell">
    <w:tc>
      <xsl:choose>
        <xsl:when test="w:tcPr">
          <xsl:copy-of select="w:tcPr"/>
        </xsl:when>
        <xsl:otherwise>
          <w:tcPr>
            <!--w:tcW w:w="1915" w:type="dxa"/-->
            <xsl:if test="@cols">
              <w:gridSpan w:val="{@cols}"/>
            </xsl:if>
          </w:tcPr>
        </xsl:otherwise>
      </xsl:choose>
      <xsl:choose>
        <xsl:when test="tei:note">
          <xsl:apply-templates/>
        </xsl:when>
        <xsl:otherwise>
          <xsl:call-template name="block-element">
            <xsl:with-param name="pPr">
              <w:pPr>
                <xsl:choose>
                  <xsl:when test="@rend">
                    <xsl:variable name="sName">
                      <xsl:call-template name="getStyleName">
                        <xsl:with-param name="in" select="@rend"/>
                      </xsl:call-template>
                    </xsl:variable>
                    <xsl:choose>
                      <xsl:when test="$sName=''">
                        <w:pStyle w:val="{$TableText}"/>
                      </xsl:when>
                      <xsl:otherwise>
                        <w:pStyle w:val="{$sName}"/>
                      </xsl:otherwise>
                    </xsl:choose>
                  </xsl:when>
                  <xsl:otherwise>
                    <w:pStyle w:val="{$TableText}"/>
                  </xsl:otherwise>
                </xsl:choose>
                <xsl:choose>
                  <xsl:when test="@teidocx:align">
                    <w:jc w:val="{@teidocx:align}"/>
                  </xsl:when>
                  <xsl:when test="parent::tei:row[@role='label']          or @role='label'">
                    <w:jc w:val="left"/>
                  </xsl:when>
                  <xsl:when test="starts-with(.,'[0-9]')">
                    <w:jc w:val="right"/>
                  </xsl:when>
                  <xsl:otherwise>
                    <w:jc w:val="left"/>
                  </xsl:otherwise>
                </xsl:choose>
              </w:pPr>
            </xsl:with-param>
          </xsl:call-template>
        </xsl:otherwise>
      </xsl:choose>
      <!-- If we have no children, put an empty p here -->
      <xsl:if test="not(descendant::text())">
        <w:p>
          <w:pPr>
            <w:pStyle w:val="Tabletext9"/>
          </w:pPr>
          <w:r>
            <w:t/>
          </w:r>
        </w:p>
      </xsl:if>
    </w:tc>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc>Turn iso:style attribute back into Word styles for
        table, or work out some defaults based on @rowsep and
        @colsep</desc>
  </doc>
  <xsl:template name="calculateTableBorders">
    <xsl:param name="htmlStyles"/>
    <xsl:choose>
      <xsl:when test="$htmlStyles!=''">
        <xsl:for-each select="tokenize($htmlStyles,';')">
          <xsl:variable name="val">
            <xsl:value-of select="normalize-space(substring-after(.,':'))"/>
          </xsl:variable>
          <xsl:if test="matches(.,'border-top')">
            <xsl:choose>
              <xsl:when test="$val=0">
                <w:top w:val="nil"/>
              </xsl:when>
              <xsl:otherwise>
                <w:top w:val="single" w:sz="{$val}" w:space="0" w:color="auto"/>
              </xsl:otherwise>
            </xsl:choose>
          </xsl:if>
          <xsl:if test="matches(.,'border-left')">
            <xsl:choose>
              <xsl:when test="$val=0">
                <w:left w:val="nil"/>
              </xsl:when>
              <xsl:otherwise>
                <w:left w:val="single" w:sz="{$val}" w:space="0" w:color="auto"/>
              </xsl:otherwise>
            </xsl:choose>
          </xsl:if>
          <xsl:if test="matches(.,'border-bottom')">
            <xsl:choose>
              <xsl:when test="$val=0">
                <w:bottom w:val="nil"/>
              </xsl:when>
              <xsl:otherwise>
                <w:bottom w:val="single" w:sz="{$val}" w:space="0" w:color="auto"/>
              </xsl:otherwise>
            </xsl:choose>
          </xsl:if>
          <xsl:if test="matches(.,'border-right')">
            <xsl:choose>
              <xsl:when test="$val=0">
                <w:right w:val="nil"/>
              </xsl:when>
              <xsl:otherwise>
                <w:right w:val="single" w:sz="{$val}" w:space="0" w:color="auto"/>
              </xsl:otherwise>
            </xsl:choose>
          </xsl:if>
        </xsl:for-each>
      </xsl:when>
      <xsl:otherwise>
        <xsl:if test="@rowsep='1' or (not(@rowsep ='0') and ancestor::cals:tgroup/@rowsep='1')">
          <w:top w:val="single" w:sz="1"/>
        </xsl:if>
        <xsl:if test="@colsep='1' or (not(@colsep ='0') and ancestor::cals:tgroup/@colsep='1')">
          <w:left w:val="single" w:sz="1"/>
        </xsl:if>
        <xsl:if test="@rowsep='1' or (not(@rowsep ='0') and ancestor::cals:tgroup/@rowsep='1')">
          <w:bottom w:val="single" w:sz="1"/>
        </xsl:if>
        <xsl:if test="@colsep='1' or (not(@colsep ='0') and ancestor::cals:tgroup/@colsep='1')">
          <w:right w:val="single" w:sz="1"/>
        </xsl:if>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  <!-- Handle CALS tables -->
  <xsl:template match="cals:table">
    <xsl:call-template name="tableheading-from-cals"/>
    <w:tbl>
      <w:tblPr>
        <w:tblW>
	  <xsl:choose>
	    <xsl:when test="not($tableWidthPercentage='')">
	      <xsl:attribute name="w:w">
		<xsl:value-of  select="round(number($tableWidthPercentage)* 50)"/>
	      </xsl:attribute>
	      <xsl:attribute name="w:type">pct</xsl:attribute>
	    </xsl:when>
	    <xsl:otherwise>
	      <xsl:attribute name="w:w">0</xsl:attribute>
	      <xsl:attribute name="w:type">auto</xsl:attribute>
	    </xsl:otherwise>
	  </xsl:choose>
	</w:tblW>
        <w:jc w:val="center"/>
        <w:tblBorders>
          <xsl:variable name="tblBorders">
            <xsl:call-template name="calculateTableBorders">
              <xsl:with-param name="htmlStyles">
                <xsl:value-of select="@iso:style"/>
              </xsl:with-param>
            </xsl:call-template>
          </xsl:variable>
          <xsl:choose>
            <!-- only @frame turns borders on/off. If a
		   border is 'on' *and* there is a corresponding 
		   border in $tblBorders (ie created from
		   info in iso:style) then the size from $tblBorders
		   overrides the default size -->
            <xsl:when test="@frame='none'">
              <w:top w:val="none" w:sz="0" w:space="0" w:color="auto"/>
              <w:left w:val="none" w:sz="0" w:space="0" w:color="auto"/>
              <w:bottom w:val="none" w:sz="0" w:space="0" w:color="auto"/>
              <w:right w:val="none" w:sz="0" w:space="0" w:color="auto"/>
            </xsl:when>
            <xsl:when test="@frame='top'">
              <xsl:choose>
                <xsl:when test="$tblBorders/w:top">
                  <xsl:copy-of select="$tblBorders/w:top"/>
                </xsl:when>
                <xsl:otherwise>
                  <w:top w:val="none" w:sz="6" w:space="0" w:color="auto"/>
                </xsl:otherwise>
              </xsl:choose>
              <w:left w:val="none" w:sz="0" w:space="0" w:color="auto"/>
              <w:bottom w:val="none" w:sz="0" w:space="0" w:color="auto"/>
              <w:right w:val="none" w:sz="0" w:space="0" w:color="auto"/>
            </xsl:when>
            <xsl:when test="@frame='bottom'">
              <w:top w:val="none" w:sz="0" w:space="0" w:color="auto"/>
              <w:left w:val="none" w:sz="0" w:space="0" w:color="auto"/>
              <xsl:choose>
                <xsl:when test="$tblBorders/w:bottom">
                  <xsl:copy-of select="$tblBorders/w:bottom"/>
                </xsl:when>
                <xsl:otherwise>
                  <w:bottom w:val="single" w:sz="6" w:space="0" w:color="auto"/>
                </xsl:otherwise>
              </xsl:choose>
              <w:right w:val="none" w:sz="0" w:space="0" w:color="auto"/>
            </xsl:when>
            <xsl:when test="@frame='topbot'">
              <xsl:choose>
                <xsl:when test="$tblBorders/w:top">
                  <xsl:copy-of select="$tblBorders/w:top"/>
                </xsl:when>
                <xsl:otherwise>
                  <w:top w:val="none" w:sz="6" w:space="0" w:color="auto"/>
                </xsl:otherwise>
              </xsl:choose>
              <w:left w:val="none" w:sz="0" w:space="0" w:color="auto"/>
              <xsl:choose>
                <xsl:when test="$tblBorders/w:bottom">
                  <xsl:copy-of select="$tblBorders/w:bottom"/>
                </xsl:when>
                <xsl:otherwise>
                  <w:bottom w:val="single" w:sz="6" w:space="0" w:color="auto"/>
                </xsl:otherwise>
              </xsl:choose>
              <w:right w:val="none" w:sz="0" w:space="0" w:color="auto"/>
            </xsl:when>
            <xsl:when test="@frame='sides'">
              <w:top w:val="none" w:sz="0" w:space="0" w:color="auto"/>
              <xsl:choose>
                <xsl:when test="$tblBorders/w:left">
                  <xsl:copy-of select="$tblBorders/w:left"/>
                </xsl:when>
                <xsl:otherwise>
                  <w:left w:val="single" w:sz="6" w:space="0" w:color="auto"/>
                </xsl:otherwise>
              </xsl:choose>
              <w:bottom w:val="none" w:sz="0" w:space="0" w:color="auto"/>
              <xsl:choose>
                <xsl:when test="$tblBorders/w:right">
                  <xsl:copy-of select="$tblBorders/w:right"/>
                </xsl:when>
                <xsl:otherwise>
                  <w:right w:val="single" w:sz="6" w:space="0" w:color="auto"/>
                </xsl:otherwise>
              </xsl:choose>
            </xsl:when>
            <xsl:when test="@frame='all'">
              <xsl:choose>
                <xsl:when test="$tblBorders/w:top">
                  <xsl:copy-of select="$tblBorders/w:top"/>
                </xsl:when>
                <xsl:otherwise>
                  <w:top w:val="single" w:sz="6" w:space="0" w:color="auto"/>
                </xsl:otherwise>
              </xsl:choose>
              <xsl:choose>
                <xsl:when test="$tblBorders/w:left">
                  <xsl:copy-of select="$tblBorders/w:left"/>
                </xsl:when>
                <xsl:otherwise>
                  <w:left w:val="single" w:sz="6" w:space="0" w:color="auto"/>
                </xsl:otherwise>
              </xsl:choose>
              <xsl:choose>
                <xsl:when test="$tblBorders/w:bottom">
                  <xsl:copy-of select="$tblBorders/w:bottom"/>
                </xsl:when>
                <xsl:otherwise>
                  <w:bottom w:val="single" w:sz="6" w:space="0" w:color="auto"/>
                </xsl:otherwise>
              </xsl:choose>
              <xsl:choose>
                <xsl:when test="$tblBorders/w:right">
                  <xsl:copy-of select="$tblBorders/w:right"/>
                </xsl:when>
                <xsl:otherwise>
                  <w:right w:val="single" w:sz="6" w:space="0" w:color="auto"/>
                </xsl:otherwise>
              </xsl:choose>
            </xsl:when>
          </xsl:choose>
          <xsl:choose>
            <xsl:when test="@rowsep=1">
              <w:insideH w:val="single" w:sz="6" w:space="0" w:color="auto"/>
            </xsl:when>
            <xsl:otherwise>
              <w:insideH w:val="none" w:sz="6" w:space="0" w:color="auto"/>
            </xsl:otherwise>
          </xsl:choose>
          <xsl:choose>
            <xsl:when test="@colsep=1">
              <w:insideV w:val="single" w:sz="6" w:space="0" w:color="auto"/>
            </xsl:when>
            <xsl:otherwise>
              <w:insideV w:val="none" w:sz="6" w:space="0" w:color="auto"/>
            </xsl:otherwise>
          </xsl:choose>
        </w:tblBorders>
        <w:tblLayout w:type="fixed"/>
      </w:tblPr>
      <xsl:variable name="maxcols" select="max(.//cals:row/count(cals:entry))"/>
      <xsl:choose>
        <xsl:when test="cals:tgroup/cals:colspec">
          <w:tblGrid>
	    <xsl:variable name="totunits" select="sum(cals:tgroup/cals:colspec[ends-with(@colwidth,'*')]/number(substring-before(@colwidth,'*')))"/>
            <xsl:for-each select="cals:tgroup/cals:colspec">
              <w:gridCol>
		  <xsl:choose>
		    <!-- cell widths are specified in 50th of a percent -->
		    <xsl:when test="ends-with(@colwidth,'*')">
		      <xsl:attribute name="w:type">pct</xsl:attribute>
		      <xsl:attribute name="w:w">
			<xsl:value-of  select="round((number(substring-before(@colwidth,'*')) * 5000) div $totunits)"/>
		      </xsl:attribute>
		    </xsl:when>
		    <xsl:otherwise>
		      <xsl:attribute name="w:w">
			<xsl:value-of select="tei:convert-dim-pt20(@colwidth)"/>
		      </xsl:attribute>
		    </xsl:otherwise>
		  </xsl:choose>
	      </w:gridCol>
	    </xsl:for-each>
          </w:tblGrid>
        </xsl:when>
        <xsl:otherwise>
          <w:tblGrid>
            <xsl:for-each select="1 to $maxcols">
              <w:gridCol w:type="pct" w:w="{round((100 div $maxcols) *  50)}"/>
            </xsl:for-each>
          </w:tblGrid>
        </xsl:otherwise>
      </xsl:choose>
      <xsl:apply-templates select="cals:tgroup"/>
    </w:tbl>
    <w:p/>
  </xsl:template>
  <xsl:template name="tableheading-from-cals">
    <xsl:if test="cals:title">
      <xsl:for-each select="cals:title[1]">
        <w:p>
          <w:pPr>
            <w:pStyle>
              <xsl:attribute name="w:val">Tabletitle</xsl:attribute>
            </w:pStyle>
          </w:pPr>
          <xsl:if test="not(normalize-space(.)='')">
            <w:r>
              <w:t xml:space="preserve">— </w:t>
            </w:r>
          </xsl:if>
          <xsl:apply-templates/>
        </w:p>
      </xsl:for-each>
      <xsl:for-each select="preceding-sibling::tei:p[@rend='Table units']">
        <w:p>
          <w:pPr>
            <w:pStyle>
              <xsl:attribute name="w:val">Tableunits</xsl:attribute>
            </w:pStyle>
          </w:pPr>
          <xsl:apply-templates/>
        </w:p>
      </xsl:for-each>
    </xsl:if>
  </xsl:template>
  <xsl:template match="cals:tgroup">
    <xsl:variable name="TABLE">
      <xsl:copy>
        <xsl:attribute name="iso:style">
          <xsl:copy-of select="ancestor::cals:table/@iso:style"/>
        </xsl:attribute>
        <xsl:copy-of select="@*"/>
        <xsl:copy-of select="cals:colspec"/>
        <xsl:for-each select="cals:tbody|cals:thead">
          <xsl:copy>
            <xsl:for-each select="cals:row">
              <xsl:copy>
                <xsl:copy-of select="@*"/>
                <xsl:for-each select="cals:entry">
                  <xsl:copy>
                    <xsl:copy-of select="@*"/>
                    <xsl:apply-templates mode="contents" select="."/>
                  </xsl:copy>
                  <xsl:variable name="rows" select="@rowsep"/>
                  <xsl:variable name="cols" select="@colsep"/>
                  <xsl:choose>
                    <xsl:when test="@namest and ancestor::cals:tgroup/cals:colspec[@colname=current()/@namest]">
                      <xsl:variable name="start">
                        <xsl:for-each select="ancestor::cals:tgroup/cals:colspec[@colname=current()/@namest]">
                          <xsl:choose>
                            <xsl:when test="@colnum">
                              <xsl:value-of select="@colnum"/>
                            </xsl:when>
                            <xsl:otherwise>
                              <xsl:number/>
                            </xsl:otherwise>
                          </xsl:choose>
                        </xsl:for-each>
                      </xsl:variable>
                      <xsl:variable name="end">
                        <xsl:for-each select="ancestor::cals:tgroup/cals:colspec[@colname=current()/@nameend]">
                          <xsl:choose>
                            <xsl:when test="@colnum">
                              <xsl:value-of select="@colnum"/>
                            </xsl:when>
                            <xsl:otherwise>
                              <xsl:number/>
                            </xsl:otherwise>
                          </xsl:choose>
                        </xsl:for-each>
                      </xsl:variable>
                      <xsl:for-each select="ancestor::cals:tgroup/cals:colspec[position()&gt;$start and position()&lt;=$end]">
                        <cals:entry DUMMY="true" colname="{@colname}" colsep="{$cols}" rowsep="{$rows}"/>
                      </xsl:for-each>
                    </xsl:when>
                    <xsl:when test="@namest">
                      <xsl:message terminate="yes">ERROR Column <xsl:value-of select="@namest"/> 
				    <xsl:text> referenced with @namest attribute </xsl:text>
				    <xsl:text>is not named in the list of  &lt;colspec&gt; elements for this table</xsl:text>
				  </xsl:message>
                    </xsl:when>
                  </xsl:choose>
                </xsl:for-each>
              </xsl:copy>
            </xsl:for-each>
          </xsl:copy>
        </xsl:for-each>
      </xsl:copy>
    </xsl:variable>
    <!--
	<xsl:variable name="count">
	  <xsl:number level="any"/>
	</xsl:variable>
	  <xsl:result-document indent="yes" href="/tmp/T{$count}.xml">
	    <xsl:copy-of select="$TABLE"/>
	  </xsl:result-document>
-->
    <xsl:for-each select="$TABLE/cals:tgroup">
      <xsl:apply-templates/>
    </xsl:for-each>
  </xsl:template>
  <xsl:template match="cals:row">
    <xsl:variable name="borders">
      <xsl:if test="@colsep='1' or (not(@colsep ='0') and ancestor::cals:tgroup/@colsep='1')">
        <w:left w:val="single" w:sz="1"/>
      </xsl:if>
    </xsl:variable>
    <xsl:variable name="ROWPOS">
      <xsl:number/>
    </xsl:variable>
    <xsl:variable name="TEMPLATE">
      <xsl:for-each select="ancestor::cals:tgroup/cals:colspec">
        <CELL name="{@colname}" num="{@colnum}" rowpos="{$ROWPOS}"/>
      </xsl:for-each>
    </xsl:variable>
    <xsl:variable name="lastColnum">
      <xsl:value-of select="ancestor::cals:tgroup/cals:colspec[last()]/@colnum"/>
    </xsl:variable>
    <xsl:variable name="lastColname">
      <xsl:value-of select="ancestor::cals:tgroup/cals:colspec[last()]/@colname"/>
    </xsl:variable>
    <xsl:variable name="ME" select="."/>
    <xsl:variable name="topEdge">
      <xsl:choose>
        <xsl:when test="parent::cals:tbody">
          <xsl:if test="not(parent::cals:tbody/preceding-sibling::cals:thead/cals:row)">true</xsl:if>
        </xsl:when>
        <xsl:otherwise>
          <xsl:if test="not(preceding-sibling::cals:row)">true</xsl:if>
        </xsl:otherwise>
      </xsl:choose>
    </xsl:variable>
    <xsl:variable name="bottomEdge">
      <xsl:choose>
        <xsl:when test="parent::cals:thead">
          <xsl:if test="not(parent::cals:thead/following-sibling::cals:tbody/cals:row)">true</xsl:if>
        </xsl:when>
        <xsl:otherwise>
          <xsl:if test="not(following-sibling::cals:row)">true</xsl:if>
        </xsl:otherwise>
      </xsl:choose>
    </xsl:variable>
    <w:tr>
      <w:tblPrEx>
        <w:tblLayout w:type="autofit"/>
      </w:tblPrEx>
      <xsl:for-each select="$TEMPLATE/CELL">
        <xsl:variable name="N" select="@name"/>
        <xsl:variable name="leftEdge">
          <xsl:choose>
            <xsl:when test="$N='c1' or $N='1'">true</xsl:when>
            <xsl:when test="not(preceding-sibling::CELL)">true</xsl:when>
          </xsl:choose>
        </xsl:variable>
        <xsl:variable name="rightEdge">
          <xsl:choose>
            <xsl:when test="not(following-sibling::CELL)">true</xsl:when>
          </xsl:choose>
        </xsl:variable>
        <xsl:choose>
          <xsl:when test="$ME/cals:entry[@colname=$N and @DUMMY='true']"/>
          <xsl:when test="$ME/cals:entry[@colname=$N]">
            <xsl:apply-templates select="$ME/cals:entry[@colname=$N]">
              <xsl:with-param name="topEdge" select="$topEdge"/>
              <xsl:with-param name="bottomEdge" select="$bottomEdge"/>
              <xsl:with-param name="leftEdge" select="$leftEdge"/>
              <xsl:with-param name="rightEdge" select="$rightEdge"/>
            </xsl:apply-templates>
          </xsl:when>
          <xsl:otherwise>
            <w:tc>
              <w:tcPr>
                <w:vMerge/>
                <xsl:if test="$borders/w:left">
                  <w:tcBorders>
                    <xsl:copy-of select="$borders/w:left"/>
                  </w:tcBorders>
                </xsl:if>
              </w:tcPr>
              <w:p>    </w:p>
            </w:tc>
          </xsl:otherwise>
        </xsl:choose>
      </xsl:for-each>
    </w:tr>
  </xsl:template>
  <xsl:template match="cals:entry">
    <xsl:param name="topEdge"/>
    <xsl:param name="bottomEdge"/>
    <xsl:param name="leftEdge"/>
    <xsl:param name="rightEdge"/>
    <xsl:variable name="cellBorders">
      <xsl:call-template name="calculateTableBorders">
        <xsl:with-param name="htmlStyles">
          <xsl:value-of select="@iso:style"/>
        </xsl:with-param>
      </xsl:call-template>
    </xsl:variable>
    <xsl:variable name="tableBorders">
      <xsl:call-template name="calculateTableBorders">
        <xsl:with-param name="htmlStyles">
          <xsl:value-of select="ancestor::cals:tgroup/@iso:style"/>
        </xsl:with-param>
      </xsl:call-template>
    </xsl:variable>
    <xsl:variable name="colname" select="@colname"/>
    <w:tc>
      <w:tcPr>
        <xsl:if test="@namest">
          <xsl:variable name="start">
            <xsl:for-each select="ancestor::cals:tgroup/cals:colspec[@colname=current()/@namest]">
              <xsl:choose>
                <xsl:when test="@colnum">
                  <xsl:value-of select="@colnum"/>
                </xsl:when>
                <xsl:otherwise>
                  <xsl:number/>
                </xsl:otherwise>
              </xsl:choose>
            </xsl:for-each>
          </xsl:variable>
          <xsl:variable name="end">
            <xsl:for-each select="ancestor::cals:tgroup/cals:colspec[@colname=current()/@nameend]">
              <xsl:choose>
                <xsl:when test="@colnum">
                  <xsl:value-of select="@colnum"/>
                </xsl:when>
                <xsl:otherwise>
                  <xsl:number/>
                </xsl:otherwise>
              </xsl:choose>
            </xsl:for-each>
          </xsl:variable>
          <w:gridSpan w:val="{number($end)-number($start)+1}"/>
        </xsl:if>
        <xsl:if test="@morerows">
          <w:vMerge w:val="restart"/>
        </xsl:if>
        <!--
	    <xsl:message><xsl:value-of select="$colname"/>
	    (<xsl:value-of select="."/>):    <xsl:value-of
	    select="@rowsep"/> and <xsl:value-of
	    select="parent::cals:row/preceding-sibling::cals:row[1]/cals:entry[@colname=$colname]/@rowsep"/></xsl:message>
	    -->
        <xsl:variable name="borders">
          <!-- top border -->
          <xsl:choose>
            <xsl:when test="parent::cals:row/preceding-sibling::cals:row[1]/cals:entry[@colname=$colname]/@rowsep=0">
              <w:top w:val="nil"/>
            </xsl:when>
            <xsl:when test="$topEdge='true'">
              <!-- HERE -->
              <xsl:if test="$tableBorders/w:top[@w:sz]">
                <w:top w:val="single" w:sz="{$tableBorders/w:top/@w:sz}" w:space="0" w:color="auto"/>
              </xsl:if>
            </xsl:when>
            <xsl:when test="@rowsep=0">
              <w:top w:val="nil"/>
            </xsl:when>
            <xsl:when test="$cellBorders/w:top">
              <xsl:copy-of select="$cellBorders/w:top"/>
            </xsl:when>
          </xsl:choose>
          <!-- left border -->
          <xsl:choose>
            <xsl:when test="$leftEdge='true'">
              <xsl:if test="$tableBorders/w:left[@w:sz]">
                <w:left w:val="single" w:sz="{$tableBorders/w:left/@w:sz}" w:space="0" w:color="auto"/>
              </xsl:if>
            </xsl:when>
            <xsl:when test="$cellBorders/w:left">
              <xsl:copy-of select="$cellBorders/w:left"/>
            </xsl:when>
          </xsl:choose>
          <!-- bottom border -->
          <xsl:choose>
            <xsl:when test="$bottomEdge='true'">
              <xsl:if test="$tableBorders/w:bottom[@w:sz]">
                <w:bottom w:val="single" w:sz="{$tableBorders/w:bottom/@w:sz}" w:space="0" w:color="auto"/>
              </xsl:if>
            </xsl:when>
            <xsl:when test="@rowsep=0 or parent::cals:row/@rowsep=0">
              <w:bottom w:val="nil"/>
            </xsl:when>
            <xsl:when test="@rowsep=1 or parent::cals:row/@rowsep=1">
              <xsl:choose>
                <xsl:when test="$cellBorders/w:bottom">
                  <xsl:copy-of select="$cellBorders/w:bottom"/>
                </xsl:when>
                <xsl:otherwise>
                  <w:bottom w:val="single" w:sz="6" w:space="0" w:color="auto"/>
                </xsl:otherwise>
              </xsl:choose>
            </xsl:when>
          </xsl:choose>
          <!-- right border -->
          <xsl:choose>
            <xsl:when test="following-sibling::cals:entry[1]/@colsep=0">
              <w:right w:val="nil"/>
            </xsl:when>
            <xsl:when test="$rightEdge='true'">
              <xsl:if test="$tableBorders/w:right[@w:sz]">
                <w:right w:val="single" w:sz="{$tableBorders/w:right/@w:sz}" w:space="0" w:color="auto"/>
              </xsl:if>
            </xsl:when>
            <xsl:when test="@colsep=0">
              <w:right w:val="nil"/>
            </xsl:when>
            <xsl:when test="@colsep=1">
              <xsl:choose>
                <xsl:when test="$cellBorders/w:right">
                  <xsl:copy-of select="$cellBorders/w:right"/>
                </xsl:when>
                <xsl:otherwise>
                  <w:right w:val="single" w:sz="6" w:space="0" w:color="auto"/>
                </xsl:otherwise>
              </xsl:choose>
            </xsl:when>
          </xsl:choose>
        </xsl:variable>
        <xsl:if test="$borders/*">
          <w:tcBorders>
            <xsl:copy-of select="$borders/w:top"/>
            <xsl:copy-of select="$borders/w:left"/>
            <xsl:copy-of select="$borders/w:bottom"/>
            <xsl:copy-of select="$borders/w:right"/>
          </w:tcBorders>
        </xsl:if>
        <xsl:if test="@rotate='1'">
          <w:textDirection w:val="btLr"/>
        </xsl:if>
        <xsl:if test="@valign">
          <w:vAlign>
            <xsl:attribute name="w:val">
              <xsl:attribute name="valign">
                <xsl:choose>
                  <xsl:when test="@valign='middle'">center</xsl:when>
                  <xsl:otherwise>
                    <xsl:value-of select="@valign"/>
                  </xsl:otherwise>
                </xsl:choose>
              </xsl:attribute>
            </xsl:attribute>
          </w:vAlign>
        </xsl:if>
      </w:tcPr>
      <xsl:copy-of select="*"/>
    </w:tc>
  </xsl:template>
  <xsl:template match="cals:entry" mode="contents">
    <xsl:call-template name="block-element">
      <xsl:with-param name="pPr">
        <w:pPr>
          <xsl:choose>
            <xsl:when test="@rend">
              <xsl:variable name="sName">
                <xsl:call-template name="getStyleName">
                  <xsl:with-param name="in" select="@rend"/>
                </xsl:call-template>
              </xsl:variable>
              <xsl:choose>
                <xsl:when test="$sName=''">
                  <w:pStyle w:val="{$TableText}"/>
                </xsl:when>
                <xsl:otherwise>
                  <w:pStyle w:val="{$sName}"/>
                </xsl:otherwise>
              </xsl:choose>
            </xsl:when>
            <xsl:otherwise>
              <w:pStyle w:val="{$TableText}"/>
            </xsl:otherwise>
          </xsl:choose>
          <xsl:choose>
            <xsl:when test="@align">
              <w:jc w:val="{@align}"/>
            </xsl:when>
            <xsl:when test="parent::tei:row[@role='label']    or @role='label'">
              <w:jc w:val="left"/>
            </xsl:when>
            <xsl:when test="starts-with(.,'[0-9]')">
              <w:jc w:val="right"/>
            </xsl:when>
            <xsl:otherwise>
              <w:jc w:val="left"/>
            </xsl:otherwise>
          </xsl:choose>
        </w:pPr>
      </xsl:with-param>
      <xsl:with-param name="nop">
        <xsl:choose>
          <xsl:when test="not(text()) and tei:note[(not(@place))]">true</xsl:when>
          <!--NEN: next when commented to make footnotes in table possible-->
          <!--<xsl:when test="not(text()) and tei:note[@place='foot']">true</xsl:when>-->
	    <xsl:otherwise>false</xsl:otherwise>
        </xsl:choose>
      </xsl:with-param>
    </xsl:call-template>
    <!-- If we have no children, put an empty p here -->
    <xsl:choose>
      <xsl:when test="*"/>
      <xsl:when test="text()"/>
      <xsl:otherwise>
        <w:p>
          <w:pPr>
            <w:pStyle w:val="Tabletext9"/>
          </w:pPr>
          <w:r>
            <w:t/>
          </w:r>
        </w:p>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  <!-- 
	 Inline Templates:
    -->
  <xsl:template match="tei:g[@ref='x:tab']">
    <w:r>
      <w:tab/>
    </w:r>
  </xsl:template>
  <xsl:template match="tei:c[@rend='ptab']">
    <w:r>
      <w:ptab w:relativeTo="margin" w:alignment="{@type}" w:leader="none"/>
    </w:r>
  </xsl:template>
  <xsl:template match="tei:lb">
    <w:r>
      <w:br/>
    </w:r>
  </xsl:template>
  <!-- hyperlink -->
  <xsl:template match="tei:ptr">
    <xsl:call-template name="linkMe">
      <xsl:with-param name="anchor">
        <xsl:choose>
          <xsl:when test="@type='cit'">[</xsl:when>
          <xsl:when test="@type='figure'">Figure </xsl:when>
          <xsl:when test="@type='table'">Table </xsl:when>
        </xsl:choose>
        <xsl:choose>
          <xsl:when test="starts-with(@target,'#')  and id(substring(@target,2))">
            <xsl:variable name="target" select="substring(@target,2)"/>
            <xsl:apply-templates select="id($target)" mode="xref"/>
          </xsl:when>
          <xsl:otherwise>
            <xsl:value-of select="@target"/>
          </xsl:otherwise>
        </xsl:choose>
        <xsl:choose>
          <xsl:when test="@type='cit'">]</xsl:when>
        </xsl:choose>
      </xsl:with-param>
    </xsl:call-template>
  </xsl:template>
  <xsl:template match="tei:ref[@target]">
    <xsl:choose>
      <xsl:when test="starts-with(@target,'#') and id(substring(@target,2))">
        <xsl:call-template name="linkMe">
          <xsl:with-param name="anchor">
            <xsl:apply-templates/>
          </xsl:with-param>
        </xsl:call-template>
      </xsl:when>
      <xsl:otherwise>
        <xsl:apply-templates/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  <xsl:template name="linkMeUsingHyperlink">
    <xsl:param name="anchor"/>
    <xsl:choose>
      <xsl:when test="starts-with(@target,'#')">
        <w:r>
          <w:fldChar w:fldCharType="begin"/>
        </w:r>
        <w:r>
          <w:instrText>HYPERLINK "<xsl:value-of select="@target"/>" \h</w:instrText>
        </w:r>
        <w:r>
          <w:fldChar w:fldCharType="separate"/>
        </w:r>
        <w:r w:rsidR="00765EBE">
          <w:rPr>
            <w:rStyle w:val="Hyperlink"/>
            <w:u w:val="none"/>
            <xsl:if test="ancestor::tei:cell">
              <w:sz w:val="18"/>
            </xsl:if>
          </w:rPr>
          <w:t>
            <xsl:copy-of select="$anchor"/>
          </w:t>
        </w:r>
        <w:r>
          <w:fldChar w:fldCharType="end"/>
        </w:r>
      </xsl:when>
      <xsl:otherwise>
        <xsl:variable name="rid">
          <xsl:text>rId</xsl:text>
          <xsl:variable name="n">
            <xsl:number level="any"/>
          </xsl:variable>
          <xsl:value-of select="$n + 3000"/>
        </xsl:variable>
        <w:hyperlink r:id="{$rid}">
          <w:r>
            <w:rPr>
              <w:rStyle w:val="Hyperlink"/>
            </w:rPr>
            <w:t>
              <xsl:copy-of select="$anchor"/>
            </w:t>
          </w:r>
        </w:hyperlink>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  <xsl:template name="linkMe">
    <xsl:param name="anchor"/>
    <!-- create the field codes for the complex field -->
    <!-- based on information in tei:ref/@tei:rend -->
    <xsl:variable name="instrText">
      <xsl:choose>
        <xsl:when test="starts-with(@target,'#')">
          <xsl:variable name="target" select="substring(@target,2)"/>
          <xsl:variable name="rend" select="@rend"/>
          <xsl:choose>
            <xsl:when test="contains($rend,'noteref')">
              <xsl:text>NOTEREF </xsl:text>
            </xsl:when>
            <xsl:otherwise>
              <xsl:text>REF </xsl:text>
            </xsl:otherwise>
          </xsl:choose>
          <xsl:value-of select="$target"/>
          <xsl:for-each select="id($target)">
            <xsl:choose>
              <xsl:when test="contains($rend,'instr_')">
                <xsl:if test="contains($rend,'instr_f')">
                  <xsl:text> \f</xsl:text>
                </xsl:if>
                <xsl:if test="contains($rend,'instr_r')">
                  <xsl:text> \r</xsl:text>
                </xsl:if>
                <xsl:if test="contains($rend,'instr_n')">
                  <xsl:text> \n</xsl:text>
                </xsl:if>
              </xsl:when>
              <xsl:when test="@type='refdoc'"/>
              <xsl:otherwise>
                <xsl:text> \n</xsl:text>
              </xsl:otherwise>
            </xsl:choose>
          </xsl:for-each>
          <xsl:text> \h</xsl:text>
          <xsl:if test="contains(@rend,'mergeformat')">
            <xsl:text> \* MERGEFORMAT</xsl:text>
          </xsl:if>
        </xsl:when>
        <xsl:otherwise>
          <xsl:text>HYPERLINK "</xsl:text>
          <xsl:value-of select="@target"/>
          <xsl:text>" \h</xsl:text>
        </xsl:otherwise>
      </xsl:choose>
    </xsl:variable>
    <w:r>
      <w:fldChar w:fldCharType="begin"/>
    </w:r>
    <w:r>
      <w:instrText>
        <xsl:value-of select="$instrText"/>
      </w:instrText>
    </w:r>
    <w:r>
      <w:fldChar w:fldCharType="separate"/>
    </w:r>
    <w:r>
      <xsl:variable name="rPr">
        <xsl:apply-templates>
          <xsl:with-param name="character-style" select="@iso:class"/>
        </xsl:apply-templates>
      </xsl:variable>
      <w:rPr>
        <xsl:if test="$rPr/w:r/w:rPr/w:rStyle/@w:val">
          <w:rStyle w:val="{$rPr/w:r/w:rPr/w:rStyle/@w:val}"/>
        </xsl:if>
        <xsl:copy-of select="$anchor/w:r/w:rPr/*[not(self::w:rStyle)]"/>
        <!-- oucs0037: why are we overriding special font styles??
	       Comment this out for now! -->
        <!--	  <xsl:if test="ancestor::tei:p[@rend='Special']">
	    <w:rFonts w:ascii="Courier New" w:hAnsi="Courier New"/>
	  </xsl:if> -->
        <xsl:if test="$rPr/w:r/w:rPr/w:rStyle/@w:val='Hyperlink'">
          <w:u w:val="none"/>
        </xsl:if>
      </w:rPr>
      <xsl:choose>
        <xsl:when test="$anchor/w:r">
          <xsl:copy-of select="$anchor/w:r/w:t"/>
        </xsl:when>
        <xsl:otherwise>
          <w:t>
            <xsl:copy-of select="$anchor"/>
          </w:t>
        </xsl:otherwise>
      </xsl:choose>
    </w:r>
    <w:r>
      <w:fldChar w:fldCharType="end"/>
    </w:r>
  </xsl:template>
  <xsl:template match="tei:bibl|tei:biblStruct" mode="xref">
    <xsl:number/>
  </xsl:template>
  <xsl:template match="tei:note|tei:figure|tei:table|tei:item" mode="xref">
    <xsl:number/>
  </xsl:template>
  <xsl:template match="tei:div" mode="xref">
    <xsl:choose>
      <xsl:when test="ancestor::tei:front">
        <xsl:number count="tei:div" from="tei:front" format="i" level="multiple"/>
      </xsl:when>
      <xsl:when test="ancestor::tei:body">
        <xsl:number count="tei:div" from="tei:body" format="1" level="multiple"/>
      </xsl:when>
      <xsl:when test="ancestor::tei:back">
	   Annex <xsl:number count="tei:div" from="tei:back" format="A.1.1" level="multiple"/>
         </xsl:when>
    </xsl:choose>
  </xsl:template>
  <!-- 
       §< Handle elements from different namespaces, such as wordML, wordMathML, MathML ...
        -->
  <xsl:template match="w:drawing">
    <w:r>
      <xsl:apply-templates select="." mode="iden"/>
    </w:r>
  </xsl:template>
  <xsl:template match="iso:wordObject">
    <xsl:apply-templates/>
  </xsl:template>
  <xsl:template match="w:*">
    <xsl:if test="ancestor::w:tbl">
      <xsl:copy>
        <xsl:copy-of select="@*"/>
        <xsl:apply-templates/>
      </xsl:copy>
    </xsl:if>
  </xsl:template>
  <xsl:template match="tei:titlePage">
    <xsl:apply-templates/>
  </xsl:template>
  <xsl:template match="tei:docTitle/tei:titlePart[@type='main']">
    <xsl:call-template name="block-element">
      <xsl:with-param name="style">Title</xsl:with-param>
    </xsl:call-template>
  </xsl:template>
  <xsl:template match="tei:docTitle/tei:titlePart[not(@type)]">
    <xsl:call-template name="block-element">
      <xsl:with-param name="style">Title</xsl:with-param>
    </xsl:call-template>
  </xsl:template>
  <xsl:template match="tei:titlePage/tei:titlePart">
    <xsl:call-template name="block-element">
      <xsl:with-param name="style">Title</xsl:with-param>
    </xsl:call-template>
  </xsl:template>
  <xsl:template match="tei:docTitle/tei:titlePart[@type='sub']">
    <xsl:call-template name="block-element">
      <xsl:with-param name="style">Subtitle</xsl:with-param>
    </xsl:call-template>
  </xsl:template>
  <xsl:template match="tei:docTitle/tei:titlePart[2]" priority="99">
    <xsl:call-template name="block-element">
      <xsl:with-param name="style">Subtitle</xsl:with-param>
    </xsl:call-template>
  </xsl:template>
  <xsl:template match="tei:titlePage/tei:docAuthor" priority="99">
    <xsl:call-template name="block-element">
      <xsl:with-param name="style">Author</xsl:with-param>
    </xsl:call-template>
  </xsl:template>
  <xsl:template match="tei:titlePage/tei:byline" priority="99">
    <xsl:call-template name="block-element">
      <xsl:with-param name="style">Author</xsl:with-param>
    </xsl:call-template>
  </xsl:template>
  <xsl:template match="tei:titlePage/tei:docDate" priority="99">
    <xsl:call-template name="block-element">
      <xsl:with-param name="style">Docdate</xsl:with-param>
    </xsl:call-template>
  </xsl:template>
  <!-- place holders, used by ISO profile -->
  <xsl:template name="getStyleFonts">
    <xsl:param name="css"/>
  </xsl:template>
  <!-- no handling of index terms -->
  <xsl:template match="tei:index"/>
  <xsl:template name="applyRendition"/>
  <xsl:template name="emphasize">
    <xsl:param name="class"/>
    <xsl:param name="content"/>
    <w:r>
      <xsl:choose>
        <xsl:when test="$class='titlem'">
          <w:rPr>
            <w:i/>
          </w:rPr>
        </xsl:when>
        <xsl:when test="$class='titlej'">
          <w:rPr>
            <w:i/>
          </w:rPr>
        </xsl:when>
      </xsl:choose>
      <w:t>
        <xsl:attribute name="xml:space">preserve</xsl:attribute>
        <xsl:choose>
          <xsl:when test="$class='titles'">
            <xsl:text>, </xsl:text>
          </xsl:when>
          <xsl:when test="$class='titleu'">
            <xsl:text>‘</xsl:text>
          </xsl:when>
          <xsl:when test="$class='titlea'">
            <xsl:text>‘</xsl:text>
          </xsl:when>
        </xsl:choose>
        <xsl:value-of select="$content"/>
        <xsl:choose>
          <xsl:when test="$class='titleu'">
            <xsl:text>’</xsl:text>
          </xsl:when>
          <xsl:when test="$class='titlea'">
            <xsl:text>’</xsl:text>
          </xsl:when>
        </xsl:choose>
      </w:t>
    </w:r>
  </xsl:template>
  <!-- List Bibl -->
  <xsl:template match="tei:listBibl">
    <xsl:for-each select="tei:bibl|tei:biblStruct">
      <xsl:choose>
        <xsl:when test="@xml:id">
          <xsl:variable name="number">
            <xsl:number level="any"/>
          </xsl:variable>
          <xsl:call-template name="block-element">
            <xsl:with-param name="style">Bibliography</xsl:with-param>
            <xsl:with-param name="bookmark-id">
              <xsl:value-of select="9000+$number"/>
            </xsl:with-param>
            <xsl:with-param name="bookmark-name">
              <xsl:value-of select="@xml:id"/>
            </xsl:with-param>
          </xsl:call-template>
        </xsl:when>
        <xsl:otherwise>
          <xsl:call-template name="block-element">
            <xsl:with-param name="style">Bibliography</xsl:with-param>
          </xsl:call-template>
        </xsl:otherwise>
      </xsl:choose>
    </xsl:for-each>
  </xsl:template>
  <xsl:template name="tei:makeText">
    <xsl:param name="letters"/>
    <w:r>
      <w:t>
        <xsl:attribute name="xml:space">preserve</xsl:attribute>
        <xsl:value-of select="$letters"/>
      </w:t>
    </w:r>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc>
	Page break
      </desc>
  </doc>
  <xsl:template match="tei:pb">
    <w:r>
      <w:br w:type="page"/>
    </w:r>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc>
        A signature block
    </desc>
  </doc>
  <xsl:template match="tei:signed">
    <xsl:call-template name="block-element">
      <xsl:with-param name="style">teisigned</xsl:with-param>
    </xsl:call-template>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc>
	If you meet an inline element with a link inside it, pass by
	on the other side
      </desc>
  </doc>
  <xsl:template match="tei:title[tei:ref]|tei:hi[tei:ref]" priority="-1">
    <xsl:apply-templates/>
  </xsl:template>
  <xsl:template name="makeSpan">
    <xsl:apply-templates/>
  </xsl:template>
  <xsl:template name="makeExternalLink">
    <xsl:param name="ptr" as="xs:boolean" select="false()"/>
    <xsl:param name="dest"/>
    <xsl:value-of select="$dest"/>
  </xsl:template>
</xsl:stylesheet>
