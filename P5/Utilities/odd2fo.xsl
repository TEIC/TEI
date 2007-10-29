<xsl:stylesheet
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
  version="1.0"
  xmlns:estr="http://exslt.org/strings"
  xmlns:fotex="http://www.tug.org/fotex"
  xmlns:tei="http://www.tei-c.org/ns/1.0"
  extension-element-prefixes="edate estr"
  exclude-result-prefixes="edate estr" 
  xmlns:fo="http://www.w3.org/1999/XSL/Format"
  xmlns:edate="http://exslt.org/dates-and-times"
>

<!--
$Date$ $Author$
odd2html.xsl

 XSLT script for converting TEI Odds (XML) to XSL FO

 Sebastian Rahtz, March 2002

     Copyright 2002 Sebastian Rahtz/Oxford University/TEI Consortium  
      <sebastian.rahtz@oucs.ox.ac.uk>

 Permission is hereby granted, free of charge, to any person obtaining
 a copy of this software and any associated documentation files (the
 ``Software''), to deal in the Software without restriction, including
 without limitation the rights to use, copy, modify, merge, publish,
 distribute, sublicense, and/or sell copies of the Software, and to
 permit persons to whom the Software is furnished to do so, subject to
 the following conditions:
 
 The above copyright notice and this permission notice shall be included
 in all copies or substantial portions of the Software.
-->

<xsl:import href="../web/Stylesheets/P5/fo/tei.xsl"/>
<xsl:import href="../web/Stylesheets/P5/odds/teiodds.xsl"/>

<xsl:param name="pageWidth">210mm</xsl:param>
<xsl:param name="pageHeight">279mm</xsl:param>
<xsl:param name="inlineTables">true</xsl:param>
<xsl:param name="readColSpecFile">colspecs.xml</xsl:param>
<xsl:param name="makeTableCaption"></xsl:param>
<xsl:param name="listItemsep">2pt</xsl:param>
<xsl:param name="tableAlign">start</xsl:param>

 <xsl:variable name="dtdcat" select="document('dtdcat.xml')/Elements"/>

<xsl:variable name="Master"></xsl:variable>
<xsl:variable name="activeLinebreaks">true</xsl:variable>
<xsl:variable name="authorPrefix"></xsl:variable>
<xsl:variable name="autoToc">true</xsl:variable>
<xsl:variable name="bodyMarginTop">12pt</xsl:variable>
<xsl:variable name="divRunningheads">true</xsl:variable>
<xsl:variable name="flowMarginLeft">72pt</xsl:variable>
<xsl:variable name="giColor">black</xsl:variable>
<xsl:variable name="headingNumberSuffix"><xsl:text> </xsl:text></xsl:variable>
<xsl:variable name="headingOutdent">-72pt</xsl:variable>
<xsl:variable name="identColor">black</xsl:variable>
<xsl:variable name="institution">Text Encoding Initiative Consortium</xsl:variable>
<xsl:variable name="listLeftGlossIndent">12pt</xsl:variable>
<xsl:variable name="listLeftGlossInnerIndent">0pt</xsl:variable>
<xsl:variable name="listLeftIndent">10pt</xsl:variable>
<xsl:variable name="listNormalIndent">15pt</xsl:variable>
<xsl:variable name="numberBackHeadings">A.1</xsl:variable>
<xsl:variable name="numberFrontHeadings"></xsl:variable>
<xsl:variable name="numberHeadings">1.1.</xsl:variable>
<xsl:variable name="oddmode">pdf</xsl:variable>
<xsl:variable name="pageMarginBottom">60pt</xsl:variable>
<xsl:variable name="pageMarginLeft">120pt</xsl:variable>
<xsl:variable name="pageMarginRight">60pt</xsl:variable>
<xsl:variable name="pageMarginTop">40pt</xsl:variable>
<xsl:variable name="parIndent">0em</xsl:variable>
<xsl:variable name="parSkip">4pt</xsl:variable>
<xsl:variable name="pdfBookmarks">true</xsl:variable>
<xsl:variable name="revisionPrefix"></xsl:variable>
<xsl:variable name="sansFont">Computer-Modern-Sans</xsl:variable>
<xsl:variable name="shortinstitution">TEI Consortium</xsl:variable>
<xsl:variable name="subTocDepth">3</xsl:variable>
<xsl:variable name="titlePage">true</xsl:variable>
<xsl:variable name="tocDepth">3</xsl:variable>
<xsl:variable name="tocFront"></xsl:variable>
<xsl:variable name="tocNumberSuffix"></xsl:variable>
<xsl:variable name="typewriterFont">LucidaTypewriter</xsl:variable>
<xsl:variable name="xrefShowPage">true</xsl:variable>
<xsl:param name="exampleSize">
 <xsl:value-of select="$bodyMaster * 0.8"/><xsl:text>pt</xsl:text>
</xsl:param>


<xsl:template name="locateParentdiv">
  <xsl:apply-templates select="ancestor::div1" mode="ident"/>
</xsl:template>

<xsl:template name="locateParent">
      <xsl:apply-templates select="ancestor::div1" mode="ident"/>
</xsl:template>

<xsl:template match="titlePage//author">
 <fo:inline font-weight="bold"><xsl:apply-templates/></fo:inline>: 
</xsl:template>

<xsl:template match="titlePage//editor">
 <fo:inline font-weight="bold"><xsl:apply-templates/></fo:inline> (ed): 
</xsl:template>


<xsl:template match="teiHeader"/>

<xsl:template name="mainTOC">
   <fo:block>
     <xsl:call-template name="setupDiv1"/>
     <xsl:text>Contents</xsl:text>
   </fo:block>
    <xsl:if test="$tocFront='true'">
          <xsl:for-each select="ancestor::text/front/div1|ancestor::text/front//div2|ancestor::text/front//div3">
            <xsl:apply-templates mode="toc" select="(.)"/>
          </xsl:for-each>
    </xsl:if>
   <xsl:for-each select="ancestor::text/body/div0|ancestor::text/body//div1|ancestor::text/body//div2|ancestor::text/body//div3|ancestor::text/body//div4">
            <xsl:apply-templates mode="toc" select="(.)"/>
        </xsl:for-each>
        <xsl:if test="$tocBack='true'">
          <xsl:for-each select="ancestor::text/back/div1|ancestor::text/back//div2|ancestor::text/back//div3">
            <xsl:apply-templates mode="toc" select="(.)"/>
          </xsl:for-each>
        </xsl:if>
</xsl:template>

<xsl:template match="term">
  <fo:inline font-style="italic"><xsl:apply-templates/></fo:inline>
  <xsl:if test="not(@rend='noindex')">
      <xsl:processing-instruction name="xmltex">
    <xsl:text>\index{</xsl:text>
    <xsl:value-of select="normalize-space(.)"/>
    <xsl:text>}</xsl:text>
  </xsl:processing-instruction>
  </xsl:if>
</xsl:template>

<xsl:template name="makeLink">
 <xsl:param name="class"/>
 <xsl:param name="url"/>
 <xsl:param name="text"/>
 <fo:basic-link color="{$linkColor}" internal-destination="{$url}">
   <xsl:copy-of select="$text"/>
 </fo:basic-link>
</xsl:template>

<xsl:template name="makeAnchor">
 <xsl:param name="name"/>
 <!-- <a name="{$name}"/>-->
</xsl:template>

<xsl:template match="div0">
  <fo:page-sequence  
	force-page-count="end-on-even"
	format="{$formatBodypage}"
        text-align="{$alignment}"
	hyphenate="{$hyphenate}" 
	language="{$language}">
    <xsl:attribute name="initial-page-number">
    <xsl:choose>
      <xsl:when test="preceding-sibling::div0">auto</xsl:when>
      <xsl:otherwise>1</xsl:otherwise>
    </xsl:choose>
    </xsl:attribute>
    <xsl:call-template name="choosePageMaster">
      <xsl:with-param name="where"><xsl:value-of select="$bodyMulticolumns"/></xsl:with-param>
   </xsl:call-template>
   <xsl:call-template name="headers-footers-twoside"/>
   <fo:flow  
	     flow-name="xsl-region-body"
             font-family="{$bodyFont}"  
	     font-size="{$bodySize}">
       <xsl:if test="not($flowMarginLeft='')">
        <xsl:attribute name="margin-left">
         <xsl:value-of select="$flowMarginLeft"/>
        </xsl:attribute>
       </xsl:if>
    <fotex:bookmark  
	xmlns:fotex="http://www.tug.org/fotex" 
	fotex-bookmark-level="0" 
	fotex-bookmark-label="{@id}">
     <xsl:if test="not($numberHeadings='')">
       <xsl:number format="I"/>
       <xsl:value-of select="$headingNumberSuffix"/>
       <xsl:value-of select="translate(head,'&lt;&gt;','')"/>
     </xsl:if>
    </fotex:bookmark>
    <fo:block space-after="4in">&#x2003;</fo:block>
    <fo:block
      id="{@id}"
      break-after="page" 
      space-before="4in"
      text-align="right" 
      font-weight="bold"
      font-family="{$sansFont}"
      padding="12pt"
      background-color="yellow"
      font-size="36pt">
     <fo:marker marker-class-name="section0"/>
     <fo:marker marker-class-name="section1"/>
     <fo:marker marker-class-name="section2"/>
     <fo:marker marker-class-name="section3"/>
     <fo:marker marker-class-name="section4"/>
     <xsl:number format="I"/>:
     <xsl:value-of select="normalize-space(head)"/>
    </fo:block>
    <xsl:apply-templates/>
   </fo:flow>
  </fo:page-sequence>
</xsl:template>

<xsl:template name="footerRight">
<fo:inline>
  <xsl:value-of select="/TEI.2/text/front/titlePage/docDate"/>
  <!--
  (<xsl:value-of select="edate:date-time()"/>)
   -->
</fo:inline>
<fo:leader rule-thickness="0pt"/>
              <fo:page-number/>
<fo:leader rule-thickness="0pt"/>
<fo:inline><xsl:value-of select="$shortinstitution"/></fo:inline>
</xsl:template>

<xsl:template name="footerLeft">
           <fo:inline><xsl:value-of select="$shortinstitution"/></fo:inline>
           <fo:leader rule-thickness="0pt"/>
              <fo:page-number/>
           <fo:leader rule-thickness="0pt"/>
           <fo:inline>
             <xsl:value-of select="/TEI.2/text/front/titlePage/docDate"/>
<!--(<xsl:value-of select="edate:date-time()"/>)-->
           </fo:inline>
</xsl:template>

<xsl:template name="footerRightSimple">
<fo:leader rule-thickness="0pt"/>
              <fo:page-number/>
<fo:leader rule-thickness="0pt"/>
</xsl:template>

<xsl:template name="footerLeftSimple">
           <fo:leader rule-thickness="0pt"/>
              <fo:page-number/>
           <fo:leader rule-thickness="0pt"/>
</xsl:template>

<xsl:template name="headerLeft">
           <fo:inline>
                 <fo:retrieve-marker 
                   retrieve-position="last-starting-within-page"
	           retrieve-class-name="section1"/>
           </fo:inline>
           <fo:leader rule-thickness="0pt"/>
</xsl:template>

<xsl:template name="headerRight">
           <fo:leader rule-thickness="0pt"/>
           <fo:inline>
                 <fo:retrieve-marker 
                   retrieve-position="last-starting-within-page"
                   retrieve-class-name="section2"/>
           </fo:inline>
</xsl:template>

<xsl:template name="divXRefHeading">
  <xsl:text> </xsl:text><fo:inline font-style="italic">
      <xsl:value-of select="normalize-space(head)"/>
  </fo:inline></xsl:template>

<xsl:variable name="forcePageMaster">twoside1nofirst</xsl:variable>



<xsl:template name="setupDiv1">
 <xsl:attribute name="break-before">odd-page</xsl:attribute>
 <xsl:attribute name="font-size">14pt</xsl:attribute>
 <xsl:attribute name="font-weight">bold</xsl:attribute>
 <xsl:attribute name="space-after">3pt</xsl:attribute>
 <xsl:attribute name="space-before.optimum">9pt</xsl:attribute>
 <xsl:if test="ancestor-or-self::body">
   <xsl:attribute name="text-indent">
     <xsl:value-of select="$headingOutdent"/></xsl:attribute>
 </xsl:if>
</xsl:template>

<xsl:template name="setupDiv2">
 <xsl:attribute name="font-size">12pt</xsl:attribute>
 <xsl:attribute name="font-weight">bold</xsl:attribute>
 <xsl:attribute name="space-after">2pt</xsl:attribute>
 <xsl:attribute name="space-before.optimum">10pt</xsl:attribute>
 <xsl:if test="ancestor-or-self::body">
   <xsl:attribute name="text-indent">
     <xsl:value-of select="$headingOutdent"/></xsl:attribute>
 </xsl:if>
</xsl:template>

<xsl:template name="hookDefinepagemasters">
      <fo:simple-page-master
        master-name="front"
        page-width="{$pageWidth}"
        page-height="{$pageHeight}"
        margin-top="{$pageMarginTop}"
        margin-bottom="{$pageMarginBottom}"
        margin-left="120pt"
        margin-right="60pt">
        <fo:region-body 
		margin-bottom="{$bodyMarginBottom}"
		margin-top="{$bodyMarginTop}"/>
        <fo:region-after extent="{$regionAfterExtent}"/>
        <fo:region-before extent="{$regionBeforeExtent}"/>
      </fo:simple-page-master>
</xsl:template>

<xsl:template match="front">
  <fo:page-sequence
	force-page-count="end-on-even"
	hyphenate="{$hyphenate}"
	language="{$language}"
	master-reference="front"
    >
   <fo:static-content flow-name="xsl-region-before-right">
     <fo:block/>
   </fo:static-content>
   <fo:static-content flow-name="xsl-region-before-left">
     <fo:block/>
   </fo:static-content>
   <fo:static-content flow-name="xsl-region-after-left">
     <fo:block/>
   </fo:static-content>
   <fo:static-content flow-name="xsl-region-after">
          <fo:block/>
   </fo:static-content>
   <fo:static-content flow-name="xsl-region-before">
     <fo:block/>
   </fo:static-content>

   <fo:flow flow-name="xsl-region-body">
      <xsl:apply-templates select="titlePage"/>
   </fo:flow>
   </fo:page-sequence>

  <fo:page-sequence
        format="{$formatFrontpage}"
	force-page-count="end-on-even"
	hyphenate="{$hyphenate}"
	language="{$language}"
	master-reference="front"
    >
   <fo:static-content flow-name="xsl-region-before-right">
     <fo:block/>
   </fo:static-content>
   <fo:static-content flow-name="xsl-region-before-left">
     <fo:block/>
   </fo:static-content>
   <fo:static-content flow-name="xsl-region-after-left">
          <fo:block  text-align="justify" font-size="{$bodySize}">
             <xsl:call-template name="footerLeftSimple"/>
          </fo:block>
   </fo:static-content>
   <fo:static-content flow-name="xsl-region-after">
          <fo:block  text-align="justify" font-size="{$bodySize}">
             <xsl:call-template name="footerRightSimple"/>
          </fo:block>
   </fo:static-content>
   <fo:static-content flow-name="xsl-region-before">
     <fo:block/>
   </fo:static-content>

   <fo:flow flow-name="xsl-region-body">
      <xsl:apply-templates select="div"/>
      <fo:block break-before="page">
        <xsl:call-template name="mainTOC"/>
      </fo:block>
     <fo:block space-before="12pt">
         <fo:inline font-weight="bold">Revision History: </fo:inline>
         <xsl:apply-templates select="titlePage/docDate/text()"/>
     </fo:block>
     <fo:block space-before="20pt">
        <fo:block text-align="center">&#169; 
        <xsl:value-of select="$institution"/>
     </fo:block>
         
     </fo:block>
     <xsl:apply-templates select="*[not(self::titlePage)]"/>
  </fo:flow>
 </fo:page-sequence>
</xsl:template>


<xsl:template name="headers-footers-twoside">
  <xsl:param name="runodd"/>
  <xsl:param name="runeven"/>
  <fo:static-content flow-name="xsl-region-after-right">
          <fo:block  text-align="justify" font-size="{$bodySize}">
             <xsl:call-template name="footerRight"/>
          </fo:block>
  </fo:static-content>
  <fo:static-content flow-name="xsl-region-after-left">
          <fo:block  text-align="justify" font-size="{$bodySize}">
             <xsl:call-template name="footerLeft"/>
          </fo:block>
  </fo:static-content>
  <fo:static-content flow-name="xsl-region-before-right">
          <fo:block text-align="justify" font-size="{$bodySize}">
             <xsl:call-template name="headerRight"/>
          </fo:block>
  </fo:static-content>
  <fo:static-content flow-name="xsl-region-before-left">
          <fo:block text-align="justify" font-size="{$bodySize}">
             <xsl:call-template name="headerLeft"/>
          </fo:block>
  </fo:static-content>

  <fo:static-content flow-name="xsl-region-before-first">
          <fo:block/>
  </fo:static-content>

  <fo:static-content flow-name="xsl-region-after-first">
    <fo:block font-size="{$bodySize}">
       <fo:leader  text-align="center" rule-thickness="0pt"/>
              <fo:page-number/>
       <fo:leader  text-align="center" rule-thickness="0pt"/>
    </fo:block>
  </fo:static-content>
</xsl:template>

<xsl:template match="index">
  <xsl:processing-instruction name="xmltex">
    <xsl:text>\index{</xsl:text>
    <xsl:value-of select="@level1"/>
    <xsl:if test="@level2">
      <xsl:text>!</xsl:text><xsl:value-of select="@level2"/>
    </xsl:if>
    <xsl:if test="@level3">
          <xsl:text>!</xsl:text><xsl:value-of select="@level3"/>
    </xsl:if>
    <xsl:text>}</xsl:text>
  </xsl:processing-instruction>
</xsl:template>


<xsl:template match="ident">
    <fo:inline color="{$identColor}" font-family="{$sansFont}">
        <xsl:apply-templates/>
    </fo:inline>
    <xsl:if test="@type">
     <xsl:processing-instruction name="xmltex">
      <xsl:text>\index{</xsl:text>
      <xsl:value-of select="normalize-space(.)"/>
      <xsl:text> (</xsl:text>
      <xsl:value-of select="@type"/>
      <xsl:text>)}</xsl:text>
     </xsl:processing-instruction>
    </xsl:if>
</xsl:template>

<xsl:template match="p[@rend='centered obeylines']">
  <fo:block 
    space-after.minimum="3in">&#x2003;</fo:block>
  <fo:block
        font-weight="bold" 
	text-align="center"
	font-size="20pt"
	space-before="3in">
    <xsl:apply-templates/>
  </fo:block>
</xsl:template>

<xsl:template match="docImprint">
       <fo:inline font-style="italic">
        <xsl:apply-templates/>
       </fo:inline>
</xsl:template>

<xsl:template match="docDate">
       <fo:inline font-style="italic">
        <xsl:apply-templates/>
       </fo:inline>
</xsl:template>


<xsl:template match="ref[@type='url']">
<fo:inline font-family="{$typewriterFont}" color="{$linkColor}">
   <xsl:apply-templates/>
</fo:inline>
</xsl:template>

<xsl:template match="titlePage"> 
  <fo:block text-align="center" space-after=".5in"
     font-size="22pt" >
    <xsl:value-of select="titlePart[1]"/>
  </fo:block>
  <fo:block text-align="center" space-after="1in"
     font-size="16pt" >
    <xsl:apply-templates select="titlePart[2]"/>
  </fo:block>
  <fo:block text-align="center" font-style="normal"
     font-size="24pt" >
    <xsl:value-of select="docTitle/titlePart[1]"/>
  </fo:block>
  <fo:block text-align="center" font-style="normal"
     font-size="28pt" >
    <xsl:value-of select="docTitle/titlePart[2]"/>
  </fo:block>
  <fo:block text-align="center" space-after=".5in"
     font-size="24pt" >
    <xsl:value-of select="docTitle/titlePart[3]"/>
  </fo:block>
  <fo:block text-align="center" font-style="italic"  space-after=".25in"
     font-size="24pt" >
    <xsl:apply-templates select="docAuthor[1]"/>
  </fo:block>
  <fo:block  text-align="center"  font-style="italic" 
     font-size="20pt" >
    <xsl:value-of select="docAuthor[2]"/>
  </fo:block>
  <fo:block space-before="0.5in" text-align="center" 
     font-size="20pt" >
    <xsl:value-of select="docImprint"/>
  </fo:block>
  <fo:block break-after="page" text-align="center" 
     font-size="20pt" >
    <xsl:value-of select="docDate"/>
  </fo:block>


  <fo:block space-after="4in">&#160;</fo:block>
  <fo:block space-before="4in" font-weight="bold">
    Guidelines for Electronic Text Encoding and Interchange</fo:block>

    <fo:block space-before="14pt" text-indent="2em">&#169; 1990, 1992, 1993, 1994 ACH, ACL, ALLC.</fo:block>
    <fo:block text-indent="2em">&#169; 2002 TEI Consortium</fo:block>


    <fo:block space-before="14pt">Published for the TEI Consortium by the Humanities
Computing Unit,  University of Oxford</fo:block>

    <fo:block space-before="20pt">ISBN 0-952-33013-X</fo:block>

</xsl:template>

<xsl:template match="titlePart"><xsl:apply-templates/></xsl:template>

<xsl:template match="docAuthor"><xsl:apply-templates/></xsl:template>


<xsl:template match="back">
<xsl:comment>Back matter</xsl:comment>
    <fo:page-sequence 
	format="{$formatBackpage}"
	hyphenate="{$hyphenate}" 
	language="{$language}">
   <xsl:call-template name="choosePageMaster">
      <xsl:with-param name="where"><xsl:value-of select="$backMulticolumns"/></xsl:with-param>
   </xsl:call-template>

  <fo:static-content flow-name="xsl-region-after-right">
          <fo:block  text-align="justify" font-size="{$bodySize}">
             <xsl:call-template name="footerRightSimple"/>
          </fo:block>
  </fo:static-content>
  <fo:static-content flow-name="xsl-region-after-left">
          <fo:block  text-align="justify" font-size="{$bodySize}">
             <xsl:call-template name="footerLeftSimple"/>
          </fo:block>
  </fo:static-content>
  <fo:static-content flow-name="xsl-region-before-right">
          <fo:block text-align="justify" font-size="{$bodySize}">
             <xsl:call-template name="headerRight"/>
          </fo:block>
  </fo:static-content>
  <fo:static-content flow-name="xsl-region-before-left">
          <fo:block text-align="justify" font-size="{$bodySize}">
             <xsl:call-template name="headerLeft"/>
          </fo:block>
  </fo:static-content>

        <fo:flow  flow-name="xsl-region-body"
             font-family="{$bodyFont}"  font-size="{$bodySize}">
             <xsl:apply-templates/>
        </fo:flow>
      </fo:page-sequence>

</xsl:template>


<xsl:template match="body">
  <xsl:apply-templates select="div0"/>
</xsl:template>



</xsl:stylesheet>

