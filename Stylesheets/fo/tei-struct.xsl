<!-- $Date$, $Revision$, $Author$

XSL FO stylesheet to formatt TEI XML documents 

#include LICENSE
-->
<xsl:stylesheet
  xmlns:tei="http://www.tei-c.org/ns/1.0"
  xmlns:fotex="http://www.tug.org/tei:fotex"
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0"
  xmlns:fo="http://www.w3.org/1999/XSL/Format"
  >

<!-- *************************************************************** -->
<!-- page layout -->
<xsl:template match="tei:TEI">
   <fo:root  xmlns:fo="http://www.w3.org/1999/XSL/Format">
     <xsl:call-template name="setupPagemasters"/>
     <xsl:call-template name="mainAction"/>
    </fo:root>
</xsl:template>

<xsl:template name="mainAction">
    <xsl:choose>
    <xsl:when test="text/tei:group">
      <xsl:apply-templates select="tei:text/tei:front"/>
      <xsl:apply-templates select="tei:text/tei:group"/>
    </xsl:when>
    <xsl:otherwise>
      <xsl:apply-templates select="tei:text/tei:front"/>
      <xsl:apply-templates select="tei:text/tei:body"/>
    </xsl:otherwise>
    </xsl:choose>
    <xsl:apply-templates select="tei:text/tei:back"/>
</xsl:template>

<xsl:template match="tei:front">
<xsl:comment>Front matter</xsl:comment>
<xsl:choose>
    <xsl:when test="ancestor::tei:group">
      <xsl:apply-templates/>
    </xsl:when>
    <xsl:otherwise>
    <xsl:if test="$titlePage='true'">
       <fo:page-sequence 
         format="{$formatFrontpage}"
         force-page-count="end-on-even"
         hyphenate="{$hyphenate}"
         language="{$language}">
       <xsl:call-template name="choosePageMaster">
          <xsl:with-param name="where">
            <xsl:value-of select="$frontMulticolumns"/>
          </xsl:with-param>
       </xsl:call-template>
       <fo:static-content flow-name="xsl-region-before">
         <fo:block/>
       </fo:static-content>
       <fo:static-content flow-name="xsl-region-after">
         <fo:block/>
       </fo:static-content>
       <fo:flow  font-family="{$bodyFont}">
             <xsl:call-template name="Header"/>
       </fo:flow>
       </fo:page-sequence>
    </xsl:if>
    <fo:page-sequence 
	format="{$formatFrontpage}"
        text-align="{$alignment}"
	hyphenate="{$hyphenate}" 
	language="{$language}"
        initial-page-number="1">
    <xsl:call-template name="choosePageMaster">
      <xsl:with-param name="where">
         <xsl:value-of select="$frontMulticolumns"/>
      </xsl:with-param>
    </xsl:call-template>

<!-- static areas -->
    <xsl:choose>
       <xsl:when test="$twoSided">
         <xsl:call-template name="headers-footers-twoside"/>
       </xsl:when>
       <xsl:otherwise>
         <xsl:call-template name="headers-footers-oneside"/>
       </xsl:otherwise>
    </xsl:choose>

<!-- now start the main flow -->

      <fo:flow  flow-name="xsl-region-body"
           font-family="{$bodyFont}"  font-size="{$bodySize}">
        <xsl:for-each select="tei:*">
          <xsl:comment>
            <xsl:value-of select="name(.)"/> in front
          </xsl:comment>
          <xsl:apply-templates select="."/>
        </xsl:for-each>
           
      </fo:flow>
    </fo:page-sequence>
    </xsl:otherwise>
  </xsl:choose>
</xsl:template>

<xsl:template match="tei:body">
  <xsl:choose>
    <xsl:when test="ancestor::tei:group">
      <xsl:apply-templates/>
    </xsl:when>
    <xsl:otherwise>
 <!-- start page sequence -->
      <fo:page-sequence  
	format="{$formatBodypage}"
        text-align="{$alignment}"
	hyphenate="{$hyphenate}" 
	language="{$language}"
	initial-page-number="1">
   <xsl:call-template name="choosePageMaster">
      <xsl:with-param name="where"><xsl:value-of select="$bodyMulticolumns"/></xsl:with-param>
   </xsl:call-template>
<!-- static areas -->
     <xsl:choose>
      <xsl:when test="$twoSided">
       <xsl:call-template name="headers-footers-twoside"/>
      </xsl:when>
      <xsl:otherwise>
       <xsl:call-template name="headers-footers-oneside"/>
      </xsl:otherwise>
     </xsl:choose>

<!-- now start the main  flow -->
       <fo:flow  
	     flow-name="xsl-region-body"
             font-family="{$bodyFont}"  
	     font-size="{$bodySize}">
       <xsl:if test="not($flowMarginLeft='')">
        <xsl:attribute name="margin-left">
         <xsl:value-of select="$flowMarginLeft"/>
        </xsl:attribute>
       </xsl:if>
<!--include front matter if there is no separate titlepage -->
<xsl:if test="not($titlePage='true') and not(preceding-sibling::tei:front)">
         <xsl:call-template name="Header"/>
       </xsl:if>

       <xsl:apply-templates/>

       <xsl:if test="//tei:note[@place='end']">
      <fo:block>
      <xsl:call-template name="setupDiv2"/>
      <xsl:text>Notes</xsl:text>
     </fo:block>
           <xsl:apply-templates select="//tei:note[@place='end']" mode="endnote"/>
      </xsl:if>
       </fo:flow>
      </fo:page-sequence>
    </xsl:otherwise>
  </xsl:choose>
</xsl:template>

<xsl:template match="tei:group/tei:text/tei:body">

       <xsl:apply-templates/>

       <xsl:if test="//tei:note[@place='end']">
      <fo:block>
      <xsl:call-template name="setupDiv2"/>
      <xsl:text>Notes</xsl:text>
     </fo:block>
           <xsl:apply-templates select="//tei:note[@place='end']" mode="endnote"/>
      </xsl:if>
</xsl:template>

<xsl:template match="tei:back">
<xsl:comment>Back matter</xsl:comment>
<xsl:choose>
    <xsl:when test="ancestor::tei:group">
      <xsl:apply-templates/>
    </xsl:when>
    <xsl:otherwise>
    <fo:page-sequence 
	format="{$formatBackpage}"
        text-align="{$alignment}"
	hyphenate="{$hyphenate}" 
	language="{$language}">
   <xsl:call-template name="choosePageMaster">
      <xsl:with-param name="where"><xsl:value-of select="$backMulticolumns"/></xsl:with-param>
   </xsl:call-template>

<!-- static areas -->
     <xsl:choose>
      <xsl:when test="$twoSided">
       <xsl:call-template name="headers-footers-twoside-back"/>
      </xsl:when>
      <xsl:otherwise>
       <xsl:call-template name="headers-footers-oneside-back"/>
      </xsl:otherwise>
     </xsl:choose>

<!-- now start the main flow -->

        <fo:flow  flow-name="xsl-region-body"
             font-family="{$bodyFont}"  font-size="{$bodySize}">
             <xsl:apply-templates/>
             <xsl:call-template name="backHook"/>
        </fo:flow>
      </fo:page-sequence>
    </xsl:otherwise>
  </xsl:choose>
</xsl:template>


<!-- Normal headings -->
<xsl:template match="tei:div">
<xsl:choose>

<xsl:when test="@type='bibliog'">
  <xsl:apply-templates/>
</xsl:when>

<xsl:when test="@type='abstract'">
 <fo:block keep-with-next.within-page="always"
	end-indent="{$exampleMargin}"
	start-indent="{$exampleMargin}"
  >
  <xsl:attribute name="text-align">center</xsl:attribute>
  <xsl:call-template name="setupDiv2"/>
  <fo:inline font-style="italic">Abstract</fo:inline>
 </fo:block>
 <xsl:apply-templates/>
</xsl:when>
	
<xsl:when test="@type='ack'">
 <fo:block keep-with-next.within-page="always">
  <xsl:attribute name="text-align">start</xsl:attribute>
  <xsl:call-template name="setupDiv3"/>
 <fo:inline font-style="italic">Acknowledgements</fo:inline>
 </fo:block>
 <xsl:apply-templates/>
</xsl:when>
	
<xsl:otherwise>
<!-- behaviour depends on the nesting level of <div> elements -->
  <xsl:variable name="divlevel" select="count(ancestor::tei:div)"/>
  <xsl:call-template name="NumberedHeading">
    <xsl:with-param name="level">
        <xsl:value-of select="$divlevel"/>
    </xsl:with-param>
  </xsl:call-template>
  <xsl:apply-templates/>
</xsl:otherwise>

</xsl:choose>
</xsl:template>

<xsl:template match="tei:div0|tei:div1|tei:div2|tei:div3|tei:div4">
   <xsl:call-template name="NumberedHeading">
    <xsl:with-param name="level">
     <xsl:value-of select="substring-after(name(),'div')"/>
    </xsl:with-param>
   </xsl:call-template>
  <xsl:apply-templates/>
</xsl:template>

<xsl:template name="NumberedHeading">
  <xsl:param name="level"/>
  <fo:block keep-with-next.within-page="always">
  <xsl:variable name="divid">        
     <xsl:call-template name="idLabel"/>
  </xsl:variable>
  <xsl:attribute name="id">
   <xsl:value-of select="$divid"/>
  </xsl:attribute>
  <xsl:attribute name="text-align">start</xsl:attribute>
  <xsl:attribute name="font-family">
       <xsl:value-of select="$divFont"/></xsl:attribute>

  <xsl:choose>
   <xsl:when test="$level=0"><xsl:call-template name="setupDiv0"/></xsl:when>
   <xsl:when test="$level=1"><xsl:call-template name="setupDiv1"/></xsl:when>
   <xsl:when test="$level=2"><xsl:call-template name="setupDiv2"/></xsl:when>
   <xsl:when test="$level=3"><xsl:call-template name="setupDiv3"/></xsl:when>
   <xsl:when test="$level=4"><xsl:call-template name="setupDiv4"/></xsl:when>
  </xsl:choose>
  <xsl:call-template name="blockStartHook"/>

<xsl:variable name="Number">
 <xsl:if test="$numberHeadings and $numberHeadingsDepth &gt; $level">
  <xsl:call-template name="calculateNumber">
   <xsl:with-param name="numbersuffix" select="$headingNumberSuffix"/>
  </xsl:call-template>
 </xsl:if>
</xsl:variable>
<!--
<xsl:message>**  Calculated   [<xsl:value-of select="$Number"/>] [<xsl:value-of select="$headingNumberSuffix"/>] for <xsl:value-of select="@id"/></xsl:message>
-->
    <xsl:value-of select="$Number"/>
    <xsl:text> </xsl:text>
    <xsl:apply-templates mode="section" select="tei:head"/>
    <xsl:if test="$divRunningheads">
<!-- markers for use in running heads -->
     <xsl:choose>
       <xsl:when test="$level=0">
             <fo:marker marker-class-name="section1"/>
             <fo:marker marker-class-name="section2"/>
             <fo:marker marker-class-name="section3"/>
             <fo:marker marker-class-name="section4"/>
       </xsl:when>
       <xsl:when test="$level=1">
             <fo:marker marker-class-name="section2"/>
             <fo:marker marker-class-name="section3"/>
             <fo:marker marker-class-name="section4"/>
        </xsl:when>
       <xsl:when test="$level=2">
             <fo:marker marker-class-name="section3"/>
             <fo:marker marker-class-name="section4"/>
       </xsl:when>
       <xsl:when test="$level=3">
             <fo:marker marker-class-name="section4"/>
       </xsl:when>
       <xsl:when test="$level=4"></xsl:when>
     </xsl:choose>
      <fo:marker marker-class-name="section{$level}">
       <xsl:if test="not($numberHeadings='')">
         <xsl:value-of select="$Number"/>
         <xsl:value-of select="$headingNumberSuffix"/>
       </xsl:if>
       <xsl:value-of select="tei:head"/>
      </fo:marker>
    </xsl:if>
<xsl:if test="$pdfBookmarks='true'">
<!-- Passive TeX extension, to get PDF bookmarks -->
     <fotex:bookmark  
	xmlns:fotex="http://tug.org/tei:fotex" 
	fotex-bookmark-level="{$level}" 
	fotex-bookmark-label="{$divid}">
     <xsl:if test="not($numberHeadings='')">
       <xsl:value-of select="$Number"/>
     </xsl:if>
     <xsl:value-of select="tei:head"/>
     </fotex:bookmark>
</xsl:if>
<!-- -->
  </fo:block>
</xsl:template>

<!-- headings when cross-referenced -->

<xsl:template mode="xref" match="tei:text">
      <xsl:choose>
       <xsl:when test="@n">
         <xsl:value-of select="@n"/>
       </xsl:when>
       <xsl:otherwise>
         <xsl:number/>
       </xsl:otherwise>
       </xsl:choose>
</xsl:template>

<xsl:template mode="xref" match="tei:div">
  <xsl:variable name="divlevel" select="count(ancestor::tei:div)"/>
  <xsl:call-template name="xheading">
    <xsl:with-param name="level">div<xsl:value-of select="$divlevel"/></xsl:with-param>
  </xsl:call-template>
</xsl:template>

<xsl:template mode="xref" match="tei:div1|tei:div2|tei:div3|tei:div4">
   <xsl:call-template name="xheading">
    <xsl:with-param name="level"><xsl:value-of select="name()"/></xsl:with-param>
   </xsl:call-template>
</xsl:template>

<xsl:template name="xheading">
  <xsl:param name="level"/>
<xsl:if test="not($numberHeadings='')">
  <xsl:call-template name="calculateNumber"/>
</xsl:if>
<xsl:call-template name="divXRefHeading"/>
</xsl:template>

<!-- headings in TOC -->
<xsl:template mode="toc" match="tei:div">
  <xsl:variable name="divlevel" select="count(ancestor::tei:div)"/>
  <xsl:call-template name="tocheading">
    <xsl:with-param name="level">
        <xsl:value-of select="$divlevel"/>
    </xsl:with-param>
  </xsl:call-template>
</xsl:template>

<xsl:template mode="toc" match="tei:div0|tei:div1|tei:div2|tei:div3|tei:div4">
   <xsl:call-template name="tocheading">
    <xsl:with-param name="level">
        <xsl:value-of select="substring-after(name(),'div')"/></xsl:with-param>
   </xsl:call-template>
</xsl:template>

<xsl:template name="tocheading">
  <xsl:param name="level"/>
  <xsl:variable name="tocindent">
   <xsl:choose>
    <xsl:when test="$level='0'">
         <xsl:value-of select="$div0Tocindent"/></xsl:when>
    <xsl:when test="$level='1'">
         <xsl:value-of select="$div1Tocindent"/></xsl:when>
    <xsl:when test="$level='2'">
         <xsl:value-of select="$div2Tocindent"/></xsl:when>
    <xsl:when test="$level='3'">
         <xsl:value-of select="$div3Tocindent"/></xsl:when>
    <xsl:when test="$level='4'">
         <xsl:value-of select="$div4Tocindent"/></xsl:when>
    <xsl:otherwise><xsl:value-of select="$div1Tocindent"/></xsl:otherwise>
   </xsl:choose>
  </xsl:variable>
  <fo:block>
      <xsl:if test="$level='0'">
        <xsl:attribute name="font-weight">bold</xsl:attribute>
      </xsl:if>
      <xsl:attribute name="text-indent">
       <xsl:value-of select="$tocindent"/>
      </xsl:attribute>
  <xsl:variable name="Number">
    <xsl:if test="$numberHeadings and $numberHeadingsDepth &gt; $level">
     <xsl:call-template name="calculateNumber">
       <xsl:with-param name="numbersuffix" select="$tocNumberSuffix"/>
     </xsl:call-template>
   </xsl:if>
   </xsl:variable>
   <xsl:value-of select="$Number"/>
   <xsl:text>&#x2003;</xsl:text>
   <fo:inline>
     <xsl:apply-templates mode="section" select="tei:head"/>
   </fo:inline>
    <fo:leader rule-thickness="0pt"/>
    <fo:inline>
      <xsl:call-template name="linkStyle"/>
      <xsl:variable name="pagref">
        <xsl:call-template name="idLabel"/>
      </xsl:variable>
     <fo:basic-link internal-destination="{$pagref}">
      <fo:page-number-citation ref-id="{$pagref}"/>
     </fo:basic-link>
    </fo:inline> 
  </fo:block>
</xsl:template>


<!-- simple head -->
<xsl:template mode="section" match="tei:head">
<!-- if we have multiple <head> elements together, 
  separate by spaces -->
  
<!--
   <xsl:if test="preceding-sibling::tei:head">
	<xsl:text> </xsl:text>
     </xsl:if>
-->
  <xsl:apply-templates/>
</xsl:template>


<!-- unqualified <head> -->
<xsl:template match="tei:head">
 <xsl:variable name="parent" select="name(..)"/>
 <xsl:if test="not(starts-with($parent,'div'))">
  <xsl:apply-templates/>
 </xsl:if>
</xsl:template>


<!-- table of contents -->
<xsl:template match="tei:divGen">
  <xsl:choose>
    <xsl:when test="@type='toc' and ancestor::tei:text/tei:group">
  <xsl:call-template name="bookTOC"/>
 </xsl:when>
 <xsl:when test="@type='toc'">
  <xsl:call-template name="mainTOC"/>
 </xsl:when>
</xsl:choose>
</xsl:template>

<xsl:template name="mainTOC">
   <fo:block>
     <xsl:call-template name="setupDiv1"/>
     <xsl:text>Contents</xsl:text>
   </fo:block>
   <xsl:choose>
     <xsl:when test="ancestor::tei:text/tei:group">
       <xsl:for-each select="ancestor::tei:text/tei:group">
         <xsl:apply-templates select="tei:text" mode="toc"/>
       </xsl:for-each>
     </xsl:when>
     <xsl:when test="ancestor::tei:text/tei:body/tei:div1">
        <xsl:if test="$tocFront='true'">
          <xsl:for-each select="ancestor::tei:text/tei:front/tei:div1|ancestor::tei:text/tei:front//tei:div2|ancestor::tei:text/tei:front//tei:div3">
            <xsl:apply-templates mode="toc" select="(.)"/>
          </xsl:for-each>
        </xsl:if>
        <xsl:for-each select="ancestor::tei:text/tei:body/tei:div1|ancestor::tei:text/tei:body//div2|ancestor::tei:text/tei:body//tei:div3">
            <xsl:apply-templates mode="toc" select="(.)"/>
        </xsl:for-each>
        <xsl:if test="$tocBack='true'">
          <xsl:for-each select="ancestor::tei:text/tei:back/tei:div1|ancestor::tei:text/tei:back//tei:div2|ancestor::tei:text/tei:back//tei:div3">
            <xsl:apply-templates mode="toc" select="(.)"/>
          </xsl:for-each>
        </xsl:if>
     </xsl:when>
     <xsl:otherwise>
       <xsl:call-template name="tocBits"/>
     </xsl:otherwise>
   </xsl:choose>
</xsl:template>


<xsl:template name="tocBits">
  
        <xsl:if test="$tocFront='true'">
          <xsl:for-each select="ancestor::tei:text/tei:front//tei:div">
            <xsl:apply-templates mode="toc" select="(.)"/>
          </xsl:for-each>
        </xsl:if>
        <xsl:for-each select="ancestor::tei:text/tei:body//tei:div">
            <xsl:apply-templates mode="toc" select="(.)"/>
        </xsl:for-each>
        <xsl:if test="$tocBack='true'">
          <xsl:for-each select="ancestor::tei:text/tei:back//tei:div">
            <xsl:apply-templates mode="toc" select="(.)"/>
          </xsl:for-each>
        </xsl:if>
</xsl:template>


<xsl:template match="tei:titlePart">
	<xsl:apply-templates/>
</xsl:template>

<xsl:template match="tei:titlePage/tei:titlePart[@type='main']">
    <fo:block font-size="{$titleSize}" 
	space-after="8pt"
	text-align="center">
	<xsl:apply-templates/>
    </fo:block>
</xsl:template>

<xsl:template match="tei:titlePage">
    <fo:block 	text-align="center">
	<xsl:apply-templates/>
    </fo:block>
</xsl:template>

<xsl:template match="tei:titlePage/tei:titlePart[@type='sub']">
 <fo:block>
    <fo:block font-size="{$authorSize}" 
	space-after="8pt"
	text-align="center">
	<xsl:apply-templates/></fo:block>
 </fo:block>
</xsl:template>

<xsl:template match="tei:text">
 <xsl:apply-templates/>
</xsl:template>

<xsl:template match="tei:group/tei:text">
  <xsl:variable name="N"><xsl:number/></xsl:variable>
 <fo:page-sequence
        format="1"
        text-align="{$alignment}"
	hyphenate="{$hyphenate}" 
	language="{$language}"
        master-reference="twoside1">
   <xsl:attribute name="initial-page-number">
     <xsl:choose>
       <xsl:when test="$N = 1">1</xsl:when>
       <xsl:otherwise>auto-odd</xsl:otherwise>
     </xsl:choose>
   </xsl:attribute>
   <xsl:call-template name="grouptextStatic"/>
    <fo:flow  font-family="{$bodyFont}">
      <xsl:call-template name="textTitle">
        <xsl:with-param name="N" select="$N"/>
      </xsl:call-template>
     <xsl:apply-templates select="tei:body"/>
     <xsl:apply-templates select="tei:back"/>
     <xsl:if test=".//tei:note[@place='end']">
      <fo:block>
      <xsl:call-template name="setupDiv2"/>
      <xsl:text>Notes</xsl:text>
      </fo:block>
      <xsl:apply-templates select="//tei:note[@place='end']" mode="endnote"/>
     </xsl:if>
    </fo:flow>
 </fo:page-sequence>
</xsl:template>


<xsl:template name="grouptextStatic">
  <fo:static-content flow-name="xsl-region-after-right">
          <fo:block  text-align="end" font-size="{$bodySize}">
              <fo:page-number/></fo:block></fo:static-content>

  <fo:static-content flow-name="xsl-region-after-left">
          <fo:block  text-align="justify" font-size="{$bodySize}">
              <fo:page-number/>
           <fo:leader rule-thickness="0pt"/>
            </fo:block>
        </fo:static-content>
  <fo:static-content flow-name="xsl-region-before-left">
        <fo:block 
          text-indent="0em" 
          background-color="blue" 
          font-weight="bold" padding="3pt" 
          color="white" 
          font-family="{$runFont}"
          text-align="justify" font-size="{$runSize}">
          <fo:inline><xsl:number/></fo:inline>
           <fo:leader rule-thickness="0pt"/>
           <fo:inline>
             <xsl:choose>
               <xsl:when test="front/tei:docTitle[@n]">
                 <xsl:value-of select="tei:front/tei:docTitle[@n]"/>
               </xsl:when>
               <xsl:otherwise>
                 <xsl:value-of select="tei:front/tei:docTitle"/>
               </xsl:otherwise>
             </xsl:choose>
         </fo:inline>
        </fo:block>
  </fo:static-content>
  <fo:static-content flow-name="xsl-region-before-right">
        <fo:block background-color="blue" 
          text-indent="0em" 
          font-weight="bold" padding="3pt" 
          color="white" text-align="left" 
          font-family="{$runFont}"
          font-size="{$runSize}">
          <xsl:value-of select="tei:front//tei:docAuthor"/>
        </fo:block>
  </fo:static-content>
</xsl:template>

<xsl:template match="tei:*" mode="heading">
 <xsl:apply-templates/>
</xsl:template>

<xsl:template name="headers-footers-twoside-back">
  <xsl:param name="runodd"/>
  <xsl:param name="runeven"/>
  <xsl:call-template name="headers-footers-twoside">
  <xsl:with-param name="runeven"><xsl:value-of select="$runeven"/></xsl:with-param>
  <xsl:with-param name="runodd"><xsl:value-of select="$runodd"/></xsl:with-param>
 </xsl:call-template>
</xsl:template>

<xsl:template name="headers-footers-oneside-back">
  <xsl:param name="runhead"/>
  <xsl:call-template name="headers-footers-oneside">
  <xsl:with-param name="runhead"><xsl:value-of select="$runhead"/></xsl:with-param>
 </xsl:call-template>
</xsl:template>

<xsl:template name="headers-footers-twoside">
  <xsl:param name="runodd"/>
  <xsl:param name="runeven"/>
        <fo:static-content flow-name="xsl-region-before-right">
          <fo:block  text-align="justify" font-size="{$bodySize}">
          <xsl:choose>	
	   <xsl:when test="$runodd">
             <xsl:value-of select="$runodd"/>
           </xsl:when>
	   <xsl:when test="$sectionHeaders">
            <fo:block>
              <xsl:if test="$divRunningheads">
               <fo:inline>
                 <fo:retrieve-marker retrieve-class-name="section2"/>
               </fo:inline>
              </xsl:if>
              <fo:leader rule-thickness="0pt"/>
              <fo:inline>
               <fo:page-number/>
              </fo:inline>
            </fo:block>
           </xsl:when>
           <xsl:otherwise>
            <xsl:call-template name="runninghead-title"/>
           </xsl:otherwise>
          </xsl:choose>
          </fo:block>
        </fo:static-content>

        <fo:static-content flow-name="xsl-region-before-left">
          <fo:block text-align="justify" font-size="{$bodySize}">
          <xsl:choose>	
	   <xsl:when test="$runodd">
             <xsl:value-of select="$runeven"/>
           </xsl:when>
	   <xsl:when test="$sectionHeaders">
            <fo:block>
              <fo:inline>
               <fo:page-number/>
              </fo:inline>
              <fo:leader rule-thickness="0pt"/>
              <xsl:if test="$divRunningheads">
               <fo:inline>
                 <fo:retrieve-marker retrieve-class-name="section1"/>
               </fo:inline>
              </xsl:if>
            </fo:block>
           </xsl:when>
            <xsl:otherwise>
           <xsl:call-template name="runninghead-author"/>
           </xsl:otherwise>
          </xsl:choose>
          </fo:block>
        </fo:static-content>

        <fo:static-content flow-name="xsl-region-before-first">
          <fo:block/>
        </fo:static-content>

         <fo:static-content flow-name="xsl-region-after-right">
          <fo:block  text-align="end" font-size="{$bodySize}">
              <fo:page-number/></fo:block></fo:static-content>

        <fo:static-content flow-name="xsl-region-after-left">
          <fo:block  text-align="justify" font-size="{$bodySize}">
              <fo:page-number/>
           <fo:leader rule-thickness="0pt"/>
            </fo:block>
        </fo:static-content>

        <fo:static-content flow-name="xsl-region-after-first">
          <fo:block font-size="{$bodySize}">
           <fo:leader  text-align="center" rule-thickness="0pt"/>
              <fo:page-number/>
            </fo:block>
        </fo:static-content>
</xsl:template>

<xsl:template name="headers-footers-oneside">
  <xsl:param name="runhead"/>
        <fo:static-content flow-name="xsl-region-before">
          <fo:block font-size="{$bodySize}">
          <xsl:choose>	
	   <xsl:when test="$runhead">
             <xsl:value-of select="$runhead"/>
           </xsl:when>
           <xsl:otherwise>
           <xsl:call-template name="runninghead-author"/>
           </xsl:otherwise>
          </xsl:choose>
           </fo:block>
        </fo:static-content>

        <fo:static-content flow-name="xsl-region-after">
          <fo:block  text-align="center" font-size="{$bodySize}">
              <fo:page-number/>
            </fo:block>
        </fo:static-content>

        <fo:static-content flow-name="xsl-region-before-first">
        </fo:static-content>

        <fo:static-content flow-name="xsl-region-after-first">
          <fo:block  text-align="center"  font-size="{$bodySize}">
              <fo:page-number/>
            </fo:block>
        </fo:static-content>
</xsl:template>

<xsl:template match="tei:titlePart[@type='running']"/>

<xsl:template name="runninghead-title">
<xsl:comment>Running Head: title</xsl:comment>
<xsl:choose>
 <xsl:when test="ancestor-or-self::tei:text[1]/tei:front//tei:docTitle[1]/titlePart[@type='running']">
 <xsl:apply-templates select="ancestor-or-self::tei:text[1]/tei:front//tei:docTitle[1]/titlePart[@type='running']" mode="heading"/> 
 </xsl:when>
 <xsl:otherwise>
<xsl:apply-templates select="ancestor-or-self::tei:text[1]/tei:front//tei:docTitle[1]/titlePart" mode="heading"/> 
 </xsl:otherwise>
</xsl:choose>
</xsl:template>

<xsl:template name="runninghead-author">
<xsl:choose>
 <xsl:when test="ancestor::tei:text[1]/tei:front//tei:docAuthor[@type='running']">
 <xsl:apply-templates select="ancestor-or-self::tei:text[1]/tei:front//tei:docAuthor[@type='running']" mode="heading"/> 
 </xsl:when>
 <xsl:otherwise>
   <xsl:apply-templates select="ancestor-or-self::tei:text[1]/tei:front//tei:docAuthor" mode="heading"/> 
 </xsl:otherwise>
</xsl:choose>
</xsl:template>


<xsl:template name="choosePageMaster">
 <xsl:param name="where"/>
 <xsl:param name="force"/>
<xsl:variable name="mn">
   <xsl:choose>
     <xsl:when test="$forcePageMaster">
           <xsl:value-of select="$forcePageMaster"/>
     </xsl:when>
     <xsl:when test="not($where='')">
     <xsl:choose>
      <xsl:when test="$twoSided">twoside2</xsl:when>
      <xsl:otherwise>oneside2</xsl:otherwise>
     </xsl:choose>
     </xsl:when>
     <xsl:otherwise>
     <xsl:choose>
      <xsl:when test="$twoSided">twoside1</xsl:when>
      <xsl:otherwise>oneside1</xsl:otherwise>
     </xsl:choose>
     </xsl:otherwise>
   </xsl:choose>
</xsl:variable>
 <xsl:attribute name="master-reference">
  <xsl:value-of select="$mn"/>
 </xsl:attribute>
</xsl:template>

<xsl:template name="calculateNumber">
   <xsl:param name="numbersuffix"/>
   <xsl:choose>
     <xsl:when test="@n">
       <xsl:value-of select="@n"/><xsl:value-of select="$numbersuffix"/>
     </xsl:when>
     <xsl:when test="ancestor::tei:front">
      <xsl:if test="not($numberFrontHeadings='')">
       <xsl:number format="{$numberFrontHeadings}" level="multiple" from="text"
           count="div|tei:div1|tei:div2|tei:div3|tei:div4"/>
        <xsl:value-of select="$numbersuffix"/>
       </xsl:if>
     </xsl:when>
     <xsl:when test="ancestor::tei:back">
      <xsl:if test="not($numberBackHeadings='')">
       <xsl:value-of select="$appendixWords"/>
       <xsl:text> </xsl:text>
       <xsl:number format="{$numberBackHeadings}" level="multiple"  from="text"
          count="div|tei:div1|tei:div2|tei:div3|tei:div4"/>
      </xsl:if>
     </xsl:when>
     <xsl:otherwise>
      <xsl:number level="multiple"  from="text"
         count="div|tei:div1|tei:div2|tei:div3|tei:div4"/>
       <xsl:value-of select="$numbersuffix"/>
     </xsl:otherwise>
   </xsl:choose>
</xsl:template>

<xsl:template name="setupPagemasters">
  <fo:layout-master-set>

<!-- one sided, single column -->
     <fo:simple-page-master
        master-name="simple1"
        page-width="{$pageWidth}"
        page-height="{$pageHeight}"
        margin-top="{$pageMarginTop}"
        margin-bottom="{$pageMarginBottom}"
        margin-left="{$pageMarginLeft}"
        margin-right="{$pageMarginRight}">
        <fo:region-body 
		margin-bottom="{$bodyMarginBottom}"
		margin-top="{$bodyMarginTop}"/>
        <fo:region-before extent="{$regionBeforeExtent}"/>
        <fo:region-after extent="{$regionAfterExtent}"/>
      </fo:simple-page-master>

<!-- for left-hand/tei:even pages in twosided mode, single column -->
     <fo:simple-page-master
        master-name="left1"
        page-width="{$pageWidth}"
        page-height="{$pageHeight}"
        margin-top="{$pageMarginTop}"
        margin-bottom="{$pageMarginBottom}"
        margin-left="{$pageMarginLeft}"
        margin-right="{$pageMarginRight}">
        <fo:region-body 
		margin-bottom="{$bodyMarginBottom}"
		margin-top="{$bodyMarginTop}"/>
        <fo:region-before
		region-name="xsl-region-before-left" 
		extent="{$regionBeforeExtent}"/>
        <fo:region-after 
		region-name="xsl-region-after-left" 
		extent="{$regionAfterExtent}"/>
      </fo:simple-page-master>

<!-- for right-hand/tei:odd pages in twosided mode, single column -->
      <fo:simple-page-master
        master-name="right1"
        page-width="{$pageWidth}"
        page-height="{$pageHeight}"
        margin-top="{$pageMarginTop}"
        margin-bottom="{$pageMarginBottom}"
        margin-left="{$pageMarginLeft}"
        margin-right="{$pageMarginRight}">
        <fo:region-body 
		margin-bottom="{$bodyMarginBottom}"
		margin-top="{$bodyMarginTop}"/>
        <fo:region-before 
		region-name="xsl-region-before-right" 
		extent="{$regionBeforeExtent}"/>
        <fo:region-after
		region-name="xsl-region-after-right" 
		extent="{$regionAfterExtent}"/>
     </fo:simple-page-master>

<!-- special case of first page in either mode, single column -->
      <fo:simple-page-master
        master-name="first1"
        page-width="{$pageWidth}"
        page-height="{$pageHeight}"
        margin-top="{$pageMarginTop}"
        margin-bottom="{$pageMarginBottom}"
        margin-left="{$pageMarginLeft}"
        margin-right="{$pageMarginRight}">
        <fo:region-body 
		margin-bottom="{$bodyMarginBottom}"
		margin-top="{$bodyMarginTop}"/>
        <fo:region-before  
		region-name="xsl-region-before-first"
		extent="{$regionBeforeExtent}"/>
        <fo:region-after 
		region-name="xsl-region-after-first"
		extent="{$regionAfterExtent}"/>
     </fo:simple-page-master>


<!-- for pages in one-side mode, 2 column -->
     <fo:simple-page-master
        master-name="simple2"
        page-width="{$pageWidth}"
        page-height="{$pageHeight}"
        margin-top="{$pageMarginTop}"
        margin-bottom="{$pageMarginBottom}"
        margin-left="{$pageMarginLeft}"
        margin-right="{$pageMarginRight}">
        <fo:region-body 
	        column-count="{$columnCount}"
		margin-bottom="{$bodyMarginBottom}"
		margin-top="{$bodyMarginTop}"/>
        <fo:region-before extent="{$regionBeforeExtent}"/>
        <fo:region-after extent="{$regionAfterExtent}"/>
      </fo:simple-page-master>

<!-- for left-hand/tei:even pages in twosided mode, 2 column -->
     <fo:simple-page-master
        master-name="left2"
        page-width="{$pageWidth}"
        page-height="{$pageHeight}"
        margin-top="{$pageMarginTop}"
        margin-bottom="{$pageMarginBottom}"
        margin-left="{$pageMarginLeft}"
        margin-right="{$pageMarginRight}">
        <fo:region-body 
	        column-count="{$columnCount}"
		margin-bottom="{$bodyMarginBottom}"
		margin-top="{$bodyMarginTop}"/>
        <fo:region-before
		region-name="xsl-region-before-left" 
		extent="{$regionBeforeExtent}"/>
        <fo:region-after 
		region-name="xsl-region-after-left" 
		extent="{$regionAfterExtent}"/>
      </fo:simple-page-master>

<!-- for right-hand/tei:odd pages in twosided mode, 2 column -->
      <fo:simple-page-master
        master-name="right2"
        page-width="{$pageWidth}"
        page-height="{$pageHeight}"
        margin-top="{$pageMarginTop}"
        margin-bottom="{$pageMarginBottom}"
        margin-left="{$pageMarginLeft}"
        margin-right="{$pageMarginRight}">
        <fo:region-body 
	        column-count="{$columnCount}"
		margin-bottom="{$bodyMarginBottom}"
		margin-top="{$bodyMarginTop}"/>
        <fo:region-before 
		region-name="xsl-region-before-right" 
		extent="{$regionBeforeExtent}"/>
        <fo:region-after
		region-name="xsl-region-after-right" 
		extent="{$regionAfterExtent}"/>
     </fo:simple-page-master>

<!-- special case of first page in either mode -->
      <fo:simple-page-master
        master-name="first2"
        margin-top="{$pageMarginTop}"
        margin-bottom="{$pageMarginBottom}"
        margin-left="{$pageMarginLeft}"
        margin-right="{$pageMarginRight}">
        <fo:region-body 
	        column-count="{$columnCount}"
		margin-bottom="{$bodyMarginBottom}"
		margin-top="{$bodyMarginTop}"/>
        <fo:region-before  
		region-name="xsl-region-before-first"
		extent="{$regionBeforeExtent}"/>
        <fo:region-after 
		region-name="xsl-region-after-first"
		extent="{$regionAfterExtent}"/>
     </fo:simple-page-master>

<!-- setup for double-sided, 1 column, no first page -->
     <fo:page-sequence-master master-name="twoside1nofirst">
      <fo:repeatable-page-master-alternatives>
        <fo:conditional-page-master-reference
		master-reference="right1"
		odd-or-even="odd"/>
        <fo:conditional-page-master-reference
		master-reference="left1"
		odd-or-even="even"/>
      </fo:repeatable-page-master-alternatives>
     </fo:page-sequence-master>

<!-- setup for double-sided, 1 column -->
     <fo:page-sequence-master master-name="twoside1">
      <fo:repeatable-page-master-alternatives>
        <fo:conditional-page-master-reference
		master-reference="first1"
		page-position="first"/>
        <fo:conditional-page-master-reference
		master-reference="right1"
		odd-or-even="odd"/>
        <fo:conditional-page-master-reference
		master-reference="left1"
		odd-or-even="even"/>
      </fo:repeatable-page-master-alternatives>
     </fo:page-sequence-master>

<!-- setup for single-sided, 1 column -->
     <fo:page-sequence-master master-name="oneside1">
      <fo:repeatable-page-master-alternatives>
        <fo:conditional-page-master-reference
		master-reference="first1"
		page-position="first"/>
        <fo:conditional-page-master-reference 
		master-reference="simple1"/>
      </fo:repeatable-page-master-alternatives>
     </fo:page-sequence-master>


<!-- setup for double-sided, 2 column -->
     <fo:page-sequence-master master-name="twoside2">
      <fo:repeatable-page-master-alternatives>
        <fo:conditional-page-master-reference
		master-reference="first2"
		page-position="first"/>
        <fo:conditional-page-master-reference
		master-reference="right2"
		odd-or-even="odd"/>
        <fo:conditional-page-master-reference
		master-reference="left2"
		odd-or-even="even"/>
      </fo:repeatable-page-master-alternatives>
     </fo:page-sequence-master>

<!-- setup for single-sided, 2 column -->
     <fo:page-sequence-master master-name="oneside2">
      <fo:repeatable-page-master-alternatives>
        <fo:conditional-page-master-reference
		master-reference="first2"
		page-position="first"/>
        <fo:conditional-page-master-reference 
		master-reference="simple2"/>
      </fo:repeatable-page-master-alternatives>
     </fo:page-sequence-master>
    <xsl:call-template name="hookDefinepagemasters"/>

    </fo:layout-master-set>
</xsl:template>

<xsl:template name="bookTOC">
 <fo:page-sequence 
   format="1"
   initial-page-number="{$tocStartPage}" 
   master-reference="twoside1">
   <xsl:call-template name="headers-footers-twoside">
     <xsl:with-param name="runeven">&#160;</xsl:with-param>
     <xsl:with-param name="runodd">&#160;</xsl:with-param>
   </xsl:call-template>
   <fo:flow font-family="{$bodyFont}">
   <fo:block text-align="center">
     <xsl:attribute name="font-size">
	<xsl:value-of select="$tocSize"/>
     </xsl:attribute>
     <xsl:attribute name="text-indent">
         <xsl:value-of select="$headingOutdent"/>
     </xsl:attribute>
     <xsl:attribute name="font-weight">bold</xsl:attribute>
     <xsl:attribute name="space-after">24pt</xsl:attribute>
     <xsl:attribute name="space-before.optimum">24pt</xsl:attribute>
     <xsl:text>Contents</xsl:text>
   </fo:block>

   <xsl:for-each select="ancestor::tei:text/tei:group/tei:text">
    <xsl:apply-templates select="." mode="toc"/>
   </xsl:for-each>
   </fo:flow>
 </fo:page-sequence>
</xsl:template>

<xsl:template match="tei:text" mode="toc">
  <fo:block>
        <xsl:attribute name="font-weight">bold</xsl:attribute>
        <xsl:number/>
   <xsl:text>&#x2003;</xsl:text>
   <fo:inline>
            <xsl:choose>
               <xsl:when test="front/tei:docTitle[@n]">
                 <xsl:value-of select="tei:front/tei:docTitle/@n"/>
               </xsl:when>
               <xsl:otherwise>
                 <xsl:value-of select="tei:front/tei:docTitle"/>
               </xsl:otherwise>
             </xsl:choose>
    </fo:inline>
    <fo:leader rule-thickness="0pt"/>
    <fo:inline>
      <xsl:call-template name="linkStyle"/>
      <xsl:variable name="pagref">
        <xsl:call-template name="idLabel"/>
      </xsl:variable>
     <fo:basic-link internal-destination="{$pagref}">
      <fo:page-number-citation ref-id="{$pagref}"/>
     </fo:basic-link>
    </fo:inline> 
  </fo:block>
  <fo:block  font-style="italic"  space-after="10pt" space-before="6pt">
    <xsl:apply-templates select="tei:front//tei:docAuthor" mode="heading"/>
  </fo:block>
  
</xsl:template>

<xsl:template match="tei:*" mode="plain">
  <xsl:apply-templates/>
</xsl:template>

<xsl:template match="text()" mode="plain">
  <xsl:value-of select="normalize-space(.)"/>
</xsl:template>


<xsl:template name="Header">
<fo:block>
<fo:inline-container>
 <fo:table>
   <fo:table-column column-number="1" fotex:column-align="R" column-width="100%"/>
   <fo:table-body>
       <fo:table-row>
<fo:table-cell border-top-style="solid" >
 <fo:block font-size="12pt" padding-before="6pt">
   <fo:inline><xsl:value-of select="$institution"/></fo:inline>
</fo:block></fo:table-cell>
       </fo:table-row>
       <fo:table-row>
<fo:table-cell><fo:block font-size="12pt" font-weight="bold">
<xsl:call-template name="generateTitle"/>
</fo:block></fo:table-cell>
       </fo:table-row>
       <fo:table-row>
<fo:table-cell><fo:block><xsl:call-template name="generateAuthor"/>
</fo:block></fo:table-cell>
       </fo:table-row>
       <fo:table-row>
<fo:table-cell border-bottom-style="solid"
><fo:block padding-after="6pt"><xsl:call-template name="generateDate"/>
</fo:block></fo:table-cell>
       </fo:table-row>
   </fo:table-body>
 </fo:table>
</fo:inline-container>
</fo:block>
</xsl:template>

<xsl:template match="tei:trailer">
  <fo:block>
    <xsl:apply-templates/>
  </fo:block>
</xsl:template>
</xsl:stylesheet>
