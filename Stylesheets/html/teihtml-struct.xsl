<?xml version="1.0" encoding="utf-8"?>
<!-- 
Text Encoding Initiative Consortium XSLT stylesheet family
$Date$, $Revision$, $Author$

XSL stylesheet to format TEI XML documents to HTML or XSL FO

 
##LICENSE
--> 
<xsl:stylesheet
  xmlns:tei="http://www.tei-c.org/ns/1.0"

  xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0">

<!-- ************************************************* -->

<!-- basic handles for divisions -->
<xsl:variable name="linkPrefix"/>

<xsl:template match="tei:front/tei:div|tei:front/tei:div0|tei:front/tei:div1|tei:front/tei:div2|tei:front/tei:div3|tei:front/tei:div4|tei:front/tei:front/tei:div5|tei:front/tei:div6"> 
    <xsl:variable name="depth">
     <xsl:apply-templates select="." mode="depth"/>
    </xsl:variable>
 <xsl:choose>
 <xsl:when test ="not($depth &gt; $splitLevel) and $splitFrontmatter">
  <xsl:if test="not($STDOUT='true')">
  <xsl:call-template name="outputChunk">
   <xsl:with-param name="ident">
    <xsl:apply-templates select="." mode="ident"/>
   </xsl:with-param>
   <xsl:with-param name="content">
    <xsl:call-template name="writeDiv"/>
   </xsl:with-param>
   </xsl:call-template>
  </xsl:if>
  </xsl:when>
  <xsl:otherwise>
   <div>
     <xsl:attribute name="class">
       <xsl:choose>
         <xsl:when test="@type">
           <xsl:value-of select="@type"/>
         </xsl:when>
         <xsl:otherwise>teidiv</xsl:otherwise>
       </xsl:choose>
     </xsl:attribute>
    <xsl:call-template name="doDivBody">
      <xsl:with-param name="Type" select="$depth"/>
    </xsl:call-template>
   </div>
  </xsl:otherwise>
 </xsl:choose>
</xsl:template>

<xsl:template match="tei:back/tei:div|tei:back/tei:div0|tei:back/tei:div1|tei:back/tei:div2|tei:back/tei:div3|tei:back/tei:div4|tei:back/tei:div5|tei:back/tei:div6"> 
  <xsl:variable name="depth">
     <xsl:apply-templates select="." mode="depth"/>
  </xsl:variable>
 <xsl:choose>
 <xsl:when test="not($depth &gt; $splitLevel) and $splitBackmatter">
  <xsl:if test="not($STDOUT='true')">
  <xsl:call-template name="outputChunk">
  <xsl:with-param name="ident">
   <xsl:apply-templates select="." mode="ident"/>
  </xsl:with-param>
  <xsl:with-param name="content">
    <xsl:call-template name="writeDiv"/>
  </xsl:with-param>
  </xsl:call-template>
  </xsl:if>
  </xsl:when>
  <xsl:otherwise>
   <div class="teidiv">
    <xsl:call-template name="doDivBody">
      <xsl:with-param name="Type" select="$depth"/>
    </xsl:call-template>
   </div>
  </xsl:otherwise>
 </xsl:choose>
</xsl:template>

<xsl:template
 match="tei:div|tei:div0|tei:div1|tei:div2|tei:div3|tei:div4|tei:div5|tei:div6"> 


<!-- establish section depth -->
  <xsl:variable name="depth">
   <xsl:apply-templates select="." mode="depth"/>
 </xsl:variable>

<!-- depending on depth and splitting level, 
     we may do one of two things: -->

<!--
<xsl:message>div <xsl:value-of select="@id"/>: <xsl:value-of
select="$depth"/></xsl:message>
-->
 <xsl:choose>
<!-- 1. our section depth is below the splitting level -->
 <xsl:when test="$depth &gt; $splitLevel">
   <div  class="teidiv">
   <xsl:call-template name="doDivBody">
      <xsl:with-param name="Type" select="$depth"/>
   </xsl:call-template>
  <xsl:if test="$sectionUpLink and $depth='0'">
   <xsl:call-template name="topLink"/>
  </xsl:if>
  </div>
 </xsl:when>

<!-- 2. we are at or above splitting level, 
        so start a new file
-->
 <xsl:when test="$depth &lt;= $splitLevel">
  <xsl:if test="not($STDOUT='true')">
  <xsl:call-template name="outputChunk">
  <xsl:with-param name="ident">
   <xsl:apply-templates select="." mode="ident"/>
  </xsl:with-param>
  <xsl:with-param name="content">
    <xsl:call-template name="writeDiv"/>
  </xsl:with-param>
  </xsl:call-template>
 </xsl:if>
 </xsl:when>
 </xsl:choose>
</xsl:template>

<!-- table of contents -->
<xsl:template match="tei:divGen[@type='toc']">
     <h2><xsl:value-of select="$tocWords"/></h2>
      <xsl:call-template name="maintoc"/>
</xsl:template>

<!-- anything with a head can go in the TOC -->

<xsl:template match="tei:*" mode="maketoc">
 <xsl:param name="forcedepth"/>
<xsl:if test="tei:head or $autoHead='true'">
 <xsl:variable name="Depth">
 <xsl:choose>
  <xsl:when test="not($forcedepth='')">
     <xsl:value-of select="$forcedepth"/>
  </xsl:when>
  <xsl:otherwise>
     <xsl:value-of select="$tocDepth"/>
  </xsl:otherwise>
 </xsl:choose>
 </xsl:variable>
 <xsl:variable name="thislevel">
  <xsl:choose>
  <xsl:when test="name(.) = 'div'">
    <xsl:value-of select="count(ancestor::tei:div)"/>
  </xsl:when>
  <xsl:when test="starts-with(name(.),'div')">
   <xsl:choose>
    <xsl:when test="ancestor-or-self::tei:div0">
      <xsl:value-of select="substring-after(name(.),'div')"/>
    </xsl:when>
    <xsl:otherwise>
      <xsl:value-of select="substring-after(name(.),'div') - 1"/>
    </xsl:otherwise>
   </xsl:choose>
 </xsl:when>
 <xsl:otherwise>99</xsl:otherwise>
 </xsl:choose>
 </xsl:variable>
 <xsl:variable name="pointer">
      <xsl:apply-templates mode="xrefheader" select="."/>
 </xsl:variable>
 <li class="toc">
       <xsl:call-template name="header">
           <xsl:with-param name="toc" select="$pointer"/>
           <xsl:with-param name="minimal"></xsl:with-param>
      </xsl:call-template>
      <xsl:if test="$thislevel &lt; $Depth">
          <xsl:call-template name="continuedToc"/>
      </xsl:if>
 </li>
</xsl:if>
</xsl:template>

<xsl:template name="continuedToc">
   <xsl:if test="tei:div|tei:div0|tei:div1|tei:div2|tei:div3|tei:div4|tei:div5|tei:div6">
   <ul class="toc">
   <xsl:apply-templates select="tei:div|tei:div0|tei:div1|tei:div2|tei:div3|tei:div4|tei:div5|tei:div6" mode="maketoc"/>
   </ul>
 </xsl:if>
</xsl:template>

<xsl:template match="tei:div|tei:div0|tei:div1|tei:div2|tei:div3|tei:div4|tei:div5|tei:div6" mode="depth">
  <xsl:choose>
  <xsl:when test="name(.) = 'div'">
    <xsl:value-of select="count(ancestor::tei:div)"/>
  </xsl:when>
  <xsl:otherwise>
   <xsl:choose>
    <xsl:when test="ancestor-or-self::tei:div0">
      <xsl:value-of select="substring-after(name(.),'div')"/>
    </xsl:when>
    <xsl:otherwise>
      <xsl:value-of select="substring-after(name(.),'div') - 1"/>
    </xsl:otherwise>
   </xsl:choose>
  </xsl:otherwise>
  </xsl:choose>
</xsl:template>

<xsl:template match="tei:*" mode="depth">99</xsl:template>

<!-- headings etc -->
<xsl:template match="tei:head">
 <xsl:variable name="parent" select="name(..)"/>
 <xsl:if test="not(starts-with($parent,'div'))">
   <xsl:apply-templates/>
 </xsl:if>
</xsl:template>

<xsl:template mode="plain" match="tei:head">
     <xsl:if test="preceding-sibling::tei:head">
	<xsl:text> </xsl:text>
     </xsl:if>
   <xsl:apply-templates mode="plain"/>
</xsl:template>

<xsl:template match="tei:p">
  <xsl:choose>
    <xsl:when test="list">
      <xsl:apply-templates select="tei:list[1]" mode="inpara"/> 
    </xsl:when> 
    <xsl:otherwise>
     <p>
    <xsl:choose>
        <xsl:when test="@rend and starts-with(@rend,'class:')">
    <xsl:attribute name="class">
      <xsl:value-of select="substring-after(@rend,'class:')"/>
    </xsl:attribute>
      </xsl:when>
    <xsl:when test="@rend">
       <xsl:attribute name="class"><xsl:value-of select="@rend"/></xsl:attribute>
      </xsl:when>
    </xsl:choose>
      <xsl:choose>
        <xsl:when test="@id">
          <a name="{@id}"/>
        </xsl:when>
        <xsl:when test="$generateParagraphIDs='true'">
         <a name="{generate-id()}"/>
        </xsl:when>
      </xsl:choose>
      <xsl:if test="$numberParagraphs='true'">
        <xsl:number/><xsl:text> </xsl:text>
      </xsl:if>
      <xsl:apply-templates/>
     </p>
    </xsl:otherwise>
</xsl:choose>
</xsl:template>

<xsl:template match="tei:list" mode="inpara">
  <p><xsl:apply-templates select="preceding-sibling::node()"/></p>
  <xsl:apply-templates select="."/>
  <p><xsl:apply-templates select="following-sibling::node()"/></p>
</xsl:template>

<xsl:template match="tei:gi" mode="plain">
  <xsl:text>&lt;</xsl:text>
   <xsl:apply-templates/>
  <xsl:text>&gt;</xsl:text>
</xsl:template>

<xsl:template match="tei:*" mode="plain">
  <xsl:apply-templates/>
</xsl:template>

<!-- why do this? it makes embedded elements fall over-->

<!--
<xsl:template match="tei:text()" mode="plain">
  <xsl:value-of select="normalize-space(.)"/>
</xsl:template>
-->

<xsl:template name="writeDiv">
<xsl:variable name="BaseFile">
  <xsl:value-of select="$masterFile"/>
  <xsl:call-template name="addCorpusID"/>
</xsl:variable>
    <html> <xsl:call-template name="addLangAtt"/>
      <xsl:comment>THIS IS A GENERATED FILE. DO NOT EDIT</xsl:comment>
      <head>
        <xsl:variable name="pagetitle">
          <xsl:call-template name="generateDivtitle"/>
        </xsl:variable>
        <title><xsl:value-of select="$pagetitle"/></title>
         <xsl:call-template name="headHook"/>
         <xsl:call-template name="includeCSS"/>
        <xsl:call-template name="metaHook">
         <xsl:with-param name="title" select="$pagetitle"/>
        </xsl:call-template>
        <xsl:call-template name="javaScript"/>
       </head>
       <body>
       <xsl:call-template name="bodyHook"/>
       <xsl:call-template name="bodyJavaScript"/>
       <a name="TOP"/>
       <div  class="teidiv">
        <xsl:call-template name="stdheader">
           <xsl:with-param name="title">
               <xsl:call-template name="generateDivheading"/>
           </xsl:with-param>
        </xsl:call-template>

        <xsl:if test="$topNavigationPanel='true'">
         <xsl:call-template name="xrefpanel">
          <xsl:with-param name="homepage" 
              select="concat($BaseFile,$standardSuffix)"/>
          <xsl:with-param name="mode" 
              select="name(.)"/>
	 </xsl:call-template>
        </xsl:if>
        <xsl:if test="$subTocDepth &gt;= 0">
          <xsl:call-template name="subtoc"/>
        </xsl:if>

        <xsl:call-template name="startHook"/>
        
        <xsl:call-template name="doDivBody"/>

        <xsl:if test="descendant::tei:note[@place='foot'] and $footnoteFile=''">
         <hr/>
         <p><b>Notes</b></p>
           <xsl:call-template name="printDivnotes"/>
        </xsl:if>
        <xsl:if test="$bottomNavigationPanel='true'">
         <xsl:call-template name="xrefpanel">
          <xsl:with-param name="homepage" select="concat($BaseFile,$standardSuffix)"/>
          <xsl:with-param name="mode" select="name(.)"/>
	 </xsl:call-template>
        </xsl:if>

 	 <xsl:call-template name="stdfooter">
          <xsl:with-param name="date">
           <xsl:call-template name="generateDate"/>
          </xsl:with-param>
          <xsl:with-param name="author">
            <xsl:call-template name="generateAuthorList"/>
          </xsl:with-param>
         </xsl:call-template>

        </div>
     </body>
   </html>
</xsl:template>


<xsl:template name="subtoc">
 <xsl:if test="child::tei:div|tei:div1|tei:div2|tei:div3|tei:div4|tei:div5|tei:div6">
  <xsl:variable name="parent">
   <xsl:choose>
   <xsl:when test="ancestor::tei:div">
     <xsl:apply-templates select="ancestor::tei:div[last()]" mode="ident"/>
   </xsl:when>
   <xsl:otherwise>
     <xsl:apply-templates select="." mode="ident"/>
   </xsl:otherwise>
   </xsl:choose>
  </xsl:variable>
  <xsl:variable name="depth">
   <xsl:apply-templates select="." mode="depth"/>
 </xsl:variable>
     <p><span class="subtochead"><xsl:value-of select="$tocWords"/></span></p>
     <div class="subtoc">
      <ul class="subtoc">
      <xsl:for-each select="tei:div|tei:div1|tei:div2|tei:div3|tei:div4|tei:div5|tei:div6">
       <xsl:variable name="innerdent">
         <xsl:apply-templates select="." mode="xrefheader"/>
      </xsl:variable>
      <li class="subtoc">
   <xsl:call-template name="makeHyperLink">     
    <xsl:with-param name="url">
      <xsl:value-of select="$innerdent"/>
    </xsl:with-param>
    <xsl:with-param name="class">
       <xsl:value-of select="$class_subtoc"/>
    </xsl:with-param>
    <xsl:with-param name="body">
    <xsl:call-template name="header"/>
    </xsl:with-param>
   </xsl:call-template>
     </li>
     </xsl:for-each>
    </ul></div>
  </xsl:if>
</xsl:template> 

<xsl:template name="maintoc"> 
 <xsl:param name="force"/>

 <xsl:if test="$tocFront">
   <xsl:for-each select="ancestor-or-self::tei:TEI/tei:text/tei:front">
    <xsl:if test="tei:div|tei:div0|tei:div1|tei:div2|tei:div3|tei:div4|tei:div5|tei:div6">
    <ul class="toc{$force}">
    <xsl:apply-templates 
      select="tei:div|tei:div0|tei:div1|tei:div2|tei:div3|tei:div4|tei:div5|tei:div6" mode="maketoc">
     <xsl:with-param name="forcedepth" select="$force"/>
    </xsl:apply-templates>
    </ul>
    </xsl:if>
   </xsl:for-each>
 </xsl:if>
   <xsl:for-each select="ancestor-or-self::tei:TEI/tei:text/tei:body">
    <xsl:if test="tei:div|tei:div0|tei:div1|tei:div2|tei:div3|tei:div4|tei:div5|tei:div6">
    <ul class="toc{$force}">
    <xsl:apply-templates 
      select="tei:div|tei:div0|tei:div1|tei:div2|tei:div3|tei:div4|tei:div5|tei:div6" mode="maketoc">
     <xsl:with-param name="forcedepth" select="$force"/>
    </xsl:apply-templates>
    </ul>
    </xsl:if>
   </xsl:for-each>
 <xsl:if test="$tocBack">
   <xsl:for-each select="ancestor-or-self::tei:TEI/tei:text/tei:back">
    <xsl:if test="tei:div|tei:div0|tei:div1|tei:div2|tei:div3|tei:div4|tei:div5|tei:div6">
    <ul class="toc{$force}">
    <xsl:apply-templates 
      select="tei:div|tei:div0|tei:div1|tei:div2|tei:div3|tei:div4|tei:div5|tei:div6" mode="maketoc">
     <xsl:with-param name="forcedepth" select="$force"/>
    </xsl:apply-templates>
    </ul>
    </xsl:if>
   </xsl:for-each>
  </xsl:if>
</xsl:template>


<!-- xref to previous and last sections -->

<xsl:template name="xrefpanel">
<xsl:param name="homepage"/>
<xsl:param name="mode"/>

<p align="{$alignNavigationPanel}">

   <xsl:variable name="Parent">
     <xsl:call-template name="locateParent"/>
     <xsl:value-of select="$standardSuffix"/>
   </xsl:variable>
   <xsl:choose>
    <xsl:when test="$Parent = $standardSuffix">
      <xsl:call-template name="upLink">
        <xsl:with-param name="up" select="$homepage"/>
        <xsl:with-param name="title">
          <xsl:call-template name="contentsWord"/>
        </xsl:with-param>
      </xsl:call-template>
    </xsl:when>
    <xsl:otherwise>
        <xsl:call-template name="generateUpLink"/>
    </xsl:otherwise>
   </xsl:choose>

<xsl:choose>
 <xsl:when test="ancestor-or-self::tei:TEI[@rend='nomenu']">
      <xsl:call-template name="upLink">
        <xsl:with-param name="up" select="$homepage"/>
        <xsl:with-param name="title">
          <xsl:call-template name="contentsWord"/>
        </xsl:with-param>
      </xsl:call-template>
 </xsl:when>
 <xsl:when test="$mode = 'div' and preceding-sibling::tei:div">
  <xsl:call-template name="previousLink">
    <xsl:with-param name="previous"  select="preceding-sibling::tei:div[1]"/>
  </xsl:call-template>
 </xsl:when>
 <xsl:when test="$mode = 'div' and parent::tei:body/preceding-sibling::tei:front/tei:div">
  <xsl:call-template name="previousLink">
    <xsl:with-param name="previous"  select="parent::tei:body/preceding-sibling::tei:front/tei:div[tei:head][last()]"/>
  </xsl:call-template>
 </xsl:when>
 <xsl:when test="$mode = 'div' and parent::tei:back/preceding-sibling::tei:body/tei:div">
  <xsl:call-template name="previousLink">
    <xsl:with-param name="previous"  select="parent::tei:back/preceding-sibling::tei:body/tei:div[last()]"/>
  </xsl:call-template>
 </xsl:when>
 <xsl:otherwise>
  <xsl:choose>
    <xsl:when test="name(.)='div0'">
      <xsl:call-template name="previousLink">
      <xsl:with-param name="previous" select="preceding-sibling::tei:div0[1]"/>
     </xsl:call-template>
    </xsl:when>
    <xsl:when test="name(.)='div1'">
      <xsl:call-template name="previousLink">
      <xsl:with-param name="previous" select="preceding-sibling::tei:div1[1]"/>
     </xsl:call-template>
    </xsl:when>
    <xsl:when test="name(.)='div2'">
      <xsl:call-template name="previousLink">
      <xsl:with-param name="previous" select="preceding-sibling::tei:div2[1]"/>
     </xsl:call-template>
    </xsl:when>
    <xsl:when test="name(.)='div3'">
      <xsl:call-template name="previousLink">
      <xsl:with-param name="previous" select="preceding-sibling::tei:div3[1]"/> 
     </xsl:call-template>
   </xsl:when>
  </xsl:choose>
 </xsl:otherwise>
</xsl:choose>

<xsl:choose>
 <xsl:when test="ancestor-or-self::tei:TEI[@rend='nomenu']">
 </xsl:when>
 <xsl:when test="$mode = 'div'">
  <xsl:call-template name="nextLink">
   <xsl:with-param name="next" 
     select="(following-sibling::tei:div[tei:head][1]|parent::tei:body/following-sibling::tei:back/tei:div[1]|parent::tei:front/following-sibling::tei:body/tei:div[1])[1]"/>
  </xsl:call-template>
 </xsl:when>
 <xsl:otherwise>
  <xsl:choose>
    <xsl:when test="name(.)='div0'">
      <xsl:call-template name="nextLink">
      <xsl:with-param name="next" select="following-sibling::tei:div0[1]"/>
     </xsl:call-template>
    </xsl:when>
    <xsl:when test="name(.)='div1'">
      <xsl:call-template name="nextLink">
      <xsl:with-param name="next" select="following-sibling::tei:div1[1]"/>
     </xsl:call-template>
    </xsl:when>
    <xsl:when test="name(.)='div2'">
      <xsl:call-template name="nextLink">
      <xsl:with-param name="next" select="following-sibling::tei:div2[1]"/>
     </xsl:call-template>
    </xsl:when>
    <xsl:when test="name(.)='div3'">
      <xsl:call-template name="nextLink">
      <xsl:with-param name="next" select="following-sibling::tei:div3[1]"/> 
     </xsl:call-template>
   </xsl:when>
  </xsl:choose>
 </xsl:otherwise>
</xsl:choose>

</p>
</xsl:template>


<xsl:template name="upLink">
<xsl:param name="up"/>
<xsl:param name="title"/>
<xsl:if test="$up and not($makeFrames='true')">
 <i><xsl:text> </xsl:text> <xsl:value-of select="$upWord"/>: </i>
 <a  class="navlink"> 
     <xsl:choose>
      <xsl:when test="$title">
         <xsl:attribute name="href">
           <xsl:value-of select="$linkPrefix"/>
           <xsl:value-of select="$up"/>
         </xsl:attribute>
         <xsl:value-of select="$title"/>
      </xsl:when>
      <xsl:otherwise>
       <xsl:attribute name="href">
           <xsl:value-of select="$linkPrefix"/>
        <xsl:apply-templates mode="xrefheader" select="$up"/>
       </xsl:attribute>
       <xsl:for-each select="$up">
        <xsl:call-template name="header">
         <xsl:with-param name="minimal" select="$minimalCrossRef"/>
        </xsl:call-template>
       </xsl:for-each>
      </xsl:otherwise>
     </xsl:choose>
   </a>  
</xsl:if>
</xsl:template>

<xsl:template name="previousLink">
<xsl:param name="previous"/>
<xsl:if test="$previous">
 <i><xsl:text> </xsl:text>
    <xsl:value-of select="$previousWord"/>: </i> 
    <a  class="navlink"> <xsl:attribute name="href">
           <xsl:value-of select="$linkPrefix"/>
     <xsl:apply-templates mode="xrefheader" select="$previous"/>
     </xsl:attribute>
     <xsl:for-each select="$previous">
       <xsl:call-template name="header">
        <xsl:with-param name="minimal" select="$minimalCrossRef"/>
       </xsl:call-template>
     </xsl:for-each>
    </a>  
</xsl:if>
</xsl:template>

<xsl:template name="nextLink">
<xsl:param name="next"/>
<xsl:if test="$next">
 <i><xsl:text> </xsl:text>
     <xsl:value-of select="$nextWord"/>: </i> <a  class="navlink">
       <xsl:attribute name="href">
           <xsl:value-of select="$linkPrefix"/>
      <xsl:apply-templates mode="xrefheader" select="$next"/>
     </xsl:attribute>
     <xsl:for-each select="$next">
       <xsl:call-template name="header">
        <xsl:with-param name="minimal" select="$minimalCrossRef"/>
       </xsl:call-template>
     </xsl:for-each>
     </a>  
</xsl:if>
</xsl:template> 

<xsl:template name="generateDivtitle">
  <xsl:apply-templates select="tei:head/text()"/>
</xsl:template> 

<xsl:template name="generateDivheading">
      <xsl:apply-templates select="." mode="header"/>
</xsl:template> 

<xsl:template name="startHook"/>

<xsl:template name="doDivBody">
<xsl:param name="Type"/>
<xsl:call-template name="startDivHook"/>
<xsl:variable name="ident">
   <xsl:apply-templates select="." mode="ident"/>
</xsl:variable>
 <xsl:choose>
  <xsl:when test="$leftLinks and $Type =''">
       <xsl:call-template name="linkList">
         <xsl:with-param name="side" select="'left'"/>
       </xsl:call-template>
  </xsl:when>
  <xsl:when test="$rightLinks and $Type  =''">
       <xsl:call-template name="linkList">
         <xsl:with-param name="side" select="'right'"/>
       </xsl:call-template>
  </xsl:when>
  <xsl:when test="parent::tei:div/@rend='multicol'">
     <td valign="top">
      <xsl:if test="not($Type = '')">
       <xsl:element name="h{$Type + $divOffset}">
        <a name="{$ident}"></a><xsl:call-template name="header"/>
       </xsl:element>
      </xsl:if>
      <xsl:apply-templates/>
     </td>
  </xsl:when>
  <xsl:when test="@rend='multicol'">
     <xsl:apply-templates select="*[not(name(.)='div')]"/>
   <table>
   <tr>
     <xsl:apply-templates select="tei:div"/>
   </tr>
   </table>
  </xsl:when>
  <xsl:otherwise>
      <xsl:if test="not($Type = '')">
       <xsl:element name="h{$Type + $divOffset}">
        <a name="{$ident}"></a><xsl:call-template name="header"/>
       </xsl:element>
      </xsl:if>
   <xsl:apply-templates/>
  </xsl:otherwise>
 </xsl:choose>
</xsl:template>


<xsl:template name="generateUpLink">
<xsl:variable name="BaseFile">
  <xsl:value-of select="$masterFile"/>
  <xsl:call-template name="addCorpusID"/>
</xsl:variable>
<xsl:choose>
 <xsl:when test="name(.) = 'div'">
  <xsl:call-template name="upLink">
    <xsl:with-param name="up" select="ancestor::tei:div[last()]"/>
  </xsl:call-template>
 </xsl:when>
 <xsl:otherwise>
  <xsl:choose>
    <xsl:when test="name(.)='div0'">
      <xsl:call-template name="upLink">
        <xsl:with-param name="up" select="concat($BaseFile,$standardSuffix)"/>
        <xsl:with-param name="title" select="$homeLabel"/>
      </xsl:call-template>
    </xsl:when>
    <xsl:when test="name(.)='div1'">
      <xsl:call-template name="upLink">
        <xsl:with-param name="up" select="concat($BaseFile,$standardSuffix)"/>
        <xsl:with-param name="title" select="$homeLabel"/>
      </xsl:call-template>
    </xsl:when>
    <xsl:when test="name(.)='div2'">
      <xsl:call-template name="upLink">
       <xsl:with-param name="up" select="ancestor::tei:div1"/>
      </xsl:call-template>
    </xsl:when>
    <xsl:when test="name(.)='div3'">
     <xsl:call-template name="upLink">
      <xsl:with-param name="up" select="ancestor::tei:div2"/>
     </xsl:call-template>
    </xsl:when>
    <xsl:when test="name(.)='div4'">
      <xsl:call-template name="upLink">
       <xsl:with-param name="up" select="ancestor::tei:div3"/>
      </xsl:call-template>
    </xsl:when>
    <xsl:when test="name(.)='div5'">
     <xsl:call-template name="upLink">
      <xsl:with-param name="up" select="ancestor::tei:div4"/>
     </xsl:call-template>
    </xsl:when>
    <xsl:otherwise>
     <xsl:call-template name="upLink">
      <xsl:with-param name="up" select="(ancestor::tei:div1|ancestor::tei:div)[1]"/>
     </xsl:call-template>
    </xsl:otherwise>
  </xsl:choose>
 </xsl:otherwise>
</xsl:choose>
</xsl:template>


<xsl:template name="addLangAtt">
  <xsl:variable name="supplied">
    <xsl:value-of select="ancestor-or-self::tei:*[@lang][1]/@lang"/>
  </xsl:variable>
  <xsl:attribute name="lang">
    <xsl:choose>
      <xsl:when test="$supplied">
        <xsl:text>en</xsl:text>
      </xsl:when>
      <xsl:otherwise>
        <xsl:value-of select="$supplied"/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:attribute>
</xsl:template>

<xsl:template name="addCorpusID">
 <xsl:if test="ancestor-or-self::tei:teiCorpus">
   <xsl:for-each select="ancestor-or-self::tei:TEI">
    <xsl:text>-</xsl:text>
     <xsl:choose>
      <xsl:when test="@id"><xsl:value-of select="@id"/></xsl:when> 
      <xsl:otherwise><xsl:number/></xsl:otherwise>
     </xsl:choose>
   </xsl:for-each>
  </xsl:if>
</xsl:template>


</xsl:stylesheet>
