<xsl:stylesheet version="1.0"
  xmlns:a="http://relaxng.org/ns/compatibility/annotations/1.0"
  xmlns:teix="http://www.tei-c.org/ns/Examples/"
  xmlns:rng="http://relaxng.org/ns/structure/1.0"
  xmlns:estr="http://exslt.org/strings"
  xmlns:pantor="http://www.pantor.com/ns/local"
  xmlns:exsl="http://exslt.org/common"
  xmlns:tei="http://www.tei-c.org/ns/1.0"
  xmlns:edate="http://exslt.org/dates-and-times"
  extension-element-prefixes="exsl estr edate"
  exclude-result-prefixes="exsl rng edate estr tei a pantor teix" 
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform">

<!--
$Date$ $Author$
odd2html.xsl

 XSLT script for converting TEI Odds (XML) to HTML

 Sebastian Rahtz, 26th April 2000, from SGMLSpm Perl version by LDB
                  15th May 2001, revised to deal with full TEI
		  October 2003, for P5

     Copyright 2001 Sebastian Rahtz/Oxford University/TEI Consortium  
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


<xsl:import href="../html/teihtml-teic.xsl"/>
<xsl:import href="../html/teihtml-odds.xsl"/>

<xsl:param name="indent-width" select="3"/>
<xsl:param name="line-width" select="80"/>

<xsl:key name="NameToID" match="tei:*" use="@ident"/>
<xsl:param name="institution">Text Encoding Initiative</xsl:param>
<xsl:param name="STDOUT"></xsl:param>
<xsl:param name="BITS">Bits</xsl:param>
<xsl:param name="masterFile">index</xsl:param>
<xsl:param name="oddmode">html</xsl:param>
<xsl:param name="leftLinks">true</xsl:param>
<xsl:param name="footnoteFile">true</xsl:param>
<xsl:param name="linksWidth">15%</xsl:param>
<xsl:param name="outputDir">Guidelines</xsl:param>
<xsl:param name="splitBackmatter">yes</xsl:param>
<xsl:param name="splitFrontmatter">yes</xsl:param>
<xsl:param name="splitLevel">1</xsl:param>
<xsl:param name="verbose">true</xsl:param>
<xsl:param name="autoToc">false</xsl:param>
<xsl:param name="topNavigationPanel">true</xsl:param>
<xsl:param name="numberHeadings">1.1.</xsl:param>
<xsl:param name="numberFrontHeadings"></xsl:param>
<xsl:param name="numberBackHeadings">A.1</xsl:param>
<xsl:param name="searchURL"/>
<xsl:param name="searchWords"/>
<xsl:param name="homeLabel">TEI P5 Home</xsl:param>
<xsl:variable name="linkColor"/>

<xsl:variable name="top" select="/"/>

<!-- the level of division for each mini TOCs are built -->
<xsl:param name="subTocDepth">-1</xsl:param>

<xsl:param name="tocDepth">3</xsl:param>



<xsl:template match="/">
<xsl:message>Start processing.  

splitLevel: <xsl:value-of select="$splitLevel"/>
</xsl:message>
  <xsl:apply-templates select="tei:TEI"/>
</xsl:template>

<xsl:template match="tei:titlePage">
  <p><center><xsl:apply-templates/></center></p><hr/>
</xsl:template>

<xsl:template match="tei:term">
  <a><xsl:attribute name="name">TDX-<xsl:number level="any"/>
  </xsl:attribute></a>
  <em><xsl:apply-templates/></em>
</xsl:template>

<xsl:template match="tei:titlePart">
  <p align="center"><b><xsl:apply-templates/></b></p>
</xsl:template>

<xsl:template match="tei:docTitle">
  <p align="center">
   <b><xsl:apply-templates/></b>
  </p>
</xsl:template>

<xsl:template match="tei:docAuthor">
  <p align="center"><em><xsl:value-of select="@n"/><xsl:text> </xsl:text>
   <xsl:apply-templates/></em></p>
</xsl:template>

<xsl:template match="processing-instruction()">
<!--
  <xsl:if test="name(.) = 'tei'">
    <xsl:choose>
      <xsl:when test="starts-with(.,'winita')">
    <p>
    <span style="color: red">NOTE: the following example 
    may not have been converted to XML yet!</span>
    </p>
     </xsl:when>
    </xsl:choose>
  </xsl:if>
-->
</xsl:template>


<!-- all notes go in the note file, numbered sequentially, whatever
     they say -->
<xsl:template match="tei:note">
<xsl:choose>
 <xsl:when test="ancestor::bibl">
  (<xsl:apply-templates/>)
 </xsl:when>

 <xsl:when test="@place='display'">
   <blockquote>NOTE <xsl:number level="any"/>:
    <xsl:apply-templates/>
   </blockquote>
 </xsl:when>

 <xsl:when test="@place='divtop'">
   <blockquote><i><xsl:apply-templates/></i></blockquote>
 </xsl:when>

 <xsl:otherwise>
 <xsl:variable name="identifier">
    <xsl:number level="any"/>
  </xsl:variable>
  <a class="notelink" 
      href="{$masterFile}-notes.html#{concat('Note',$identifier)}">
    <sup><xsl:value-of select="$identifier"/></sup></a>
 </xsl:otherwise>
</xsl:choose>
</xsl:template>

<xsl:template name="processFootnotes">
  <xsl:apply-templates select="//tei:note" mode="printnotes"/>
</xsl:template>

<xsl:template match="tei:note" mode="printnotes">
 <xsl:param name="root"/>
<xsl:if test="not(ancestor::bibl)">
<xsl:variable name="identifier">
    <xsl:number level="any"/>
</xsl:variable>
<p>
 <a name="{concat('Note',$identifier)}"><xsl:value-of select="$identifier"/>. </a>
 <xsl:apply-templates/>
</p>
</xsl:if>
</xsl:template>

<xsl:template name="bodyHook">
  <xsl:attribute name="background">background.gif</xsl:attribute>
</xsl:template>

<xsl:variable name="cssFile">tei.css</xsl:variable>

<xsl:template name="logoPicture">
<img src="jaco001d.gif" alt="" width="180" />
</xsl:template>

<xsl:variable name="headingNumberSuffix"><xsl:text> </xsl:text></xsl:variable>

<xsl:template name="GI">
    <tt><xsl:text>&lt;</xsl:text>
        <xsl:apply-templates/>
      <xsl:text>&gt;</xsl:text></tt>
</xsl:template>

<xsl:template name="generateSubTitle">
<h3 class="subtitle">
   <xsl:value-of 
        select="ancestor-or-self::TEI/teiHeader/tei:fileDesc/tei:titleStmt/tei:title"/>
</h3>
</xsl:template>


<xsl:template match="tei:figdesc">
<p><i><small>[<xsl:apply-templates/>]</small></i></p>
</xsl:template>


<xsl:template match="tei:revisionDesc//date">
  <xsl:apply-templates/>
</xsl:template>

<xsl:template match="tei:soCalled">
 <xsl:text>`</xsl:text>
 <xsl:apply-templates/>
 <xsl:text>'</xsl:text>
</xsl:template>





<xsl:template match="tei:divGen[@type='index']">
     <h2>Index</h2>
</xsl:template>


<xsl:template match="tei:div0">
<xsl:message>Process section <xsl:value-of select="@id"/></xsl:message>
    <xsl:apply-templates/>
</xsl:template>

<xsl:template name="locateParentdiv">
  <xsl:apply-templates select="ancestor::tei:div1" mode="ident"/>
</xsl:template>

<xsl:template name="locateParent">
      <xsl:apply-templates select="ancestor::tei:div1" mode="ident"/>
</xsl:template>

<xsl:template name="maintoc"> 
 <xsl:param name="force"/>

<xsl:call-template name="outputChunk">
  <xsl:with-param name="ident">fulltoc</xsl:with-param>
 <xsl:with-param name="content">
    <html> 
      <xsl:comment>THIS IS A GENERATED FILE. DO NOT EDIT</xsl:comment>
      <head>
      <title>TEI P5: Table of contents</title>
      <xsl:call-template name="headHook"/>
      <xsl:if test="not($cssFile = '')">
           <link rel="stylesheet" type="text/tei:css" href="{$cssFile}"/>
      </xsl:if>
      <xsl:call-template name="javaScript"/>
      </head>


      <body>
       <xsl:call-template name="bodyHook"/>
       <xsl:call-template name="bodyJavaScript"/>
       <a name="TOP"/>
       <div  class="teidiv">
        <xsl:call-template name="stdheader">
          <xsl:with-param name="title">Full Table of Contens</xsl:with-param>
        </xsl:call-template>
         <h2>Detailed table of contents</h2>

  <p><a class="toc" href="index.html">Back to start</a></p>
  <hr/>
   <xsl:for-each select="ancestor-or-self::tei.2/tei:text/tei:front">
    <xsl:apply-templates 
      select=".//tei:div1" mode="maketoc">
     <xsl:with-param name="forcedepth" select="$force"/>
    </xsl:apply-templates>
   </xsl:for-each>

   <xsl:for-each select="ancestor-or-self::tei.2/tei:text/tei:body">
    <xsl:apply-templates 
      select=".//tei:div1" mode="maketoc">
     <xsl:with-param name="forcedepth" select="$force"/>
    </xsl:apply-templates>
   </xsl:for-each>

   <xsl:for-each select="ancestor-or-self::tei.2/tei:text/tei:back">
    <xsl:apply-templates 
      select=".//tei:div1" mode="maketoc">
     <xsl:with-param name="forcedepth" select="$force"/>
   </xsl:apply-templates>
   </xsl:for-each>
 </div>
</body>
</html>
</xsl:with-param>
</xsl:call-template>

</xsl:template>

<xsl:template name="linkList">
<xsl:param name="style" select="'toc-left'"/>
<xsl:variable name="thisname">
      <xsl:value-of select="name()"/>
</xsl:variable>
   <table class="leftlinks">
   <tr>
     <td width="15%" valign="top">
<xsl:choose>
 <xsl:when test="$thisname='TEI'">
   <xsl:for-each select="tei:text/tei:front">
    <xsl:for-each select=".//tei:div1">
       <xsl:variable name="pointer">
          <xsl:apply-templates mode="xrefheader" select="."/>
       </xsl:variable>
       <p class="{$style}">
       <a class="{$style}" href="{$pointer}">
         <xsl:call-template name="header"/></a>
       </p>
    </xsl:for-each>
    <hr/>
   </xsl:for-each>
   <xsl:for-each select="tei:text/tei:body">
    <xsl:for-each select=".//tei:div1">
       <xsl:variable name="pointer">
          <xsl:apply-templates mode="xrefheader" select="."/>
       </xsl:variable>
       <p class="{$style}">
       <a class="{$style}" href="{$pointer}">
         <xsl:call-template name="header"/></a>
       </p>
    </xsl:for-each>
   </xsl:for-each>
   <xsl:for-each select="tei:text/tei:back">
    <hr/>
    <xsl:for-each select=".//tei:div1">
       <xsl:variable name="pointer">
          <xsl:apply-templates mode="xrefheader" select="."/>
       </xsl:variable>
       <p class="{$style}">
       <a class="{$style}" href="{$pointer}">
         <xsl:call-template name="header"/></a>
       </p>
    </xsl:for-each>
   </xsl:for-each>
 </xsl:when>
 <xsl:otherwise>
<!-- root -->
  <xsl:variable name="BaseFile">
   <xsl:value-of select="$masterFile"/>
   <xsl:if test="ancestor::teiCorpus">
    <xsl:text>-</xsl:text>
     <xsl:choose>
      <xsl:when test="@id"><xsl:value-of select="@id"/></xsl:when> 
      <xsl:otherwise><xsl:number/></xsl:otherwise>
     </xsl:choose>
   </xsl:if>
  </xsl:variable>
  <p class="{$style}">
    <a class="{$style}" href="{$BaseFile}.html">
    <xsl:value-of select="$homeLabel"/></a>
  </p>
    <hr/>
 <xsl:for-each select="ancestor::tei:div2|ancestor::tei:div3|ancestor::tei:div4|ancestor::tei:div5">
  <p class="{$style}">
      <a class="{$style}">
        <xsl:attribute name="href">
         <xsl:apply-templates mode="xrefheader" select="."/>
        </xsl:attribute>
       <xsl:call-template name="header"/>
      </a>
    </p>
      <hr/>
 </xsl:for-each>


<p class="{$style}">
 <a class="{$style}">
        <xsl:attribute name="href">
         <xsl:apply-templates mode="xrefheader" select="."/>
        </xsl:attribute>
       <xsl:call-template name="header"/>
     </a></p>

<!-- ... any children it has -->
   <xsl:for-each select="tei:div2|tei:div3|tei:div4|tei:div5">
      <p class="{$style}-sub"><a class="{$style}-sub">
        <xsl:attribute name="href">
         <xsl:apply-templates mode="xrefheader" select="."/>
        </xsl:attribute>
        <xsl:call-template name="header"/>
      </a></p>
   </xsl:for-each>


 <hr/>
<!-- preceding divisions -->
   <xsl:for-each select="preceding::tei:div1">
      <p class="{$style}">
      <a class="{$style}">
        <xsl:attribute name="href">
         <xsl:apply-templates mode="xrefheader" select="."/>
        </xsl:attribute>
       <xsl:call-template name="header"/>
     </a></p>
   </xsl:for-each>

<!-- current division -->
      <p class="{$style}-this">
      <a class="{$style}-this">
        <xsl:attribute name="href">
         <xsl:apply-templates mode="xrefheader" select="."/>
        </xsl:attribute>
       <xsl:call-template name="header"/>
     </a></p>

<!-- following divisions -->
   <xsl:for-each select="following::tei:div1">
      <p class="{$style}">
      <a class="{$style}">
        <xsl:attribute name="href">
         <xsl:apply-templates mode="xrefheader" select="."/>
        </xsl:attribute>
        <xsl:call-template name="header"/>
      </a></p>
   </xsl:for-each>
 </xsl:otherwise>
</xsl:choose>
     </td>
     <td width="85%" valign="top"><xsl:apply-templates/></td>
   </tr>
   </table>
</xsl:template>

<!-- this overrides the standard template, to allow for
     <div1> elements being numbered across <div0>
-->
<xsl:template name="xrefpanel">
<xsl:param name="homepage"/>
<xsl:param name="mode"/>

<p align="{$alignNavigationPanel}">

   <xsl:variable name="Parent">
     <xsl:call-template name="locateParent"/>
     <xsl:text>.html</xsl:text>
   </xsl:variable>
   <xsl:choose>
    <xsl:when test="$Parent = '.html'">
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
    <xsl:when test="name(.)='div0'">
      <xsl:call-template name="previousLink">
      <xsl:with-param name="previous" select="preceding-sibling::tei:div0[1]"/>
     </xsl:call-template>
    </xsl:when>
    <xsl:when test="name(.)='div1'">
      <xsl:call-template name="previousLink">
      <xsl:with-param name="previous" select="preceding::tei:div1[1]"/>
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

  <xsl:choose>
    <xsl:when test="name(.)='div0'">
      <xsl:call-template name="nextLink">
      <xsl:with-param name="next" select="following-sibling::tei:div0[1]"/>
     </xsl:call-template>
    </xsl:when>
    <xsl:when test="name(.)='div1'">
      <xsl:call-template name="nextLink">
      <xsl:with-param name="next" select="following::tei:div1[1]"/>
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

</p>
</xsl:template>

<xsl:template match="tei:divGen[@type='index']">
<xsl:message>    Building the index   </xsl:message>
<xsl:message>   ....of index  elements  </xsl:message>
<xsl:variable name="Index">
<Indexterms>
 <xsl:for-each select="//tei:index">
  <index c="{@level}" a="{@level1}" b="{@level2}">
     <file>
       <xsl:apply-templates select="ancestor::tei:div1" mode="xrefheader"/>
     </file>
     <section>
       <xsl:apply-templates select="(ancestor::tei:div1|ancestor::tei:div2|ancestor::tei:div3|ancestor::tei:div4|ancestor::tei:div5)[last()]" mode="header">
         <xsl:with-param name="minimal"></xsl:with-param>
       </xsl:apply-templates>
     </section>
     <loc>
       <xsl:text>IDX-</xsl:text><xsl:number level="any"/>
     </loc>
  </index>
</xsl:for-each>
  <xsl:message>   ....of term  elements  </xsl:message>
<xsl:for-each select="//tei:term">
   <xsl:if test="not(@rend='noindex')">
  <index c="{text()}" a="{text()}">
     <file>
       <xsl:apply-templates select="ancestor::tei:div1" mode="xrefheader"/>
     </file>
     <section>
       <xsl:apply-templates select="(ancestor::tei:div1|ancestor::tei:div2|ancestor::tei:div3|ancestor::tei:div4|ancestor::tei:div5)[last()]" mode="header">
         <xsl:with-param name="minimal"></xsl:with-param>
       </xsl:apply-templates>
     </section>
     <loc>
       <xsl:text>TDX-</xsl:text><xsl:number level="any"/>
     </loc>
  </index>
   </xsl:if>
</xsl:for-each>
<!--
  <xsl:message>   ....of gi  elements  </xsl:message>
<xsl:for-each select="//gi">
   <xsl:if test="not(@rend='noindex')">
  <index a="{text()}">
    <xsl:attribute name="c">
      <xsl:text>&lt;</xsl:text>
      <xsl:value-of select="."/><xsl:text>&gt;</xsl:text>
    </xsl:attribute>
     <file>
       <xsl:apply-templates select="ancestor::tei:div1" mode="xrefheader"/>
     </file>
     <section>
       <xsl:apply-templates select="(ancestor::tei:div1|ancestor::tei:div2|ancestor::tei:div3|ancestor::tei:div4|ancestor::tei:div5)[last()]" mode="header">
         <xsl:with-param name="minimal"></xsl:with-param>
       </xsl:apply-templates>
     </section>
     <loc>
       <xsl:text>GDX-</xsl:text><xsl:number level="any"/>
     </loc>
  </index>
   </xsl:if>
</xsl:for-each>
-->
</Indexterms>
</xsl:variable>
  <xsl:message>   ....sorting </xsl:message>
  <xsl:variable name="sindex">
    <Indexterms>
    <xsl:for-each select="exsl:node-set($Index)/Indexterms/index">
    <xsl:sort select="@a"/>
    <xsl:sort select="@b"/>
    <xsl:copy-of select="."/>
  </xsl:for-each>
</Indexterms>
</xsl:variable>
  <xsl:message>   done  </xsl:message>
<dl>
<xsl:for-each select="exsl:node-set($sindex)/Indexterms/index">
  <xsl:if test="not(@a=preceding-sibling::tei:index/@a)">
    <dt><xsl:value-of select="@c"/></dt>
    <dd>
      <xsl:for-each select=".|following-sibling::tei:index[@a=current()/@a]">
          <xsl:if test="@b and not(@b=preceding-sibling::tei:index/@b)">
            <br/>&#160;&#160;<xsl:value-of select="@b"/><br/>
          </xsl:if>
          <a href="{file}#{loc}"><xsl:value-of select="section"/></a>&#160;
      </xsl:for-each>
    </dd>
  </xsl:if>
</xsl:for-each>
</dl>
</xsl:template>

<xsl:template name="calculateNumber">
 <xsl:param name="numbersuffix"/>
 <xsl:choose>
   <xsl:when test="local-name() = 'TEI'">
        <xsl:value-of select="tei:teiHeader//tei:title"/>
   </xsl:when>
   <xsl:when test="local-name(.)='div0'">
     <xsl:number format="I"/>
     <xsl:value-of select="$numbersuffix"/>
   </xsl:when>
   <xsl:when test="local-name(.)='div1'">
    <xsl:choose>
     <xsl:when test="ancestor::tei:back">
       <xsl:if test="not($numberBackHeadings='')">
        <xsl:value-of select="$appendixWords"/><xsl:text> </xsl:text>
        <xsl:number format="{$numberBackHeadings}" from="tei:back" level="any"/>
        <xsl:value-of select="$numbersuffix"/>
       </xsl:if>
     </xsl:when>
     <xsl:when test="ancestor::tei:front">
       <xsl:if test="not($numberFrontHeadings='')">
         <xsl:number format="{$numberFrontHeadings}" from="tei:front" level="any"/>
        <xsl:value-of select="$numbersuffix"/>
       </xsl:if>
     </xsl:when>
     <xsl:when test="not($numberHeadings ='')">
       <xsl:choose>
       <xsl:when test="$prenumberedHeadings">
       		<xsl:value-of select="@n"/>
       </xsl:when>
       <xsl:otherwise>
 	 <xsl:number format="1" from="tei:body" level="any"/>
        <xsl:value-of select="$numbersuffix"/>
       </xsl:otherwise>
       </xsl:choose>
     </xsl:when>
   </xsl:choose>
  </xsl:when>
  <xsl:when test="ancestor::tei:back">
     <xsl:if test="not($numberBackHeadings='')">
        <xsl:value-of select="$appendixWords"/><xsl:text> </xsl:text>
        <xsl:for-each select="ancestor::tei:div1">
          <xsl:number level="any" from="tei:back" format="{$numberBackHeadings}"/>
          <!--          <xsl:text>.</xsl:text>-->
        </xsl:for-each>
        <xsl:number format="{$numberHeadings}" from="tei:div1"
         level="multiple" count="tei:div2|tei:div3|tei:div4|tei:div5|tei:div6"/>
          <xsl:value-of select="$numbersuffix"/>
     </xsl:if>
   </xsl:when>
   <xsl:when test="ancestor::tei:front">
     <xsl:if test="not($numberFrontHeadings='')">
      <xsl:for-each select="ancestor::tei:div1">
         <xsl:number level="any" from="tei:front" format="{$numberFrontHeadings}"/>
         <xsl:value-of select="$numbersuffix"/>
      </xsl:for-each>
      <xsl:number format="{$numberFrontHeadings}" from="tei:div1"
            level="multiple" count="tei:div2|tei:div3|tei:div4|tei:div5|tei:div6"/>
         <xsl:value-of select="$numbersuffix"/>
      </xsl:if>
   </xsl:when>
   <xsl:when test="not($numberHeadings ='')">
       <xsl:choose>
       <xsl:when test="$prenumberedHeadings">
       		<xsl:value-of select="@n"/>
   </xsl:when>
   <xsl:otherwise>
       <xsl:variable name="pre">
        <xsl:for-each select="ancestor::tei:div1">
          <xsl:number level="any" from="tei:body" format="{$numberHeadings}"/>
        </xsl:for-each>
       </xsl:variable>
       <xsl:variable name="post">
            <xsl:choose>
             <xsl:when test="local-name(.)='div2'">
          <xsl:number level="multiple" from="tei:div1" format="1"
            count="tei:div2|tei:div3|tei:div4|tei:div5|tei:div6"/>
           </xsl:when>
             <xsl:when test="local-name(.)='div3'">
          <xsl:number level="multiple" from="tei:div1" format="1.1"
            count="tei:div2|tei:div3|tei:div4|tei:div5|tei:div6"/>
             </xsl:when>
              <xsl:when test="local-name(.)='div4'">
       <xsl:number level="multiple" from="tei:div1" format="1.1.1"
            count="tei:div2|tei:div3|tei:div4|tei:div5|tei:div6"/>
                </xsl:when>
            </xsl:choose>

       </xsl:variable>
       <xsl:value-of select="$pre"/><xsl:value-of select="$post"/>
         <xsl:value-of select="$numbersuffix"/>
   </xsl:otherwise>
  </xsl:choose>
</xsl:when>
</xsl:choose>
</xsl:template>

<xsl:template name="header">
 <xsl:param name="minimal"/>
 <xsl:param name="toc"/>
 <xsl:variable name="depth">
     <xsl:apply-templates select="." mode="depth"/>
 </xsl:variable>
 <xsl:if test="not($depth &gt; $numberHeadingsDepth)">
   <xsl:call-template name="calculateNumber">
       <xsl:with-param name="numbersuffix" select="$headingNumberSuffix"/>
     </xsl:call-template>
 </xsl:if>
 <xsl:if test="not($minimal)">
    <xsl:value-of select="$headingNumberSuffix"/>
    <xsl:choose>
      <xsl:when test="not($toc='')">
        <xsl:call-template name="makeLink">
          <xsl:with-param name="class">toc</xsl:with-param>
          <xsl:with-param name="url"><xsl:value-of select="$toc"/></xsl:with-param>
          <xsl:with-param name="text">
            <xsl:apply-templates mode="plain" select="tei:head"/>
          </xsl:with-param>
        </xsl:call-template>
      </xsl:when>
      <xsl:otherwise>
       <xsl:apply-templates mode="plain" select="tei:head"/>
      </xsl:otherwise>
     </xsl:choose>
 </xsl:if>

</xsl:template>


</xsl:stylesheet>

