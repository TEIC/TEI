<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet version="1.0"
    xmlns:html="http://www.w3.org/1999/xhtml" 
    xmlns:xd="http://www.pnp-software.com/XSLTdoc"
    xmlns:a="http://relaxng.org/ns/compatibility/annotations/1.0"
    xmlns:teix="http://www.tei-c.org/ns/Examples"
    xmlns:xs="http://www.w3.org/2001/XMLSchema" 
    xmlns:rng="http://relaxng.org/ns/structure/1.0"
    xmlns:estr="http://exslt.org/strings"
    xmlns:pantor="http://www.pantor.com/ns/local"
    xmlns:exsl="http://exslt.org/common"
    xmlns:tei="http://www.tei-c.org/ns/1.0"
    xmlns:edate="http://exslt.org/dates-and-times"
    extension-element-prefixes="exsl estr edate"
    exclude-result-prefixes="exsl rng edate estr tei html a pantor teix xs xd" 
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform">


<xsl:import href="teiodds.xsl"/>
<xsl:import href="../html/tei.xsl"/>
<xsl:import href="../html/tagdocs.xsl"/>
<xsl:import href="RngToRnc.xsl"/>

<xd:doc type="stylesheet">
    <xd:short>
    TEI stylesheet for making HTML from ODD
      </xd:short>
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
    <xd:author>Sebastian Rahtz sebastian.rahtz@oucs.ox.ac.uk</xd:author>
    <xd:cvsId>$Id$</xd:cvsId>
    <xd:copyright>2005, TEI Consortium</xd:copyright>
  </xd:doc>

<xsl:output method="html"/>
<xsl:key name="NameToID" match="tei:*" use="@ident"/>

<xsl:param name="BITS">Bits</xsl:param>
<xsl:param name="STDOUT">false</xsl:param>
<xsl:param name="TAG"/>
<xsl:param name="alignNavigationPanel">left</xsl:param>
<xsl:param name="authorWord"></xsl:param>
<xsl:param name="autoToc">false</xsl:param>
<xsl:param name="bottomNavigationPanel">true</xsl:param>
<xsl:param name="cssFile">http://www.tei-c.org/stylesheet/tei.css</xsl:param>
<xsl:param name="dateWord"></xsl:param>
<xsl:param name="displayMode">rnc</xsl:param>
<xsl:param name="feedbackURL">http://www.tei-c.org/Consortium/TEI-contact.html</xsl:param>
<xsl:param name="feedbackWords">Contact</xsl:param>
<xsl:param name="footnoteFile">true</xsl:param>
<xsl:param name="homeLabel">TEI P5 Home</xsl:param>
<xsl:param name="homeURL">http://www.tei-c.org/</xsl:param>
<xsl:param name="homeWords">TEI Home</xsl:param>
<xsl:param name="indent-width" select="3"/>
<xsl:param name="institution">Text Encoding Initiative</xsl:param>
<xsl:param name="line-width" select="80"/>
<xsl:param name="numberBackHeadings">A.1</xsl:param>
<xsl:param name="numberFrontHeadings"></xsl:param>
<xsl:param name="numberBodyHeadings">1.1.</xsl:param>
<xsl:param name="oddmode">html</xsl:param>
<xsl:param name="outputDir">Guidelines</xsl:param>
<xsl:param name="pageLayout">CSS</xsl:param>
<xsl:param name="searchURL">http://search.ox.ac.uk/web/related/natproj/tei</xsl:param>
<xsl:param name="searchWords">Search this site</xsl:param>
<xsl:param name="showTitleAuthor">1</xsl:param>
<xsl:param name="splitBackmatter">yes</xsl:param>
<xsl:param name="splitFrontmatter">yes</xsl:param>
<xsl:param name="splitLevel">1</xsl:param>
<xsl:param name="subTocDepth">-1</xsl:param>
<xsl:param name="tocDepth">3</xsl:param>
<xsl:param name="topNavigationPanel"></xsl:param>
<xsl:param name="verbose">false</xsl:param>
<xsl:template name="copyrightStatement">Copyright TEI Consortium 2005</xsl:template>
<xsl:variable name="top" select="/"/>

<xsl:template name="metaHook">
  <xsl:param name="title"/>
 <meta name="DC.Title" content="{$title}"/>
 <meta name="DC.Language" content="(SCHEME=iso639) en"/> 
 <meta name="DC.Creator" content="TEI,Oxford University Computing Services, 13 Banbury Road, Oxford OX2 6NN, United Kingdom"/>
 <meta name="DC.Creator.Address" content="tei@oucs.ox.ac.uk"/>
</xsl:template>

<xsl:template name="bodyHook">
  <xsl:attribute name="background">background.gif</xsl:attribute>
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
<xsl:template match="tei:div0">
    <xsl:apply-templates/>
</xsl:template>

<xsl:template match="tei:divGen[@type='index']">
<xsl:variable name="Index">
<Indexterms>
 <xsl:for-each select="//tei:index">
  <index c="{@level}" a="{@level1}" b="{@level2}">
     <file>
       <xsl:apply-templates select="ancestor::tei:div1" mode="generateLink"/>
     </file>
     <section>
       <xsl:apply-templates select="(ancestor::tei:div1|ancestor::tei:div2|ancestor::tei:div3|ancestor::tei:div4|ancestor::tei:div5)[last()]" mode="ident">
         <xsl:with-param name="minimal">false</xsl:with-param>
       </xsl:apply-templates>
     </section>
     <loc>
       <xsl:text>IDX-</xsl:text><xsl:number level="any"/>
     </loc>
  </index>
</xsl:for-each>
<xsl:for-each select="//tei:term">
   <xsl:if test="not(@rend='noindex')">
  <index c="{text()}" a="{text()}">
     <file>
       <xsl:apply-templates select="ancestor::tei:div1" mode="generateLink"/>
     </file>
     <section>
       <xsl:apply-templates select="(ancestor::tei:div1|ancestor::tei:div2|ancestor::tei:div3|ancestor::tei:div4|ancestor::tei:div5)[last()]" mode="ident">
         <xsl:with-param name="minimal">false</xsl:with-param>
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
       <xsl:apply-templates select="ancestor::tei:div1" mode="generateLink"/>
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
  <xsl:variable name="sindex">
    <Indexterms>
    <xsl:for-each select="exsl:node-set($Index)/Indexterms/index">
    <xsl:sort select="@a"/>
    <xsl:sort select="@b"/>
    <xsl:copy-of select="."/>
  </xsl:for-each>
</Indexterms>
</xsl:variable>
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

<xsl:template match="tei:docAuthor">
  <p align="center"><em><xsl:value-of select="@n"/><xsl:text> </xsl:text>
   <xsl:apply-templates/></em></p>
</xsl:template>

<xsl:template match="tei:docTitle">
  <p align="center">
   <b><xsl:apply-templates/></b>
  </p>
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

<xsl:template match="tei:revisionDesc//date">
  <xsl:apply-templates/>
</xsl:template>

<xsl:template match="tei:term">
  <a><xsl:attribute name="name">TDX-<xsl:number level="any"/>
  </xsl:attribute></a>
  <em><xsl:apply-templates/></em>
</xsl:template>

<xsl:template match="tei:titlePage">
  <p><center><xsl:apply-templates/></center></p><hr/>
</xsl:template>

<xsl:template match="tei:titlePart">
  <p align="center"><b><xsl:apply-templates/></b></p>
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
        <xsl:call-template name="i18n"><xsl:with-param name="word">appendixWords</xsl:with-param></xsl:call-template>
	<xsl:text> </xsl:text>
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
     <xsl:when test="$numberHeadings ='true'">
       <xsl:choose>
       <xsl:when test="$prenumberedHeadings='true'">
       		<xsl:value-of select="@n"/>
       </xsl:when>
       <xsl:otherwise>
	 <xsl:choose>
	   <xsl:when test="@n">
	     <xsl:value-of select="@n"/>
	   </xsl:when>
	   <xsl:otherwise>
	   <xsl:number format="{$numberBodyHeadings}" from="tei:body"
		       level="any"/>
	   </xsl:otherwise>
	 </xsl:choose>
        <xsl:value-of select="$numbersuffix"/>
       </xsl:otherwise>
       </xsl:choose>
     </xsl:when>
   </xsl:choose>
  </xsl:when>
  <xsl:when test="ancestor::tei:back">
     <xsl:if test="not($numberBackHeadings='')">
        <xsl:call-template name="i18n"><xsl:with-param name="word">appendixWords</xsl:with-param></xsl:call-template>
	<xsl:text> </xsl:text>
        <xsl:for-each select="ancestor::tei:div1">
          <xsl:number level="any" from="tei:back" format="{$numberBackHeadings}"/>
          <!--          <xsl:text>.</xsl:text>-->
        </xsl:for-each>
        <xsl:number format="{$numberBodyHeadings}" from="tei:div1"
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
   <xsl:when test="$numberHeadings ='true'">
       <xsl:choose>
       <xsl:when test="$prenumberedHeadings='true'">
       		<xsl:value-of select="@n"/>
   </xsl:when>
   <xsl:otherwise>
       <xsl:variable name="pre">
        <xsl:for-each select="ancestor::tei:div1">
          <xsl:number level="any" from="tei:body" format="{$numberBodyHeadings}"/>
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
 <xsl:param name="minimal">false</xsl:param>
 <xsl:param name="toc"/>
 <xsl:variable name="depth">
     <xsl:apply-templates select="." mode="depth"/>
 </xsl:variable>
 <xsl:if test="$numberHeadingsDepth &gt;= $depth">
   <xsl:call-template name="calculateNumber">
       <xsl:with-param name="numbersuffix" select="$headingNumberSuffix"/>
     </xsl:call-template>
 </xsl:if>
 <xsl:if test="$minimal='false'">
    <xsl:value-of select="$headingNumberSuffix"/>
    <xsl:choose>
      <xsl:when test="contains(name(.),'Spec')">
	<xsl:call-template name="makeLink">
	  <xsl:with-param name="class">toc</xsl:with-param>
	  <xsl:with-param name="name">
	    <xsl:value-of select="@ident"/>
	  </xsl:with-param>
	</xsl:call-template>
      </xsl:when>
      <xsl:when test="not($toc='')">
        <xsl:call-template name="makeInternalLink">
          <xsl:with-param name="class">toc</xsl:with-param>
          <xsl:with-param name="dest"><xsl:value-of select="$toc"/></xsl:with-param>
          <xsl:with-param name="body">
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

<xsl:template name="linkListContents">
<xsl:param name="style"/>
  <xsl:variable name="thisname">
    <xsl:value-of select="local-name()"/>
  </xsl:variable>
    <xsl:choose>
      <xsl:when test="$thisname='TEI'">
	<xsl:for-each select="tei:text/tei:front">
	  <xsl:for-each select=".//tei:div1">
	    <xsl:variable name="pointer">
	      <xsl:apply-templates mode="generateLink" select="."/>
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
	      <xsl:apply-templates mode="generateLink" select="."/>
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
	      <xsl:apply-templates mode="generateLink" select="."/>
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
	      <xsl:when test="@xml:id"><xsl:value-of select="@xml:id"/></xsl:when> 
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
		    <xsl:apply-templates mode="generateLink" select="."/>
		  </xsl:attribute>
		  <xsl:call-template name="header"/>
		</a>
	      </p>
	      <hr/>
	    </xsl:for-each>
	    
	    
	    <p class="{$style}">
	      <a class="{$style}">
		<xsl:attribute name="href">
		  <xsl:apply-templates mode="generateLink" select="."/>
		</xsl:attribute>
		<xsl:call-template name="header"/>
	    </a></p>
	    
	    <!-- ... any children it has -->
	    <xsl:for-each select="tei:div2|tei:div3|tei:div4|tei:div5">
	      <p class="{$style}-sub"><a class="{$style}-sub">
		<xsl:attribute name="href">
		  <xsl:apply-templates mode="generateLink" select="."/>
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
		    <xsl:apply-templates mode="generateLink" select="."/>
		  </xsl:attribute>
		  <xsl:call-template name="header"/>
	      </a></p>
	    </xsl:for-each>
	    
	    <!-- current division -->
	    <p class="{$style}-this">
	      <a class="{$style}-this">
		<xsl:attribute name="href">
		  <xsl:apply-templates mode="generateLink" select="."/>
		</xsl:attribute>
		<xsl:call-template name="header"/>
	    </a></p>
	    
	    <!-- following divisions -->
	    <xsl:for-each select="following::tei:div1">
	      <p class="{$style}">
		<a class="{$style}">
		  <xsl:attribute name="href">
		    <xsl:apply-templates mode="generateLink" select="."/>
		  </xsl:attribute>
		  <xsl:call-template name="header"/>
	      </a></p>
	    </xsl:for-each>
	  </xsl:otherwise>
	</xsl:choose>
	<xsl:if test="$splitLevel=-1">
	  <xsl:call-template name="listSpecs"/>
	</xsl:if>
</xsl:template>

<!-- this overrides the standard template, to allow for
     <div1> elements being numbered across <div0>
-->
<xsl:template name="locateParent">
      <xsl:apply-templates select="ancestor::tei:div1" mode="ident"/>
</xsl:template>

<xsl:template name="locateParentdiv">
  <xsl:apply-templates select="ancestor::tei:div1" mode="ident"/>
</xsl:template>

<xsl:template name="logoPicture">
<img src="jaco001d.gif" alt="" width="180" />
</xsl:template>

<xsl:variable name="headingNumberSuffix"><xsl:text> </xsl:text></xsl:variable>

<xsl:template name="maintoc"> 
  <xsl:param name="force"/>
   <xsl:for-each select="ancestor-or-self::tei:TEI/tei:text/tei:front">
    <xsl:apply-templates 
      select=".//tei:div1" mode="maketoc">
     <xsl:with-param name="forcedepth" select="$force"/>
    </xsl:apply-templates>
   </xsl:for-each>

   <xsl:for-each select="ancestor-or-self::tei:TEI/tei:text/tei:body">
    <xsl:apply-templates 
      select=".//tei:div1" mode="maketoc">
     <xsl:with-param name="forcedepth" select="$force"/>
    </xsl:apply-templates>
   </xsl:for-each>

   <xsl:for-each select="ancestor-or-self::tei:TEI/tei:text/tei:back">
    <xsl:apply-templates 
      select=".//tei:div1" mode="maketoc">
     <xsl:with-param name="forcedepth" select="$force"/>
   </xsl:apply-templates>
   </xsl:for-each>
</xsl:template>

<xsl:template name="processFootnotes">
  <xsl:apply-templates select="//tei:note" mode="printnotes"/>
</xsl:template>

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
    <xsl:when test="local-name(.)='div0'">
      <xsl:call-template name="previousLink">
      <xsl:with-param name="previous" select="preceding-sibling::tei:div0[1]"/>
     </xsl:call-template>
    </xsl:when>
    <xsl:when test="local-name(.)='div1'">
      <xsl:call-template name="previousLink">
      <xsl:with-param name="previous" select="preceding::tei:div1[1]"/>
     </xsl:call-template>
    </xsl:when>
    <xsl:when test="local-name(.)='div2'">
      <xsl:call-template name="previousLink">
      <xsl:with-param name="previous" select="preceding-sibling::tei:div2[1]"/>
     </xsl:call-template>
    </xsl:when>
    <xsl:when test="local-name(.)='div3'">
      <xsl:call-template name="previousLink">
      <xsl:with-param name="previous" select="preceding-sibling::tei:div3[1]"/> 
     </xsl:call-template>
   </xsl:when>
  </xsl:choose>

  <xsl:choose>
    <xsl:when test="local-name(.)='div0'">
      <xsl:call-template name="nextLink">
      <xsl:with-param name="next" select="following-sibling::tei:div0[1]"/>
     </xsl:call-template>
    </xsl:when>
    <xsl:when test="local-name(.)='div1'">
      <xsl:call-template name="nextLink">
      <xsl:with-param name="next" select="following::tei:div1[1]"/>
     </xsl:call-template>
    </xsl:when>
    <xsl:when test="local-name(.)='div2'">
      <xsl:call-template name="nextLink">
      <xsl:with-param name="next" select="following-sibling::tei:div2[1]"/>
     </xsl:call-template>
    </xsl:when>
    <xsl:when test="local-name(.)='div3'">
      <xsl:call-template name="nextLink">
      <xsl:with-param name="next" select="following-sibling::tei:div3[1]"/> 
     </xsl:call-template>
   </xsl:when>
  </xsl:choose>

</p>
</xsl:template>


  <xsl:template name="processSchemaFragment">
    <xsl:param name="filename"/>
        <hr/>
    
	<xsl:if test="tei:classSpec">
	  <h1>
        <xsl:call-template name="i18n">
	  <xsl:with-param  name="word">Classes defined</xsl:with-param>
	</xsl:call-template>
	  </h1>
	  <xsl:apply-templates mode="weave" select="tei:classSpec">
	    <xsl:sort select="tei:altIdent|@ident"/>
	  </xsl:apply-templates>
	</xsl:if>
    
    
	<h1>
	  <xsl:call-template name="i18n">
	    <xsl:with-param  name="word">Elements defined</xsl:with-param>
	  </xsl:call-template>
	</h1>
	<xsl:apply-templates mode="weave" select="tei:elementSpec">
	  <xsl:sort select="tei:altIdent|@ident"/>
	</xsl:apply-templates>
	
	<xsl:if test="tei:macroSpec">
	  <h1>
        <xsl:call-template name="i18n">
	  <xsl:with-param  name="word">Macros defined</xsl:with-param>
	</xsl:call-template>

	  </h1>
	  <xsl:apply-templates mode="weave" select="tei:macroSpec">
	    <xsl:sort select="tei:altIdent|@ident"/>
	  </xsl:apply-templates>
	</xsl:if>
      <hr/>
  </xsl:template>

  <xsl:template name="listSpecs">
    <xsl:for-each select="..//tei:schemaSpec">
      <hr/>
      <xsl:for-each select="tei:classSpec">
	<xsl:sort select="tei:altIdent"/>
	<xsl:sort select="@ident"/>
	<p class="toclist0"><a class="toclist" href="#{@ident}">
	<xsl:choose>
	  <xsl:when test="tei:altIdent">
	    <xsl:value-of select="tei:altIdent"/>
	  </xsl:when>
	  <xsl:otherwise>
	    <xsl:value-of select="@ident"/>
	  </xsl:otherwise>
	</xsl:choose>
      </a>
	</p>
      </xsl:for-each>
      <hr/>
      <xsl:for-each select="tei:elementSpec"> 
	<xsl:sort select="tei:altIdent"/>
	<xsl:sort select="@ident"/>
	<p class="toclist0"><a class="toclist" href="#{@ident}">
	<xsl:choose>
	  <xsl:when test="tei:altIdent">
	    <xsl:value-of select="tei:altIdent"/>
	  </xsl:when>
	  <xsl:otherwise>
	    <xsl:value-of select="@ident"/>
	  </xsl:otherwise>
	</xsl:choose>
      </a>
	</p>
      </xsl:for-each>
      <hr/>
      <xsl:for-each select="tei:macroSpec">
	<xsl:sort select="tei:altIdent"/>
	<xsl:sort select="@ident"/>
	<p class="toclist0"><a class="toclist" href="#{@ident}">
	<xsl:choose>
	  <xsl:when test="tei:altIdent">
	    <xsl:value-of select="tei:altIdent"/>
	  </xsl:when>
	  <xsl:otherwise>
	    <xsl:value-of select="@ident"/>
	  </xsl:otherwise>
	</xsl:choose>
      </a>
	</p>
      </xsl:for-each>
    </xsl:for-each>
  </xsl:template>

<xd:doc>
    <xd:short>Process elements  tei:ptr</xd:short>
    <xd:detail>&#160;</xd:detail>
  </xd:doc>
  <xsl:template match="tei:ptr" mode="weave">
    <xsl:choose>
      <xsl:when test="parent::tei:listRef">
        <xsl:if test="count(preceding-sibling::tei:ptr)=0">
          <tr>
            <td valign="top"></td>
            <td colspan="2">
              <xsl:if test="count(preceding-sibling::tei:ptr)&gt;0">;
	      </xsl:if>
	      <xsl:call-template name="makeInternalLink">
		<xsl:with-param name="target" select="substring-after(@target,'#')"/>
		<xsl:with-param name="ptr">true</xsl:with-param>
		<xsl:with-param name="dest">
		  <xsl:call-template name="generateEndLink">
		    <xsl:with-param name="where">
		      <xsl:value-of select="substring-after(@target,'#')"/>
		    </xsl:with-param>
		  </xsl:call-template>
		</xsl:with-param>
	      </xsl:call-template>
            </td>
          </tr>
        </xsl:if>
      </xsl:when>
      <xsl:otherwise>
        <xsl:apply-imports/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  

  <xsl:template match="rng:*">
    <xsl:copy>
      <xsl:copy-of select="@*"/>
      <xsl:apply-templates select="rng:*|tei:*|text()|comment()"/>
    </xsl:copy>
  </xsl:template>
  
  <xsl:template match="rng:zeroOrMore">
    <xsl:choose>
      <xsl:when test="count(rng:*)=1 and rng:zeroOrMore">
	<xsl:apply-templates select="rng:*|tei:*|text()|comment()"/>
      </xsl:when>
      <xsl:otherwise>
	<xsl:copy>
	  <xsl:copy-of select="@*"/>
	  <xsl:apply-templates select="rng:*|tei:*|text()|comment()"/>
	</xsl:copy>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>

</xsl:stylesheet>

