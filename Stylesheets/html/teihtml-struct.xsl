<?xml version="1.0" encoding="utf-8"?>
<!-- 
Text Encoding Initiative Consortium XSLT stylesheet family
$Date$, $Revision$, $Author$

XSL stylesheet to format TEI XML documents to HTML or XSL FO


##LICENSE
--> 
<xsl:stylesheet
    xmlns:tei="http://www.tei-c.org/ns/1.0"
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform" 
    version="1.0">
  
  <!-- ************************************************* -->

<xsl:param name="spacer">&gt;</xsl:param>

  
  <xsl:template
      match="tei:div|tei:div0|tei:div1|tei:div2|tei:div3|tei:div4|tei:div5|tei:div6"> 
    <xsl:variable name="depth">
      <xsl:apply-templates select="." mode="depth"/>
    </xsl:variable>
    <!--
	<xsl:message>div <xsl:value-of select="@id"/>: <xsl:value-of
	select="$depth"/></xsl:message>
    -->
    <!-- depending on depth and splitting level, 
	 we may do one of two things: -->
    
    <xsl:choose>
      <!-- 1. our section depth is below the splitting level -->
      <xsl:when test="$STDOUT='true' or $depth &gt; $splitLevel or
		      @rend='nosplit' or ancestor::tei:TEI/@rend='nosplit'">
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
      </xsl:when>
      
      <!-- 2. we are at or above splitting level, 
	   so start a new file
      -->

      <xsl:when test ="$depth &lt;= $splitLevel and parent::tei:front
		       and $splitFrontmatter">
	  <xsl:call-template name="outputChunk">
	    <xsl:with-param name="ident">
	      <xsl:apply-templates select="." mode="ident"/>
	    </xsl:with-param>
	    <xsl:with-param name="content">
	      <xsl:call-template name="writeDiv"/>
	    </xsl:with-param>
	  </xsl:call-template>
      </xsl:when>

      <xsl:when test ="$depth &lt;= $splitLevel and parent::tei:back
		       and $splitBackmatter">
	  <xsl:call-template name="outputChunk">
	    <xsl:with-param name="ident">
	      <xsl:apply-templates select="." mode="ident"/>
	    </xsl:with-param>
	    <xsl:with-param name="content">
	      <xsl:call-template name="writeDiv"/>
	    </xsl:with-param>
	  </xsl:call-template>
      </xsl:when>

      <xsl:when test="$depth &lt;= $splitLevel">
	  <xsl:call-template name="outputChunk">
	    <xsl:with-param name="ident">
	      <xsl:apply-templates select="." mode="ident"/>
	    </xsl:with-param>
	    <xsl:with-param name="content">
	      <xsl:call-template name="writeDiv"/>
	    </xsl:with-param>
	  </xsl:call-template>
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
  
  <!-- table of contents -->
  <xsl:template match="tei:divGen[@type='toc']">
    <h2><xsl:value-of select="$tocWords"/></h2>
    <xsl:call-template name="maintoc"/>
  </xsl:template>
  
  <!-- anything with a head can go in the TOC -->
  <xsl:template match="tei:*" mode="maketoc">
    <xsl:param name="forcedepth"/>
    <xsl:variable name="myName">
      <xsl:value-of select="local-name(.)"/>
    </xsl:variable>
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
	  <xsl:when test="$myName = 'div'">
	    <xsl:value-of select="count(ancestor::tei:div)"/>
	  </xsl:when>
	  <xsl:when test="starts-with($myName,'div')">
	    <xsl:choose>
	      <xsl:when test="ancestor-or-self::tei:div0">
		<xsl:value-of select="substring-after($myName,'div')"/>
	      </xsl:when>
	      <xsl:otherwise>
		<xsl:value-of select="substring-after($myName,'div') - 1"/>
	      </xsl:otherwise>
	    </xsl:choose>
	  </xsl:when>
	  <xsl:otherwise>99</xsl:otherwise>
	</xsl:choose>
      </xsl:variable>
      <xsl:variable name="pointer">
	<xsl:apply-templates mode="generateLink" select="."/>
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
      <xsl:when test="local-name(.) = 'div'">
	<xsl:value-of select="count(ancestor::tei:div)"/>
      </xsl:when>
      <xsl:otherwise>
	<xsl:choose>
	  <xsl:when test="ancestor-or-self::tei:div0">
	    <xsl:value-of select="substring-after(local-name(.),'div')"/>
	  </xsl:when>
	  <xsl:otherwise>
	    <xsl:value-of select="substring-after(local-name(.),'div') - 1"/>
	  </xsl:otherwise>
	</xsl:choose>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  
  <xsl:template match="tei:*" mode="depth">99</xsl:template>
  
  <!-- headings etc -->
  <xsl:template match="tei:head">
    <xsl:variable name="parent" select="local-name(..)"/>
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
	    <xsl:when test="@id|@xml:id">
	      <a name="{@id|@xml:id}"/>
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
	      <xsl:apply-templates select="." mode="generateLink"/>
	    </xsl:variable>
	    <li class="subtoc">
	      <xsl:call-template name="makeInternalLink">     
		<xsl:with-param name="dest">
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
	</ul>
      </div>
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
      
      <xsl:if test="not(ancestor-or-self::tei:TEI[@rend='nomenu'])">
	  <xsl:call-template name="nextLink"/>
	  <xsl:call-template name="previousLink"/>
      </xsl:if>
    </p>
  </xsl:template>
  
  
  <xsl:template name="upLink">
    <xsl:param name="up"/>
    <xsl:param name="title"/>
    <xsl:if test="$up and not($makeFrames='true')">
      <i><xsl:text> </xsl:text> <xsl:value-of select="$upWord"/>: </i>
      <a class="navlink"> 
	<xsl:choose>
	  <xsl:when test="$title">
	    <xsl:attribute name="href">
	      <xsl:value-of select="$up"/>
	    </xsl:attribute>
	    <xsl:value-of select="$title"/>
	  </xsl:when>
	  <xsl:otherwise>
	    <xsl:attribute name="href">
	      <xsl:apply-templates mode="generateLink" select="$up"/>
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
    <xsl:variable name="myName">
      <xsl:value-of select="local-name(.)"/>
    </xsl:variable>
    

    <xsl:choose>
      <xsl:when test="preceding-sibling::tei:TEI">
	<xsl:apply-templates mode="generatePreviousLink"
			     select="preceding-sibling::tei:TEI[1]"/>
      </xsl:when>
      <xsl:when test="preceding-sibling::tei:div">
	<xsl:apply-templates mode="generatePreviousLink"
			     select="preceding-sibling::tei:div[1]"/>
      </xsl:when>
      <xsl:when
	  test="parent::tei:body/preceding-sibling::tei:back/tei:div">
	<xsl:apply-templates mode="generatePreviousLink"
			     select="parent::tei:body/preceding-sibling::tei:back/tei:div[1]"/>
      </xsl:when>
      <xsl:when
	  test="parent::tei:front/preceding-sibling::tei:body/tei:div">
	<xsl:apply-templates mode="generatePreviousLink"
			     select="parent::tei:front/preceding-sibling::tei:body/tei:div[1]"/>
      </xsl:when>
      <xsl:when test="$myName='div0' and preceding-sibling::tei:div0">
	<xsl:apply-templates mode="generatePreviousLink" select="preceding-sibling::tei:div0[1]"/>
      </xsl:when>
      <xsl:when test="$myName='div1' and preceding-sibling::tei:div1">
	<xsl:apply-templates mode="generatePreviousLink" select="preceding-sibling::tei:div1[1]"/>
      </xsl:when>
      <xsl:when test="$myName='div2' and preceding-sibling::tei:div2">
	<xsl:apply-templates mode="generatePreviousLink" select="preceding-sibling::tei:div2[1]"/>
      </xsl:when>
      <xsl:when test="$myName='div3' and preceding-sibling::tei:div3">
	<xsl:apply-templates mode="generatePreviousLink" select="preceding-sibling::tei:div3[1]"/> 
      </xsl:when>
    </xsl:choose>
    
  </xsl:template>

  <xsl:template name="nextLink">
    <xsl:variable name="myName">
      <xsl:value-of select="local-name(.)"/>
    </xsl:variable>
    
    <xsl:choose>
      <xsl:when test="preceding-sibling::tei:TEI">
	<xsl:apply-templates mode="generateNextLink"
			     select="following-sibling::tei:TEI[1]"/>
      </xsl:when>
      <xsl:when test="following-sibling::tei:div">
	<xsl:apply-templates mode="generateNextLink"
			     select="following-sibling::tei:div[1]"/>
      </xsl:when>
      <xsl:when
	  test="parent::tei:body/following-sibling::tei:back/tei:div">
	<xsl:apply-templates mode="generateNextLink"
			     select="parent::tei:body/following-sibling::tei:back/tei:div[1]"/>
      </xsl:when>
      <xsl:when
	  test="parent::tei:front/following-sibling::tei:body/tei:div">
	<xsl:apply-templates mode="generateNextLink"
			     select="parent::tei:front/following-sibling::tei:body/tei:div[1]"/>
      </xsl:when>
      <xsl:when test="$myName='div0' and following-sibling::tei:div0">
	<xsl:apply-templates mode="generateNextLink" select="following-sibling::tei:div0[1]"/>
      </xsl:when>
      <xsl:when test="$myName='div1' and following-sibling::tei:div1">
	<xsl:apply-templates mode="generateNextLink" select="following-sibling::tei:div1[1]"/>
      </xsl:when>
      <xsl:when test="$myName='div2' and following-sibling::tei:div2">
	<xsl:apply-templates mode="generateNextLink" select="following-sibling::tei:div2[1]"/>
      </xsl:when>
      <xsl:when test="$myName='div3' and following-sibling::tei:div3">
	<xsl:apply-templates mode="generateNextLink" select="following-sibling::tei:div3[1]"/> 
      </xsl:when>
    </xsl:choose>
    
  </xsl:template>

  <xsl:template match="tei:*" mode="generateNextLink">
    <i><xsl:text> </xsl:text>
    <xsl:value-of select="$nextWord"/>: </i> 
    <a  class="navlink">
      <xsl:attribute name="href">
	<xsl:apply-templates select="." mode="generateLink"/>
      </xsl:attribute>
      <xsl:call-template name="header">
	<xsl:with-param name="minimal" select="$minimalCrossRef"/>
      </xsl:call-template>
    </a>  
  </xsl:template> 
  
  <xsl:template match="tei:*" mode="generatePreviousLink">
    <i><xsl:text> </xsl:text>
    <xsl:value-of select="$previousWord"/>: </i> 
    <a  class="navlink">
      <xsl:attribute name="href">
	<xsl:apply-templates select="." mode="generateLink"/>
      </xsl:attribute>
      <xsl:call-template name="header">
	<xsl:with-param name="minimal" select="$minimalCrossRef"/>
      </xsl:call-template>
    </a>  
  </xsl:template> 
  
  <xsl:template name="generateDivtitle">
    <xsl:apply-templates select="tei:head/text()"/>
  </xsl:template> 
  
  <xsl:template name="generateDivheading">
    <xsl:apply-templates select="." mode="xref"/>
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
	<xsl:apply-templates select="*[not(local-name(.)='div')]"/>
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
    <xsl:variable name="myName">
      <xsl:value-of select="local-name(.)"/>
    </xsl:variable>
    
    <xsl:variable name="BaseFile">
      <xsl:value-of select="$masterFile"/>
      <xsl:call-template name="addCorpusID"/>
    </xsl:variable>
    <xsl:choose>
      <xsl:when test="$myName = 'div'">
	<xsl:call-template name="upLink">
	  <xsl:with-param name="up" select="ancestor::tei:div[last()]"/>
	</xsl:call-template>
      </xsl:when>
      <xsl:otherwise>
	<xsl:choose>
	  <xsl:when test="$myName='div0'">
	    <xsl:call-template name="upLink">
	      <xsl:with-param name="up" select="concat($BaseFile,$standardSuffix)"/>
	      <xsl:with-param name="title" select="$homeLabel"/>
	    </xsl:call-template>
	  </xsl:when>
	  <xsl:when test="$myName='div1'">
	    <xsl:call-template name="upLink">
	      <xsl:with-param name="up" select="concat($BaseFile,$standardSuffix)"/>
	      <xsl:with-param name="title" select="$homeLabel"/>
	    </xsl:call-template>
	  </xsl:when>
	  <xsl:when test="$myName='div2'">
	    <xsl:call-template name="upLink">
	      <xsl:with-param name="up" select="ancestor::tei:div1"/>
	    </xsl:call-template>
	  </xsl:when>
	  <xsl:when test="$myName='div3'">
	    <xsl:call-template name="upLink">
	      <xsl:with-param name="up" select="ancestor::tei:div2"/>
	    </xsl:call-template>
	  </xsl:when>
	  <xsl:when test="$myName='div4'">
	    <xsl:call-template name="upLink">
	      <xsl:with-param name="up" select="ancestor::tei:div3"/>
	    </xsl:call-template>
	  </xsl:when>
	  <xsl:when test="$myName='div5'">
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
      <xsl:value-of select="ancestor-or-self::tei:*[@lang|@xml:lang][1]/@lang|@xml:lang"/>
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
	  <xsl:when test="@id|@xml:id"><xsl:value-of select="@id|@xml:id"/></xsl:when> 
	  <xsl:otherwise><xsl:number/></xsl:otherwise>
	</xsl:choose>
      </xsl:for-each>
    </xsl:if>
  </xsl:template>
  


<xsl:template name="doFrames">

 <xsl:variable name="BaseFile">
   <xsl:value-of select="$masterFile"/>
   <xsl:call-template name="addCorpusID"/>
 </xsl:variable>

 <xsl:choose>
  <xsl:when test="$ID='frametoc___'">
     <xsl:call-template name="writeFrameToc"/>
  </xsl:when>
  <xsl:when test="$STDOUT='true'">
        <xsl:call-template name="writeFrameMain">
 	 <xsl:with-param name="base" select="$BaseFile"/>
	</xsl:call-template>        
  </xsl:when>
  <xsl:otherwise>
        
   <xsl:call-template name="outputChunk">
   <xsl:with-param name="ident">
     <xsl:value-of select="concat($BaseFile,'-menubar')"/>
   </xsl:with-param>
   <xsl:with-param name="content">
    <xsl:call-template name="writeFrameToc"/>
   </xsl:with-param>
   </xsl:call-template>

   <xsl:call-template name="outputChunk">
   <xsl:with-param name="ident">
     <xsl:value-of select="concat($BaseFile,'-frames')"/>
   </xsl:with-param>
   <xsl:with-param name="content">
         <xsl:call-template name="writeFrameMain">
 	 <xsl:with-param name="base" select="$BaseFile"/>
	</xsl:call-template>
   </xsl:with-param>
   </xsl:call-template>
  
   <xsl:apply-templates select="tei:TEI" mode="split"/>
  </xsl:otherwise>
 </xsl:choose>
</xsl:template>

<xsl:template name="writeFrameToc">
<html><xsl:call-template name="addLangAtt"/>
 <xsl:comment>THIS FILE IS GENERATED FROM AN XML MASTER. 
 DO NOT EDIT</xsl:comment>
 <head>
 <title><xsl:call-template name="generateTitle"/></title>
 <xsl:call-template name="includeCSS"/>
 <base target="framemain"/>
 </head>
 <body class="framemenu">
 <xsl:call-template name="logoFramePicture"/><br/><xsl:text>&#10;</xsl:text>
 <xsl:call-template name="linkListContents">
      <xsl:with-param name="style" select="'frametoc'"/>
 </xsl:call-template>
 <xsl:call-template name="stdfooterFrame">
          <xsl:with-param name="date">
           <xsl:call-template name="generateDate"/>
          </xsl:with-param>
          <xsl:with-param name="author">
            <xsl:call-template name="generateAuthorList"/>
          </xsl:with-param>
          <xsl:with-param name="style" select="'framestdlink'"/>
  </xsl:call-template>
 </body>
</html>
</xsl:template>

<xsl:template name="writeFrameMain">
 <xsl:param name="base"/>
 <xsl:variable name="firstfile">
   <xsl:if test="$STDOUT='true'">
     <xsl:value-of select="$masterFile"/>
     <xsl:value-of select="$urlChunkPrefix"/>
   </xsl:if>
   <!-- we need to locate the first interesting object in the file, ie
    the first grandchild of <text > -->
    <xsl:for-each select="descendant::tei:text/tei:*[1]/tei:*[1]">
      <xsl:choose>
        <xsl:when test="starts-with(local-name(),'div')">
            <xsl:apply-templates select="." mode="ident"/>
        </xsl:when>
        <xsl:otherwise>
          <xsl:choose>
           <xsl:when test="$STDOUT='true'">
	    <xsl:text>prelim___</xsl:text>
           </xsl:when>
           <xsl:otherwise>
             <xsl:value-of select="$base"/>
           </xsl:otherwise>
          </xsl:choose>
        </xsl:otherwise>
      </xsl:choose>
    </xsl:for-each>
  <xsl:value-of select="$standardSuffix"/>
 </xsl:variable>
 <xsl:variable name="frameAlternate">
  <xsl:choose>
   <xsl:when test="$STDOUT='true'">
     <xsl:value-of select="concat($base,$standardSuffix)"/>  
     <xsl:text>?makeFrames=false&amp;autoToc=true</xsl:text>
   </xsl:when>
   <xsl:when test="$frameAlternateURL">
     <xsl:value-of select="$frameAlternateURL"/>  
   </xsl:when>
   <xsl:otherwise>
     <xsl:value-of select="concat($base,$standardSuffix)"/>  
   </xsl:otherwise>
  </xsl:choose>
 </xsl:variable>

 <xsl:variable name="frametoc">
 <xsl:value-of select="$base"/>
 <xsl:choose>
   <xsl:when test="$STDOUT='true'">
     <xsl:text>.ID=frametoc___</xsl:text>
   </xsl:when>
   <xsl:otherwise>
     <xsl:text>-menubar.html</xsl:text>
   </xsl:otherwise>
 </xsl:choose>
</xsl:variable>

<html><xsl:call-template name="addLangAtt"/>
 <xsl:comment>THIS FILE IS GENERATED FROM AN XML MASTER. 
 DO NOT EDIT</xsl:comment>
 <head>
 <meta name="robots" content="noindex,follow"/>
 <title><xsl:call-template name="generateTitle"/></title>
 <xsl:call-template name="includeCSS"/>
 </head>
  <frameset cols="{$frameCols}"> 
    <frame src="{$frametoc}"  name="framemenu"/>
    <frame src="{$firstfile}" name="framemain"/> 
    <noframes> 
     <body bgcolor="#ffffff"> 
    <p><i>Sorry, this document requires a browser that can view frames.</i></p>
    <p><b>Look at <a href="{$frameAlternate}">
         <xsl:value-of select="$frameAlternate"/></a> instead.</b></p>
     </body> 
    </noframes>
</frameset>
</html>
</xsl:template>


<xsl:template name="linkList">
<xsl:param name="side"/>
<xsl:param name="simple"/>

   <table class="{$side}links">
   <tr>
     <xsl:if test="$side='right'">
       <td valign="top">         <xsl:choose>
         <xsl:when test="$simple='true'">
           <xsl:call-template name="simpleBody"/>
         </xsl:when>
         <xsl:otherwise>
           <xsl:apply-templates/>
         </xsl:otherwise>
       </xsl:choose>
       </td>
     </xsl:if>
     <td width="{$linksWidth}" class="framemenu" valign="top">
     <xsl:call-template name="linkListContents">
        <xsl:with-param name="style" select="'frametoc'"/>
     </xsl:call-template>
     </td>
     <xsl:if test="$side='left'">
       <td valign="top">
         <xsl:choose>
         <xsl:when test="$simple='true'">
           <xsl:call-template name="simpleBody"/>
         </xsl:when>
         <xsl:otherwise>
           <xsl:apply-templates/>
         </xsl:otherwise>
       </xsl:choose>
       </td>
     </xsl:if>
   </tr>
   </table>
</xsl:template>

<xsl:template name="linkListContents">
  <xsl:param name="style" select="'toc'"/>
  <xsl:variable name="BaseFile">
    <xsl:value-of select="$masterFile"/>
    <xsl:call-template name="addCorpusID"/>
  </xsl:variable>
  
  <xsl:variable name="thisname">
    <xsl:value-of select="local-name(.)"/>
  </xsl:variable>
  
  <xsl:choose>
    <xsl:when test="$thisname='TEI' or $thisname='TEI.2' or $thisname=''">
      <xsl:for-each select=".//tei:text">
	<xsl:for-each select="tei:front">
	  <xsl:if test="tei:div|tei:div0|tei:div1"><hr/></xsl:if>
	  <xsl:for-each select="tei:div|tei:div0|tei:div1">
	    <xsl:if test="tei:head">
	      <xsl:variable name="pointer">
		<xsl:apply-templates mode="generateLink" select="."/>
	      </xsl:variable>
	      <p class="{$style}"><a class="{$style}" href="{$pointer}">
	      <xsl:call-template name="header"/></a></p>
	      <xsl:text>&#10;</xsl:text>
	    </xsl:if>
	  </xsl:for-each>
	</xsl:for-each>
	<xsl:for-each select="tei:body">
	  <xsl:if test="tei:div|tei:div0|tei:div1"><hr/></xsl:if>
	  <xsl:for-each select="tei:div|tei:div0|tei:div1">
	    <xsl:if test="tei:head">
	      <xsl:variable name="pointer">
		<xsl:apply-templates mode="generateLink" select="."/>
	      </xsl:variable>
	      <p class="{$style}"><a class="{$style}" href="{$pointer}">
		<xsl:call-template name="header"/></a>
		</p><xsl:text>&#10;</xsl:text>
		
	    </xsl:if>
	  </xsl:for-each>
	</xsl:for-each>
	<xsl:for-each select="tei:back">
	  <xsl:for-each select="tei:div|tei:div0|tei:div1">
	    <xsl:if test="tei:div|tei:div0|tei:div1"><hr/></xsl:if>
	    <xsl:if test="tei:head">
	      <xsl:variable name="pointer">
		<xsl:apply-templates mode="generateLink" select="."/>
	      </xsl:variable>
	      <p class="{$style}"><a class="{$style}" href="{$pointer}">
	      <xsl:call-template name="header"/></a></p>
	      <xsl:text>&#10;</xsl:text>
	    </xsl:if>
	  </xsl:for-each>
	</xsl:for-each>
      </xsl:for-each>
    </xsl:when>
    <xsl:otherwise>
      <xsl:text>&#10;</xsl:text><hr/>
      <xsl:for-each select="ancestor::tei:div|ancestor::tei:div2|ancestor::tei:div3|ancestor::tei:div4|ancestor::tei:div5">
	<xsl:if test="tei:head">
	  <p class="{$style}"><a class="{$style}">
	    <xsl:attribute name="href">
	      <xsl:apply-templates mode="generateLink" select="."/>
	    </xsl:attribute>
	    <xsl:call-template name="header"/>
	    </a></p><xsl:text>&#10;</xsl:text><hr/>
	</xsl:if>
      </xsl:for-each>
      <!-- preceding divisions -->
      <xsl:for-each select="preceding-sibling::tei:*[local-name()=$thisname]">
	<xsl:if test="tei:head">
	  <p class="{$style}"><a class="{$style}">
	    <xsl:attribute name="href">
	      <xsl:apply-templates mode="generateLink" select="."/>
	    </xsl:attribute>
	    <xsl:call-template name="header"/>
	    </a></p><xsl:text>&#10;</xsl:text>
	</xsl:if>
      </xsl:for-each>
      
      <!-- current division -->
      <p class="{$style}"><a class="{$style}-this">
	<xsl:attribute name="href">
	  <xsl:apply-templates mode="generateLink" select="."/>
	</xsl:attribute>
	<xsl:call-template name="header"/>
	</a></p><xsl:text>&#10;</xsl:text>
	<!-- ... and any children it has -->
	<xsl:for-each select="tei:div|tei:div2|tei:div3|tei:div4|tei:div5">
	  <p class="{$style}-sub"><a class="{$style}-sub">
	    <xsl:attribute name="href">
	      <xsl:apply-templates mode="generateLink" select="."/>
	    </xsl:attribute>
	    <xsl:call-template name="header"/>
	    </a></p><xsl:text>&#10;</xsl:text>
	</xsl:for-each>
	
	<!-- following divisions -->
	<xsl:for-each select="following-sibling::tei:*[local-name()=$thisname]">
	  <xsl:if test="tei:head">
	    <p class="{$style}"><a class="{$style}">
	      <xsl:attribute name="href">
		<xsl:apply-templates mode="generateLink" select="."/>
	      </xsl:attribute>
	      <xsl:call-template name="header"/>
	      </a></p><xsl:text>&#10;</xsl:text>
	  </xsl:if>
	</xsl:for-each>
    </xsl:otherwise>
  </xsl:choose>
</xsl:template>

<xsl:template name="stdfooterFrame">
  <xsl:param name="date"/>
  <xsl:param name="author"/>

  <xsl:param name="style" select="'plain'"/>
<hr/>
  <xsl:variable name="BaseFile">
   <xsl:value-of select="$masterFile"/>
   <xsl:call-template name="addCorpusID"/>
  </xsl:variable>
<xsl:if test="$linkPanel='true'">
 <div class="footer">
 <a  class="{$style}" target="_top">
  <xsl:attribute name="href">
    <xsl:value-of select="concat($BaseFile,$standardSuffix)"/>
    <xsl:text>?makeFrames=false</xsl:text>
  </xsl:attribute>
<xsl:value-of select="$noframeWords"/></a>
<xsl:text> | </xsl:text> 
<a class="{$style}" target="_top">
  <xsl:attribute name="href">
    <xsl:value-of select="concat($BaseFile,$standardSuffix)"/>
    <xsl:text>?style=printable</xsl:text>
  </xsl:attribute>
  <xsl:call-template name="singleFileLabel"/>
 </a> 
 </div>
<hr/>
<div class="footer">
<xsl:if test="$searchURL">
 <a  class="{$style}" target="_top"
href="{$searchURL}"><xsl:call-template name="searchWords"/></a> 
</xsl:if>
<xsl:if test="$feedbackURL">
  <br/><xsl:text>&#10;</xsl:text>
  <br/><xsl:text>&#10;</xsl:text>
<a class="{$style}" target="_top" 
href="{$feedbackURL}"><xsl:call-template name="feedbackWords"/></a> 
</xsl:if>
 </div>

</xsl:if>
<xsl:call-template name="preAddress"/>
<address>
 <xsl:comment>
<xsl:text>
  Generated using an XSLT version </xsl:text>
<xsl:value-of select="system-property('xsl:version')"/> stylesheet
based on <xsl:value-of select="$teixslHome"/>teihtml.xsl
processed using: <xsl:value-of select="system-property('xsl:vendor')"/> 
<!-- <xsl:call-template name="whatsTheDate"/> -->
</xsl:comment>
</address>
</xsl:template>

<xsl:template name="walkTree">
  <xsl:param name="path"/>
  <xsl:param name="class"/>
  <xsl:param name="whole" select="''"/>
  <xsl:choose>
    <xsl:when test="contains($path,'/')">
      <xsl:variable name="current">
        <xsl:value-of select="substring-before($path,'/')"/>            
      </xsl:variable>
      <xsl:variable name="rest">
        <xsl:value-of select="substring-after($path,'/')"/>            
      </xsl:variable>
      <xsl:call-template name="aCrumb">
	<xsl:with-param name="crumbBody">
	  <xsl:choose>
	    <xsl:when test="$rest='index.xsp' and $ID=''">
	      <xsl:value-of select="$current"/>
	    </xsl:when>
	    <xsl:when test="$rest='index.xml' and $ID=''">
	      <xsl:value-of select="$current"/>
	    </xsl:when>
	    <xsl:otherwise>
	      <a class="{$class}" target="_top">
		<xsl:attribute name="href">
		  <xsl:value-of select="$whole"/>/<xsl:value-of select="$current"/>
		  <xsl:text>/</xsl:text>
		</xsl:attribute>
		<xsl:value-of select="$current"/>
	      </a>
	    </xsl:otherwise>
	  </xsl:choose>
	</xsl:with-param>
      </xsl:call-template>
    <xsl:call-template name="walkTree"> 
      <xsl:with-param name="class"><xsl:value-of select="$class"/></xsl:with-param>
      <xsl:with-param name="path" select="$rest"/>
       <xsl:with-param name="spacer" select="$spacer"/>
       <xsl:with-param name="whole">
         <xsl:value-of select="$whole"/>/<xsl:value-of select="$current"/>
       </xsl:with-param>
    </xsl:call-template>
    </xsl:when>
    <xsl:otherwise>
      <xsl:if test="not($path='index.xsp' or $path='index.xml')">
	<xsl:value-of select="$spacer"/>
	<a class="{$class}" target="_top">
	  <xsl:attribute name="href">
	    <xsl:value-of select="$path"/>
	  </xsl:attribute>
          <xsl:value-of select="$path"/>
	</a>
      </xsl:if>
    </xsl:otherwise>

  </xsl:choose>
</xsl:template>

<xsl:template name="crumbPath">
<a target="_top" class="frametoc" href="{$homeURL}">
  <xsl:value-of select="$homeLabel"/>
</a>
  <xsl:call-template name="walkTree">
    <xsl:with-param name="class">frametoc</xsl:with-param>
    <xsl:with-param name="path">
       <xsl:value-of select="substring-after($REQUEST,'/')"/> 
    </xsl:with-param>
 </xsl:call-template>
</xsl:template>



<xsl:template name="aCrumb">
  <xsl:param name="crumbBody"/>
  <xsl:value-of select="$spacer"/>
  <xsl:copy-of select="$crumbBody"/>
</xsl:template>
  
</xsl:stylesheet>
