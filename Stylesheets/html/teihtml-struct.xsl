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
	    <xsl:choose>
	      <xsl:when test="$pageLayout='CSS'">
		<xsl:call-template name="pageLayoutCSS">       
		  <xsl:with-param name="currentID" select="'current'"/>
		</xsl:call-template>
	      </xsl:when>
	      <xsl:when test="$pageLayout='Table'">
		<xsl:call-template name="pageLayoutTable">       
		  <xsl:with-param name="currentID" select="'current'"/>
		</xsl:call-template>
	      </xsl:when>
	      <xsl:otherwise>
		<xsl:call-template name="writeDiv"/>
	      </xsl:otherwise>
	    </xsl:choose>
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
	    <xsl:choose>
	      <xsl:when test="$pageLayout='CSS'">
		<xsl:call-template name="pageLayoutCSS">       
		  <xsl:with-param name="currentID" select="'current'"/>
		</xsl:call-template>
	      </xsl:when>
	      <xsl:when test="$pageLayout='Table'">
		<xsl:call-template name="pageLayoutTable">       
		  <xsl:with-param name="currentID" select="'current'"/>
		</xsl:call-template>
	      </xsl:when>
	      <xsl:otherwise>
		<xsl:call-template name="writeDiv"/>
	      </xsl:otherwise>
	    </xsl:choose>
	  </xsl:with-param>
	</xsl:call-template>
      </xsl:when>

      <xsl:when test="$depth &lt;= $splitLevel">
	<xsl:call-template name="outputChunk">
	  <xsl:with-param name="ident">
	    <xsl:apply-templates select="." mode="ident"/>
	  </xsl:with-param>
	  <xsl:with-param name="content">
	    <xsl:choose>
	      <xsl:when test="$pageLayout='CSS'">
		<xsl:call-template name="pageLayoutCSS">       
		  <xsl:with-param name="currentID" select="'current'"/>
		</xsl:call-template>
	      </xsl:when>
	      <xsl:when test="$pageLayout='Table'">
		<xsl:call-template name="pageLayoutTable">       
		  <xsl:with-param name="currentID" select="'current'"/>
		</xsl:call-template>
	      </xsl:when>
	      <xsl:otherwise>
		<xsl:call-template name="writeDiv"/>
	      </xsl:otherwise>
	    </xsl:choose>
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
  
<xsl:template match="tei:gi" mode="plain">
    <xsl:text>&lt;</xsl:text>
    <xsl:apply-templates/>
    <xsl:text>&gt;</xsl:text>
  </xsl:template>
  
<xsl:template match="tei:*" mode="plain">
    <xsl:apply-templates/>
  </xsl:template>

<xsl:template match="tei:title">
  <xsl:if test="preceding-sibling::tei:title"><br/></xsl:if>
  <xsl:apply-templates/>
</xsl:template>


</xsl:stylesheet>
