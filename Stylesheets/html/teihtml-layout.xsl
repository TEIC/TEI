<!-- 
     Text Encoding Initiative Consortium XSLT stylesheet family
     $Date$, $Revision$, $Author$
     XSL stylesheet to format TEI XML documents to HTML or XSL FO
     ##LICENSE
--> 
<xsl:stylesheet
    xmlns:tei="http://www.tei-c.org/ns/1.0"
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform" 
    version="1.0"  >
  
  <xsl:template match="*" mode="paging">
    <xsl:choose>
      <xsl:when test="self::tei:divGen[@type='summary']">
	<xsl:call-template name="summaryToc"/>
      </xsl:when>
      <xsl:when test="self::tei:divGen">
	<xsl:apply-templates select="."/>
      </xsl:when>
      <xsl:when test="starts-with(local-name(),'div')">
	<xsl:if test="not(preceding-sibling::tei:*) or preceding-sibling::tei:titlePage">
	  <h2><xsl:apply-templates select="." mode="xref"/></h2>
	  <xsl:call-template name="doDivBody"/>
	  <xsl:call-template name="printDivnotes"/>
	  <xsl:if test="$bottomNavigationPanel='true'">
	    <xsl:call-template name="xrefpanel">
	      <xsl:with-param name="homepage" 
			      select="concat($masterFile,$standardSuffix)"/>
	      <xsl:with-param name="mode" select="local-name(.)"/>
	    </xsl:call-template>
	  </xsl:if>
	</xsl:if>
      </xsl:when>
      <xsl:otherwise>
	<xsl:apply-templates select="."/>
	<xsl:apply-templates select="following-sibling::tei:*[1]" mode="paging"/>
      </xsl:otherwise>    
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
  
  <xsl:template name="aCrumb">
    <xsl:param name="crumbBody"/>
    <xsl:value-of select="$spacer"/>
    <xsl:copy-of select="$crumbBody"/>
  </xsl:template>
  
  
  <xsl:template name="crumbPath">
    <a target="_top" class="breadcrumb" href="{$homeURL}">
      <xsl:value-of select="$homeLabel"/>
    </a>
    <xsl:call-template name="walkTree">
      <xsl:with-param name="path">
	<xsl:value-of select="substring-after($REQUEST,'/')"/> 
      </xsl:with-param>
      <xsl:with-param name="class">breadcrumb</xsl:with-param>
    </xsl:call-template>
  </xsl:template>
  
<!-- making the breadcrumb trail into a list
     <xsl:template name="crumbPath">
     <ul>
     <li><a target="_top" class="breadcrumb" href="{$homeURL}">
     <xsl:value-of select="$homeLabel"/>
     </a>
     </li>
     <xsl:call-template name="walkTree">
     <xsl:with-param name="path">
     <xsl:value-of select="substring-after($REQUEST,'/')"/> 
     </xsl:with-param>
     <xsl:with-param name="class">breadcrumb</xsl:with-param>
     </xsl:call-template>
     </ul>
     </xsl:template>
-->

  <xsl:template name="doDivBody">
    <xsl:param name="Type"/>
    <xsl:call-template name="startDivHook"/>
    <xsl:variable name="ident">
      <xsl:apply-templates select="." mode="ident"/>
    </xsl:variable>
    <xsl:choose>
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
  
  <xsl:template name="doFrames">
    <xsl:variable name="BaseFile">
      <xsl:value-of select="$masterFile"/>
      <xsl:call-template name="addCorpusID"/>
    </xsl:variable>
    <xsl:choose>
      <xsl:when test="$ID='toclist___'">
	<xsl:call-template name="writeFrameToc"/>
      </xsl:when>
      <xsl:when test="$STDOUT='true'">
	<xsl:call-template name="pageLayoutSimple">
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
	    <xsl:call-template name="pageLayoutSimple">
	      <xsl:with-param name="base" select="$BaseFile"/>
	    </xsl:call-template>
	  </xsl:with-param>
	</xsl:call-template>	
	<xsl:apply-templates select="tei:TEI" mode="split"/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  
  <xsl:template name="doPageTable">
    <xsl:param name="currentID"/>
    <xsl:variable name="BaseFile">
      <xsl:value-of select="$masterFile"/>
      <xsl:call-template name="addCorpusID"/>
    </xsl:variable>
    <xsl:call-template name="outputChunk">
      <xsl:with-param name="ident">
	<xsl:choose>
	  <xsl:when test="$STDOUT='true'"/>
	  <xsl:when test="not($currentID='')">
	    <xsl:value-of select="$currentID"/>
	  </xsl:when>
	  <xsl:otherwise>
	    <xsl:value-of select="$BaseFile"/>         
	  </xsl:otherwise>
	</xsl:choose>
      </xsl:with-param>
      <xsl:with-param name="content">
	<xsl:choose>
	  <xsl:when test="$pageLayout='CSS'">
	    <xsl:call-template name="pageLayoutCSS">       
	      <xsl:with-param name="currentID" select="$currentID"/>
	    </xsl:call-template>
	  </xsl:when>
	  <xsl:when test="$pageLayout='Table'">
	    <xsl:call-template name="pageLayoutTable">       
	      <xsl:with-param name="currentID" select="$currentID"/>
	    </xsl:call-template>
	  </xsl:when>
	</xsl:choose>
      </xsl:with-param>
    </xsl:call-template>
  </xsl:template>
  
  <xsl:template name="generateDivheading">
    <xsl:apply-templates select="." mode="xref"/>
  </xsl:template> 
  
  <xsl:template name="generateDivtitle">
    <xsl:apply-templates select="tei:head/text()"/>
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
  
  <xsl:template name="leftHandFrame">
    <xsl:param name="currentID"/>
    <xsl:choose>
      <xsl:when test="$currentID=''">
	<xsl:call-template name="linkListContents">
	  <xsl:with-param name="style" select="'toclist'"/>
	</xsl:call-template>
      </xsl:when>
      <xsl:otherwise>
	<xsl:choose>
	  <xsl:when test="count(key('IDS',$currentID))&gt;0">
	    <xsl:for-each select="key('IDS',$currentID)">  
	      <xsl:call-template name="linkListContents">
		<xsl:with-param name="style" select="'toclist'"/>
	      </xsl:call-template>
	    </xsl:for-each>
	  </xsl:when>
	  <xsl:otherwise>
	    <xsl:apply-templates select="descendant::tei:text" mode="xpath">
	      <xsl:with-param name="xpath" select="$currentID" />
	      <xsl:with-param name="action" select="'toclist'" />
	    </xsl:apply-templates>
	  </xsl:otherwise>
	</xsl:choose>
      </xsl:otherwise>
    </xsl:choose>
    
    <xsl:for-each select="tei:text/tei:body/tei:div[@type='sidebar']">
      <xsl:for-each select="tei:list">
	<xsl:for-each select=".//tei:xref">
	  <p class="sidebar">
	    <a href="{@url}" class="toclist"><xsl:apply-templates/></a>
	  </p>
	</xsl:for-each>
      </xsl:for-each>
      <hr/>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="linkListContents">
    <xsl:param name="style" select="'toc'"/>
    <xsl:variable name="BaseFile">
      <xsl:value-of select="$masterFile"/>
      <xsl:call-template name="addCorpusID"/>
    </xsl:variable>
    <xsl:variable name="thisOne">
      <xsl:value-of select="generate-id()"/>
    </xsl:variable>
    <xsl:for-each select="ancestor-or-self::tei:TEI/tei:text">
      <!-- front matter -->  
      <xsl:for-each select="tei:front">
	<xsl:call-template name="tocLine">
	  <xsl:with-param name="id" select="$thisOne"/>
	  <xsl:with-param name="style" select="$style"/>
	</xsl:call-template>
      </xsl:for-each>
      <!-- body matter -->
      <xsl:for-each select="tei:body">
	<xsl:call-template name="tocLine">
	  <xsl:with-param name="id" select="$thisOne"/>
	  <xsl:with-param name="style" select="$style"/>
	</xsl:call-template>
      </xsl:for-each>
      <!-- back matter -->
      <xsl:for-each select="tei:back">
	<xsl:call-template name="tocLine">
	  <xsl:with-param name="id" select="$thisOne"/>
	  <xsl:with-param name="style" select="$style"/>
	</xsl:call-template>
      </xsl:for-each>
    </xsl:for-each>
  </xsl:template>
  
  <xsl:template name="mainFrame">
    <xsl:param name="currentID"/>
    <xsl:choose>
      <xsl:when test="$currentID='current'">
	<xsl:apply-templates/>
      </xsl:when>
      <xsl:when test="$currentID='' and $splitLevel=-1">
	<xsl:apply-templates/>
      </xsl:when>
      <xsl:when test="$currentID=''">
	<!-- we need to locate the first interesting object in the file, ie
	     the first grandchild of <text > -->
	<xsl:for-each select=" descendant::tei:text/tei:*[1]/tei:*[1]">
	  <xsl:apply-templates select="." mode="paging"/>
	  
	  <xsl:if test="following-sibling::tei:div/tei:head and not(ancestor-or-self::tei:TEI[@rend='nomenu'])">
	    <xsl:call-template name="contentsHeading"/>
	    <ul class="toc">
	      <xsl:apply-templates 
		  select="following-sibling::tei:div" mode="maketoc">
		<xsl:with-param name="forcedepth" select="'0'"/>
	      </xsl:apply-templates>
	    </ul>
	  </xsl:if>
	</xsl:for-each>
      </xsl:when>
      <xsl:otherwise>
	<xsl:choose>
	  <xsl:when test="count(key('IDS',$currentID))&gt;0">
	    <xsl:for-each select="key('IDS',$currentID)">  
	      <h2><xsl:apply-templates select="." mode="xref"/></h2>
	      <xsl:call-template name="doDivBody"/>
	      <xsl:call-template name="printDivnotes"/>
	      <xsl:if test="$bottomNavigationPanel='true'">
		<xsl:call-template name="xrefpanel">
		  <xsl:with-param name="homepage" 
				  select="concat($masterFile,$standardSuffix)"/>
		  <xsl:with-param name="mode" select="local-name(.)"/>
		</xsl:call-template>
	      </xsl:if>
	    </xsl:for-each>
	  </xsl:when>
	  <xsl:otherwise>
	    <!-- the passed ID is a pseudo-XPath expression
		 which starts below TEI/tei:text.
		 The real XPath syntax is changed to avoid problems
	    -->
	    <xsl:apply-templates
		select="ancestor-or-self::tei:TEI/descendant::tei:text" 
		mode="xpath">
	       <xsl:with-param name="xpath" select="$currentID" />
	    </xsl:apply-templates>
	  </xsl:otherwise>
	</xsl:choose>
      </xsl:otherwise>
    </xsl:choose>
    <xsl:call-template name="stdfooter">
      <xsl:with-param name="date">
	<xsl:call-template name="generateDate"/>
      </xsl:with-param>
      <xsl:with-param name="author">
	<xsl:call-template name="generateAuthorList"/>
      </xsl:with-param>
    </xsl:call-template>
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
  
  <xsl:template name="startHook"/>
  
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
    <xsl:call-template name="preAddressHook"/>
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
  
  <xsl:template name="pageLayoutTable">
    <xsl:param name="currentID"/>
    <html><xsl:call-template name="addLangAtt"/>
    <xsl:comment>THIS FILE IS GENERATED FROM AN XML MASTER. 
    DO NOT EDIT (1)</xsl:comment>
    <xsl:text>
    </xsl:text>
    <head>
      <xsl:variable name="pagetitle">
	<xsl:choose>
	  <xsl:when test="$currentID=''">
	    <xsl:call-template name="generateTitle"/>
	  </xsl:when>
	  <xsl:otherwise>
	    <xsl:call-template name="generateTitle"/>:
	    <xsl:choose>
	      <xsl:when test="count(key('IDS',$currentID))&gt;0">
		<xsl:for-each select="key('IDS',$currentID)">  
		  <xsl:apply-templates select="." mode="xref"/>
		</xsl:for-each>
	      </xsl:when>
	      <xsl:otherwise>
		<xsl:apply-templates select="descendant::tei:text" mode="xpath">
		  <xsl:with-param name="xpath" select="$currentID" />
		  <xsl:with-param name="action" select="'header'" />
		</xsl:apply-templates>
	      </xsl:otherwise>
	    </xsl:choose>
	  </xsl:otherwise>
	</xsl:choose>
      </xsl:variable>
      <title>
	<xsl:value-of select="$htmlTitlePrefix"/>
	<xsl:value-of select="$pagetitle"/>
      </title>
      <link rel="icon" href="/favicon.ico" type="image/x-icon"/>
      <link rel="shortcut icon" href="/favicon.ico" type="image/x-icon"/>
      <xsl:call-template name="includeCSS"/>
      <xsl:call-template name="metaHook">
	<xsl:with-param name="title" select="$pagetitle"/>
      </xsl:call-template>
      <xsl:call-template name="javaScript"/>
    </head>
    <body class="pagetable">
      <xsl:call-template name="bodyHook"/>
      <xsl:call-template name="bodyJavaScriptHook"/>
      <xsl:call-template name="pageTableHeader">
	<xsl:with-param name="mode">table</xsl:with-param>
      </xsl:call-template>
      <table>
	<tr>
	  <td colspan="2">
	    <xsl:call-template name="crumbPath"/>
	  </td>
	</tr>
	<tr>
	  <td align="left" valign="top" rowspan="2" width="{$linksWidth}"
	      class="linktext">
	    <xsl:call-template name="searchbox"/>
	    <xsl:call-template name="leftHandFrame">
	      <xsl:with-param name="currentID" select="$ID"/>
	    </xsl:call-template>
	    <hr/>
	  </td>
	</tr>
	<tr>
	  <td  valign="top" class="maintext" colspan="2">
	    <xsl:call-template name="mainFrame">
	      <xsl:with-param name="currentID" select="$currentID"/>
	    </xsl:call-template>
	  </td>
	</tr>
      </table>
    </body>
    </html>
  </xsl:template>

  <xsl:template name="tocLine">
    <xsl:param name="style"/>
    <xsl:param name="id"/>
    <xsl:if test="tei:div|tei:div0|tei:div1"><hr/></xsl:if>
    <xsl:for-each select="tei:div|tei:div0|tei:div1">
      <xsl:if test="tei:head">
	<xsl:variable name="pointer">
	  <xsl:apply-templates mode="generateLink" select="."/>
	</xsl:variable>
	<xsl:variable name="class">
	  <xsl:value-of select="$style"/>
	  <xsl:if test="generate-id(.)=$id">
	    <xsl:text>-this</xsl:text>
	  </xsl:if>
	</xsl:variable>
	<p class="{$class}">
	  <xsl:choose>
	    <xsl:when test="generate-id(.)=$id">
	      <xsl:call-template name="header"/>
	      <xsl:text>&#10;</xsl:text>
	      <!-- ... and any children it has -->
	      <xsl:for-each select="tei:div|tei:div2|tei:div3|tei:div4|tei:div5">
		<p class="{$style}-sub"><a class="{$style}-sub">
		  <xsl:attribute name="href">
		    <xsl:apply-templates mode="generateLink" select="."/>
		  </xsl:attribute>
		  <xsl:call-template name="header"/>
		</a>
		</p>
		<xsl:text>&#10;</xsl:text>
	      </xsl:for-each>
	    </xsl:when>
	    <xsl:otherwise>
	      <a class="{$class}" href="{$pointer}">
	      <xsl:call-template name="header"/></a>
	      <xsl:text>&#10;</xsl:text>
	    </xsl:otherwise>
	  </xsl:choose>
	</p>
      </xsl:if>
    </xsl:for-each>
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
  
  <xsl:template name="writeDiv">
    <xsl:variable name="BaseFile">
      <xsl:value-of select="$masterFile"/>
      <xsl:call-template name="addCorpusID"/>
    </xsl:variable>
    <html> 
      <xsl:call-template name="addLangAtt"/>
      <xsl:comment>THIS IS A GENERATED FILE. DO NOT EDIT (2)</xsl:comment>
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
	<xsl:call-template name="bodyJavaScriptHook"/>
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
			      select="local-name(.)"/>
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
	      <xsl:with-param name="mode" select="local-name(.)"/>
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
  
  <xsl:template name="pageLayoutSimple">
    <html><xsl:call-template name="addLangAtt"/> 
    <xsl:call-template name="htmlFileTop"/>
    <body class="simple">
      <xsl:call-template name="bodyHook"/>
      <xsl:call-template name="bodyJavaScriptHook"/>
      <a name="TOP"/>
      <xsl:call-template name="stdheader">
	<xsl:with-param name="title">
	  <xsl:call-template name="generateTitle"/>
	</xsl:with-param>
      </xsl:call-template>
      
      <xsl:call-template name="mainbody"/>
      
      <xsl:call-template name="printNotes"/>
      <xsl:call-template name="htmlFileBottom"/>
    </body>
    </html>
  </xsl:template>

  <xsl:template name="writeFrameToc">
    <html><xsl:call-template name="addLangAtt"/>
    <xsl:comment>THIS FILE IS GENERATED FROM AN XML MASTER. 
    DO NOT EDIT (3)</xsl:comment>
    <head>
      <title><xsl:call-template name="generateTitle"/></title>
      <xsl:call-template name="includeCSS"/>
      <base target="framemain"/>
    </head>
    <body class="framemenu">
      <xsl:call-template name="logoFramePicture"/><br/><xsl:text>&#10;</xsl:text>
      <xsl:call-template name="linkListContents">
	<xsl:with-param name="style" select="'toclist'"/>
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
	<xsl:call-template name="previousLink"/>
	<xsl:call-template name="nextLink"/>
      </xsl:if>
    </p>
  </xsl:template>

  <xsl:template name="pageTableHeader">
    <xsl:param name="mode"/>
    <xsl:choose>
      <xsl:when test="$mode='table'">
	<table width="100%" border="0">
	  <tr>
	    <td height="98" class="bgimage" onClick="window.location='{$homeURL}'" cellpadding="0">
	      <h2 class="subtitle"><xsl:call-template name="generateSubTitle"/></h2>
	      <h1 class="maintitle"><xsl:call-template name="generateTitle"/></h1>
	    </td>
	    <td valign="top"></td>
	  </tr>
	</table>
      </xsl:when>
      <xsl:otherwise>
	<h2 class="subtitle"><xsl:call-template name="generateSubTitle"/></h2>
	<h1 class="maintitle"><xsl:call-template name="generateTitle"/></h1>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  

<xsl:template name="pageLayoutCSS">
  <xsl:param name="currentID"/>
  <html>
    <xsl:call-template name="addLangAtt"/>
    <xsl:comment>THIS FILE IS GENERATED FROM AN XML MASTER. 
    DO NOT EDIT (4)</xsl:comment>
    <xsl:text>&#10;</xsl:text>
    <head>
      <xsl:variable name="pagetitle">
	<xsl:choose>
	  <xsl:when test="$currentID=''">
	    <xsl:call-template name="generateTitle"/>
	  </xsl:when>
	  <xsl:otherwise>
	    <xsl:call-template name="generateTitle"/>:
	    <xsl:choose>
	      <xsl:when test="$currentID='current'">
		<xsl:apply-templates select="." mode="xref"/>
	      </xsl:when>
	      <xsl:when test="count(key('IDS',$currentID))&gt;0">
		<xsl:for-each select="key('IDS',$currentID)">  
		  <xsl:apply-templates select="." mode="xref"/>
		</xsl:for-each>
	      </xsl:when>
	      <xsl:otherwise>
		<xsl:apply-templates select="descendant::text" mode="xpath">
		  <xsl:with-param name="xpath" select="$currentID" />
		  <xsl:with-param name="action" select="'header'" />
		</xsl:apply-templates>
	      </xsl:otherwise>
	    </xsl:choose>
	  </xsl:otherwise>
	</xsl:choose>
      </xsl:variable>
      <title><xsl:value-of select="$htmlTitlePrefix"/>
      <xsl:text> </xsl:text><xsl:value-of select="$pagetitle"/></title>
      <link rel="icon" href="/favicon.ico" type="image/x-icon"/>
      <link rel="shortcut icon" href="/favicon.ico" type="image/x-icon"/>
      <xsl:call-template name="includeCSS"/>
      <xsl:call-template name="metaHook">
	<xsl:with-param name="title" select="$pagetitle"/>
      </xsl:call-template>
      <xsl:call-template name="javaScript"/>
    </head>
    <body>
      <xsl:call-template name="bodyHook"/>
      <xsl:call-template name="bodyJavaScriptHook"/>
      
      <!-- header -->
      <div id="hdr"><span class="tocontent"><a href="{$REQUEST}?style=text">Text only</a> |
      <a href="#rh-col" title="Go to main page content" class="skiplinks">Skip links</a></span>
      <xsl:call-template name="pageTableHeader"/>
      </div>
      
      <!-- breadcrumb trail -->
      <div id="hdr2">
	<a href="#rh-col" title="Go to main page content" class="skiplinks">Skip links</a>  <a class="hide">|</a>
	<xsl:call-template name="crumbPath"/>
	<a class="hide">|</a>
	
	<a class="bannerright" href="{$parentURL}" 
	   title="Go to home page">	  
	  <xsl:value-of select="$parentWords"/><xsl:text> home page</xsl:text>
	</a>
      </div>
      <!-- right column -->
      <div id="rh-col"><a name="rh-col"></a> 
      <xsl:call-template name="mainFrame">
	<xsl:with-param name="currentID" select="$currentID"/>
      </xsl:call-template>
      </div>
      <!-- left hand column -->
      <div id="lh-col"> 
	
	<xsl:call-template name="searchbox"/>
	<xsl:call-template name="printLink"/>
	<xsl:call-template name="leftHandFrame">
	  <xsl:with-param name="currentID" select="$ID"/>
	</xsl:call-template>
	
	<hr/>
      </div>
    </body>
  </html>
</xsl:template>

<xsl:template name="printLink">
  <xsl:variable name="href">
    <xsl:value-of select="concat($masterFile,$standardSuffix)"/>
    <xsl:text>?style=printable</xsl:text>
  </xsl:variable>  
  <p class="sidebar"><a class="toclist">
    <xsl:attribute name="onClick">
      <xsl:text>popUpPage('</xsl:text>
      <xsl:value-of select="$href"/>
      <xsl:text>','status=no,scrollbars=yes,toolbar=yes,resizable=yes,menubar=yes,width=600,height=445','Printer')</xsl:text>
    </xsl:attribute>
    <xsl:attribute name="href"><xsl:value-of  select="$href"/></xsl:attribute>
    <xsl:attribute name="target">Printer</xsl:attribute>
    <xsl:call-template name="singleFileLabel"/>
  </a></p>
</xsl:template>
  
</xsl:stylesheet>
