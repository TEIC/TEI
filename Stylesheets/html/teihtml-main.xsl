<!-- 
Text Encoding Initiative Consortium XSLT stylesheet family
$Date$, $Revision$, $Author$

XSL stylesheet to format TEI XML documents to HTML or XSL FO

 
##LICENSE
--> 
<xsl:stylesheet
  xmlns:tei="http://www.tei-c.org/ns/1.0"
  extension-element-prefixes="edate"
  exclude-result-prefixes="edate" 
  xmlns:edate="http://exslt.org/dates-and-times"
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform" 
  version="1.0">


<xsl:variable name="masterFile">
 <xsl:choose>

 <xsl:when test="not($inputName ='')">
  <xsl:choose>
   <xsl:when test="contains($inputName,'.xml')">
       <xsl:value-of select="substring-before($inputName,'.xml')"/>
   </xsl:when>
   <xsl:otherwise><xsl:value-of select="$inputName"/></xsl:otherwise>
  </xsl:choose>
 </xsl:when>

 <xsl:when test="not($REQUEST='')">
   <xsl:call-template name="get-basename">
     <xsl:with-param name="file">
       <xsl:value-of select="$REQUEST"/>
    </xsl:with-param>
   </xsl:call-template>
 </xsl:when>

 <xsl:when test="contains($processor,'SAXON')">
   <xsl:call-template name="get-basename">
     <xsl:with-param name="file">
    <xsl:value-of 
       xmlns:saxon="http://icl.com/saxon"  
       select="substring-after(saxon:system-id(),':')"/>
    </xsl:with-param>
   </xsl:call-template>
 </xsl:when>

 <xsl:otherwise>index</xsl:otherwise>

 </xsl:choose>
</xsl:variable>

<xsl:param name="standardSuffix">
  <xsl:choose>
    <xsl:when test="$rawIE='true'">.xml</xsl:when>
    <xsl:when test="$STDOUT='true'"/>
    <xsl:otherwise>.html</xsl:otherwise>
  </xsl:choose>
</xsl:param>

<xsl:template name="get-basename">
  <xsl:param name="file"/>
  <xsl:choose>
   <xsl:when test="contains($file,'/')">
    <xsl:call-template name="get-basename">
     <xsl:with-param name="file">
       <xsl:value-of select="substring-after($file,'/')"/>
     </xsl:with-param>
    </xsl:call-template>
  </xsl:when>
  <xsl:otherwise>
  <xsl:choose>
   <xsl:when test="$STDOUT='true'">
     <xsl:value-of select="$file"/>
   </xsl:when>
   <xsl:when test="contains($file,'.xml')">
       <xsl:value-of select="substring-before($file,'.xml')"/>
   </xsl:when>
   <xsl:otherwise><xsl:value-of select="$file"/></xsl:otherwise>
  </xsl:choose>
  </xsl:otherwise>
 </xsl:choose>
</xsl:template>

<!-- processors must support `key' -->
<xsl:key name="IDS" match="tei:*[@id|@xml:id]" use="@id|@xml:id"/>


<xsl:template match="/">
<xsl:if test="contains($processor,'Clark')">
<xsl:message terminate="yes">
XT is not supported by the TEI stylesheets, as it does not implement 
the "key" function
</xsl:message>
</xsl:if>


<xsl:choose> 
  
  <xsl:when test="$makePageTable = 'true' and tei:TEI">
    <xsl:for-each select="tei:TEI">
      <xsl:call-template name="doPageTable">
	<xsl:with-param name="currentID" select="$ID"/>
      </xsl:call-template>
    </xsl:for-each>
    <xsl:if test="not($STDOUT='true')">
            <xsl:for-each select="tei:TEI/tei:text/tei:front/tei:div">
	<xsl:variable name="currentID">
	  <xsl:apply-templates select="." mode="ident"/>
	</xsl:variable>
	<xsl:call-template name="doPageTable">
	  <xsl:with-param name="currentID" select="$currentID"/>
	</xsl:call-template>
      </xsl:for-each>
      <xsl:for-each select="tei:TEI/tei:text/tei:body/tei:div">
	<xsl:variable name="currentID">
	  <xsl:apply-templates select="." mode="ident"/>
	</xsl:variable>
	<xsl:call-template name="doPageTable">
	  <xsl:with-param name="currentID" select="$currentID"/>
	</xsl:call-template>
      </xsl:for-each>
      <xsl:for-each select="tei:TEI/tei:text/tei:back/tei:div">
	<xsl:variable name="currentID">
	  <xsl:apply-templates select="." mode="ident"/>
	</xsl:variable>
	<xsl:call-template name="doPageTable">
	  <xsl:with-param name="currentID" select="$currentID"/>
	</xsl:call-template>
      </xsl:for-each>
    </xsl:if>      
  </xsl:when>
  
  <xsl:when test="not($ID='')">    
    <xsl:choose>
      <xsl:when test="$ID='frametoc___'">
	<xsl:call-template name="writeFrameToc"/>
      </xsl:when>
      <xsl:when test="$ID='prelim___'">
	<xsl:apply-templates/>
      </xsl:when>
      <xsl:when test="count(key('IDS',$ID))&gt;0">
	<xsl:for-each select="key('IDS',$ID)">  
	  <xsl:call-template name="writeDiv"/>
	</xsl:for-each>
      </xsl:when>
      <xsl:otherwise>
	<!-- the passed iD is a pseudo-XPath expression
	     which starts below TEI/text.
	     The real XPath syntax is changed to avoid problems
	-->
	<xsl:apply-templates select="tei:TEI/tei:text" mode="xpath">
	  <xsl:with-param name="xpath" select="$ID" />
	</xsl:apply-templates>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:when>
  
  <xsl:when test="$masterFile= '' or $STDOUT='true'">
    <xsl:apply-templates/>
  </xsl:when>
  
    <xsl:when test="$splitLevel=-1"> 
   <xsl:call-template name="outputChunk">
      <xsl:with-param name="ident">
	<xsl:value-of select="$masterFile"/>
      </xsl:with-param>
      <xsl:with-param name="content">
	<xsl:apply-templates/>
      </xsl:with-param>
    </xsl:call-template>
  </xsl:when>
  
  <xsl:when test="tei:TEI or tei:teiCorpus">
    <xsl:apply-templates mode="split"/>
  </xsl:when>
  
  <xsl:when test="$makeFrames='true'">
    <xsl:call-template name="doFrames"/>
  </xsl:when>
  
  <xsl:otherwise>
    <xsl:call-template name="outputChunk">
      <xsl:with-param name="ident">
	<xsl:value-of select="$masterFile"/>
      </xsl:with-param>
      <xsl:with-param name="content">
	<xsl:apply-templates/>
      </xsl:with-param>
    </xsl:call-template>
  </xsl:otherwise>
  
</xsl:choose>

</xsl:template>

<!-- *****************************************-->
<xsl:template match="tei:TEI">
   <xsl:if test="$verbose='true'">
     <xsl:message>TEI HTML: run start hook template teiStartHook</xsl:message>
   </xsl:if>
   <xsl:call-template name="teiStartHook"/>
   <xsl:if test="$verbose='true'">
     <xsl:message>TEI HTML in single document mode </xsl:message>
   </xsl:if>
   <html><xsl:call-template name="addLangAtt"/> 
   <xsl:comment>THIS FILE IS GENERATED FROM AN XML MASTER. 
   DO NOT EDIT</xsl:comment>
   <head>
     <xsl:variable name="pagetitle">
       <xsl:call-template name="generateTitle"/>
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
   <xsl:call-template name="stdheader">
     <xsl:with-param name="title">
       <xsl:call-template name="generateTitle"/>
     </xsl:with-param>
   </xsl:call-template>
   <xsl:call-template name="startHook"/>
   <xsl:choose>
   <xsl:when test="$leftLinks">
    <xsl:call-template name="linkList">
         <xsl:with-param name="side" select="'left'"/>
         <xsl:with-param name="simple" select="'true'"/>
    </xsl:call-template>
   </xsl:when>
   <xsl:when test="$rightLinks">
    <xsl:call-template name="linkList">
         <xsl:with-param name="side" select="'right'"/>
         <xsl:with-param name="simple" select="'true'"/>
    </xsl:call-template>
   </xsl:when>
   <xsl:otherwise>
     <xsl:call-template name="simpleBody"/>
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
 </body>
 </html>
<xsl:if test="$verbose='true'">
 <xsl:message>TEI HTML: run end hook template teiEndHook</xsl:message>
</xsl:if>
  <xsl:call-template name="teiEndHook"/>
</xsl:template>

<!-- *****************************************-->

<xsl:template match="tei:TEI" mode="split">
<xsl:variable name="BaseFile">
  <xsl:value-of select="$masterFile"/>
  <xsl:call-template name="addCorpusID"/>
</xsl:variable>
<xsl:if test="$verbose='true'">
 <xsl:message>TEI HTML: run start hook template teiStartHook</xsl:message>
</xsl:if>
  <xsl:call-template name="teiStartHook"/>
<xsl:if test="$verbose='true'">
  <xsl:message>TEI HTML in splitting mode, base file is <xsl:value-of select="$BaseFile"/> </xsl:message>
</xsl:if>
  <xsl:call-template name="outputChunk">
  <xsl:with-param name="ident">
    <xsl:value-of select="$BaseFile"/>
  </xsl:with-param>
  <xsl:with-param name="content">
    <xsl:call-template name="writeMain"/>
  </xsl:with-param>
  </xsl:call-template>
<xsl:if test="$verbose='true'">
 <xsl:message>TEI HTML: run end hook template teiEndHook</xsl:message>
</xsl:if>
  <xsl:call-template name="teiEndHook"/>

</xsl:template>

<!-- *****************************************-->

<xsl:template name="htmlFileBottom">
    <xsl:call-template name="topNavigation"/>

    <xsl:call-template name="stdfooter">
     <xsl:with-param name="date">
       <xsl:call-template name="generateDate"/>
     </xsl:with-param>
     <xsl:with-param name="author">
       <xsl:call-template name="generateAuthorList"/>
     </xsl:with-param>
    </xsl:call-template>

</xsl:template>

<xsl:template name="htmlFileTop">
 <xsl:comment>THIS FILE IS GENERATED FROM AN XML MASTER. 
 DO NOT EDIT ME</xsl:comment>
     <xsl:variable name="pagetitle">
       <xsl:call-template name="generateTitle"/>
     </xsl:variable>
 <head>
 <title><xsl:value-of select="$pagetitle"/></title>
 <xsl:call-template name="headHook"/>
 <xsl:call-template name="includeCSS"/>
 <xsl:if test="$makeFrames='true'">
   <base target="framemain"/>
 </xsl:if>
 <xsl:call-template name="metaHook">
     <xsl:with-param name="title" select="$pagetitle"/>
 </xsl:call-template>
 <xsl:call-template name="javaScript"/>
 </head>
</xsl:template>

<xsl:template name="writeMain">
  <html><xsl:call-template name="addLangAtt"/> 
 <xsl:call-template name="htmlFileTop"/>
 <body>
    <xsl:call-template name="bodyHook"/>
    <xsl:call-template name="bodyJavaScript"/>
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
<!-- *****************************************-->

<xsl:template name="mainbody">
  <xsl:comment> process front matter </xsl:comment>
 <xsl:apply-templates select="tei:text/tei:front"/>

 <xsl:if test="$autoToc='true' and (descendant::tei:div or descendant::tei:div0 or descendant::tei:div1) and not(descendant::tei:divGen[@type='toc'])">
   <h2><xsl:value-of select="$tocWords"/></h2>
   <xsl:call-template name="maintoc"/>
 </xsl:if>
 <xsl:choose>
 <xsl:when test="tei:text/tei:group">
  <xsl:apply-templates select="tei:text/tei:group"/>
 </xsl:when>
 <xsl:otherwise>

 <xsl:comment>process body matter </xsl:comment>

 <xsl:call-template name="startHook"/>
 <xsl:call-template name="doBody"/>

 </xsl:otherwise>
 </xsl:choose>

  <xsl:comment>back matter </xsl:comment>
 <xsl:apply-templates select="tei:text/tei:back"/>

</xsl:template>


<xsl:template name="simpleBody">
<!-- front matter -->
  <xsl:apply-templates select="tei:text/tei:front"/>

 <xsl:if test="$autoToc='true' and (descendant::tei:div or descendant::tei:div0 or descendant::tei:div1) and not(descendant::tei:divGen[@type='toc'])">
   <h2><xsl:value-of select="$tocWords"/></h2>
   <xsl:call-template name="maintoc"/>
 </xsl:if>

 <!-- main text -->
 <xsl:choose>
 <xsl:when test="tei:text/tei:group">
  <xsl:apply-templates select="tei:text/tei:group"/>
 </xsl:when>
 <xsl:otherwise>
  <xsl:apply-templates select="tei:text/tei:body"/>
 </xsl:otherwise>
 </xsl:choose>

 <!-- back matter -->
 <xsl:apply-templates select="tei:text/tei:back"/>

 <xsl:call-template name="printNotes"/>

</xsl:template>

<xsl:template name="teiStartHook"/>

<xsl:template name="teiEndHook"/>


<xsl:template name="stdheader">
  <xsl:param name="title" select="'(no title)'"/>
<xsl:choose>
 <xsl:when test="$makeFrames='true'">
     <h2 class="maintitle"><xsl:call-template name="generateTitle"/></h2>
     <h1 class="maintitle"><xsl:value-of select="$title"/></h1>
      <xsl:if test="$showTitleAuthor='true'">
       <xsl:if test="$verbose='true'">
          <xsl:message>displaying author and date</xsl:message>
       </xsl:if>
       <xsl:call-template name="generateAuthorList"/>
       <xsl:text> </xsl:text>
       <xsl:call-template name="generateDate"/>
      </xsl:if>
 </xsl:when>
 <xsl:otherwise>
 <table class="header" width="100%">
 <tr><td rowspan="3"><xsl:call-template name="logoPicture"/></td>
    <td align="left">
      <h2 class="institution"><xsl:value-of select="$institution"/></h2>
    </td></tr>
    <xsl:if test="not($department = '')">
     <tr><td align="left">
      <h2 class="department"><xsl:value-of select="$department"/></h2>
     </td>
     </tr>
    </xsl:if>
    <tr><td align="left">
      <xsl:call-template name="generateSubTitle"/>
      <h1 class="maintitle"><xsl:value-of select="$title"/></h1>
    </td></tr>
    <xsl:if test="$showTitleAuthor='true'">
      <xsl:if test="$verbose='true'">
       <xsl:message>displaying author and date</xsl:message>
      </xsl:if>
      <tr><td>&#160;</td><td  align="left">
        <xsl:call-template name="generateAuthorList"/>
      <xsl:text> </xsl:text>
      <xsl:call-template name="generateDate"/>
      </td></tr>
    </xsl:if>
 </table>
</xsl:otherwise>
</xsl:choose>
 <hr/>
</xsl:template>


<xsl:template name="stdfooter">
  <xsl:param name="date"/>
  <xsl:param name="author"/>
  <xsl:param name="style" select="'plain'"/>
<hr/>
<xsl:if test="$linkPanel='true' and not($makeFrames='true')">
 <div class="footer">
<xsl:if test="not($parentURL='')">
  <a class="{$style}" target="_top" 
     href="{$parentURL}"><xsl:value-of select="$parentWords"/></a> |
</xsl:if>
<a  class="{$style}" target="_top" 
    href="{$homeURL}"><xsl:value-of select="$homeWords"/></a> 
<xsl:if test="$searchURL">
  | <a class="{$style}" target="_top" 
  href="{$searchURL}"><xsl:call-template name="searchWords"/></a> 
</xsl:if>
<xsl:if test="$searchURL">
  | <a  class="{$style}" target="_top"
  href="{$feedbackURL}"><xsl:call-template name="feedbackWords"/></a> 
</xsl:if>
 </div>
 <hr/>
</xsl:if>
<xsl:call-template name="preAddress"/>
<address>
  <xsl:value-of select="$date"/>
  <xsl:if test="not($author='')">
    <xsl:value-of select="$author"/>
    </xsl:if>.
    <br/>
    <xsl:call-template name="copyrightStatement"/>
    <xsl:comment><xsl:text>
      Generated </xsl:text>
      <xsl:if test="not($masterFile='index')">
	<xsl:text>from </xsl:text>
	<xsl:value-of select="$masterFile"/>
      </xsl:if>
      <xsl:text> using an XSLT version </xsl:text>
      <xsl:value-of select="system-property('xsl:version')"/> stylesheet
      based on <xsl:value-of select="$teixslHome"/>teihtml.xsl
      processed using <xsl:value-of select="system-property('xsl:vendor')"/>
      on <xsl:call-template name="whatsTheDate"/>
    </xsl:comment>
</address>
</xsl:template>




<xsl:template name="topNavigation">
  <xsl:if test="ancestor::teiCorpus">
    <p align="{$alignNavigationPanel}">
      <xsl:call-template name="nextLink">
	<xsl:with-param name="next" select="following-sibling::tei:TEI[1]"/>
      </xsl:call-template>
      
      <xsl:call-template name="previousLink">
	<xsl:with-param name="previous" select="preceding-sibling::tei:TEI[1]"/>
      </xsl:call-template>
      
      <xsl:call-template name="upLink">
	<xsl:with-param name="h" select="concat($masterFile,$standardSuffix)"/>
	<xsl:with-param name="u">
	  <xsl:call-template name="contentsWord"/>
	</xsl:with-param>
      </xsl:call-template>
    </p>
  </xsl:if>
</xsl:template>


<xsl:template name="doBody">
  <xsl:param name="Head"/>
  <xsl:variable name="ident">
    <xsl:apply-templates select="." mode="ident"/>
  </xsl:variable>
  <xsl:choose>
    <xsl:when test="$leftLinks">
      <xsl:call-template name="linkList">
	<xsl:with-param name="side" select="'left'"/>
      </xsl:call-template>
    </xsl:when>
    <xsl:when test="$rightLinks">
      <xsl:call-template name="linkList">
	<xsl:with-param name="side" select="'right'"/>
      </xsl:call-template>
    </xsl:when>
    <xsl:when test="parent::tei:div/@rend='multicol'">
      <td valign="top">
	<xsl:if test="not($Head = '')">
	  <xsl:element name="h{$Head + $divOffset}">
	    <a name="{$ident}"></a><xsl:call-template name="header"/>
	  </xsl:element>
	</xsl:if>
	<xsl:apply-templates  select="tei:text/tei:body"/>
      </td>
    </xsl:when>
    <xsl:when test="@rend='multicol'">
      <table>
	<tr>
	  <xsl:apply-templates select="tei:text/tei:body"/>
	</tr>
      </table>
    </xsl:when>
    <xsl:otherwise>
      <xsl:if test="not($Head = '')">
	<xsl:element name="h{$Head + $divOffset}">
	  <a name="{$ident}"></a><xsl:call-template name="header"/>
	</xsl:element>
      </xsl:if>
      <xsl:apply-templates select="tei:text/tei:body"/>
    </xsl:otherwise>
  </xsl:choose>
</xsl:template>

<xsl:template match="processing-instruction()[name()='xmltex']" >
  <xsl:value-of select="."/>
</xsl:template>

<!-- This nice bit of code is from Jeni Tennison -->
<xsl:template match="tei:*" mode="xpath">
  <xsl:param name="xpath" />
  <xsl:param name="action" />
  <xsl:choose>
    <!-- if there is a path -->
    <xsl:when test="$xpath">
      <!-- step is the part before the '_' (if there is one) -->
      <xsl:variable name="step">
        <xsl:choose>
          <xsl:when test="contains($xpath, '_')">
            <xsl:value-of select="substring-before($xpath, '_')" />
          </xsl:when>
          <xsl:otherwise>
            <xsl:value-of select="$xpath" />
          </xsl:otherwise>
        </xsl:choose>
      </xsl:variable>
      <!-- the child's name is the part before the '.' -->
      <xsl:variable name="childName"
                    select="substring-before($step, '.')" />
      <!-- and its index is the part after '.' -->
      <xsl:variable name="childIndex"
        select="substring-after($step, '.')"/>
      <!-- so apply templates to that child, passing in the $xpath
           left after the first step -->
      <xsl:apply-templates select="*[name() = $childName]
                                    [number($childIndex)]" mode="xpath">
         <xsl:with-param name="xpath"
                         select="substring-after($xpath, '_')" />
         <xsl:with-param name="action"
                         select="$action" />
      </xsl:apply-templates>
    </xsl:when>
    <!-- if there's no path left, then this is the element we want -->
    <xsl:otherwise>
      <xsl:choose>
        <xsl:when test="$action='header'">
              <xsl:apply-templates select="." mode="xref"/>
        </xsl:when>
        <xsl:when test="$action='toclist'">
         <xsl:call-template name="linkListContents">
           <xsl:with-param name="style" select="'frametoc'"/>
         </xsl:call-template>
        </xsl:when>
        <xsl:when test="local-name()='div' and $makeFrames='true'">
          <xsl:call-template name="writeDiv"/>
        </xsl:when>
        <xsl:when test="local-name()='div' and $makePageTable='true'">
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
        </xsl:when>
        <xsl:when test="self::tei:divGen[@type='summary']">
           <xsl:call-template name="summaryToc"/>
        </xsl:when>
	<xsl:otherwise>
	  <html><xsl:call-template name="addLangAtt"/> 
	  <xsl:call-template name="htmlFileTop"/>
	  <body>
	    <xsl:call-template name="bodyHook"/>
	    <xsl:call-template name="bodyJavaScript"/>
	    <a name="TOP"/>
	    <xsl:call-template name="stdheader">
	      <xsl:with-param name="title">
		<xsl:call-template name="generateTitle"/>
	      </xsl:with-param>
	    </xsl:call-template>
	    
	    <h2><xsl:apply-templates select="." mode="xref"/></h2>
	    <xsl:apply-templates/>
	    
	    <xsl:call-template name="printNotes"/>
	    <xsl:call-template name="htmlFileBottom"/>
	  </body>
	  </html>	  
	</xsl:otherwise>
      </xsl:choose>
    </xsl:otherwise>
  </xsl:choose>
</xsl:template>

<xsl:template name="summaryToc">
         <div  class="teidiv">
  <p>Select headings on the left-hand side to see  
 more explanation of the links on the right.</p>
 <table  cellspacing="7">
  <thead><tr><th nowrap="nowrap"/><th/></tr></thead>
  <xsl:for-each select="//tei:body/tei:div">
<xsl:text>&#10;</xsl:text> 
   <tr class="summaryline">
    <td class="summarycell" valign="top" align="right">
    <b><a class="nolink" target="_top">
    <xsl:attribute name="href">
            <xsl:apply-templates mode="generateLink" select="."/>
    </xsl:attribute>
    <xsl:value-of select="tei:head"/></a></b>
    </td>
    <td  class="link" valign="top" >
       <xsl:for-each select=".//xref|.//xptr">
       <xsl:if test="position() &gt; 1">
           <xsl:text>&#160;</xsl:text>
           <img alt="*" src="/images/dbluball.gif"/>
           <xsl:text> </xsl:text>
	</xsl:if>
        <span class="nowrap"><xsl:apply-templates select="."/></span>
       </xsl:for-each>
    </td>
   </tr>
  </xsl:for-each>
 </table>
        </div>
 </xsl:template>

<xsl:template name="generateSubTitle"/>

<xsl:template name="metaHook">
  <xsl:param name="title"/>
 <meta name="author">
   <xsl:attribute name="content">
     <xsl:call-template name="generateAuthor"/>
   </xsl:attribute>
 </meta>
 <meta name="generator" content="Text Encoding Initiative Consortium XSLT stylesheets"/>
 <meta name="DC.Title" content="{$title}"/>
 <meta name="DC.Type" content="Text"/>
 <meta name="DC.Format" content="text/html"/>
</xsl:template>

<xsl:template name="javaScript">
  <xsl:if test="$rawIE='true'">
  <xsl:text>&#10;</xsl:text>
 <script language="javascript">
   <xsl:comment>
<![CDATA[
function makeitsoyoubastard(hash){
        alert("Fragment "+hash);
	var as = document.all.tags("A");
	for (var i=0; i < as.length; i++){
		if (as[i].name == hash) as[i].scrollIntoView(true);
	}
}]]>
function gotoSection(frag,section){
   var s = new ActiveXObject("MSXML2.FreeThreadedDOMDocument");
   var x = document.XMLDocument;
   if (x == null){
    x = navigator.XMLDocument;
    s = navigator.XSLDocument;
   }else{
     s.async = false;
     s.load(document.XSLDocument.url);
     x.load(document.XMLDocument.url);
   }
    var tem = new ActiveXObject("MSXML2.XSLTemplate"); 
    tem.stylesheet = s; 
    var proc = tem.createProcessor();
    proc.addParameter("ID", section);
    proc.input = x;
    proc.transform();
    var str = proc.output;
    var newDoc = document.open("text/html", "replace");
    newDoc.write(str);
    newDoc.close();
    navigator.XMLDocument = x;
    navigator.XSLDocument = s;
    if (frag == '') {}  else { 
    makeitsoyoubastard(frag); 
  }
}
     <xsl:text>
       //
</xsl:text>
   </xsl:comment>
 </script>
 </xsl:if>
</xsl:template>

<xsl:template name="includeCSS">
  <xsl:if test="not($cssFile='')">
   <link rel="stylesheet" type="text/css" href="{$cssFile}"/>
  </xsl:if>
  <xsl:if test="not($cssSecondaryFile='')">
   <link rel="stylesheet" type="text/css" href="{$cssSecondaryFile}"/>
  </xsl:if>
</xsl:template>

</xsl:stylesheet>
