<?xml version="1.0" encoding="utf-8"?>
<!-- 
Text Encoding Initiative Consortium XSLT stylesheet family
$Date$, $Revision$, $Author$

XSL stylesheet to format TEI XML documents to HTML or XSL FO

 
#include LICENSE
--> 
<xsl:stylesheet
    xmlns:tei="http://www.tei-c.org/ns/1.0"
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    extension-element-prefixes="edate"
    exclude-result-prefixes="edate" 
    xmlns:edate="http://exslt.org/dates-and-times"
    version="1.0">

<xsl:param name="teiP4Compat">false</xsl:param>
<xsl:param name="processor">
   <xsl:value-of select="system-property('xsl:vendor')"/>
</xsl:param>

<!-- common parameters -->
<xsl:param name="baseURL">http://www.tei-c.org</xsl:param>
<xsl:param name="homeLabel">Home</xsl:param>
<xsl:param name="homeURL">http://www.tei-c.org/</xsl:param>
<xsl:param name="homeWords">TEI</xsl:param>
<xsl:param name="department"/>
<xsl:param name="parentURL">http://www.tei-c.org/</xsl:param>
<xsl:param name="parentWords">My Parent Institution</xsl:param>
<xsl:param name="searchURL">http://www.google.com</xsl:param>
<xsl:param name="institution">My Institution</xsl:param>
<xsl:template name="feedbackWords">Feedback</xsl:template>
<xsl:param name="class_toc">toc</xsl:param>

<!-- numbering -->
<!-- fill in these with a valid number format (eg A.1) if needed-->
<xsl:param name="useHeaderFrontMatter"/>
<xsl:param name="numberSpacer"><xsl:text> </xsl:text></xsl:param><!--&#160;-->
<xsl:param name="headingNumberSuffix">.<xsl:value-of 
select="$numberSpacer"/></xsl:param>
<xsl:param name="numberFigures">true</xsl:param>
<xsl:param name="numberFrontFigures">false</xsl:param>
<xsl:param name="numberBackFigures">false</xsl:param>
<xsl:param name="numberTables">true</xsl:param>
<xsl:param name="numberFrontTables">true</xsl:param>
<xsl:param name="numberBackTables">true</xsl:param>
<xsl:param name="numberHeadings">true</xsl:param>
<xsl:param name="numberHeadingsDepth">9</xsl:param>
<xsl:param name="prenumberedHeadings"></xsl:param>
<xsl:param name="numberBackHeadings">A.1</xsl:param>
<xsl:param name="numberFrontHeadings"></xsl:param>
<xsl:param name="minimalCrossRef"/>
<xsl:param name="autoHead"></xsl:param>

<xsl:template name="numberBackDiv">
 <xsl:if test="not($numberBackHeadings='')">
        <xsl:number format="A.1.1.1.1.1" 
         level="multiple" count="tei:div|tei:div0|tei:div1|tei:div2|tei:div3|tei:div4|tei:div5|tei:div6"/>
  </xsl:if>
</xsl:template>

<xsl:template name="numberFrontDiv">
 <xsl:if test="not($numberFrontHeadings='')">
         <xsl:number level="multiple" 
                     count="tei:div|tei:div0|tei:div1|tei:div2|tei:div3|tei:div4|tei:div5|tei:div6"/>
 </xsl:if>
</xsl:template>

<xsl:template name="numberBodyDiv">
 <xsl:if test="not($numberHeadings='')">
   <xsl:number level="multiple"
                     count="tei:div|tei:div0|tei:div1|tei:div2|tei:div3|tei:div4|tei:div5|tei:div6"/>
  </xsl:if>
</xsl:template>



<!-- Words for I18N -->
<xsl:param name="appendixWords">Appendix</xsl:param>
<xsl:param name="authorWord">Author:</xsl:param>
<xsl:param name="biblioWords">Bibliography</xsl:param>
<xsl:param name="dateWord">Date:</xsl:param>
<xsl:param name="figureWord">Figure</xsl:param>
<xsl:param name="figureWords">Figures</xsl:param>
<xsl:param name="nextWord">Next</xsl:param>
<xsl:param name="previousWord">Previous</xsl:param>
<xsl:param name="revisedWord">revised</xsl:param>
<xsl:param name="tableWord">Table</xsl:param>
<xsl:param name="tableWords">Tables</xsl:param>
<xsl:param name="tocWords">Contents</xsl:param>
<xsl:param name="upWord">Up</xsl:param>
<xsl:template name="contentsWord">Contents</xsl:template>
<xsl:template name="contentsHeading">Sections in this document:</xsl:template>
<xsl:template name="searchWords">Search</xsl:template>



<xsl:template name="generateTitle">
 <xsl:choose>
   <xsl:when test="$useHeaderFrontMatter='true' and ancestor-or-self::tei:TEI/tei:text/tei:front//tei:docTitle">
     <xsl:apply-templates 
       select="ancestor-or-self::tei:TEI/tei:text/tei:front//tei:docTitle"/>
     </xsl:when>
   <xsl:otherwise>
    <xsl:apply-templates 
      select="ancestor-or-self::tei:TEI/tei:teiHeader/tei:fileDesc/tei:titleStmt/tei:title"
      mode="htmlheader"/>
   </xsl:otherwise>
</xsl:choose>
</xsl:template>


<xsl:template name="generateDate">
  <xsl:param name="showRev">true</xsl:param>
<xsl:variable name="realdate">
 <xsl:choose>
   <xsl:when test="$useHeaderFrontMatter='true' and ancestor-or-self::tei:TEI/tei:text/tei:front//tei:docDate">
  <xsl:apply-templates 
    select="ancestor-or-self::tei:TEI/tei:text/tei:front//tei:docDate" 
    mode="date"/>
  </xsl:when>
  <xsl:when test="ancestor-or-self::tei:TEI/tei:teiHeader/tei:fileDesc/tei:editionStmt/descendant::tei:date">
  <xsl:apply-templates select="ancestor-or-self::tei:TEI/tei:teiHeader/tei:fileDesc/tei:editionStmt/descendant::tei:date[1]"/>
    </xsl:when>
  </xsl:choose>
</xsl:variable>

<xsl:variable name="revdate">
<xsl:apply-templates 
 select="ancestor-or-self::tei:TEI/tei:teiHeader/tei:revisionDesc/descendant::tei:date[1]"/>
</xsl:variable>
<xsl:value-of select="$dateWord"/><xsl:text> </xsl:text>
<xsl:if test="not($realdate = '')">
  <xsl:value-of select="$realdate"/>
</xsl:if>


<xsl:if test="$showRev='true' and not($revdate = '') and not ($revdate='&#36;Date$')">
  (<xsl:value-of select="$revisedWord"/><xsl:text> </xsl:text>
  <xsl:choose>
  <xsl:when test="starts-with($revdate,'$Date')"> <!-- it's RCS -->
    <xsl:value-of select="substring($revdate,16,2)"/>
    <xsl:text>/</xsl:text>
    <xsl:value-of select="substring($revdate,13,2)"/>
    <xsl:text>/</xsl:text>
    <xsl:value-of select="substring($revdate,8,4)"/> 
  </xsl:when>
  <xsl:when test="starts-with($revdate,'$LastChangedDate')"> <!-- it's SVN -->
    <xsl:value-of select="substring-before(substring-after($revdate,'('),')')"/>
  </xsl:when>
  <xsl:otherwise>
   <xsl:value-of select="$revdate"/>    
  </xsl:otherwise>
 </xsl:choose>
 <xsl:text>) </xsl:text></xsl:if>

</xsl:template>

<xsl:template name="generateAuthor">
 <xsl:choose>
   <xsl:when test="$useHeaderFrontMatter='true' and ancestor-or-self::tei:TEI/tei:text/tei:front//tei:docAuthor">
     <xsl:apply-templates select="ancestor-or-self::tei:TEI/tei:text/tei:front//tei:docAuthor[1]"  mode="author"/>
  </xsl:when>
  <xsl:when test="ancestor-or-self::tei:TEI/tei:teiHeader/tei:fileDesc/tei:titleStmt/tei:author">
  <xsl:apply-templates select="ancestor-or-self::tei:TEI/tei:teiHeader/tei:fileDesc/tei:titleStmt/tei:author"/>
    </xsl:when>
    <xsl:when test="ancestor-or-self::tei:TEI/tei:text/tei:front//tei:docAuthor">
      <xsl:apply-templates select="ancestor-or-self::tei:TEI/tei:text/tei:front//tei:docAuthor[1]" mode="author"/>
  </xsl:when>
  </xsl:choose>
</xsl:template>

<xsl:template name="generateAuthorList">
<xsl:variable name="realauthor">
  <xsl:call-template name="generateAuthor"/>
</xsl:variable>
<xsl:variable name="revauthor">
<xsl:apply-templates 
select="ancestor-or-self::tei:TEI/tei:teiHeader/tei:revisionDesc/tei:change[1]/tei:respStmt/tei:name/text()"/>
</xsl:variable>
<xsl:text> </xsl:text>
 <xsl:value-of select="$authorWord"/>
<xsl:text> </xsl:text>
<xsl:if test="not($realauthor = '')">
 <xsl:value-of select="$realauthor"/>
</xsl:if>
<xsl:if test="not($revauthor = '') and not(normalize-space($revauthor)='&#36;Author$')">
 (<xsl:value-of select="$revisedWord"/><xsl:text> </xsl:text>
 <xsl:choose>
  <xsl:when test="starts-with($revauthor,'$Author')"> <!-- it's RCS -->
    <xsl:value-of 
select="normalize-space(substring-before(substring-after($revauthor,'Author'),'$'))"/>
  </xsl:when>
  <xsl:when test="starts-with($revauthor,'$LastChangedBy')"> <!-- it's Subversion -->
    <xsl:value-of 
select="normalize-space(substring-before(substring-after($revauthor,'LastChangedBy:'),'$'))"/>
  </xsl:when>
  <xsl:otherwise>
   <xsl:value-of select="$revauthor"/>    
  </xsl:otherwise>
 </xsl:choose>
 <xsl:text>)</xsl:text>
</xsl:if>

</xsl:template>

<xsl:template match="tei:title" mode="htmlheader">
  <xsl:apply-templates/>
</xsl:template>

<xsl:template name="whatsTheDate">
 <xsl:choose>
 <xsl:when test="function-available('edate:date-time')">
  <xsl:value-of select="edate:date-time()"/>
 </xsl:when>
 <xsl:when test="contains($processor,'SAXON')">
   <xsl:value-of 
     xmlns:Date="/java.util.Date" 
     select="Date:toString(Date:new())"/>
 </xsl:when>
 <xsl:otherwise>
   (unknown)
  </xsl:otherwise>
 </xsl:choose>
</xsl:template>

<xsl:template match="tei:note" mode="xref">
   <xsl:number level="any"/>
</xsl:template>

<xsl:template match="tei:anchor|tei:p" mode="xref">
  <xsl:text>here</xsl:text>
</xsl:template>

<xsl:template match="tei:TEI" mode="xref"> 
  <xsl:apply-templates select="tei:teiHeader/tei:fileDesc/tei:titleStmt/tei:title"/>
</xsl:template>


<xsl:template match="tei:table" mode="xref">
<xsl:choose>
  <xsl:when test="$numberTables='true'">
    <xsl:value-of select="tei:tableWord"/>
    <xsl:text> </xsl:text>
    <xsl:number level="any"/>
    <xsl:if test="tei:head">
      <xsl:text>, </xsl:text>
      <xsl:apply-templates select="tei:head"/>
    </xsl:if>
</xsl:when>
<xsl:otherwise>
  <xsl:text>this table</xsl:text>
</xsl:otherwise>
</xsl:choose>
</xsl:template>

<xsl:template match="tei:figure" mode="xref">
<xsl:choose>
  <xsl:when test="$numberFigures='true'">
    <xsl:if test="not($figureWord='')">
      <xsl:value-of select="$figureWord"/>
      <xsl:text> </xsl:text>
    </xsl:if>
    <xsl:number level="any"/>
    <xsl:if test="tei:head">
      <xsl:text>, </xsl:text>
      <xsl:apply-templates select="tei:head"/>
    </xsl:if>
</xsl:when>
<xsl:otherwise>
  <xsl:text>this figure</xsl:text>
</xsl:otherwise>
</xsl:choose>
</xsl:template>

<xsl:template match="tei:biblStruct" mode="xref">
 <xsl:choose>
   <xsl:when test="descendant::tei:author">
     <xsl:apply-templates select="descendant::tei:author[position()=1]" mode="first"/>
   </xsl:when>
   <xsl:otherwise>
     <xsl:apply-templates select="descendant::tei:editor[position()=1]" mode="first"/>
   </xsl:otherwise>
 </xsl:choose>
 <xsl:choose>
   <xsl:when test="descendant::tei:title[@type='short']">
     <xsl:apply-templates select="descendant::tei:title[@type='short']"/>
   </xsl:when>
   <xsl:otherwise>
     <xsl:apply-templates select="descendant::tei:title[@type='main'][1]"/>
   </xsl:otherwise>
 </xsl:choose>
</xsl:template>


<xsl:template match="tei:bibl" mode="xref">
  <xsl:text>[</xsl:text><xsl:number/><xsl:text>]</xsl:text>
</xsl:template>


<xsl:template match="tei:div|tei:div0|tei:div1|tei:div2|tei:div3|tei:div4|tei:div5|tei:div6" mode="xref"> 
  <xsl:param name="minimal"/>
  <xsl:call-template name="header">
    <xsl:with-param name="minimal" select="$minimalCrossRef"/>
  </xsl:call-template>
</xsl:template>

<xsl:template name="header">
  <xsl:param name="minimal"/>
  <xsl:param name="toc"/>

  <xsl:variable name="depth">
    <xsl:apply-templates select="." mode="depth"/>
  </xsl:variable>
  <xsl:choose>
    <xsl:when test="local-name(.) = 'TEI'"/>
    <xsl:when test="$depth &gt; $numberHeadingsDepth">
    </xsl:when>
    <xsl:when test="ancestor::tei:back">
      <xsl:if test="not($numberBackHeadings='')">
	<xsl:value-of select="$appendixWords"/><xsl:text> </xsl:text>
	<xsl:call-template name="numberBackDiv">
	  <xsl:with-param name="minimal" select="$minimal"/>
	</xsl:call-template>
	<xsl:if test="$minimal=''">
	  <xsl:value-of select="$numberSpacer"/>
	</xsl:if>
      </xsl:if>
    </xsl:when>
    <xsl:when test="ancestor::tei:front">
      <xsl:if test="not($numberFrontHeadings='')">
	<xsl:call-template name="numberFrontDiv">
	  <xsl:with-param name="minimal" select="$minimal"/>
	</xsl:call-template>
	<xsl:if test="$minimal=''">
	  <xsl:value-of select="$numberSpacer"/>
	</xsl:if>
      </xsl:if>
    </xsl:when>
    <xsl:when test="$numberHeadings ='true'">
      <xsl:choose>
	<xsl:when test="$prenumberedHeadings">
	  <xsl:value-of select="@n"/>
	</xsl:when>
	<xsl:otherwise>
	  <xsl:call-template name="numberBodyDiv">
	    <xsl:with-param name="minimal" select="$minimal"/>
	  </xsl:call-template>
	</xsl:otherwise>
      </xsl:choose>
      <xsl:if test="$minimal=''"><xsl:value-of
      select="$headingNumberSuffix"/>
      </xsl:if>
    </xsl:when>
  </xsl:choose>
  <xsl:if test="$minimal=''">
    <xsl:choose>
      <xsl:when test="local-name(.) = 'TEI'">
	<xsl:apply-templates select="tei:teiHeader/tei:fileDesc/tei:titleStmt/tei:title"/>
      </xsl:when>
      <xsl:when test="not(tei:head) and @n">
	<xsl:value-of select="@n"/>
      </xsl:when>
      <xsl:when test="not($toc='')">
	  <xsl:call-template name="makeInternalLink">     
	    <xsl:with-param name="dest">
	      <xsl:value-of select="$toc"/>
	    </xsl:with-param>
	    <xsl:with-param name="class">
	      <xsl:value-of select="$class_toc"/>
	    </xsl:with-param>
	    <xsl:with-param name="body">
	      <xsl:choose>
		<xsl:when test="$autoHead='true'">
		  <xsl:call-template name="autoMakeHead"/>
		</xsl:when>
		<xsl:otherwise>
		  <xsl:apply-templates mode="plain" select="tei:head"/>
		</xsl:otherwise>
	      </xsl:choose>
	    </xsl:with-param>
	  </xsl:call-template>
      </xsl:when>
      <xsl:when test="$autoHead='true'">
	<xsl:call-template name="autoMakeHead"/>
      </xsl:when>
      <xsl:otherwise>
	<xsl:apply-templates mode="plain" select="tei:head"/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:if>
</xsl:template>

<xsl:template name="autoMakeHead">
  <xsl:choose>
    <xsl:when test="@type"><xsl:value-of select="@type"/></xsl:when>
    <xsl:otherwise>Heading</xsl:otherwise>
  </xsl:choose>
</xsl:template>

<!-- cross-referencing -->
<xsl:template name="makeTEILink">
<xsl:param name="ptr"/><!-- is this a ptr or a ref? -->
<xsl:choose>
  <!-- If there is a target attribute starting with #, it is always a local reference -->
  <xsl:when test="@target and starts-with(@target,'#')">
    <xsl:call-template name="makeInternalLink">
      <xsl:with-param name="target" select="substring-after(@target,'#')"/>
      <xsl:with-param name="ptr" select="$ptr"/>
      <xsl:with-param name="dest">
	<xsl:call-template name="generateEndLink">
	  <xsl:with-param name="where">
	    <xsl:value-of select="substring-after(@target,'#')"/>
	  </xsl:with-param>
	</xsl:call-template>
      </xsl:with-param>
    </xsl:call-template>
  </xsl:when>
<!-- if we are doing TEI P4, all targets are local -->
  <xsl:when test="@target and $teiP4Compat='true'">
    <xsl:call-template name="makeInternalLink">
      <xsl:with-param name="target" select="@target"/>
      <xsl:with-param name="ptr" select="$ptr"/>
      <xsl:with-param name="dest">
	<xsl:call-template name="generateEndLink">
	  <xsl:with-param name="where">
	    <xsl:value-of select="@target"/>
	  </xsl:with-param>
	</xsl:call-template>
      </xsl:with-param>
    </xsl:call-template>
  </xsl:when>
<!-- other uses of target means it is external -->
  <xsl:when test="@target">
    <xsl:call-template name="makeExternalLink">
      <xsl:with-param name="ptr" select="$ptr"/>
      <xsl:with-param name="dest">
	<xsl:value-of select="@target"/>
      </xsl:with-param>
    </xsl:call-template>
  </xsl:when>
  <!-- If there is a url attribute starting with #, it is a local
       reference -->
  <xsl:when test="@url and starts-with(@url,'#')">
    <xsl:call-template name="makeInternalLink">
      <xsl:with-param name="target" select="substring-after(@url,'#')"/>
      <xsl:with-param name="ptr" select="$ptr"/>
      <xsl:with-param name="dest">
	<xsl:call-template name="generateEndLink">
	  <xsl:with-param name="where">
	    <xsl:value-of select="substring-after(@url,'#')"/>
	  </xsl:with-param>
	</xsl:call-template>
      </xsl:with-param>
    </xsl:call-template>
  </xsl:when>
  <!-- otherwise it is an external URL -->
  <xsl:when test="@url">
    <xsl:call-template name="makeExternalLink">
      <xsl:with-param name="ptr" select="$ptr"/>
      <xsl:with-param name="dest">
	<xsl:value-of select="@url"/>
      </xsl:with-param>
    </xsl:call-template>
  </xsl:when>
<!-- A doc attribute means an external reference -->
    <xsl:when test="@doc">
      <xsl:call-template name="makeExternalLink">
	<xsl:with-param name="ptr" select="$ptr"/>
	<xsl:with-param name="dest">
	    <xsl:value-of select="unparsed-entity-uri(@doc)"/>
	</xsl:with-param>
      </xsl:call-template>
    </xsl:when>
    <xsl:otherwise>
<!--
      <xsl:for-each select="@*">
	[[markup error: <xsl:value-of select="name(.)"/>=<xsl:value-of select="."/>]]
      </xsl:for-each>
-->
      <xsl:apply-templates/>
    </xsl:otherwise>
  </xsl:choose>
</xsl:template>

<xsl:template match="tei:ref|tei:xref">
  <xsl:call-template name="makeTEILink">
    <xsl:with-param name="ptr">false</xsl:with-param>
  </xsl:call-template>
</xsl:template>

<xsl:template match="tei:ptr|tei:xptr">
  <xsl:call-template name="makeTEILink">
    <xsl:with-param name="ptr">true</xsl:with-param>
  </xsl:call-template>
</xsl:template>



</xsl:stylesheet>
