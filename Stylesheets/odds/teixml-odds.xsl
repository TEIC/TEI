<?xml version="1.0" encoding="utf-8"?>
<!--
Text Encoding Initiative Consortium XSLT stylesheet family
$Date$, $Revision$, $Author$

XSL stylesheet to process TEI documents using ODD markup


##LICENSE
-->

<xsl:stylesheet 
  xmlns:a="http://relaxng.org/ns/compatibility/annotations/1.0"
  xmlns:rng="http://relaxng.org/ns/structure/1.0"
  xmlns:tei="http://www.tei-c.org/ns/1.0"
  xmlns:teix="http://www.tei-c.org/ns/Examples"
  xmlns:local="http://www.pantor.com/ns/local"
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
  xmlns:fo="http://www.w3.org/1999/XSL/Format"
  xmlns:edate="http://exslt.org/dates-and-times"
  xmlns:exsl="http://exslt.org/common"
  xmlns:estr="http://exslt.org/strings"
  exclude-result-prefixes="exsl estr edate fo a tei rng local teix" 
  extension-element-prefixes="edate exsl estr"
  version="1.0">


<xsl:param name="oddmode">tei</xsl:param>
 <xsl:include href="teiodds.xsl"/>
 <xsl:include href="../common/teicommon.xsl"/>
  <xsl:key name="FILES"   match="tei:moduleSpec[@ident]"   use="@ident"/>
  <xsl:key name="IDS"     match="tei:*[@id]"           use="@id"/>
  <xsl:key name="DTDREFS" match="tei:specGrpRef"           use="@target"/>
  <xsl:key name="PATTERNS" match="tei:macroSpec" use="@ident"/>
  <xsl:key name="MACRODOCS" match="tei:macroSpec" use='1'/>
  <xsl:key name="CLASSDOCS" match="tei:classSpec" use='1'/>
  <xsl:key name="TAGDOCS" match="tei:elementSpec" use='1'/>
  <xsl:key name="CHUNKS" match="tei:specGrpRef" use="@target"/>
  <xsl:variable name="uc">ABCDEFGHIJKLMNOPQRSTUVWXYZ</xsl:variable>
  <xsl:variable name="lc">abcdefghijklmnopqrstuvwxyz</xsl:variable>

<xsl:param name="displayMode">rng</xsl:param>

<xsl:template match="tei:classSpec|tei:elementSpec|tei:macroSpec">
  <xsl:if test="parent::tei:specGrp">
    <tei:hi><xsl:value-of select="@ident"/></tei:hi>:
    <xsl:apply-templates select="." mode="tangle"/>
  </xsl:if>
</xsl:template>

<xsl:template  match="tei:specGrpRef">
&#171; <tei:emph>include
<tei:ref target="{@target}"><xsl:for-each select="key('IDS',@target)">
   <xsl:call-template name="compositeNumber"/>
</xsl:for-each></tei:ref></tei:emph>
<xsl:text> &#187; </xsl:text>
</xsl:template>

<xsl:template match="rng:*|tei:*|@*|processing-instruction()|tei:author|tei:title">
 <xsl:copy>
  <xsl:apply-templates select="tei:*|rng:*|@*|processing-instruction()|comment()|text()"/>
 </xsl:copy>
</xsl:template>

<xsl:template match="tei:altIdent"/>

<xsl:template match="tei:attDef" mode="summary">
 <tei:label><tei:code><xsl:call-template name="identifyMe"/></tei:code></tei:label>
 <tei:item>
   <xsl:apply-templates select="tei:desc" mode="show"/>
   <xsl:apply-templates select="tei:valList"/>
 </tei:item>
</xsl:template>

<xsl:template match="tei:attDef">
 <tei:label><tei:code><xsl:call-template name="identifyMe"/></tei:code></tei:label>
 <tei:item>
   <xsl:apply-templates select="tei:desc" mode="show"/>
   <xsl:apply-templates select="tei:valList"/>
 </tei:item>
</xsl:template>

<xsl:template match="tei:attDef/tei:datatype">
 <tei:label>
   <tei:emph>Datatype:</tei:emph>
 </tei:label>
 <tei:item>
   <xsl:call-template name="Literal"/>
 </tei:item>
</xsl:template>

<xsl:template match="tei:attDef/tei:exemplum">
 <tei:label><tei:emph>Example: </tei:emph></tei:label>
 <tei:item>
 <xsl:call-template name="verbatim">
  <xsl:with-param name="text">
  <xsl:apply-templates/>
  </xsl:with-param>
</xsl:call-template>
</tei:item>
</xsl:template>


<xsl:template match="tei:attList" mode="show">
      <xsl:call-template name="displayAttList">
	<xsl:with-param name="mode">summary</xsl:with-param>
      </xsl:call-template>
</xsl:template>

<xsl:template match="tei:attList" mode="summary">
<xsl:if test="tei:attDef">
  <tei:list type="gloss">
    <xsl:apply-templates mode="summary"/>
  </tei:list>
</xsl:if>
</xsl:template>

<xsl:template match="tei:attList[@org='choice']">
<tei:label>Choice:</tei:label>
<tei:item>
  <tei:list type="gloss">
    <xsl:apply-templates mode="summary"/>
  </tei:list>
</tei:item>
</xsl:template>

<xsl:template match="tei:equiv" mode="weave">
  <xsl:if test="@name">
    <tei:p><xsl:text> Equivalent</xsl:text>
    <xsl:if test="@uri"> in \texttt{<xsl:value-of select="@uri"/>}</xsl:if>
    <xsl:text>: </xsl:text>
    <xsl:value-of select="@name"/></tei:p>
  </xsl:if>
</xsl:template>


<xsl:template match="tei:attList" mode="weave">
  <tei:p><tei:emph>Attributes: </tei:emph>
  <xsl:call-template name="displayAttList">
    <xsl:with-param name="mode">all</xsl:with-param>
  </xsl:call-template>
  </tei:p>
</xsl:template>

<xsl:template match="tei:body">
<xsl:copy>
  <xsl:apply-templates select="tei:*|rng:*|@*|processing-instruction()|comment()|text()"/>
</xsl:copy>
</xsl:template>

<xsl:template match="tei:classSpec" mode="weavebody">
  
  <xsl:apply-templates mode="weave"/>
  
  <tei:p>    <tei:emph>Member of classes</tei:emph>
  <xsl:call-template name="generateClassParents"/>
  </tei:p>
  
  <tei:p><tei:emph>Members</tei:emph>
  <xsl:call-template name="generateMembers"/>
  </tei:p>
  
  <xsl:call-template name="HTMLmakeTagsetInfo"/>
  
</xsl:template>


<xsl:template match="tei:classes"  mode="weave">
  <xsl:if test="tei:memberOf">
    <tei:p><tei:emph>Classes</tei:emph>
      <xsl:for-each select="tei:memberOf">
	<xsl:choose>
	  <xsl:when test="key('IDENTS',@key)">
	    <xsl:variable name="Key"><xsl:value-of select="@key"/></xsl:variable>
	    <xsl:for-each select="key('IDENTS',@key)">
	      <xsl:text>: </xsl:text>
	      <xsl:call-template name="linkTogether">
		<xsl:with-param name="name" select="@ident"/>
		<xsl:with-param name="url" select="@id"/>
	      </xsl:call-template>
	    </xsl:for-each>
	  </xsl:when>
	  <xsl:otherwise>
	    <xsl:text>: </xsl:text>
	    <xsl:value-of select="@key"/>
	  </xsl:otherwise>
	</xsl:choose>
      </xsl:for-each>
    </tei:p>
  </xsl:if>
</xsl:template>

<xsl:template match="tei:defaultVal">
  <tei:label><tei:emph>Default: </tei:emph></tei:label>
  <tei:item>
    <xsl:apply-templates/>
  </tei:item>
</xsl:template>

<xsl:template match="tei:desc" mode="weave"/>

<xsl:template match="tei:div0|tei:div1|tei:div2|tei:div3|tei:div4">
<tei:div>
  <xsl:apply-templates select="tei:*|rng:*|@*|processing-instruction()|comment()|text()"/>
</tei:div>
</xsl:template>

<xsl:template match="tei:elementSpec" mode="weavebody">
  <xsl:if test="not(tei:attList)">
    <tei:p><tei:emph>Attributes: </tei:emph>
      <xsl:choose>
	<xsl:when test="count(../tei:classes/tei:memberOf)&gt;0">
	  <xsl:text>Global attributes 
	  and those inherited from </xsl:text>
	  <xsl:for-each select="..">
	    <xsl:call-template name="generateClassParents"/>
	  </xsl:for-each>
	</xsl:when>
	<xsl:otherwise>
	  Global attributes only
	</xsl:otherwise>
      </xsl:choose>
    </tei:p>
  </xsl:if>
  <xsl:apply-templates mode="weave"/>
  <xsl:call-template name="HTMLmakeTagsetInfo"/>
</xsl:template>

<xsl:template match="tei:elementSpec/tei:content" mode="weave">
<tei:p><tei:emph>Declaration: </tei:emph>
  <xsl:call-template name="bitOut">
    <xsl:with-param name="grammar"></xsl:with-param>
    <xsl:with-param name="content">
      <Wrapper>
	<rng:element name="{../@ident}">
	  <rng:ref name="tei.global.attributes"/>
	  <xsl:for-each select="../tei:classes/tei:memberOf">
	    <xsl:for-each select="key('IDENTS',@key)">
	      <xsl:if test="tei:attList">
		<rng:ref name="{@ident}.attributes"/>
	      </xsl:if>
	    </xsl:for-each>
	  </xsl:for-each>
	  <xsl:apply-templates
	   select="../tei:attList" mode="tangle"/>
	  <xsl:copy-of select="rng:*"/>
	</rng:element>
      </Wrapper>
    </xsl:with-param>
  </xsl:call-template>
</tei:p>
</xsl:template>

<xsl:template match="tei:elementSpec/tei:exemplum" mode="weave">
<tei:p><tei:emph>Example: </tei:emph></tei:p>
 <xsl:apply-templates/>
</xsl:template>

<xsl:template match="tei:elementSpec|tei:classSpec" mode="show">
  <xsl:param name="atts"/>
  <tei:hi>&lt;<xsl:call-template name="identifyMe"/>&gt; </tei:hi>
  <xsl:value-of select="tei:desc"/>
  <xsl:choose>
    <xsl:when test="tei:attList//tei:attDef">
      <xsl:choose>
	<xsl:when test="not($atts='  ')">
	  Selected attributes: <tei:list type="gloss">
	  <xsl:for-each select="tei:attList//tei:attDef">
	    <xsl:if test="contains($atts,concat(' ',@ident,' '))">
	      <tei:label>
		<xsl:call-template name="identifyMe"/>
	      </tei:label>
	      <tei:item>
		<xsl:apply-templates select="tei:desc" mode="show"/>
	      </tei:item>
	    </xsl:if>
	  </xsl:for-each>
	  </tei:list>
	</xsl:when>
	<xsl:otherwise>
	  <xsl:apply-templates select="tei:attList" mode="summary"/>
	</xsl:otherwise>
      </xsl:choose>
    </xsl:when>
    <xsl:otherwise>
      <tei:list>
	<tei:item>
	  No attributes other than those globally
	  available (see definition for tei.global.attributes)
	</tei:item>
      </tei:list>
    </xsl:otherwise>
  </xsl:choose>
</xsl:template>

<xsl:template match="tei:equiv"/>

<xsl:template match="tei:exemplum">
<tei:p><tei:emph>Example: </tei:emph></tei:p>
 <xsl:apply-templates/>
</xsl:template>



<xsl:template match="tei:gloss" mode="weave"/>
<xsl:template match="tei:gloss"/>

<xsl:template match="tei:macroSpec" mode="weavebody">
      <xsl:apply-templates mode="weave"/>
      <xsl:call-template name="HTMLmakeTagsetInfo"/>
</xsl:template>

<xsl:template match="tei:macroSpec/tei:content" mode="weave">
  <tei:p><tei:emph>Declaration: </tei:emph>
  <xsl:call-template name="bitOut">
    <xsl:with-param name="grammar">true</xsl:with-param>
    <xsl:with-param name="content">
      <Wrapper>
	<rng:define name="{../@ident}">
	  <xsl:if test="starts-with(.,'component')">
	    <xsl:attribute name="combine">choice</xsl:attribute>
	  </xsl:if>
	  <xsl:copy-of select="rng:*"/>
	</rng:define>
      </Wrapper>
    </xsl:with-param>
  </xsl:call-template>
  </tei:p>
</xsl:template>

<xsl:template match="tei:moduleSpec">
    <tei:p>Module <xsl:call-template name="identifyMe"/>:
        <xsl:apply-templates select="tei:desc"/>
    </tei:p>
</xsl:template>

<xsl:template match="tei:p">
<tei:p>
<xsl:apply-templates/>
</tei:p>
</xsl:template>

<xsl:template match="tei:remarks" mode="weave">
    <xsl:apply-templates/>
    </xsl:template>

<xsl:template match="tei:remarks">
  <xsl:if test="*//text()">
    <tei:label>Notes: </tei:label>
    <tei:item><xsl:apply-templates/></tei:item>
  </xsl:if>
</xsl:template>


<xsl:template match="tei:schemaSpec">
  <tei:div>
    <tei:head>Schema <xsl:call-template name="identifyMe"/></tei:head>
    <xsl:call-template name="processSchemaFragment"/>
  </tei:div>
</xsl:template>

<xsl:template match="tei:specDesc">
  <tei:item>  
    <xsl:call-template name="processSpecDesc"/>
  </tei:item>
</xsl:template>


<xsl:template match="tei:specList">
<tei:list rend="specList">
  <xsl:apply-templates/>
</tei:list>
</xsl:template>


<xsl:template match="tei:valDesc">
  <tei:label><tei:emph>Values: </tei:emph></tei:label>
  <tei:item>
    <xsl:apply-templates/>
  </tei:item>
</xsl:template>



<xsl:template match="tei:valList" mode="contents">
      <xsl:choose>
        <xsl:when test="@type='semi'"> Suggested values include:</xsl:when>
        <xsl:when test="@type='open'"> Sample values include:</xsl:when>
        <xsl:when test="@type='closed'"> Legal values are:</xsl:when>
        <xsl:otherwise> Values are:</xsl:otherwise>
      </xsl:choose>
      <tei:list type="gloss">
       <xsl:for-each select="tei:valItem">
         <tei:label><xsl:call-template name="identifyMe"/></tei:label>
         <tei:item>
               <xsl:value-of select="tei:gloss"/>
	 </tei:item>
        </xsl:for-each>
      </tei:list>
</xsl:template>


<xsl:template match="tei:valList">
    <xsl:apply-templates select="." mode="contents"/>
</xsl:template>

<xsl:template match="teix:egXML">
  <xsl:call-template name="verbatim">
    <xsl:with-param name="label">
      <xsl:if test="not(parent::tei:exemplum)">
	<xsl:text>Example </xsl:text>
	<xsl:call-template name="compositeNumber"/>
      </xsl:if>
    </xsl:with-param>
    <xsl:with-param name="text">
      <xsl:apply-templates mode="verbatim"/>
    </xsl:with-param>
  </xsl:call-template>
</xsl:template>

<xsl:template name="HTMLmakeTagsetInfo">
  <tei:p><tei:emph>Module: </tei:emph>
    <xsl:call-template name="makeTagsetInfo"/>
  </tei:p>
</xsl:template>


<xsl:template name="Literal">
  <tei:eg>
    <xsl:apply-templates mode="literal"/>
  </tei:eg>
</xsl:template>


<xsl:template name="attclasses">
 <xsl:param name="classes"/>
 <xsl:if test="not($classes='') and not($classes=' ')">
   <xsl:variable name="class" select="key('IDS',substring-before($classes,' '))"/>
   <!--
     <xsl:message>     look up <xsl:value-of select="$class/@type"/>, <xsl:value-of select="$class/@ident"/>   </xsl:message>
   -->
   <xsl:if test="$class/@type='atts'">
     <xsl:call-template name="bitOut">
       <xsl:with-param name="grammar">true</xsl:with-param>
       <xsl:with-param name="content">
	 <Wrapper>
	   <rng:ref name="{$class/@ident}.attributes"/>
	 </Wrapper>
       </xsl:with-param>
     </xsl:call-template>
   </xsl:if>
   <xsl:call-template name="attclasses">
     <xsl:with-param name="classes">
       <xsl:value-of select="substring-after($classes,' ') "/>
     </xsl:with-param>
   </xsl:call-template>
 </xsl:if>
</xsl:template>


<xsl:template name="bitOut">
<xsl:param name="grammar"/>
<xsl:param name="content"/>
<xsl:param  name="element">eg</xsl:param> 
<tei:eg>
<xsl:choose>
<xsl:when test="$displayMode='rng'">
  <xsl:for-each  select="exsl:node-set($content)/Wrapper">
    <xsl:apply-templates mode="verbatim"/>
  </xsl:for-each>
</xsl:when>
<xsl:when test="$displayMode='rnc'">
<xsl:call-template name="make-body-from-r-t-f">
  <xsl:with-param name="schema">
    <xsl:for-each  select="exsl:node-set($content)/Wrapper">
      <xsl:call-template name="make-compact-schema"/>
    </xsl:for-each>
  </xsl:with-param>
</xsl:call-template>
</xsl:when>
<xsl:otherwise>
  <xsl:for-each  select="exsl:node-set($content)/Wrapper">
    <xsl:apply-templates mode="verbatim"/>
  </xsl:for-each>
</xsl:otherwise>
</xsl:choose>
</tei:eg>
</xsl:template>

<xsl:template name="displayAttList">
<xsl:param name="mode"/>
	<xsl:choose>
	  <xsl:when test=".//tei:attDef">
	    <xsl:choose>
	      <xsl:when test="count(../tei:classes/tei:memberOf)&gt;0">
		<xsl:text>(In addition to global attributes 
		and those inherited from </xsl:text>
		<xsl:for-each select="..">
		  <xsl:call-template name="generateClassParents"/>
		</xsl:for-each>
		<xsl:text>)</xsl:text>
	      </xsl:when>
	      <xsl:otherwise>
		(In addition to global attributes)        
	      </xsl:otherwise>
	    </xsl:choose>
	    <tei:list type="gloss">
	      <xsl:choose>
		<xsl:when test="$mode='all'">
		  <xsl:apply-templates/>
		</xsl:when>
		<xsl:otherwise>
		  <xsl:apply-templates mode="summary"/>
		</xsl:otherwise>
	      </xsl:choose>
	    </tei:list>
	  </xsl:when>
	  <xsl:otherwise>
	    <xsl:choose>
	      <xsl:when test="count(../tei:classes/tei:memberOf)&gt;0">
		<xsl:text>Global attributes 
		and those inherited from </xsl:text>
		<xsl:for-each select="..">
		  <xsl:call-template name="generateClassParents"/>
		</xsl:for-each>
	      </xsl:when>
	      <xsl:otherwise>
		Global attributes only
	      </xsl:otherwise>
	    </xsl:choose>
	  </xsl:otherwise>
	</xsl:choose>
</xsl:template>



<xsl:template name="embolden">
      <xsl:param name="text"/>
        <tei:hi><xsl:copy-of select="$text"/></tei:hi>
</xsl:template>


<xsl:template name="identifyMe">
  <xsl:choose>
    <xsl:when test="tei:altIdent">
      <xsl:value-of select="tei:altIdent"/>
    </xsl:when>
    <xsl:otherwise>
      <xsl:value-of select="@ident"/>
    </xsl:otherwise>
  </xsl:choose>
</xsl:template>

<xsl:template name="italicize">
  <xsl:param name="text"/>
    <tei:emph><xsl:copy-of select="$text"/></tei:emph>
</xsl:template>

<xsl:template name="logoFramePicture"/>

<xsl:template match="tei:specGrp">
  <tei:anchor>
    <xsl:copy-of select="@id"/>
  </tei:anchor>
  [<xsl:call-template name="compositeNumber"/>]
  <xsl:apply-templates/>
</xsl:template>

<xsl:template name="makeAnchor">
 <xsl:param name="name"/>
</xsl:template>

<xsl:template name="makeLink">
 <xsl:param name="class"/>
 <xsl:param name="id"/>
 <xsl:param name="name"/>
 <xsl:param name="text"/>
    <tei:ref rend="{$class}" target="{$name}"><xsl:copy-of  select="$text"/></tei:ref>
</xsl:template>

<xsl:template name="refdoc">
  <xsl:param name="name"/>
  <xsl:if test="$verbose='true'">
    <xsl:message>   refdoc for <xsl:value-of select="name(.)"/> -  <xsl:value-of select="@ident"/> </xsl:message>
  </xsl:if>
    <tei:div>
	<xsl:attribute name="id"><xsl:value-of
	select="@ident"/></xsl:attribute>
      <tei:head>
	<xsl:call-template name="identifyMe"/>
	[<xsl:value-of select="substring-before(local-name(.),'Spec')"/>]
      </tei:head>
      <tei:p><tei:emph>Description: </tei:emph>
      <xsl:apply-templates select="tei:desc" mode="show"/></tei:p>
      <xsl:apply-templates select="." mode="weavebody"/>
  </tei:div>
</xsl:template>

<xsl:template name="teiStartHook"/>

<xsl:template name="ttembolden">
      <xsl:param name="text"/>
        <tei:hi><tei:code><xsl:copy-of select="$text"/></tei:code></tei:hi>
</xsl:template>

<xsl:template name="typewriter">
  <xsl:param name="text"/>
  <tei:code>
    <xsl:copy-of select="$text"/>
  </tei:code>
</xsl:template>


<xsl:template name="verbatim">
  <xsl:param name="label"/>
  <xsl:param name="text"/>
  <xsl:param name="startnewline">false</xsl:param>
  <xsl:param name="autowrap">false</xsl:param>
  <tei:eg>
    <xsl:if test="not($label='')">
      <xsl:attribute name="n">
	<xsl:value-of select="$label"/>
      </xsl:attribute>
    </xsl:if>
    <xsl:if test="$startnewline='true'">
      <xsl:text>&#10;</xsl:text>
    </xsl:if>
    <xsl:choose>
      <xsl:when test="$autowrap='false'">
	<xsl:value-of select="$text"/>
      </xsl:when>
      <xsl:otherwise>           
	<xsl:variable name="lines" select="estr:tokenize($text,'&#10;')"/>
	<xsl:apply-templates select="$lines[1]" 
			     mode="normalline"/>
      </xsl:otherwise>
    </xsl:choose>
  </tei:eg>
</xsl:template>

</xsl:stylesheet>
