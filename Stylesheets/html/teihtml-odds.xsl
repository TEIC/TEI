<?xml version="1.0" encoding="utf-8"?>
<!-- 
Text Encoding Initiative Consortium XSLT stylesheet family
$Date$, $Revision$, $Author$

XSL stylesheet to format TEI XML documents to HTML or XSL FO

 
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


 <xsl:include href="../odds/teiodds.xsl"/>
 <xsl:include href="../common/teicommon.xsl"/>

  <xsl:key name="FILES"   match="tei:module[@ident]"   use="@ident"/>
  <xsl:key name="IDS"     match="tei:*[@id]"           use="@id"/>
  <xsl:key name="DTDREFS" match="tei:specGrpRef"           use="@target"/>
  <xsl:key name="PATTERNS" match="tei:macroSpec" use="@ident"/>
  <xsl:key name="PATTERNDOCS" match="tei:macroSpec" use='1'/>
  <xsl:key name="CLASSDOCS" match="tei:classSpec" use='1'/>
  <xsl:key name="TAGDOCS" match="tei:elementSpec" use='1'/>
  <xsl:key name="CHUNKS" match="tei:specGrpRef" use="@target"/>
  <xsl:variable name="uc">ABCDEFGHIJKLMNOPQRSTUVWXYZ</xsl:variable>
  <xsl:variable name="lc">abcdefghijklmnopqrstuvwxyz</xsl:variable>

<xsl:param name="displayMode">rnc</xsl:param>

<xsl:template match="tei:attDef">
  <xsl:variable name="name">
    <xsl:choose>
      <xsl:when test="tei:altIdent">
	<xsl:value-of select="tei:altIdent"/>
      </xsl:when>
      <xsl:otherwise>
	<xsl:value-of select="@ident"/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:variable>
 <tr>
 <td valign="top"><tt><b>
 <xsl:value-of select="$name"/>
 </b></tt></td>
 <td colspan="2"><xsl:apply-templates select="tei:desc" mode="show"/></td>
 </tr>
 <xsl:apply-templates/>
</xsl:template>

<xsl:template match="tei:attDef" mode="summary">
  <xsl:variable name="name">
    <xsl:choose>
      <xsl:when test="tei:altIdent">
	<xsl:value-of select="tei:altIdent"/>
      </xsl:when>
      <xsl:otherwise>
	<xsl:value-of select="@ident"/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:variable>
 <tr>
 <td valign="top"><tt><b>
 <xsl:value-of select="$name"/>
 </b></tt></td>
 <td colspan="2"><xsl:apply-templates select="tei:desc" mode="show"/></td>
 </tr>
 <xsl:apply-templates select="valList"/>
</xsl:template>

<xsl:template match="tei:attDef/tei:datatype">
 <tr>
   <td></td>
   <td colspan="2" valign="top"><i>Datatype:</i>
     <xsl:call-template name="bitOut">
       <xsl:with-param name="grammar"></xsl:with-param>
       <xsl:with-param name="content">
	 <Wrapper>
	   <xsl:copy-of select="rng:*"/>
	 </Wrapper>
       </xsl:with-param>
       <xsl:with-param name="element">code</xsl:with-param>
   </xsl:call-template>
   </td>
  </tr>
</xsl:template>

<xsl:template match="tei:attDef/tei:eg">
 <tr>
 <td></td>
 <td valign="top" colspan='2'>
 <i>Example: </i>
 <xsl:call-template name="verbatim">
  <xsl:with-param name="text">
  <xsl:variable name="content"><xsl:apply-templates/></xsl:variable>
  <xsl:choose>
  <xsl:when test ="@copyOf">
   <xsl:variable name="copyOfstr">
    <xsl:apply-templates select="key('IDS',@copyOf)" mode="copy"/>
   </xsl:variable>
     <xsl:choose>
      <xsl:when test="$copyOfstr">
      <xsl:value-of select="$copyOfstr"/>
      </xsl:when>
      <xsl:otherwise>
       <xsl:comment>* No stuff found for copyOf $copyOf *</xsl:comment>
      </xsl:otherwise>
     </xsl:choose>
  </xsl:when>
  <xsl:otherwise>
  <xsl:value-of select="$content"/>
  </xsl:otherwise>
  </xsl:choose>
</xsl:with-param></xsl:call-template>
 </td></tr>
</xsl:template>


<xsl:template match="tei:classSpec" mode="weave">
  <xsl:variable name="name">
    <xsl:choose>
      <xsl:when test="tei:altIdent">
	<xsl:value-of select="tei:altIdent"/>
      </xsl:when>
      <xsl:otherwise>
	<xsl:value-of select="@ident"/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:variable>
  <tr>
    <td valign='top'><tt><b><xsl:value-of select="$name"/></b></tt></td>
    <td colspan="2"><xsl:apply-templates select="tei:desc" mode="show"/></td>
  </tr>
  <xsl:apply-templates mode="weave"/>
  <tr>
    <td valign="top"><i>Member of classes</i></td>
    <td colspan="2">
      <xsl:call-template name="generateClassParents"/>
      &#160;
    </td>
  </tr>
  <tr>
    <td valign="top"><i>Members</i></td>
    <td colspan="2">
      <xsl:call-template name="generateMembers"/>
    </td>
  </tr>
  <xsl:call-template name="HTMLmakeTagsetInfo"/>
</xsl:template>


<xsl:template match="tei:classes"  mode="weave">
  <xsl:if test="tei:memberOf">
    <tr><td valign="top"><i>Class</i></td><td colspan="2">
    <xsl:for-each select="tei:memberOf">
      <xsl:choose>
	<xsl:when test="key('IDENTS',@key)">
	<xsl:variable name="Key"><xsl:value-of select="@key"/></xsl:variable>
	  <xsl:for-each select="key('IDENTS',@key)">
	    <xsl:if test="not(generate-id(.)=generate-id(key('IDENTS',$Key)[1]))">          <xsl:text> |  </xsl:text>
	    </xsl:if>
	    <xsl:call-template name="linkTogether">
	      <xsl:with-param name="inner" select="@ident"/>
	      <xsl:with-param name="origid" select="@id"/>
	    </xsl:call-template>
	  </xsl:for-each>
	</xsl:when>
	<xsl:otherwise>
	  <xsl:value-of select="@key"/>
	</xsl:otherwise>
      </xsl:choose>
      <xsl:text> </xsl:text>
      
    </xsl:for-each>
    </td></tr>
  </xsl:if>
</xsl:template>

<xsl:template match="tei:defaultVal">
 <tr><td></td><td valign="top" colspan='2'><i>Default: </i>
 <xsl:apply-templates/>
 </td></tr>
</xsl:template>


<xsl:template match="tei:eg">
 <xsl:call-template name="verbatim">
  <xsl:with-param name="autowrap">false</xsl:with-param>
  <xsl:with-param name="startnewline">
   <xsl:if test="parent::tei:exemplum">true</xsl:if>
  </xsl:with-param>    
  <xsl:with-param name="text">
  <xsl:choose>
  <xsl:when test="@copyOf">
   <xsl:variable name="copyOfstr">
    <xsl:apply-templates select="key('IDS',@copyOf)" mode="copy"/>
   </xsl:variable>
   <xsl:choose>
     <xsl:when test="$copyOfstr">
      <xsl:value-of select="$copyOfstr"/>
     </xsl:when>
     <xsl:otherwise>
       <xsl:comment>* No element found for copyOf $copyOf *</xsl:comment>
     </xsl:otherwise>
     </xsl:choose>
  </xsl:when>
 </xsl:choose>
   <xsl:apply-templates/>
 </xsl:with-param></xsl:call-template>
</xsl:template>



<xsl:template match="tei:exemplum" mode="weave">
 <tr><td valign='top'><i>Example</i></td><td colspan='2'>
  <xsl:apply-templates/>
 </td></tr>
</xsl:template>


<xsl:template match="tei:item"> 
 <xsl:choose>
   <xsl:when test="parent::tei:list[@type='gloss']"> 
     &#9;<xsl:apply-templates/>
   </xsl:when>
   <xsl:when test="parent::tei:list[@type='elementlist']"> 
     &#9;<xsl:apply-templates/>
   </xsl:when>
   <xsl:otherwise>
     <li><xsl:apply-templates/></li>
   </xsl:otherwise>
 </xsl:choose>
</xsl:template>




<xsl:template match="tei:module">
<hr/>
<!--<xsl:apply-templates/>-->
  <xsl:call-template name="processSchemaFragment"/>
<hr/>
</xsl:template>

<xsl:template match="tei:macroSpec" mode="weave">
  <xsl:variable name="name">
    <xsl:choose>
      <xsl:when test="tei:altIdent">
	<xsl:value-of select="tei:altIdent"/>
      </xsl:when>
      <xsl:otherwise>
	<xsl:value-of select="@ident"/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:variable>

      <tr><td valign='top'>
      <tt><b><xsl:value-of select="$name"/></b></tt></td>
      <td colspan="2"><xsl:apply-templates select="tei:desc" mode="show"/></td>
      </tr>
      <xsl:apply-templates mode="weave"/>
      <xsl:call-template name="HTMLmakeTagsetInfo"/>
</xsl:template>

<xsl:template match="tei:macroSpec/tei:content" mode="weave">
      <tr><td valign='top'><i>Declaration</i></td><td colspan='2'>
      <xsl:call-template name="bitOut">
	<xsl:with-param name="grammar">true</xsl:with-param>
	<xsl:with-param name="content">
	  <Wrapper>
	    <xsl:variable name="entCont">
	      <Stuff>
		<xsl:apply-templates select="rng:*"/>
	      </Stuff>
	    </xsl:variable>
	    <xsl:variable name="entCount">
	      <xsl:for-each select="exsl:node-set($entCont)/Stuff">
		<xsl:value-of select="count(*)"/>
	      </xsl:for-each>
	    </xsl:variable>
	    <xsl:choose>
	      <xsl:when test='.="TEI.singleBase"'/>
	      <xsl:otherwise>
		<rng:define name="{../@ident}">
		  <xsl:if test="starts-with(.,'component')">
		    <xsl:attribute name="combine">choice</xsl:attribute>
		  </xsl:if>
		      <xsl:copy-of select="rng:*"/>
		</rng:define>
	      </xsl:otherwise>
	    </xsl:choose>
	  </Wrapper>
      </xsl:with-param></xsl:call-template>
      </td></tr>    
</xsl:template>

<xsl:template match="tei:ptr" mode="useme">
  <xsl:if test="count(preceding-sibling::tei:ptr)&gt;0">; </xsl:if>
  <xsl:variable name="xx">
    <xsl:apply-templates mode="header" select="key('IDS',@target)">
      <xsl:with-param name="minimal" select="$minimalCrossRef"/>
    </xsl:apply-templates>
  </xsl:variable>
  <a class="ptr">
    <xsl:attribute name="href">
      <xsl:apply-templates mode="xrefheader" select="key('IDS',@target)"/>
    </xsl:attribute>
    <xsl:value-of select="normalize-space($xx)"/>
  </a>
</xsl:template>

<xsl:template match="tei:ptr">
  <xsl:choose>
    <xsl:when test="parent::tei:listRef">
      <xsl:if test="count(preceding-sibling::tei:ptr)=0">
	<tr><td valign="top"><i>See further</i></td><td colspan="2">
	<xsl:apply-templates select="." mode="useme"/>
	</td></tr>
      </xsl:if>
    </xsl:when>
    <xsl:otherwise>
      <a class="ptr">
	<xsl:attribute name="href">
	  <xsl:apply-templates mode="xrefheader" select="key('IDS',@target)"/>
	</xsl:attribute>
	<xsl:variable name="xx">
	  <xsl:apply-templates mode="header" select="key('IDS',@target)">
	    <xsl:with-param name="minimal" select="$minimalCrossRef"/>
	  </xsl:apply-templates>
	</xsl:variable>
	<xsl:value-of select="normalize-space($xx)"/>
      </a>
    </xsl:otherwise>
  </xsl:choose>
</xsl:template>

<xsl:template match="tei:remarks" mode="weave">
  <xsl:if test="*//text()">
    <tr><td valign="top"><i>Note</i></td><td colspan="2">
    <xsl:apply-templates/>
    </td></tr>
  </xsl:if>
</xsl:template>


<xsl:template match="tei:schema">
  <hr/> 
  <xsl:call-template name="processSchemafrag"/>
  <hr/>
</xsl:template>

<xsl:template match="tei:specDesc">
  <li>  
    <xsl:call-template name="processSpecDesc"/>
  </li>
</xsl:template>


<xsl:template match="tei:elementSpec" mode="weave">
  <xsl:variable name="name">
    <xsl:choose>
      <xsl:when test="tei:altIdent">
	<xsl:value-of select="tei:altIdent"/>
      </xsl:when>
      <xsl:otherwise>
	<xsl:value-of select="@ident"/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:variable>

  <tr><td valign='top'>
  <tt><b><xsl:value-of select="$name"/></b></tt></td>
  <td colspan="2"><xsl:apply-templates select="tei:desc" mode="show"/></td>
  </tr>
  <xsl:if test="not(tei:attList)">
    <tr>
      <td valign="top"><i>Attributes </i></td>
      <td>
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
      </td>
    </tr>
  </xsl:if>
  <xsl:apply-templates mode="weave"/>
  <xsl:call-template name="HTMLmakeTagsetInfo"/>
</xsl:template>

<xsl:template match="tei:elementSpec|tei:classSpec" mode="show">
  <xsl:param name="atts"/>
  <xsl:variable name="name">
    <xsl:choose>
      <xsl:when test="tei:altIdent">
	<xsl:value-of select="tei:altIdent"/>
      </xsl:when>
      <xsl:otherwise>
	<xsl:value-of select="@ident"/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:variable>
  <b>&lt;<a href="ref-{@id}.html"><xsl:value-of select="$name"/></a>&gt; </b>
  <xsl:value-of select="tei:desc"/>
  <xsl:choose>
    <xsl:when test="tei:attList//tei:attDef">
      <xsl:choose>
	<xsl:when test="not($atts='')">
	  <table class="attList">
	    <xsl:variable name="HERE" select="."/>
	    <xsl:for-each select="estr:tokenize($atts)">
	      <xsl:variable name="TOKEN" select="."/>
	      <xsl:for-each  select="$HERE/tei:attList//tei:attDef[@ident=$TOKEN]">
		<tr>
		  <td valign="top"><b><xsl:value-of select="$name"/></b></td>
		  <td colspan="2"><xsl:apply-templates select="tei:desc" mode="show"/></td>
		</tr>
	      </xsl:for-each>
	    </xsl:for-each>
	  </table>
	</xsl:when>
	<xsl:otherwise>
	  <table class="attList">
	    <xsl:apply-templates select="tei:attList" mode="summary"/>
	  </table>
	</xsl:otherwise>
      </xsl:choose>
    </xsl:when>
    <xsl:otherwise>
      <table  class="attList">
	<tr>
	  <td valign="top" colspan='2'>
	  No attributes other than those globally
	  available (see definition for tei.global.attributes)</td>
	</tr>
      </table>
    </xsl:otherwise>
  </xsl:choose>
</xsl:template>

<xsl:template match="tei:elementSpec/tei:content" mode="weave">
  <xsl:variable name="name">
    <xsl:choose>
      <xsl:when test="../tei:altIdent">
	<xsl:value-of select="../tei:altIdent"/>
      </xsl:when>
      <xsl:otherwise>
	<xsl:value-of select="../@ident"/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:variable>
  <tr><td valign='top'><i>Declaration</i></td><td colspan='2'>
  <xsl:call-template name="bitOut">
    <xsl:with-param name="grammar"></xsl:with-param>
    <xsl:with-param name="content">
      <Wrapper>
	<rng:element name="{$name}">
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
  </td>
  </tr>
</xsl:template>

<xsl:template match="tei:specList">
<ul class="specList">
  <xsl:apply-templates/>
</ul>
</xsl:template>


<xsl:template match="tei:valDesc">
  <tr>
    <td></td><td><i>Values: </i> <xsl:apply-templates/></td>
  </tr>
</xsl:template>



<xsl:template match="tei:valList" mode="contents">
      <xsl:choose>
        <xsl:when test="@type='semi'">Suggested values include:</xsl:when>
        <xsl:when test="@type='open'">Sample values include:</xsl:when>
        <xsl:when test="@type='closed'">Legal values are:</xsl:when>
        <xsl:otherwise>Values are:</xsl:otherwise>
      </xsl:choose>
      <table class="valList">
       <xsl:for-each select="tei:valItem">
	 <xsl:variable name="name">
	   <xsl:choose>
	     <xsl:when test="tei:altIdent">
	       <xsl:value-of select="tei:altIdent"/>
	     </xsl:when>
	     <xsl:otherwise>
	       <xsl:value-of select="@ident"/>
	     </xsl:otherwise>
	   </xsl:choose>
	 </xsl:variable>
         <tr><td valign="top"><b><xsl:value-of select="$name"/></b></td>
         <td valign="top">
               <xsl:value-of select="tei:gloss"/></td>
         </tr>
        </xsl:for-each>
      </table>
</xsl:template>


<xsl:template match="tei:valList">
 <xsl:choose>
 <xsl:when test="ancestor::tei:elementSpec or ancestor::tei:classSpec or ancestor::tei:macroSpec">
    <tr><td></td><td valign="top">
    <xsl:apply-templates select="." mode="contents"/>
    </td></tr>
 </xsl:when>
 <xsl:otherwise>
   <xsl:apply-templates select="." mode="contents"/>
  </xsl:otherwise>
 </xsl:choose>
</xsl:template>

<xsl:template match="teix:egXML">
<pre>
  <xsl:apply-templates/>
</pre>
</xsl:template>




<xsl:template match="tei:attList" mode="weave">
  <tr>
    <td valign="top"><i>Attributes </i></td>
    <td>
      <xsl:call-template name="displayAttList">
	<xsl:with-param name="mode">all</xsl:with-param>
      </xsl:call-template>
    </td>
  </tr>
</xsl:template>

<xsl:template match="tei:attList" mode="show">
      <xsl:call-template name="displayAttList">
	<xsl:with-param name="mode">summary</xsl:with-param>
      </xsl:call-template>
</xsl:template>

<xsl:template name="displayAttList">
<xsl:param name="mode"/>
      <table class="attList">
	<tr><td>
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
	    <table>
	      <xsl:choose>
		<xsl:when test="$mode='all'">
		  <xsl:apply-templates/>
		</xsl:when>
		<xsl:otherwise>
		  <xsl:apply-templates mode="summary"/>
		</xsl:otherwise>
	      </xsl:choose>
	    </table>
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
	</td>
	</tr>
      </table>
</xsl:template>



<xsl:template name="HTMLmakeTagsetInfo">
 <tr><td valign="top"><i>Module</i></td><td colspan="2">
  <xsl:call-template name="makeTagsetInfo"/>
 </td></tr>
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
<xsl:param  name="element">pre</xsl:param> 
<xsl:element name="{$element}">
 <xsl:attribute name="class">eg</xsl:attribute>
<xsl:choose>
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
  <xsl:apply-templates mode="literal"/>
    </xsl:for-each>
</xsl:otherwise>
</xsl:choose>
</xsl:element>
</xsl:template>


<xsl:template name="embolden">
      <xsl:param name="text"/>
        <b><xsl:copy-of select="$text"/></b>
</xsl:template>


<xsl:template name="typewriter">
      <xsl:param name="text"/>
        <tt><xsl:copy-of select="$text"/></tt>
</xsl:template>


<xsl:template name="ttembolden">
      <xsl:param name="text"/>
        <b><tt><xsl:copy-of select="$text"/></tt></b>
</xsl:template>


<xsl:template name="italicize">
  <xsl:param name="text"/>
    <i><xsl:copy-of select="$text"/></i>
</xsl:template>

<xsl:template name="logoFramePicture"/>


<xsl:template name="makeLink">
 <xsl:param name="class"/>
 <xsl:param name="url"/>
 <xsl:param name="text"/>
    <a class="{$class}" href="{$url}"><xsl:copy-of  select="$text"/></a>
</xsl:template>

<xsl:template name="makeAnchor">
 <xsl:param name="name"/>
    <a  name="{$name}"/>
</xsl:template>

<xsl:template name="refdoc">
  <xsl:if test="$verbose='true'">
    <xsl:message>   refdoc for <xsl:value-of 
    select="name(.)"/> - <xsl:value-of select="@id"/> </xsl:message>
  </xsl:if>
  <xsl:variable name="objectname">
    <xsl:choose>
      <xsl:when test="tei:altIdent">
	<xsl:value-of select="tei:altIdent"/>
      </xsl:when>
      <xsl:otherwise>
	<xsl:value-of select="@ident"/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:variable>

  <xsl:variable name="name">
    <xsl:choose>
      <xsl:when test="name(.)='elementSpec'">
	<xsl:text>&lt;</xsl:text>
	<xsl:value-of select="$objectname"/>
	<xsl:text>&gt;</xsl:text>
      </xsl:when>
      <xsl:otherwise>
	<xsl:value-of select="$objectname"/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:variable>
  [<a href="ref-{@id}.html"><xsl:value-of select="$name"/></a>]
  <xsl:variable name="BaseFile">
    <xsl:value-of select="$masterFile"/>
    <xsl:if test="ancestor::tei:teiCorpus">
      <xsl:text>-</xsl:text>
      <xsl:choose>
	<xsl:when test="@id"><xsl:value-of select="@id"/></xsl:when> 
	<xsl:otherwise><xsl:number/></xsl:otherwise>
      </xsl:choose>
    </xsl:if>
  </xsl:variable>
  <xsl:call-template name="outputChunk">
    <xsl:with-param name="ident">
      <xsl:text>ref-</xsl:text><xsl:value-of select="@id"/>
    </xsl:with-param>
    <xsl:with-param name="content">
      <html>
	<xsl:comment>THIS IS A GENERATED FILE. DO NOT EDIT</xsl:comment>
	<head><title><xsl:value-of select="$name"/></title>
	<xsl:if test="not($cssFile = '')">
	  <link rel="stylesheet" type="text/css" href="{$cssFile}"/>
	</xsl:if>
	</head>
	<body>
	  <xsl:call-template name="bodyHook"/>
	  <a name="TOP"/>
	  <div class="teidiv">
	    <xsl:call-template name="stdheader">
	      <xsl:with-param name="title">
		<xsl:value-of select="$name"/>
	      </xsl:with-param>
	    </xsl:call-template>
	    <p><a name="{@id}"></a>
	    <table border='1'>
	      <xsl:apply-templates select="." mode="weave"/>
	    </table></p>
	    <p align="{$alignNavigationPanel}">
	      <a  class="navlink" href="REFCLA.html">Classes</a> |
	      <a  class="navlink" href="REFENT.html">Macros</a> |
	      <a  class="navlink" href="REFTAG.html">Elements</a>
	    </p>
	  </div>
      </body></html>
    </xsl:with-param>
  </xsl:call-template>
</xsl:template>

<xsl:template name="teiStartHook"/>

<xsl:template name="verbatim">
  <xsl:param name="text"/>
  <xsl:param name="startnewline">false</xsl:param>
  <xsl:param name="autowrap">true</xsl:param>
     <pre class="eg">
        <xsl:if test="$startnewline='true'">
         <xsl:text>
</xsl:text>
       </xsl:if>
       <xsl:choose>
         <xsl:when test="$autowrap='false'">
           <xsl:value-of select="."/>
         </xsl:when>
       <xsl:otherwise>           
       <xsl:variable name="lines" select="estr:tokenize($text,'&#10;')"/>
           <xsl:apply-templates select="$lines[1]" 
                mode="normalline"/>
     </xsl:otherwise>
   </xsl:choose>
 </pre>
</xsl:template>


<xsl:template match="tei:desc" mode="weave"/>
<xsl:template match="tei:gloss" mode="weave"/>
<xsl:template match="tei:gloss"/>

</xsl:stylesheet>
