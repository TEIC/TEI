<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet 
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
  xmlns:t="http://www.thaiopensource.com/ns/annotations" 
  xmlns:a="http://relaxng.org/ns/compatibility/annotations/1.0" 
  xmlns:edate="http://exslt.org/dates-and-times"
  xmlns:exsl="http://exslt.org/common"
  xmlns:tei="http://www.tei-c.org/ns/1.0"
  xmlns:rng="http://relaxng.org/ns/structure/1.0"
  xmlns:fo="http://www.w3.org/1999/XSL/Format"
  exclude-result-prefixes="a t tei fo exsl rng edate" 
  extension-element-prefixes="exsl edate rng"
  version="1.0">
 
  <xsl:import href="teiodds.xsl"/>

  <xsl:key name="GIs" match="tei:elementSpec" use="@ident"/>
  <xsl:output method="text"/>
  <xsl:param name="verbose"></xsl:param>
  <xsl:param name="parameterize">false</xsl:param>
  <xsl:variable name="appendixWords">  </xsl:variable>
  <xsl:variable name="filesuffix"></xsl:variable>
  <xsl:variable name="headingNumberSuffix">  </xsl:variable>
  <xsl:variable name="numberBackHeadings">  </xsl:variable>
  <xsl:variable name="numberFrontHeadings">  </xsl:variable>
  <xsl:variable name="numberHeadings">  </xsl:variable>
  <xsl:variable name="numberHeadingsDepth">  </xsl:variable>
  <xsl:variable name="oddmode">dtd</xsl:variable>       
  <xsl:variable name="prenumberedHeadings">  </xsl:variable>
  <xsl:variable name="linkColor"/>
  <xsl:key name="Modules"   match="tei:moduleSpec" use="1"/>


<xsl:template match="/tei:TEI">
  <xsl:variable name="Master" select="."/>
  
  <xsl:for-each select="key('Modules',1)">
    <xsl:sort select="@ident" order="descending"/>
    <xsl:message>   file [<xsl:value-of select="@ident"/>]
    </xsl:message>
    <exsl:document method="text" href="DTD/{@ident}.dtd">
      <xsl:text>&lt;!-- TEI P5 DTD. Generated </xsl:text>
      <xsl:value-of select="edate:date-time()"/>
      <xsl:call-template name="copyright"/>
 --&gt;
<xsl:if test="@type='core'">
<!-- legacy declaration of omissability indicators -->
&lt;!ENTITY % TEI.XML 'INCLUDE' &gt;
&lt;![%TEI.XML;[
&lt;!ENTITY % om.RO '' &gt;
&lt;!ENTITY % om.RR '' &gt;
]]&gt;
&lt;!ENTITY % om.RO '' &gt;
&lt;!ENTITY % om.RR '' &gt;
<xsl:call-template name="NameList"/>

<xsl:for-each select="key('DefClasses',1)">
  <xsl:choose>
    <xsl:when test="@type='atts'">    
      <xsl:apply-templates select="." mode="processAtts"/>
    </xsl:when>
    <xsl:when test="@type='model'">    
      <xsl:apply-templates select="." mode="processModel"/>
    </xsl:when>
    <xsl:when test="@type='default'">    
      <xsl:apply-templates select="." mode="processDefaultAtts"/>
    </xsl:when>
    <xsl:otherwise>
      <xsl:apply-templates select="." mode="process"/>
    </xsl:otherwise>
  </xsl:choose>
<!--
  &lt;!ENTITY % <xsl:value-of select="@ident"/> ''&gt;
  <xsl:if test="@type='both' or @type='atts'">
  &lt;!ENTITY % <xsl:value-of select="@ident"/>.attributes ''&gt;
  </xsl:if>
-->

</xsl:for-each>
</xsl:if>

      <xsl:apply-templates select="key('ClassModule',@ident)"  mode="tangle"/>

      <xsl:apply-templates select="key('ElementModule',@ident)"  mode="tangle">      
	<xsl:sort select="@ident"/>
      </xsl:apply-templates>
<xsl:if test="@type='core'">
	  <xsl:for-each select="key('AllModules',1)">
	    <xsl:sort select="@ident" order="descending"/>
	    <xsl:if test="not(@type='core')">
&lt;!ENTITY % TEI.<xsl:value-of select="@ident"/> 'IGNORE' &gt;
<xsl:if test="key('DeclModules',concat(@ident,'-decl'))">
&lt;!ENTITY % file.<xsl:value-of select="@ident"/>-decl PUBLIC '-//TEI P5//<xsl:value-of select="tei:altIdent[@type='FPI']"/>//EN' '<xsl:value-of select="@ident"/>-decl.dtd' &gt;
&lt;![%TEI.<xsl:value-of select="@ident"/>;[
%file.<xsl:value-of select="@ident"/>-decl;
]]&gt;
</xsl:if>
	    </xsl:if>
	  </xsl:for-each>
</xsl:if>
<xsl:comment>
Start macro declarations
</xsl:comment>
      <xsl:for-each select="key('MacroModule',@ident)">
	  <xsl:apply-templates select="." mode="tangle"/>
      </xsl:for-each>
<xsl:comment>
End of macro declarations
</xsl:comment>
<!-- set up the modules and their guards -->
<xsl:if test="@type='core'">
	  <xsl:for-each select="key('AllModules',1)">
	    <xsl:sort select="@ident" order="descending"/>
	    <xsl:if test="not(@type='core')">
&lt;![%TEI.<xsl:value-of select="@ident"/>;[
&lt;!ENTITY % file.<xsl:value-of select="@ident"/> PUBLIC '-//TEI P5//<xsl:value-of select="tei:altIdent[@type='FPI']"/>//EN' '<xsl:value-of select="@ident"/>.dtd' &gt;
%file.<xsl:value-of select="@ident"/>;
]]&gt;
	    </xsl:if>
	  </xsl:for-each>
</xsl:if>


    </exsl:document>
  </xsl:for-each>
</xsl:template>

<xsl:template match="tei:macroSpec[@id='TEIGIS' or @xml:id='TEIGIS']" mode="tangle"/>

<xsl:template name="NameList">
  <!-- walk over all the elementSpec elements and make list of 
       elements -->
  <xsl:message> Special case: TEI GIS: make list</xsl:message>
  <xsl:for-each select="//tei:elementSpec">
    <xsl:sort select="@ident"/>
    <xsl:if test="not(starts-with(@ident,'%'))">
      <ident id="{@ident}"/>
      <xsl:text>&lt;!ENTITY % n.</xsl:text>
      <xsl:value-of select="@ident"/>
      <xsl:text> "</xsl:text>
      <xsl:choose>
	<xsl:when test="tei:altIdent">
	  <xsl:value-of select="tei:altIdent"/>
	</xsl:when>
	<xsl:otherwise>
	  <xsl:value-of select="@ident"/>
	</xsl:otherwise>
      </xsl:choose>
      <xsl:text>"&gt;
</xsl:text>
    </xsl:if>
  </xsl:for-each>
</xsl:template>


<xsl:template name="italicize"/>
<xsl:template name="linkStyle"/>
<xsl:template name="makeAnchor"/>
<xsl:template name="makeLink"/>

<xsl:template match="tei:elemDecl" mode="literal">
  <xsl:text> %om.</xsl:text>
  <xsl:value-of select="@omit"/>
  <xsl:text>; </xsl:text>
  <xsl:apply-templates/>
</xsl:template>

<xsl:template match="rng:element[rng:anyName]">
  <xsl:text>ANY</xsl:text>
</xsl:template>

<xsl:template match="rng:zeroOrMore">
  <xsl:call-template name="content">
    <xsl:with-param name="sep" select="','"/>
  </xsl:call-template>    
  <xsl:text>*</xsl:text>      
</xsl:template>

<xsl:template match="rng:oneOrMore">
  <xsl:call-template name="content">
    <xsl:with-param name="sep" select="','"/>
  </xsl:call-template>
  <xsl:text>+</xsl:text>      
</xsl:template>

<xsl:template match="rng:optional">
  <xsl:call-template name="content">
    <xsl:with-param name="sep" select="','"/>
  </xsl:call-template>
  <xsl:text>?</xsl:text>      
</xsl:template>

<xsl:template match="rng:choice">
  <xsl:choose>
    <xsl:when test="rng:value">
      (<xsl:for-each select="rng:value">
      <xsl:value-of select="."/>
      <xsl:if test="following-sibling::rng:value">|</xsl:if>
      </xsl:for-each>)
    </xsl:when>
    <xsl:otherwise>
      <xsl:call-template name="content">
	<xsl:with-param name="sep" select="' | '"/>
      </xsl:call-template>     
    </xsl:otherwise>
  </xsl:choose>
</xsl:template>


<xsl:template match="rng:group">
  <xsl:call-template name="content">
    <xsl:with-param name="sep" select="','"/>
  </xsl:call-template>
</xsl:template>

<xsl:template name="content">
  <xsl:param name="sep"/>
  <xsl:choose>
    <!--      <xsl:when test="count(*|processing-instruction())=1 and rng:element">(</xsl:when>-->
    <xsl:when test="count(rng:*|processing-instruction())=1 and rng:ref">
      <xsl:text>(</xsl:text>
    </xsl:when>
    <xsl:when test="count(rng:*|processing-instruction())=1 and rng:element[@name='when']">
      <xsl:text>(</xsl:text>
    </xsl:when>
    <xsl:when test="count(rng:*|processing-instruction())&gt;1">
      <xsl:text>(</xsl:text>
    </xsl:when>
  </xsl:choose>
  <xsl:for-each select="rng:*|processing-instruction()">
    <xsl:variable name="F">
      <xsl:value-of select="name((following-sibling::rng:*|following-sibling::processing-instruction())[1])"/>
    </xsl:variable>
    <xsl:choose>
      <xsl:when test="self::processing-instruction()">
	<xsl:apply-templates select="."/>
	<xsl:choose>
	  <xsl:when test="$F='teidtd'">
	    <xsl:value-of select="$sep"/>
	  </xsl:when>
	  <xsl:when test=".=')' and not($F='')">
	    <xsl:value-of select="$sep"/>
	  </xsl:when>                
	</xsl:choose>
      </xsl:when>
      <xsl:when test="$F='teidtd'">
	<xsl:apply-templates select="."/>
      </xsl:when>
      <xsl:when test="starts-with($F,'rng:')">
	<xsl:apply-templates select="."/>
	<xsl:value-of select="$sep"/>
      </xsl:when>
      <xsl:otherwise>
	<xsl:apply-templates select="."/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:for-each>
  <xsl:choose>
    <!--      <xsl:when test="count(*|processing-instruction())=1 and rng:element">)</xsl:when>-->
    <xsl:when test="count(rng:*|processing-instruction())=1 and rng:ref">
      <xsl:text>)</xsl:text>
    </xsl:when>
    <xsl:when test="count(rng:*|processing-instruction())=1 and rng:element[@name='when']">
      <xsl:text>)</xsl:text>
    </xsl:when>
    <xsl:when test="count(rng:*|processing-instruction())&gt;1">
      <xsl:text>)</xsl:text>
    </xsl:when>
  </xsl:choose>
</xsl:template>

<xsl:template match="tei:datatype">
  <xsl:choose>
    <xsl:when test="rng:data/@type='ID'">
      <xsl:text> ID</xsl:text>
    </xsl:when>
    
    <xsl:when test="rng:data/@type='IDREF'">
      <xsl:text> IDREF</xsl:text>
    </xsl:when>
    
    <xsl:when test="rng:data/@type='IDREFS'">
      <xsl:text> IDREFS</xsl:text>
    </xsl:when>
    
    <xsl:when test="rng:data/@type='NMTOKEN'">
      <xsl:text> NMTOKEN</xsl:text>
    </xsl:when>
    
    <xsl:when test="rng:data/@type='NMTOKENS'">
      <xsl:text> NMTOKENS</xsl:text>
    </xsl:when>
    
    <xsl:otherwise>
      <xsl:text> CDATA</xsl:text>
    </xsl:otherwise>
  </xsl:choose>
  
</xsl:template>

<xsl:template match="rng:element">
  <xsl:choose>
    <xsl:when test="parent::tei:content/parent::tei:macroSpec">
      <xsl:call-template name="topLevel"/>
    </xsl:when>
    <xsl:otherwise>
      <xsl:if test="$parameterize='true'">(%n.</xsl:if>
      <xsl:value-of select="@name"/>
      <xsl:if test="$parameterize='true'">;)</xsl:if>    
    </xsl:otherwise>
  </xsl:choose>
</xsl:template>

<!--  <xsl:template match="tei:text()"/>-->

<xsl:template match="rng:empty">
  <xsl:text>EMPTY</xsl:text>
</xsl:template>

<xsl:template match="rng:data">
  <xsl:choose>
    <xsl:when test="@type='ID'"> ID </xsl:when>
    <xsl:when test="@type='IDREF'"> IDREF </xsl:when>
    <xsl:when test="@type='IDREFS'"> IDREFS </xsl:when>
    <xsl:otherwise> CDATA </xsl:otherwise>
  </xsl:choose>
</xsl:template>

<xsl:template match="rng:text">
  <xsl:choose>
    <xsl:when test="parent::tei:content/parent::tei:macroSpec">
      <xsl:call-template name="topLevel"/>
    </xsl:when>
    <xsl:when test="parent::tei:content">
      <xsl:text> (#PCDATA)</xsl:text>              
    </xsl:when>
    <xsl:otherwise>
      <xsl:text> #PCDATA</xsl:text>              
    </xsl:otherwise>
  </xsl:choose>
</xsl:template>

<xsl:template match="tei:macroSpec[@type='dt']/tei:content/rng:text">
  <xsl:text> CDATA</xsl:text>              
</xsl:template>

<xsl:template match="tei:macroSpec[@type='epe']/tei:content/rng:text">
  <xsl:text>CDATA</xsl:text>              
</xsl:template>


<xsl:template match="rng:text" mode="simple">
  <xsl:choose>
    <xsl:when test="parent::tei:content/parent::tei:macroSpec[@type='dt']">
      <xsl:text> CDATA</xsl:text>              
    </xsl:when>
    <xsl:when test="parent::tei:content and not(following-sibling::rng:*) and not (preceding-sibling::rng:*)">
      <xsl:text>(#PCDATA)</xsl:text>
    </xsl:when>
    <xsl:when test="parent::tei:content and not(following-sibling::rng:*) and not (preceding-sibling::rng:*)">
      <xsl:text>(#PCDATA)</xsl:text>
    </xsl:when>
    <xsl:otherwise>
      <xsl:text>#PCDATA</xsl:text>      
    </xsl:otherwise>
    
  </xsl:choose>
  
</xsl:template>

<xsl:template match="processing-instruction()[name(.)='teidtd']">
  <xsl:value-of select="."/>
</xsl:template>

<xsl:template match="processing-instruction()[name(.)='teidtd']" mode="tangle">
  <xsl:value-of select="."/>
</xsl:template>

<xsl:template name="topLevel">
  <xsl:choose>
    <xsl:when test="preceding-sibling::rng:*">
      <xsl:choose>
	<xsl:when test="preceding-sibling::processing-instruction()"></xsl:when>
	<xsl:otherwise>
	  <xsl:text>|</xsl:text>                
	</xsl:otherwise>
      </xsl:choose>
      <xsl:apply-templates select="." mode="simple"/>
    </xsl:when>
    <xsl:otherwise>
      <xsl:apply-templates select="."  mode="simple"/>
    </xsl:otherwise>
  </xsl:choose>
</xsl:template>

<xsl:template match="rng:ref">
  
  <xsl:choose>
    <xsl:when test="parent::tei:content/parent::tei:macroSpec">
      <xsl:call-template name="topLevel"/>
    </xsl:when>
    <xsl:when test="contains(@name, '.')">
      <xsl:text>%</xsl:text>
      <xsl:value-of select="@name"/>   
      <xsl:text>;</xsl:text>
    </xsl:when>
    <xsl:otherwise>
      <xsl:value-of select="@name"/>   
    </xsl:otherwise>
  </xsl:choose>
</xsl:template>

<xsl:template match="rng:ref" mode="simple">
  <xsl:if test="not(@name='IGNORE' or @name='INCLUDE')">
    <xsl:text>%</xsl:text>  
  </xsl:if>
  <xsl:value-of select="@name"/>
  <xsl:if test="not(@name='IGNORE' or @name='INCLUDE')">
    <xsl:text>;</xsl:text>  
  </xsl:if>
</xsl:template>

<xsl:template match="rng:element" mode="simple">
  <xsl:if test="$parameterize='true'">(%n.</xsl:if>
  <xsl:value-of select="@name"/>
  <xsl:if test="$parameterize='true'">;)</xsl:if>
</xsl:template>


<xsl:template match="tei:gloss|tei:remarks|tei:desc"/>

<xsl:template match="tei:macroSpec" mode="tangle">
  <xsl:message>     .... macroSpec <xsl:value-of
  select="@ident"/></xsl:message>
<xsl:if test="@depend">
<xsl:message>DEpendency on <xsl:value-of select="@depend"/> for <xsl:value-of select="@ident"/></xsl:message>
&lt;![%TEI.<xsl:value-of select="@depend"/>;[
</xsl:if>
  <xsl:text>
&lt;!ENTITY </xsl:text>   
  <xsl:if test="@type='defaultpe' or @type='pe' or @type='epe' or @type='dt'">
    <xsl:text>%</xsl:text>
  </xsl:if>
  <xsl:text> </xsl:text>
  <xsl:value-of select="@ident"/>
  <xsl:text> </xsl:text>
  <xsl:text>'</xsl:text>
  <xsl:for-each select="tei:content">
    <xsl:apply-templates
     select="tei:*|rng:*|processing-instruction()"/>
  </xsl:for-each>
  <xsl:text>'</xsl:text>
  <xsl:text> &gt;</xsl:text>
  
<xsl:if test="@depend">
]]&gt;
</xsl:if>
<xsl:call-template name="CR"/>
  
</xsl:template>

<xsl:template match="tei:elementSpec" mode="tangle">
  <xsl:message>     .... elementSpec <xsl:value-of select="@ident"/></xsl:message>
  <xsl:if test="not(starts-with(@ident,'%'))">
    <xsl:text>
&lt;!ENTITY % </xsl:text>
    <xsl:value-of select="@ident"/> 'INCLUDE' &gt;
&lt;![ %<xsl:value-of select="@ident"/>; [
  </xsl:if>
  <xsl:text>
&lt;!--doc:</xsl:text>
<xsl:value-of select="tei:desc"/>
<xsl:text> --&gt;
&lt;!ELEMENT </xsl:text>
  <xsl:if test="not(starts-with(@ident,'%')) ">%n.</xsl:if>
  <xsl:value-of select="@ident"/>    
  <xsl:if test="not(starts-with(@ident,'%')) ">;</xsl:if> 
  <xsl:text> %om.RR; </xsl:text>
  <xsl:apply-templates select="tei:content/rng:*" />
  <xsl:text>&gt; </xsl:text>
  <xsl:text>
&lt;!ATTLIST </xsl:text>
  <xsl:if test="not(starts-with(@ident,'%')) ">%n.</xsl:if> 
  <xsl:value-of select="@ident"/>
  <xsl:if test="not(starts-with(@ident,'%')) ">;</xsl:if> 
  <xsl:text>
  %tei.global.attributes;</xsl:text>
  <xsl:apply-templates select="tei:classes/tei:memberOf" mode="tangleAtts"/>
  <xsl:call-template name="attributeList"/>
  <xsl:if test="not(starts-with(@ident,'%'))">
    TEIform CDATA &#39;<xsl:value-of select="@ident"/><xsl:text>&#39; </xsl:text>
  </xsl:if>
  <xsl:text> &gt;</xsl:text>
  <xsl:if test="not(starts-with(@ident,'%'))">
]]&gt;  </xsl:if>
</xsl:template>


<xsl:template name="attclasses">
  <xsl:for-each select="tei:classes/tei:memberOf">
    <xsl:for-each select="key('IDENTS',@key)[1]">
      <xsl:if test="@type='atts'">
	%<xsl:value-of select="@ident"/>
	<xsl:text>.attributes;</xsl:text>
      </xsl:if>
    </xsl:for-each>
  </xsl:for-each>
</xsl:template>

<xsl:template name="classAtt">
  <xsl:message>    ....  <xsl:value-of select="@ident"/>.attributes</xsl:message>  
  <xsl:variable name="thisclass">
    <xsl:value-of select="@ident"/>   
  </xsl:variable>
  <xsl:text>
&lt;!ENTITY % </xsl:text>
  <xsl:value-of select="$thisclass"/>
  <xsl:text>.attributes &#39;</xsl:text>
  <xsl:call-template name="attclasses"/>
  <xsl:call-template name="attributeList"/>
  <xsl:text>&#39;&gt; </xsl:text>
</xsl:template>

<xsl:template match="tei:classSpec" mode="tagatts">
  <xsl:if test="@type='atts' or @type='both'">
    <xsl:message>      .... added contents of [%<xsl:value-of select="@ident"/>.attributes;]</xsl:message>
    
<xsl:call-template name="CR"/>
%<xsl:value-of select="@ident"/>
      <xsl:text>.attributes;</xsl:text>
  </xsl:if>
</xsl:template>


<xsl:template match="tei:moduleRef" mode="tangle">
  <xsl:message>  moduleRef to <xsl:value-of select="@key"/> </xsl:message>
  <xsl:text>
&lt;!ENTITY </xsl:text>   
  <xsl:text>%</xsl:text>
  <xsl:text> file.</xsl:text>
  <xsl:value-of select="@key"/>
  <xsl:text> </xsl:text>
  <xsl:text>PUBLIC</xsl:text>
  <xsl:text> '-//TEI P5//</xsl:text>
  <xsl:value-of select="key('FILES',@key)/tei:altIdent[@type='FPI']"/>
  <xsl:text>//EN' '</xsl:text>
  <xsl:value-of select="@key"/>
  <xsl:text>.dtd'</xsl:text>
  <xsl:text> &gt;
</xsl:text>
  
  <xsl:text>%file.</xsl:text>
  <xsl:value-of select="@key"/>
  <xsl:text>;
</xsl:text>
</xsl:template>


<xsl:template match="tei:classSpec" mode="tangle">
  <xsl:message>    .. classSpec <xsl:value-of select="@id|@xml:id"/>,<xsl:value-of select="@type"/>  </xsl:message>
  <xsl:choose>
    <xsl:when test="@type='atts'">
      <xsl:call-template name="classAtt"/>
    </xsl:when>
    
    <xsl:when test="@type='model'">
      <xsl:call-template name="classModel"/>
    </xsl:when>
    
    <xsl:when test="@type='both'">
      <xsl:call-template name="classAtt"/>
    </xsl:when>
  </xsl:choose>
</xsl:template>

<xsl:template match="tei:classSpec" mode="process">
  <xsl:choose>    
    <xsl:when test="@type='atts'">
      <xsl:call-template name="classAtt"/>
    </xsl:when>
    
    <xsl:when test="@type='model'">
      <xsl:call-template name="classModel"/>
    </xsl:when>
    
    <xsl:when test="@type='both'">
      <xsl:call-template name="classModel"/>
      <!--    <xsl:call-template name="classAtt"/>-->
    </xsl:when>
  </xsl:choose>
</xsl:template>



<xsl:template match="tei:classSpec" mode="processModel">
  <xsl:call-template name="classModel"/>
</xsl:template>

<xsl:template match="tei:classSpec" mode="processAtts">
  <xsl:call-template name="classAtt"/>
</xsl:template>


<xsl:template name="classModel">
  <xsl:message>    ....model <xsl:value-of select="@ident"/></xsl:message>
  <xsl:variable name="thisclass">
    <xsl:value-of select="@ident"/>   
  </xsl:variable>
  <xsl:text>
&lt;!ENTITY % x.</xsl:text><xsl:value-of select="$thisclass"/> "" &gt;<xsl:text>
&lt;!ENTITY % </xsl:text>
    <xsl:value-of select="$thisclass"/>
    <xsl:text> "%x.</xsl:text>
    <xsl:value-of select="$thisclass"/>
    <xsl:text>; </xsl:text>
    <xsl:variable name="members">
      <M>
	<xsl:for-each select="key('CLASSMEMBERS',@ident)">
	  <xsl:sort select="@ident"/>
	  <N>
	    <xsl:choose>
	      <xsl:when test="self::tei:elementSpec">
		<xsl:text>%n.</xsl:text><xsl:value-of select="@ident"/><xsl:text>;</xsl:text> 
	      </xsl:when>
	      <xsl:otherwise>
		<xsl:text>%</xsl:text><xsl:value-of select="@ident"/><xsl:text>;</xsl:text> 
	      </xsl:otherwise>
	    </xsl:choose>
	  </N>
	</xsl:for-each>
      </M>
    </xsl:variable>
    <xsl:for-each select="exsl:node-set($members)/M/N">
      <xsl:value-of select="."/>
      <xsl:if test="position() &lt; last()"> | </xsl:if>
    </xsl:for-each>
    <xsl:text>"&gt; </xsl:text>
    
</xsl:template>


<xsl:template match="tei:commDecl" mode="tangle">
  <xsl:text>
  &lt;!--</xsl:text>
  <xsl:apply-templates/>
  <xsl:text>--&gt;</xsl:text>        
</xsl:template>

<xsl:template match="tei:classSpec" mode="processDefaultAtts">
  <xsl:text>
&lt;!ENTITY % </xsl:text><xsl:value-of select="@ident"/>
    <xsl:text>.attributes &#39;&#39;&gt;</xsl:text>
</xsl:template>

<xsl:template name="attributeList">
  <xsl:for-each select="tei:attList//tei:attDef" >
    
<xsl:call-template name="CR"/>
<xsl:text>   </xsl:text>
    <xsl:value-of select="@ident"/>
    <xsl:text> </xsl:text>
    <xsl:choose>
      <xsl:when test="tei:datatype/rng:*">
	<xsl:apply-templates select="tei:datatype"/>
      </xsl:when>
      <xsl:when test="tei:valList[@type='closed']">
	<xsl:text> (</xsl:text>
	<xsl:for-each select="tei:valList/tei:valItem">
	  <xsl:value-of select="@ident"/>
	  <xsl:if test="following-sibling::tei:valItem">|</xsl:if> 
	</xsl:for-each>
	<xsl:text>)</xsl:text>
      </xsl:when>
      <xsl:otherwise>
	<xsl:text> CDATA </xsl:text>
      </xsl:otherwise>
    </xsl:choose>
    <xsl:choose>
      <xsl:when test="string-length(tei:defaultVal) &gt;0">
	<xsl:text> &#34;</xsl:text>
	<xsl:value-of select="tei:defaultVal"/>
	<xsl:text>&#34; </xsl:text>
      </xsl:when>
      <xsl:when test="@usage='req'">
	<xsl:text> #REQUIRED</xsl:text>        
      </xsl:when>
      <xsl:otherwise>
	<xsl:text> #IMPLIED</xsl:text>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:for-each>
</xsl:template>

<xsl:template name="bitOut">
  <xsl:param name="grammar"/>
  <xsl:param name="TAG"/>
  <xsl:param name="content"/>
  <xsl:copy-of select="$content"/>
</xsl:template>

<xsl:template name="CR">
<xsl:text>
</xsl:text>
</xsl:template>

</xsl:stylesheet>
