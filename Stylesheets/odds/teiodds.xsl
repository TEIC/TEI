<?xml version="1.0" encoding="utf-8"?>
<!-- $Date: 
Text Encoding Initiative Consortium XSLT stylesheet family
2001/10/01 $, $Revision$, $Author$

##LICENSE

-->
<xsl:stylesheet 
    xmlns:s="http://www.ascc.net/xml/schematron" 
    xmlns:a="http://relaxng.org/ns/compatibility/annotations/1.0"
    xmlns:xs="http://www.w3.org/2001/XMLSchema" 
    xmlns:rng="http://relaxng.org/ns/structure/1.0"
    xmlns:teix="http://www.tei-c.org/ns/Examples"
    xmlns:tei="http://www.tei-c.org/ns/1.0"
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    xmlns:fo="http://www.w3.org/1999/XSL/Format"
    xmlns:edate="http://exslt.org/dates-and-times"
    xmlns:exsl="http://exslt.org/common"
    xmlns:estr="http://exslt.org/strings"
    exclude-result-prefixes="exsl estr edate teix fo a tei s xs" 
    extension-element-prefixes="edate exsl estr"
    version="1.0">
  <xsl:include href="RngToRnc.xsl"/>
  <xsl:param name="TEISERVER">http://tei.oucs.ox.ac.uk/TEI/Roma/xquery/</xsl:param>
  <xsl:param name="verbose"></xsl:param>
  <xsl:param name="schemaBaseURL">http://www.tei-c.org/P5/Schema/</xsl:param>
  
  <xsl:key  name="CLASSMEMBERS" match="tei:elementSpec|tei:classSpec" use="tei:classes/tei:memberOf/@key"/>
  <xsl:key name="TAGS" match="Tag|Pattern|Class" use="ident"/>
  <xsl:key name="IDENTS"   match="tei:elementSpec|tei:classSpec|tei:macroSpec"   use="@ident"/>
  <xsl:key name="TAGIDS"     match="*[@id|@xml:id]"           use="@id|@xml:id"/>
  <xsl:key name="TAGIDENTS"     match="Table/*[ident]"           use="ident"/>
  <xsl:key name="IDS"     match="tei:*[@id|@xml:id]"  use="@id|@xml:id"/>
  <xsl:key name="PATTERNS" match="tei:macroSpec" use="@ident"/>
  <xsl:key name="MACRODOCS" match="tei:macroSpec" use='1'/>
  <xsl:key name="CLASSDOCS" match="tei:classSpec" use='1'/>
  <xsl:key name="TAGDOCS" match="tei:elementSpec" use='1'/>
  <xsl:key name='NameToID' match="tei:*" use="@ident"/>
  <xsl:key name="ElementModule" match="tei:elementSpec" use="@module"/>
  <xsl:key name="ClassModule"   match="tei:classSpec" use="@module"/>
  <xsl:key name="MacroModule"   match="tei:macroSpec" use="@module"/>
  <xsl:key name="DeclModules"   match="tei:moduleSpec[@type='decls']"	 use="@ident"/>
  <xsl:key name="AllModules"   match="tei:moduleSpec[not(@type='decls')]" use="1"/>
  <xsl:key name="DefClasses"   match="tei:classSpec[@predeclare='true']" use="1"/>
  
  <!-- lookup table of element contents, and templates to access the result -->
  <xsl:key name="ELEMENTPARENTS" match="Contains" use="."/>
  <xsl:key name="ELEMENTS" match="Element" use="@id|@xml:id"/>
  <xsl:param name="wrapLength">65</xsl:param>
  
  
  
  <xsl:template match="processing-instruction()">
    <xsl:if test="name(.) = 'odds'">
      <xsl:choose>
	<xsl:when test=".='date'">
	  This formatted version of the Guidelines was 
	  created on <xsl:value-of select="edate:date-time()"/>.
	</xsl:when>
      </xsl:choose>
    </xsl:if>
  </xsl:template>
  
  
  
  <xsl:template match="*" mode="literal">
    <xsl:text>
    </xsl:text>
    <xsl:for-each select="ancestor::rng:*">
      <xsl:text> </xsl:text>
    </xsl:for-each>
    <xsl:text>&lt;</xsl:text>
    <xsl:value-of select="local-name(.)"/>
    <xsl:for-each select="@*">
      <xsl:text> </xsl:text>
    <xsl:value-of select="local-name(.)"/>="<xsl:value-of select="."/>"</xsl:for-each>
    <xsl:choose>
      <xsl:when test="child::node()">
	<xsl:text>&gt;</xsl:text>
	<xsl:apply-templates mode="literal"/>
	<xsl:if test="node()[last()]/self::rng:*"> 
	  <xsl:text>
	  </xsl:text>
	</xsl:if>
	<xsl:for-each select="ancestor::rng:*">
	  <xsl:text> </xsl:text>
	</xsl:for-each>
	<xsl:text>&lt;/</xsl:text>
	<xsl:value-of select="local-name(.)"/>
	<xsl:text>&gt;</xsl:text>
      </xsl:when>    
      <xsl:otherwise>
	<xsl:text>/&gt;</xsl:text>
	<xsl:if test="node()[last()]/self::rng:*"> 
	  <xsl:text>
	  </xsl:text>
	</xsl:if>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  
  
  
  <xsl:template match="rng:*">
    <xsl:copy>
      <xsl:copy-of select="@*"/>
      <xsl:apply-templates select="rng:*|tei:*|text()|comment()"/>
    </xsl:copy>
  </xsl:template>
  
  
  
  <xsl:template match="tei:*" mode="tangle"/>
  
  
  <xsl:template match="tei:att">
    <xsl:call-template name="italicize">
      <xsl:with-param name="text">
	<xsl:apply-templates/>
      </xsl:with-param>
    </xsl:call-template>
  </xsl:template>
  
  
  <xsl:template match="tei:attDef[@mode='delete']" mode="tangle"/>
  
  <xsl:template match="tei:attDef" mode="tangle">
    <xsl:variable name="I">
      <xsl:choose>
	<xsl:when test="starts-with(@ident,'xml:')">
	  <xsl:text>xml</xsl:text>
	  <xsl:value-of select="substring-after(@ident,'xml:')"/>
	</xsl:when>
	<xsl:otherwise>
	  <xsl:value-of select="@ident"/>
	</xsl:otherwise>
      </xsl:choose>
    </xsl:variable>
    <xsl:if test="not(@ident='xmlns')">
      <xsl:choose>
	<xsl:when test="@mode='add'"/>
	<xsl:when test="@usage='req'">
	  <rng:ref name="{ancestor::tei:attList/../@ident}.attributes.{$I}"/>
	</xsl:when>
	<xsl:when test="ancestor::tei:classSpec">
	  <rng:ref name="{ancestor::tei:attList/../@ident}.attributes.{$I}"/>
	</xsl:when>
	<xsl:otherwise>
	  <rng:ref
	      name="{ancestor::tei:attList/../@ident}.attributes.{$I}"/>
	  <!-- when is this ever needed?	
	       <rng:optional>
	       </rng:optional>
	  -->
	</xsl:otherwise>
      </xsl:choose>
    </xsl:if>
  </xsl:template>
  
  <xsl:template match="tei:attDef[@mode='change']" mode="tangle"/>
  
  <xsl:template match="tei:attList" mode="tangle">
    <xsl:choose>
      <xsl:when test="@org='choice'">
	<rng:optional >
	  <rng:choice >
	    <xsl:apply-templates select="tei:*" mode="tangle"/>
	  </rng:choice>
	</rng:optional>
      </xsl:when>
      <xsl:when test="parent::tei:elementSpec[@mode='change']">
	<xsl:variable name="loc">
	  <xsl:value-of select="$TEISERVER"/>
	  <xsl:text>copytag.xq?name=</xsl:text>
	  <xsl:value-of select="parent::tei:elementSpec/@ident"/>
	</xsl:variable>
	<xsl:if test="$verbose">
	  <xsl:message>Accessing TEISERVER: <xsl:value-of
	  select="$loc"/></xsl:message>
	</xsl:if>
	<xsl:for-each
	    select="document($loc)//tei:attDef[not(@ident=current()//tei:attDef/@ident)]">
	  <xsl:if test="not(@ident='xmlns')">
	    <xsl:variable name="I">
	      <xsl:choose>
		<xsl:when test="starts-with(@ident,'xml:')">
		  <xsl:text>xml</xsl:text>
		  <xsl:value-of select="substring-after(@ident,'xml:')"/>
		</xsl:when>
		<xsl:otherwise>
		  <xsl:value-of select="@ident"/>
		</xsl:otherwise>
	      </xsl:choose>
	    </xsl:variable>
	    <xsl:choose>
	      <xsl:when test="@usage='req'">
		<rng:ref name="{ancestor::tei:elementSpec/@ident}.attributes.{$I}"/>
	      </xsl:when>
	      <xsl:otherwise>
		<rng:optional>
		  <rng:ref
		      name="{ancestor::tei:elementSpec/@ident}.attributes.{$I}"/>
		</rng:optional>
	      </xsl:otherwise>
	    </xsl:choose>
	  </xsl:if>
	  </xsl:for-each>
	  <xsl:apply-templates select="tei:*" mode="tangle"/>
      </xsl:when>
      <xsl:otherwise>
	<xsl:apply-templates select="tei:*" mode="tangle"/>
      </xsl:otherwise>
    </xsl:choose>
    
  </xsl:template>
  
  
  
  <xsl:template match="tei:author">
    <xsl:apply-templates/>,
  </xsl:template>
  
  
  
  <xsl:template match="tei:classSpec" mode="processAtts">
    <xsl:if test="$verbose='true'">
      <xsl:message>    .... class attributes <xsl:value-of select="@ident"/></xsl:message>  
    </xsl:if>
    <xsl:variable name="thisClass">
      <xsl:value-of select="@ident"/>   
    </xsl:variable>
    <xsl:variable name="attclasscontent">
      <rng:x >
	<xsl:for-each select="tei:classes/tei:memberOf">
	  <xsl:for-each select="key('IDENTS',@key)[1]">
	    <xsl:if test="tei:attList">
	      <xsl:if test="$verbose='true'">
		<xsl:message>          ..... add link to attributes from  class [<xsl:value-of 
		select="@ident"/>]</xsl:message>
	      </xsl:if>
	      <rng:ref name="{@ident}.attributes" />
	    </xsl:if>
	  </xsl:for-each>
	</xsl:for-each>
	<xsl:apply-templates select="tei:attList" mode="tangle" />
      </rng:x>
    </xsl:variable>
    <xsl:call-template name="bitOut">
      <xsl:with-param name="grammar">true</xsl:with-param>
      <xsl:with-param name="content">
	<Wrapper>
	  <rng:define name="{$thisClass}.attributes" combine="choice" >
	    <xsl:for-each select="exsl:node-set($attclasscontent)/rng:x">
	      <xsl:choose>
		<xsl:when test="rng:*">
		  <xsl:copy-of select="rng:*"/>
		</xsl:when>
		<xsl:otherwise>
		  <rng:empty />
		</xsl:otherwise>
	      </xsl:choose>
	    </xsl:for-each>
	  </rng:define>
	  <xsl:call-template name="defineRelaxAttributes"/>
	</Wrapper>
      </xsl:with-param>
    </xsl:call-template>
  </xsl:template>
  
  
  
  <xsl:template match="tei:classSpec" mode="processDefaultAtts">
    <xsl:if test="$verbose='true'">
      <xsl:message>    .. default attribute settings for <xsl:value-of
      select="@ident"/></xsl:message>
    </xsl:if>
    <xsl:call-template name="bitOut">
      <xsl:with-param name="grammar">true</xsl:with-param>
      <xsl:with-param name="content">
	<Wrapper>
	  <rng:define name="{@ident}.attributes" combine="choice" >
	    <rng:empty />
	  </rng:define>
	</Wrapper>
      </xsl:with-param>
    </xsl:call-template>
  </xsl:template>
  
  
  
  <xsl:template match="tei:classSpec" mode="processModel">
    <xsl:if test="$verbose='true'">
      <xsl:message>    .... class model <xsl:value-of  select="@ident"/></xsl:message>
    </xsl:if>
    
    <xsl:variable name="thisClass">
      <xsl:value-of select="@ident"/>   
    </xsl:variable>
    <xsl:call-template name="bitOut">
      <xsl:with-param name="grammar">true</xsl:with-param>
      <xsl:with-param name="content">
	<Wrapper>
	  <rng:define name="{$thisClass}" combine="choice" >
	    <rng:notAllowed />
	  </rng:define>
	  <xsl:apply-templates select="tei:classes/tei:memberOf" mode="tangleModel"/>
	</Wrapper>
      </xsl:with-param>
    </xsl:call-template>
  </xsl:template>
  
  
  
  <xsl:template match="tei:classSpec" mode="tangle">
    <xsl:param name="type"/>
    <xsl:if test="$verbose='true'">
      <xsl:message>      .. process classSpec <xsl:value-of
      select="@ident"/> (type <xsl:value-of select="@type"/>)
      </xsl:message>
    </xsl:if>
    <xsl:apply-templates select="." mode="processModel"/>
    <xsl:apply-templates select="." mode="processAtts"/>
  </xsl:template>
  
  
  
  <xsl:template match="tei:classSpec" mode="tangleadd">
    <xsl:apply-templates mode="tangleadd"/>
  </xsl:template>
  
  
  
  <xsl:template match="tei:classSpec/@ident"/>
  
  
  <xsl:template match="tei:classSpec/tei:attList" mode="tangleadd">
    <xsl:for-each select="tei:attDef[@mode='add']">
      <xsl:call-template name="defineAnAttribute">
	<xsl:with-param name="Name" select="../@ident"/>
      </xsl:call-template>
    </xsl:for-each>
  </xsl:template>
  
  
  <xsl:template match="tei:classSpec[@mode='change']" mode="tangle">
    <xsl:if test="$verbose='true'">
      <xsl:message>      .. process (change mode) classSpec <xsl:value-of
      select="@ident"/> (type <xsl:value-of select="@type"/>)
      </xsl:message>
    </xsl:if>
    <xsl:if test="tei:content">
      <rng:define name="{@ident}" combine="choice">
	<xsl:apply-templates select="tei:content/*"/>
      </rng:define>
    </xsl:if>
    <xsl:if test="tei:attList/tei:attDef[@mode='add']">
      <rng:define name="{@ident}.attributes" combine="choice" >
	<xsl:for-each select="tei:attList/tei:attDef[@mode='add']">
	  <xsl:variable name="I">
	    <xsl:choose>
	      <xsl:when test="starts-with(@ident,'xml:')">
		<xsl:text>xml</xsl:text>
		<xsl:value-of select="substring-after(@ident,'xml:')"/>
	      </xsl:when>
	      <xsl:otherwise>
		<xsl:value-of select="@ident"/>
	      </xsl:otherwise>
	    </xsl:choose>
	  </xsl:variable>
	  <rng:ref name="{ancestor::tei:classSpec/@ident}.attributes.{$I}"/>
	</xsl:for-each>
      </rng:define>
    </xsl:if>
    <xsl:call-template name="defineRelaxAttributes"/>
  </xsl:template>
  

  <xsl:template match="tei:classSpec[@mode='replace']" mode="tangle">
    <xsl:if test="$verbose='true'">
      <xsl:message>      .. process (replace mode) classSpec <xsl:value-of
      select="@ident"/> (type <xsl:value-of select="@type"/>)
      </xsl:message>
    </xsl:if>
      <rng:define name="{@ident}.attributes">
	<xsl:for-each select="tei:attList/tei:attDef">
	  <xsl:variable name="I">
	    <xsl:choose>
	      <xsl:when test="starts-with(@ident,'xml:')">
		<xsl:text>xml</xsl:text>
		<xsl:value-of select="substring-after(@ident,'xml:')"/>
	      </xsl:when>
	      <xsl:otherwise>
		<xsl:value-of select="@ident"/>
	      </xsl:otherwise>
	    </xsl:choose>
	  </xsl:variable>
	  <rng:ref name="{ancestor::tei:classSpec/@ident}.attributes.{$I}"/>
	</xsl:for-each>
      </rng:define>
    <xsl:call-template name="defineRelaxAttributes"/>
  </xsl:template>
  

  <xsl:template match="tei:classSpec|tei:elementSpec|tei:macroSpec"
		mode="weave">     
    <xsl:call-template name="refdoc"/>
  </xsl:template>
  
  
  <xsl:template match="tei:code">
    <xsl:call-template name="typewriter">
      <xsl:with-param name="text">
	<xsl:apply-templates/>
      </xsl:with-param>
    </xsl:call-template>
  </xsl:template>
  
  
  
  <xsl:template match="tei:desc" mode="show">
    <xsl:apply-templates select="preceding-sibling::tei:gloss"
			 mode="show"/>
    <xsl:apply-templates/>
  </xsl:template>
  
  
  
  <xsl:template match="tei:desc"/>
  
  
  
  <xsl:template match="tei:divGen[@type='classcat']">
    <xsl:apply-templates select="key('CLASSDOCS',1)" mode="weave">
      <xsl:sort select="@ident"/>
    </xsl:apply-templates>
  </xsl:template>
  
  
  
  <xsl:template match="tei:divGen[@type='macrocat']">
    <xsl:apply-templates select="key('MACRODOCS',1)"  mode="weave">
      <xsl:sort select="@ident"/>
    </xsl:apply-templates>
  </xsl:template>
  
  
  
  <xsl:template match="tei:divGen[@type='tagcat']">
    <xsl:apply-templates select="key('TAGDOCS',1)"  mode="weave">
      <xsl:sort select="@ident"/>
    </xsl:apply-templates>
  </xsl:template>
  
  
  
  <xsl:template match="tei:editor">
    <xsl:apply-templates/>:
  </xsl:template>
  
  
  
  <xsl:template match="tei:elementSpec[@mode='delete']" mode="tangle">
    <rng:define name="{@ident}">
      <rng:notAllowed />
    </rng:define>
  </xsl:template>
  
  <xsl:template match="tei:elementSpec" mode="tangle">
    <xsl:if test="$verbose='true'">
      <xsl:message> elementSpec <xsl:value-of
      select="@ident"/>
      <xsl:if test="@id|@xml:id">: <xsl:value-of select="@id|@xml:id"/></xsl:if>
      <xsl:if test="@mode"> (mode <xsl:value-of select="@mode"/>)</xsl:if>
      </xsl:message>
    </xsl:if>
    <xsl:call-template name="bitOut">
      <xsl:with-param name="grammar"></xsl:with-param>
      <xsl:with-param name="content">
	<Wrapper>
	  <xsl:variable name="name">
	    <xsl:choose>
	      <xsl:when test="tei:altIdent">
		<xsl:value-of select="normalize-space(tei:altIdent)"/>
	      </xsl:when>
	      <xsl:otherwise>
		<xsl:value-of select="@ident"/>
	      </xsl:otherwise>
	    </xsl:choose>
	  </xsl:variable>
	  <xsl:choose>
	    <xsl:when test="tei:content/rng:notAllowed">
	      <rng:define name="{@ident}" >
		<rng:notAllowed />
	      </rng:define>
	    </xsl:when>
	    <xsl:otherwise>
	      <rng:define  name="{@ident}" >
		<rng:element  name="{$name}" >
		  <xsl:if test="@ns">
		    <xsl:attribute name="ns"><xsl:value-of select="@ns"/></xsl:attribute>
		  </xsl:if>
		    <a:documentation>
		      <xsl:value-of select="tei:desc"/>
		    </a:documentation>
		  <rng:ref name="{@ident}.content" />
		  <rng:ref name="{@ident}.attributes" />
		</rng:element>
	      </rng:define>
	      
	      <xsl:if test="tei:content or not(@mode='change')">
		<xsl:call-template name="defineContent"/>
	      </xsl:if>
<!-- we do not make a defintion for the attributes pattern if we are
in change mode and there is no attList -->	      
	      <xsl:choose>
		<xsl:when test="@mode='change' and not(tei:attList)">
		</xsl:when>
		<xsl:otherwise>
		  <xsl:call-template name="doDefineAttributePattern"/>
		</xsl:otherwise>
	      </xsl:choose>

	      <xsl:if test="not(@mode='change')">
		<xsl:call-template name="defineRelaxAttributes"/>
	      </xsl:if>
	      
	      <xsl:apply-templates select="tei:classes/tei:memberOf" mode="tangleModel"/>
	      
	    </xsl:otherwise>
	  </xsl:choose>      
	</Wrapper>
      </xsl:with-param>
    </xsl:call-template>
  </xsl:template>
  
  <xsl:template name="doDefineAttributePattern">
    <rng:define name="{@ident}.attributes" >
      <xsl:choose>
	<xsl:when test="ancestor::tei:schemaSpec and not(ancestor::tei:schemaSpec/tei:moduleRef/@key='tei')"/>
<!--
	<xsl:when test="@ident='egXML'">
	  <rng:ref name="tei.global.attributes" />
	</xsl:when>
-->
	<xsl:when test="@ns and
			not(contains(@ns,'http://www.tei-c.org/ns/1.0'))"/>
	<xsl:otherwise>
	  <rng:ref name="tei.global.attributes" />
	</xsl:otherwise>
      </xsl:choose>
      
      <xsl:if test="tei:attList//tei:attDef or not(@mode='change')">
	<xsl:choose>
	  <xsl:when test="tei:classes or not(@mode='change')">
	    <xsl:apply-templates
		select="tei:classes/tei:memberOf"
		mode="processClassAtts">
	      <xsl:with-param name="homeIdent" select="@ident"/>
	    </xsl:apply-templates>
	  </xsl:when>
	  <xsl:otherwise>
	    <xsl:variable name="loc">
	      <xsl:value-of select="$TEISERVER"/>
	      <xsl:text>copytag.xq?name=</xsl:text>
	      <xsl:value-of select="@ident"/>
	    </xsl:variable>
	    <xsl:if test="$verbose">
	      <xsl:message>Accessing TEISERVER: <xsl:value-of
	      select="$loc"/></xsl:message>
	    </xsl:if>
	    
	    <xsl:variable name="homeIdent" select="@ident"/>
	    <xsl:for-each select="document($loc)/tei:TEI/*">
	      <xsl:apply-templates
		  select=".//tei:classes/tei:memberOf"
		  mode="processClassAtts">
		<xsl:with-param name="homeIdent" select="$homeIdent"/>
	      </xsl:apply-templates>
	    </xsl:for-each>
	  </xsl:otherwise>
	</xsl:choose>
	<xsl:apply-templates select="tei:attList" mode="tangle"/>
	<xsl:if test="not(@ns) or contains(@ns,'http://www.tei-c.org')">
	  <rng:optional >
	    <rng:attribute name="TEIform" a:defaultValue="{@ident}" >
	      <rng:text />
	    </rng:attribute>
	  </rng:optional>
	</xsl:if>
	<xsl:if test="@mode='change'">
	  <xsl:call-template name="makeRelaxAttributes"/>
	</xsl:if>
      </xsl:if>
      <!-- place holder to make sure something gets into the
	   pattern -->
      <rng:empty/>
      
    </rng:define>
  </xsl:template>

  <xsl:template name="defineContent">
    <rng:define name="{@ident}.content" >
      <xsl:choose>
	<xsl:when test="tei:valList[@type='closed']">
	  <rng:choice >
	    <xsl:choose>
	      <!-- what to do when a new item is being added to a valList -->
	      <xsl:when test="ancestor::tei:elementSpec/@mode='change'">
		<xsl:for-each select="tei:valList/tei:valItem">
		  <rng:value ><xsl:value-of select="@ident"/></rng:value>
		  <a:documentation><xsl:value-of select="tei:gloss"/></a:documentation>
		</xsl:for-each>
	      </xsl:when>
	      <xsl:otherwise>
		<xsl:for-each select="tei:valList/tei:valItem">  
		  <rng:value ><xsl:value-of select="@ident"/></rng:value>
		  <a:documentation><xsl:value-of select="tei:gloss"/></a:documentation>
		</xsl:for-each>
	      </xsl:otherwise>
	    </xsl:choose>
	  </rng:choice>
	</xsl:when>
	<xsl:when test="tei:content">
	  <xsl:apply-templates select="tei:content/*"/>
	</xsl:when>
	<xsl:otherwise>
	  <rng:empty />
	</xsl:otherwise>
      </xsl:choose>
    </rng:define>
  </xsl:template>
  
  <xsl:template match="tei:memberOf" mode="processClassAtts">
    <xsl:param name="homeIdent"/>
    <xsl:choose>
      <xsl:when  test="key('IDENTS',@key)">
	<xsl:for-each select="key('IDENTS',@key)[1]">
	  <xsl:apply-templates  select="." mode="processClassAtts">
	    <xsl:with-param name="homeIdent"  select="$homeIdent"/>
	  </xsl:apply-templates>
	</xsl:for-each>
      </xsl:when>
      <xsl:otherwise>
	<xsl:if test="$verbose='true'">
	  <xsl:message>looking at class atts for <xsl:value-of
	  select="@key"/></xsl:message>
	</xsl:if>
	<xsl:variable name="loc">
	  <xsl:value-of select="$TEISERVER"/>
	  <xsl:text>copytag.xq?name=</xsl:text>
	  <xsl:value-of select="@key"/>
	</xsl:variable>
	<xsl:if test="$verbose">
	  <xsl:message>Accessing TEISERVER: <xsl:value-of
	  select="$loc"/></xsl:message>
	</xsl:if>
	
	<xsl:apply-templates select="document($loc)/tei:TEI/*" mode="processClassAtts">
	  <xsl:with-param name="homeIdent"  select="$homeIdent"/>
	</xsl:apply-templates>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  
  <xsl:template match="tei:classSpec" mode="processClassAtts">
    <xsl:param name="homeIdent"/>
    <xsl:choose>
      <xsl:when test=".//tei:attDef[@ident=$homeIdent]">
	<xsl:if test="$verbose='true'">
	  <xsl:message>copy of attributes from <xsl:value-of
	  select="@ident"/> because of <xsl:value-of
	  select="$homeIdent"/></xsl:message>
	</xsl:if>
	<xsl:for-each select=".//tei:attList">
	  <xsl:choose>
	    <xsl:when test="@org='choice'">
	      <rng:optional >
		<rng:choice >
		  <xsl:for-each select="./tei:attDef">
		    <xsl:if test="not(@ident='xmlns') and not(@ident=$homeIdent)">
		      <xsl:call-template name="makeAnAttribute"/>
		    </xsl:if>
		  </xsl:for-each>
		</rng:choice>
	      </rng:optional>
	    </xsl:when>
	    <xsl:otherwise>
	      <xsl:for-each select="./tei:attDef">
		<xsl:if test="not(@ident='xmlns')  and not(@ident=$homeIdent)">
		  <xsl:call-template name="makeAnAttribute"/>
		</xsl:if>
	      </xsl:for-each>
	    </xsl:otherwise>
	  </xsl:choose>
	</xsl:for-each>
      </xsl:when>
      <xsl:otherwise>
	<xsl:if test=".//tei:attList">
	  <rng:ref name="{@ident}.attributes" />
	</xsl:if>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  
  
  
  <xsl:template match="tei:elementSpec" mode="tangleadd"/>
  
  
  <xsl:template match="tei:elementSpec/@ident"/>
  
  <xsl:template match="tei:gloss" mode="show">
    <xsl:if test="text()">
      (<xsl:apply-templates/>)
    </xsl:if>
  </xsl:template>
  
  
  
  
  <xsl:template match="tei:gloss"/>
  
  
  
  <xsl:template match="tei:index">
    <xsl:call-template name="makeAnchor">
      <xsl:with-param name="name">IDX-<xsl:number level="any"/></xsl:with-param>
    </xsl:call-template>
  </xsl:template>
  
  
  
  <xsl:template match="tei:macroSpec" mode="tangle">
    <xsl:param name="msection"/>
    <xsl:param name="filename"/>
    <xsl:choose>
      <xsl:when test="generate-id()=generate-id(key('PATTERNS',@ident)[last()])">
	<xsl:variable name="entCont">
	  <BLAH>
	    <xsl:choose>
	      <xsl:when test="not($msection='') and tei:content/rng:group">
		<rng:choice >
		  <xsl:apply-templates select="tei:content/rng:group/rng:*"/>	     
		</rng:choice>
	      </xsl:when>
	      <xsl:otherwise>
		<xsl:apply-templates select="tei:content/rng:*"/>
	      </xsl:otherwise>
	    </xsl:choose>
	  </BLAH>
	</xsl:variable>
	<xsl:variable name="entCount">
	  <xsl:for-each select="exsl:node-set($entCont)/BLAH">
	  <xsl:value-of select="count(rng:*)"/>
	</xsl:for-each>
      </xsl:variable>
      <xsl:choose>
	<xsl:when test='@ident="TEI.singleBase"'/>
	<xsl:when test='starts-with($entCont,"&#39;")'>
	  <xsl:if test="$verbose='true'">
	    <xsl:message>Omit <xsl:value-of select="$entCont"/> for
	    <xsl:value-of select="@ident"/></xsl:message>
	  </xsl:if>
	</xsl:when>
	<xsl:when test='starts-with($entCont,"-")'>
	  <xsl:if test="$verbose='true'">
	    <xsl:message>Omit <xsl:value-of select="$entCont"/> for
	    <xsl:value-of select="@ident"/></xsl:message>
	  </xsl:if>
	</xsl:when>
	<xsl:otherwise>
	  <xsl:if test="$verbose='true'">
	    <xsl:message>         ... so define ..<xsl:value-of
	    select="@ident"/></xsl:message>
	  </xsl:if>
	  <xsl:call-template name="bitOut">
	    <xsl:with-param name="grammar">true</xsl:with-param>
	    <xsl:with-param name="content">
	      <Wrapper>
		<rng:define name="{@ident}" >
		  <xsl:if test="starts-with(@ident,'macro.component')
				or @combine='true'">
		    <xsl:attribute name="combine">choice</xsl:attribute>
		  </xsl:if>
		  <xsl:choose>
		    <xsl:when test="starts-with(@ident,'type')">
		      <xsl:copy-of select="exsl:node-set($entCont)/BLAH/rng:*"/>
		    </xsl:when>
		    <xsl:when test="$entCount=0">
		      <rng:notAllowed />
		    </xsl:when>
		    <xsl:when test="$entCount=1">
		      <xsl:copy-of select="exsl:node-set($entCont)/BLAH/rng:*"/>
		    </xsl:when>
		    <xsl:when test="tei:content/rng:text|tei:content/rng:ref">
		      <rng:choice >
			<xsl:copy-of select="exsl:node-set($entCont)/BLAH/rng:*"/>
		      </rng:choice>
		    </xsl:when>
		    <xsl:otherwise>
		      <xsl:copy-of select="exsl:node-set($entCont)/BLAH/rng:*"/>
		    </xsl:otherwise>
		  </xsl:choose>
		</rng:define>
	      </Wrapper>
	    </xsl:with-param>
	  </xsl:call-template>
	</xsl:otherwise>
      </xsl:choose>
    </xsl:when>
    <xsl:otherwise>
      <xsl:if test="$verbose='true'">
	<xsl:message>ZAP pattern <xsl:value-of
	select="@ident"/></xsl:message>
      </xsl:if>
    </xsl:otherwise>
  </xsl:choose>
  
</xsl:template>




<xsl:template match="tei:macroSpec/@ident"/>



<xsl:template match="tei:macroSpec/content/rng:*"/>



<xsl:template match="tei:memberOf" mode="tangleAtts">
  <xsl:variable name="ident">
    <xsl:value-of select="ancestor::tei:elementSpec/@ident|ancestor::tei:classSpec/@ident"/>
  </xsl:variable>
  <xsl:choose>
    <xsl:when  test="key('IDENTS',@key)">
      <xsl:for-each select="key('IDENTS',@key)[1]">
	<xsl:apply-templates  select="." mode="tagatts"/>
      </xsl:for-each>
    </xsl:when>
    <xsl:otherwise>
      <xsl:variable name="loc">
	<xsl:value-of select="$TEISERVER"/>
	<xsl:text>copytag.xq?name=</xsl:text>
	<xsl:value-of select="@key"/>
      </xsl:variable>
      <xsl:if test="$verbose">
	<xsl:message>Accessing TEISERVER: <xsl:value-of
	select="$loc"/></xsl:message>
      </xsl:if>
      
      <xsl:apply-templates select="document($loc)/tei:TEI/*" mode="tagatts"/>
    </xsl:otherwise>
  </xsl:choose>
</xsl:template>


<xsl:template match="tei:memberOf" mode="tangleModel">
  <xsl:variable name="ident">
    <xsl:value-of select="ancestor::tei:elementSpec/@ident|ancestor::tei:classSpec/@ident"/>
  </xsl:variable>
  <xsl:choose>
    <xsl:when  test="key('IDENTS',@key)">
      <xsl:for-each select="key('IDENTS',@key)[1]">
	<xsl:if test="$verbose='true'">
	  <xsl:message>      .... added [<xsl:value-of 
	  select="$ident"/>] to  class [<xsl:value-of select="@ident"/>]</xsl:message>
	</xsl:if>
	<rng:define name="{@ident}" combine="choice" >
	  <rng:ref name="{$ident}" />
	</rng:define>
      </xsl:for-each>
    </xsl:when>
    <xsl:otherwise>
      <xsl:if test="$verbose='true'">
	<xsl:message>      .... added (without lookup) [<xsl:value-of
	select="$ident"/>] to class [<xsl:value-of
	select="@key"/>]</xsl:message>
      </xsl:if>
      <rng:define name="{@key}" combine="choice" >
	<rng:ref name="{$ident}" />
      </rng:define>
    </xsl:otherwise>
  </xsl:choose>
</xsl:template>



<xsl:template match="tei:mentioned">
  <xsl:text>&#8216;</xsl:text>
  <xsl:apply-templates/>
  <xsl:text>&#8217;</xsl:text>
</xsl:template>


<xsl:template match="tei:moduleRef" mode="tangle" >
  <xsl:variable name="This" select="@key"/>
  <xsl:if test="$verbose='true'">
    <xsl:message>      .... import module [<xsl:value-of
    select="$This"/>] <xsl:value-of select="@url"/>] </xsl:message>
  </xsl:if>
  <xsl:call-template name="bitOut">
    <xsl:with-param name="grammar">true</xsl:with-param>
    <xsl:with-param name="content">
      <Wrapper>
	<xsl:choose>
	  <xsl:when test="@url">
	    <rng:include href="{@url}" />
	  </xsl:when>
	  <xsl:otherwise>
	    <rng:include href="{$schemaBaseURL}{$This}.rng" >
	      <xsl:attribute name="ns">
		<xsl:choose>
		  <xsl:when test="ancestor::tei:schemaSpec/@namespace">
		    <xsl:value-of select="ancestor::tei:schemaSpec/@namespace"/>
		  </xsl:when>
		  <xsl:otherwise>http://www.tei-c.org/ns/1.0</xsl:otherwise>
		</xsl:choose>
	      </xsl:attribute>
	      <xsl:for-each  select="../tei:*[@module=$This and not(@mode='add')]">
		<xsl:apply-templates mode="tangle" select="."/>
	      </xsl:for-each>
	    </rng:include>
	    <xsl:for-each  select="../tei:*[@module=$This and not(@mode='add')]//tei:attDef[@mode='add']">
	      <xsl:call-template name="defineAnAttribute">
		<xsl:with-param name="Name" select="../../@ident"/>
	      </xsl:call-template>
	    </xsl:for-each>
	  </xsl:otherwise>
	</xsl:choose>
      </Wrapper>
    </xsl:with-param>
  </xsl:call-template>
</xsl:template>



<xsl:template match="tei:moduleSpec[@mode='replace']" mode="tangle" >
  <xsl:call-template name="bitOut">
    <xsl:with-param name="grammar">true</xsl:with-param>
    <xsl:with-param name="content">
      <Wrapper>
	<xsl:apply-templates mode="tangle"/>
      </Wrapper>
    </xsl:with-param>
  </xsl:call-template>
</xsl:template>



<xsl:template match="tei:p/@ident">
  <xsl:call-template name="makeAnchor">
    <xsl:with-param name="name">GDX-<xsl:number level="any"/></xsl:with-param>
  </xsl:call-template>
  <xsl:call-template name="ttembolden">
    <xsl:with-param name="text">
      <xsl:apply-templates/>
    </xsl:with-param>
  </xsl:call-template>
</xsl:template>

<xsl:template match="tei:remarks" mode="tangle"/>

<xsl:template match="tei:specGrp" mode="ok">
  <xsl:param name="filename"/>
  <xsl:call-template name="processSchemaFragment">
    <xsl:with-param name="filename" select="$filename"/>
  </xsl:call-template>
</xsl:template>



<xsl:template match="tei:tag">
  <xsl:call-template name="typewriter">
    <xsl:with-param name="text">
      <xsl:text>&lt;</xsl:text>
      <xsl:apply-templates/>
      <xsl:text>&gt;</xsl:text>
    </xsl:with-param>
  </xsl:call-template>
</xsl:template>




<xsl:template match="tei:title">
  <xsl:choose>
    <xsl:when test="parent::tei:titleStmt">
      <xsl:apply-templates/>
    </xsl:when>
    <xsl:when test="@level='A'">
      &#8216;<xsl:apply-templates/>'
    </xsl:when>
    <xsl:otherwise>
      <xsl:call-template name="italicize">
	<xsl:with-param name="text">
	  <xsl:apply-templates/>
	</xsl:with-param>
      </xsl:call-template>
    </xsl:otherwise>
  </xsl:choose>
</xsl:template>


<xsl:template match="comment()" mode="verbatim">
  <xsl:text>&#10;&lt;!--</xsl:text>
  <xsl:value-of select="."/>
  <xsl:text>--&gt;&#10;</xsl:text>
</xsl:template>

<xsl:template name="wraptext">
  <xsl:param name="indent"/>
  <xsl:param name="text"/>
  <xsl:choose>
    <xsl:when test="contains($text,'&#10;')">
      <xsl:value-of select="substring-before($text,'&#10;')"/>
      <xsl:text>&#10;</xsl:text>
      <xsl:value-of select="$indent"/>
      <xsl:call-template name="wraptext">
	<xsl:with-param name="indent">
	  <xsl:value-of select="$indent"/>
	</xsl:with-param>
	<xsl:with-param name="text">
	  <xsl:value-of select="substring-after($text,'&#10;')"/>
	</xsl:with-param>
      </xsl:call-template>
    </xsl:when>
    <xsl:otherwise>
      <xsl:value-of select="$text"/>
    </xsl:otherwise>
  </xsl:choose>
</xsl:template>

<xsl:template match="text()" mode="verbatim">
  <xsl:call-template name="wraptext">
    <xsl:with-param name="indent">
      <xsl:for-each select="ancestor::teix:*|ancestor::rng:*">
	<xsl:text> </xsl:text>
      </xsl:for-each>
    </xsl:with-param>
    <xsl:with-param name="text">
      <xsl:value-of select="."/>
    </xsl:with-param>
  </xsl:call-template>
</xsl:template>


<xsl:template match="teix:*|rng:*|tei:*" mode="verbatim">
  <xsl:choose>
    <xsl:when test="preceding-sibling::node()[1]/self::*">
      <xsl:text>&#10;</xsl:text>
      <xsl:for-each select="ancestor::teix:*|ancestor::rng:*">
	<xsl:text> </xsl:text>
      </xsl:for-each>
    </xsl:when>
    <xsl:when test="not(preceding-sibling::node())">
      <xsl:text>&#10;</xsl:text>
      <xsl:for-each select="ancestor::teix:*|ancestor::rng:*">
	<xsl:text> </xsl:text>
      </xsl:for-each>
    </xsl:when>
  </xsl:choose>
  <xsl:text>&lt;</xsl:text>
  <xsl:value-of select="local-name(.)"/>
  <xsl:for-each select="@*">
    <xsl:text> </xsl:text>
  <xsl:value-of select="local-name(.)"/>="<xsl:value-of select="."/>"</xsl:for-each>
  <xsl:choose>
    <xsl:when test="child::node()">
      <xsl:text>&gt;</xsl:text>
      <xsl:apply-templates mode="verbatim"/>
      <xsl:choose>
	<xsl:when
	    test="child::node()[last()]/self::text()[normalize-space(.)='']"> 
	  <xsl:text>&#10;</xsl:text>
	  <xsl:for-each select="ancestor::teix:*|ancestor::rng:*">
	    <xsl:text> </xsl:text>
	  </xsl:for-each>
	</xsl:when>
	<xsl:when
	    test="child::node()[last()]/self::comment()"> 
	  <xsl:text>&#10;</xsl:text>
	  <xsl:for-each select="ancestor::teix:*|ancestor::rng:*">
	    <xsl:text> </xsl:text>
	  </xsl:for-each>
	</xsl:when>
	<xsl:when
	    test="child::node()[last()]/self::*"> 
	  <xsl:text>&#10;</xsl:text>
	  <xsl:for-each select="ancestor::teix:*|ancestor::rng:*">
	    <xsl:text> </xsl:text>
	  </xsl:for-each>
	</xsl:when>
      </xsl:choose>
      <xsl:text>&lt;/</xsl:text>
      <xsl:value-of select="local-name(.)"/>
      <xsl:text>&gt;</xsl:text>
    </xsl:when>    
    <xsl:otherwise>
      <xsl:text>/&gt;</xsl:text>
    </xsl:otherwise>
  </xsl:choose>
</xsl:template>

<xsl:template match="token" mode="commentline">
  <xsl:call-template name="italicize">
    <xsl:with-param name="text">
      <xsl:value-of select="translate(.,'&#10;','')"/>
    </xsl:with-param>
  </xsl:call-template>
  <xsl:if test="following-sibling::token">
    <xsl:text>
    </xsl:text>
    <xsl:choose>
      <xsl:when test="contains(.,'--&gt;')">
	<xsl:apply-templates select="following-sibling::token[1]" 
			     mode="normalline"/>  
      </xsl:when>
      <xsl:otherwise>
	<xsl:apply-templates select="following-sibling::token[1]" mode="commentline"/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:if>
</xsl:template>



<xsl:template match="token" mode="normalline">
  <xsl:choose>
    <xsl:when test="contains(.,'&lt;!--')">
      <xsl:call-template name="italicize">
	<xsl:with-param name="text">
	  <xsl:value-of select="translate(.,'&#10;','')"/>
	</xsl:with-param>
      </xsl:call-template>
      <xsl:if test="following-sibling::token">
	<xsl:text>
	</xsl:text>
	<xsl:choose>
	  <xsl:when test="contains(.,'--&gt;')">
	    <xsl:apply-templates select="following-sibling::token[1]" 
				 mode="normalline"/>  
	  </xsl:when>
	  <xsl:otherwise>
	    <xsl:apply-templates select="following-sibling::token[1]" mode="commentline"/>
	  </xsl:otherwise>
	</xsl:choose>
      </xsl:if>
    </xsl:when>
    <xsl:otherwise>
      <xsl:call-template name="breakline"/>  
      <xsl:if test="following-sibling::token">
	<xsl:text>
	</xsl:text>
	<xsl:apply-templates select="following-sibling::token[1]" 
			     mode="normalline"/>
      </xsl:if>
    </xsl:otherwise>
  </xsl:choose>
</xsl:template>



<xsl:template match="token" mode="verbatimline">
  <xsl:call-template name="breakline"/>  
  <xsl:if test="following-sibling::token">
    <xsl:text>
    </xsl:text>
    <xsl:apply-templates select="following-sibling::token[1]" 
			 mode="verbatimline"/>
  </xsl:if>
</xsl:template>



<xsl:template match="token" mode="word">
  <xsl:param name="len"/>
  <xsl:choose>
    <xsl:when test="$len +string-length(.) &gt; $wrapLength">
      <xsl:text>
      </xsl:text>
      <xsl:value-of select="."/><xsl:text> </xsl:text>
      <xsl:if test="following-sibling::token">
	<xsl:apply-templates select="following-sibling::token[1]" mode="word">
	  <xsl:with-param name="len" select="8"/>
	</xsl:apply-templates>
      </xsl:if>
    </xsl:when>
    <xsl:otherwise>
      <xsl:value-of select="."/><xsl:text> </xsl:text>
      <xsl:if test="following-sibling::token">
	<xsl:apply-templates select="following-sibling::token[1]" mode="word">
	  <xsl:with-param name="len" select="$len + string-length(.)"/>
	</xsl:apply-templates>
      </xsl:if>
    </xsl:otherwise>
  </xsl:choose>
</xsl:template>




<xsl:template name="attributeBody">		
  <xsl:variable name="name">
    <xsl:choose>
      <xsl:when test="tei:altIdent">
	<xsl:value-of select="normalize-space(tei:altIdent)"/>
      </xsl:when>
      <xsl:otherwise>
	    <xsl:value-of select="@ident"/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:variable>
  <rng:attribute name="{$name}" >
    <xsl:if test="tei:defaultVal">
      <xsl:attribute name="a:defaultValue">
	<xsl:value-of select="normalize-space(tei:defaultVal)"/>
      </xsl:attribute>
    </xsl:if>
    <xsl:variable name="I">
      <xsl:choose>
	<xsl:when test="starts-with(@ident,'xml:')">
	  <xsl:text>xml</xsl:text>
	  <xsl:value-of select="substring-after(@ident,'xml:')"/>
	</xsl:when>
	<xsl:otherwise>
	  <xsl:value-of select="@ident"/>
	</xsl:otherwise>
      </xsl:choose>
    </xsl:variable>
      <a:documentation>
	<xsl:value-of select="tei:desc"/>
      </a:documentation>
    <rng:ref
	name="{ancestor::tei:attList/../@ident}.attributes.{$I}.content" />
  </rng:attribute>
</xsl:template>



<xsl:template name="breakline">
  <xsl:choose>
    <xsl:when test="string-length(.)&lt;$wrapLength">
      <xsl:value-of select="."/>
    </xsl:when>
    <xsl:otherwise>
      <xsl:variable name="words" select="estr:tokenize(.)"/>
      <xsl:apply-templates select="$words[1]" mode="word">
	<xsl:with-param name="len" select="0"/>
      </xsl:apply-templates>
    </xsl:otherwise>
  </xsl:choose>
</xsl:template>



<xsl:template name="compositeNumber">
  <xsl:for-each select="ancestor::tei:div1|ancestor::tei:body/tei:div">
    <xsl:number level="any"/>
    <xsl:text>.</xsl:text>
  </xsl:for-each>
  <xsl:number level="any" from="tei:div1"/>
</xsl:template>

<xsl:template name="copyright">
  <xsl:apply-templates 
      select="/tei:TEI/tei:teiHeader/tei:fileDesc/tei:publicationStmt/tei:availability"/>
</xsl:template>

<xsl:template name="defineAnAttribute">
  <xsl:param name="Name"/>
    <xsl:variable name="I">
      <xsl:choose>
	<xsl:when test="starts-with(@ident,'xml:')">
	  <xsl:text>xml</xsl:text>
	  <xsl:value-of select="substring-after(@ident,'xml:')"/>
	</xsl:when>
	<xsl:otherwise>
	  <xsl:value-of select="@ident"/>
	</xsl:otherwise>
      </xsl:choose>
    </xsl:variable>

    <rng:define name="{$Name}.attributes.{$I}" >
      <xsl:choose>
	<xsl:when test="@mode='delete'">
	  <rng:notAllowed />
	</xsl:when>
	<xsl:otherwise>
	  <xsl:choose>
	    <xsl:when test="@usage='req'">
	      <xsl:call-template name="attributeBody"/>
	    </xsl:when>
	    <xsl:when test="parent::tei:attList[@org='choice']">
	      <xsl:call-template name="attributeBody"/>
	    </xsl:when>
	    <xsl:otherwise>
	      <rng:optional >
		<xsl:call-template name="attributeBody"/>
	      </rng:optional>
	    </xsl:otherwise>
	  </xsl:choose>
	</xsl:otherwise>
      </xsl:choose>	
    </rng:define>
    
    <xsl:choose>
      <xsl:when test="@mode='delete'"/>
      <xsl:when test="@mode='change' and not(tei:datatype or tei:valList)"/>
      <xsl:otherwise>
	<rng:define name="{$Name}.attributes.{$I}.content" >
	  <xsl:call-template name="attributeDatatype"/>
	</rng:define>
      </xsl:otherwise>
    </xsl:choose>
</xsl:template>

<xsl:template name="attributeDatatype">
  <xsl:variable name="this" select="@ident"/>
  <xsl:choose>
    <xsl:when test="tei:datatype[rng:ref/@name='datatype.Code']">
      <xsl:choose>
	<xsl:when test="tei:valList[@type='closed']">
	  <rng:choice >
	    <xsl:for-each select="tei:valList/tei:valItem">
	      <rng:value ><xsl:value-of select="@ident"/></rng:value>
              <a:documentation><xsl:value-of select="tei:gloss"/></a:documentation>
	    </xsl:for-each>
	    <xsl:if test="@mode='add' and
			  ancestor::tei:attList/../@mode='change'">
	      <xsl:variable name="loc">
		<xsl:value-of select="$TEISERVER"/>
		<xsl:text>copytag.xq?name=</xsl:text>
		<xsl:value-of select="ancestor::tei:attList/../@ident"/>
	      </xsl:variable>
	      <xsl:if test="$verbose">
		<xsl:message>Accessing TEISERVER: <xsl:value-of
		select="$loc"/></xsl:message>
	      </xsl:if>
	      
	      <xsl:for-each select="document($loc)/tei:TEI/*">
		<xsl:for-each
		    select=".//tei:attList/tei:attDef[@ident=$this]/tei:valList/tei:valItem">
		  <rng:value ><xsl:value-of  select="@ident"/></rng:value>
		  <a:documentation><xsl:value-of select="tei:gloss"/></a:documentation>
		</xsl:for-each>
	      </xsl:for-each>
	    </xsl:if>
	  </rng:choice>
	</xsl:when>
	<xsl:otherwise>
	  <rng:text />
	</xsl:otherwise>
      </xsl:choose>
    </xsl:when>
    <xsl:when test="tei:datatype/rng:*">
      <xsl:copy-of select="tei:datatype/rng:*"/>
    </xsl:when>
    <xsl:when test="tei:valList[@type='closed']">
      <rng:choice >
	<xsl:for-each select="tei:valList/tei:valItem">
	  <rng:value ><xsl:value-of select="@ident"/></rng:value>
	  <a:documentation><xsl:value-of select="tei:gloss"/></a:documentation>
	</xsl:for-each>
      </rng:choice>
    </xsl:when>
    <xsl:otherwise>
      <rng:text />
    </xsl:otherwise>
  </xsl:choose>
</xsl:template>

<xsl:template name="makeSimpleAttribute">
  <xsl:variable name="name">
    <xsl:choose>
      <xsl:when test="tei:altIdent">
	<xsl:value-of select="normalize-space(tei:altIdent)"/>
      </xsl:when>
      <xsl:otherwise>
	<xsl:choose>
	  <xsl:when test="starts-with(@ident,'xml:')">
	    <xsl:text>xml</xsl:text>
	    <xsl:value-of select="substring-after(@ident,'xml:')"/>
	  </xsl:when>
	  <xsl:otherwise>
	    <xsl:value-of select="@ident"/>
	  </xsl:otherwise>
	</xsl:choose>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:variable>
  
  <rng:attribute name="{$name}" >
    <xsl:if test="tei:defaultVal">
      <xsl:attribute name="a:defaultValue">
	<xsl:value-of select="normalize-space(tei:defaultVal)"/>
      </xsl:attribute>
    </xsl:if>
    <a:documentation>
	<xsl:value-of select="tei:desc"/>
    </a:documentation>
    <xsl:call-template name="attributeDatatype"/>
  </rng:attribute>
</xsl:template>

<xsl:template name="makeAnAttribute">
  <xsl:choose>
    <xsl:when test="@usage='req'">
      <xsl:call-template name="makeSimpleAttribute"/>
    </xsl:when>
    <xsl:when test="parent::tei:attList[@org='choice']">
      <xsl:call-template name="makeSimpleAttribute"/>
    </xsl:when>
    <xsl:otherwise>
      <rng:optional >
	<xsl:call-template name="makeSimpleAttribute"/>
      </rng:optional>
    </xsl:otherwise>
  </xsl:choose>
</xsl:template>


<xsl:template name="defineRelaxAttributes">
  <xsl:variable name="name" select="@ident"/>
  <xsl:for-each select="tei:attList//tei:attDef">
    <xsl:choose>
      <xsl:when test="@ident='xmlns'"/>
      <xsl:when test="@mode='add' and ../../@mode='replace'">
      </xsl:when>
      <xsl:when test="@mode='add' and ../../@mode='change'">
      </xsl:when>
      <xsl:otherwise>

	<xsl:call-template name="defineAnAttribute">
	  <xsl:with-param name="Name" select="$name"/>
	</xsl:call-template>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:for-each>
</xsl:template>

<xsl:template name="makeRelaxAttributes">
  <xsl:variable name="name" select="@ident"/>
  <xsl:for-each select="tei:attList//tei:attDef[not(@mode='delete')]">
    <xsl:if test="not(@ident='xmlns')">
      <!--      
	   <xsl:call-template name="makeAnAttribute"/>
      -->
      <rng:ref name="{$name}.attributes.{@ident}"/>
    </xsl:if>
  </xsl:for-each>
</xsl:template>




<xsl:template name="generateClassParents">
  <xsl:choose>
    <xsl:when test="not(tei:classes)"> (none)   </xsl:when>
    <xsl:otherwise>
      <xsl:for-each select="tei:classes/tei:memberOf">
	<xsl:choose>
	  <xsl:when test="key('IDENTS',@key)">
	    <xsl:for-each select="key('IDENTS',@key)">
	      <xsl:text>: </xsl:text>
	      <xsl:call-template name="makeLink">
		<xsl:with-param name="class">classlink</xsl:with-param>
		<xsl:with-param name="id"><xsl:value-of	select="@id|@xml:id"/></xsl:with-param>
		<xsl:with-param name="name"><xsl:value-of select="@ident"/></xsl:with-param>
		<xsl:with-param name="text">
		  <xsl:value-of select="@ident"/>
		</xsl:with-param>
	      </xsl:call-template>
	    </xsl:for-each>
	  </xsl:when>
	  <xsl:otherwise>
	    <xsl:text>: </xsl:text>
	    <xsl:value-of select="@key"/>
	  </xsl:otherwise>
	</xsl:choose>
      </xsl:for-each>
    </xsl:otherwise>
  </xsl:choose>
</xsl:template>




<xsl:template name="generateMembers">
  <xsl:variable name="this" select="@ident"/>
  <xsl:choose>
    <xsl:when test="key('CLASSMEMBERS',$this)">
      <xsl:for-each select="key('CLASSMEMBERS',$this)">
        <xsl:text>: </xsl:text>
	<xsl:call-template name="linkTogether">
	  <xsl:with-param name="name" select="@ident"/>
	  <xsl:with-param name="url" select="@id|@xml:id"/>
	</xsl:call-template>
	<xsl:if test="count(key('CLASSMEMBERS',@ident))&gt;0">
	  <xsl:text>  [</xsl:text>
	  <xsl:variable name="Key" select="@ident"/>
	  <xsl:for-each select="key('CLASSMEMBERS',@ident)">
	      <xsl:text>: </xsl:text>
	      <xsl:call-template name="showElement">
		<xsl:with-param name="name" select="@ident"/>
		<xsl:with-param name="id" select="@id|@xml:id"/>
	      </xsl:call-template>
	  </xsl:for-each>
	    <xsl:text>] </xsl:text>
	</xsl:if>
      </xsl:for-each>
    </xsl:when>
    <xsl:otherwise>
      <xsl:if test="$verbose">
	<xsl:message>Accessing TEISERVER: <xsl:value-of
	select="concat($TEISERVER,'classmembers.xq?class=',@ident)"/></xsl:message>
      </xsl:if>
      <xsl:for-each
	  select="document(concat($TEISERVER,'classmembers.xq?class=',@ident))/list/item">
	  <xsl:if test="key('IDENTS',.)">
	    <xsl:text>:  </xsl:text>
	    <xsl:call-template name="showElement">
	      <xsl:with-param name="name" select="."/>
	      <xsl:with-param name="id"/>
	      </xsl:call-template>:
	</xsl:if>
      </xsl:for-each>
    </xsl:otherwise>
  </xsl:choose>
  
</xsl:template>

<xsl:template name="showElement">
  <xsl:param name="id"/>
  <xsl:param name="name"/>
  <xsl:choose>
    <xsl:when test="$oddmode='tei'">
      <tei:ref target="#{$name}"><xsl:value-of select="$name"/></tei:ref>
    </xsl:when>
    <xsl:when test="$oddmode='html'">
      <xsl:choose>
	<xsl:when test="not($id='')">
	  <a class="link_element" href="ref-{$id}.html">
	    <xsl:value-of select="$name"/>
	  </a>
	</xsl:when>
	<xsl:otherwise>
	  <a href="{concat($TEISERVER,'tag.xq?name=',$name)}">
	    <xsl:value-of select="$name"/>
	  </a>
	</xsl:otherwise>
      </xsl:choose>
    </xsl:when>
    <xsl:when test="$oddmode='pdf'">
      <fo:inline font-style="italic"><xsl:value-of select="$name"/></fo:inline>
    </xsl:when>
    <xsl:otherwise>
      <xsl:value-of select="$name"/>
    </xsl:otherwise>
  </xsl:choose>
</xsl:template>



<xsl:template name="generateParents">
  <xsl:param name="what"/>
  <xsl:variable name="mums">
    <mums>
      <xsl:for-each select="document('dtdcat.xml')">
	<xsl:for-each select="key('ELEMENTPARENTS',$what)">
	  <mum><xsl:value-of select="../@id|../@xml:id"/></mum>
	</xsl:for-each>
      </xsl:for-each>
    </mums>
  </xsl:variable>
  <xsl:variable name="mums2">
    <mums>
      <xsl:for-each select="exsl:node-set($mums)/mums/mum">
	<xsl:sort select="."/>
	<mum><xsl:value-of select="."/></mum>
      </xsl:for-each>
    </mums>
  </xsl:variable>
  <xsl:for-each select="exsl:node-set($mums2)/mums/mum">
    <xsl:if test="not(. = preceding-sibling::mum)">        
      <xsl:value-of select="."/><xsl:text> </xsl:text>
    </xsl:if>
  </xsl:for-each>
</xsl:template>




<xsl:template name="linkStyle"/>



<xsl:template name="linkTogether">
  <xsl:param name="name"/>
  <xsl:param name="url"/>
  <xsl:choose>
    <xsl:when test="$oddmode='html' and starts-with($url,'http:')">
      <a href="{$url}"><xsl:value-of select="$name"/></a>
    </xsl:when>
    <xsl:when test="$oddmode='html'">
      <a class="link_odd" href="{concat('ref-',$url,'.html')}"><xsl:value-of select="$name"/></a>
    </xsl:when>
    <xsl:when test="$oddmode='pdf'">
      <fo:inline><xsl:value-of select="$name"/></fo:inline>
    </xsl:when>
    <xsl:when test="$oddmode='tei'">
      <tei:ref target="{$name}"><xsl:value-of select="$name"/></tei:ref>
    </xsl:when>
    <xsl:otherwise>
      <xsl:value-of select="$name"/>
    </xsl:otherwise>
  </xsl:choose>
</xsl:template>



<xsl:template name="makeTagsetInfo">
  <xsl:value-of select="@module"/>
  <xsl:if test="$verbose='true'">
    <xsl:message>  tagset <xsl:value-of select="@id|@xml:id"/>: <xsl:value-of select="../@module"/></xsl:message>
  </xsl:if>
</xsl:template>



<xsl:template name="processSchemaFragment">
  <xsl:param name="filename"/>
  <xsl:variable name="secnum">
    <xsl:call-template name="sectionNumber"/>
  </xsl:variable>
  <xsl:if test="@id|@xml:id">
    <xsl:comment>[<xsl:value-of select="@id|@xml:id"/>] <xsl:value-of
    select="$secnum"/>
    <xsl:if test="@n">
      <xsl:text>: </xsl:text>
      <xsl:value-of select="@n"/>
    </xsl:if>
    </xsl:comment>
  </xsl:if>
  <xsl:apply-templates mode="tangle">
    <xsl:with-param name="filename" select="$filename"/>
  </xsl:apply-templates>
  <xsl:if test="@id|@xml:id">
    <xsl:comment> end of [<xsl:value-of select="@id|@xml:id"/>]  <xsl:value-of select="$secnum"/>    
    </xsl:comment>
  </xsl:if>
</xsl:template>




<xsl:template name="processSpecDesc">
  <xsl:variable name="name">
    <xsl:value-of select="@key"/>
  </xsl:variable>
  <xsl:variable name="atts">
    <xsl:value-of select="concat(' ',normalize-space(@atts),' ')"/>
  </xsl:variable>
  <xsl:choose>
    <xsl:when test="$name=''">
      <xsl:message>ERROR: no key attribute on specDesc</xsl:message>
    </xsl:when>
    <xsl:when test="key('IDENTS',$name)">
      <xsl:apply-templates select="key('IDENTS',$name)" mode="show">
	<xsl:with-param name="atts" select="$atts"/>
      </xsl:apply-templates>
    </xsl:when>
    <xsl:otherwise>
      <xsl:variable name="loc">
	<xsl:value-of select="$TEISERVER"/>
	<xsl:text>copytag.xq?name=</xsl:text>
	<xsl:value-of select="$name"/>
      </xsl:variable>
      <xsl:if test="$verbose">
	<xsl:message>Accessing TEISERVER: <xsl:value-of
	select="$loc"/></xsl:message>
      </xsl:if>
      
      <xsl:apply-templates select="document($loc)/tei:*" mode="show">
	<xsl:with-param name="atts" select="$atts"/>
      </xsl:apply-templates>
    </xsl:otherwise>
  </xsl:choose>
</xsl:template>



<xsl:template name="processatts">
  <xsl:param name="values"/>
  <xsl:if test="not($values = '')">
    <xsl:apply-templates 
	select="key('IDS',substring-before($values,' '))"/>
    <xsl:call-template name="processatts">
      <xsl:with-param name="values" select="substring-after($values,' ')"/>
    </xsl:call-template>
  </xsl:if>
</xsl:template>




<xsl:template name="sectionNumber">
  <xsl:for-each select="(ancestor::tei:div1|ancestor::tei:div2|ancestor::tei:div3|ancestor::tei:div4)[last()]">
    <xsl:for-each select="ancestor-or-self::tei:div1">
      <xsl:number from="tei:body" level="any" /><xsl:text>.</xsl:text>
    </xsl:for-each>
    <xsl:number level="multiple" count="tei:div2|tei:div3|tei:div4" from="tei:div1"/>
  </xsl:for-each>
</xsl:template>

<xsl:template match="tei:classSpec" mode="tagatts">
  <xsl:if test="$verbose='true'">
    <xsl:message>      .... link to attributes from class [<xsl:value-of select="@ident"/>]</xsl:message>
  </xsl:if>
  <rng:ref name="{@ident}.attributes" />
</xsl:template>


<xsl:template name="make-ns-declaration">
  <xsl:param name="is-default"/>
  <xsl:param name="prefix"/>
  <xsl:param name="uri"/>
</xsl:template>

<xsl:template name="inhnamespace"/>

<xsl:template match="s:*"/>

<xsl:template match="tei:altIdent"/>

<xsl:template match="a:*">
  <xsl:copy-of select="."/>
</xsl:template>

</xsl:stylesheet>
