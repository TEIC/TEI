<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet 
    xmlns:rng="http://relaxng.org/ns/structure/1.0"
    xmlns="http://www.tei-c.org/ns/1.0"
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform" 
    xpath-default-namespace="http://www.tei-c.org/ns/1.0"
    xmlns:tei="http://www.tei-c.org/ns/1.0"
    exclude-result-prefixes="rng tei"
    version="2.0">
  <xsl:import href="../common/functions.xsl"/>
  <xsl:output method="text"/>
  <xsl:param name="module"/>
  <xsl:param name="method">ref</xsl:param>
  <xsl:param name="classtype">model</xsl:param>
  <xsl:param name="includeheader">true</xsl:param>
  <xsl:param name="minimal">false</xsl:param>
  <xsl:key name="ELEMENTS" use="@ident" match="elementSpec"/>
  <xsl:key name="CLASSES" use="@ident" match="classSpec"/>
  <xsl:key name="MEMBERS" use="classes/memberOf/@key" match="elementSpec"/>
  <xsl:key name="MACROS" use="@ident" match="macroSpec[not(contains(@ident,'any'))]"/>

  <xsl:variable name="TOP" select="/"/>

  <xsl:template match="/">
    <xsl:text>source,target,value,group&#10;</xsl:text>
    <xsl:choose>
      <xsl:when test="$method='ref'">
	<xsl:for-each-group select="tei:catalogueByRef()"
			    group-by=".">
	  <xsl:sort select="."/>
	  <xsl:value-of select="current-grouping-key()"/>
	  <xsl:text>&#10;</xsl:text>
      </xsl:for-each-group>
      </xsl:when>
      <xsl:when test="$method='classes'">
	<xsl:for-each-group select="tei:catalogueByClass()"
			    group-by=".">
	  <xsl:sort select="."/>
	  <xsl:value-of select="current-grouping-key()"/>
	  <xsl:text>&#10;</xsl:text>
      </xsl:for-each-group>
      </xsl:when>
    </xsl:choose>
  </xsl:template>

<xsl:function name="tei:catalogueByRef" as="node()*">
    <xsl:for-each select="$TOP//elementSpec">
      <xsl:choose>
	<xsl:when test="@ident='egXML'"/>
	<xsl:when test="$includeheader='false' and @module='header'"/>
	<xsl:when test="not($module ='') and not(@module=$module)"/>
	<xsl:otherwise>
	<xsl:variable name="element" select="@ident"/>
	<xsl:for-each select="content//rng:ref">
	  <xsl:call-template name="expandRef">
	    <xsl:with-param name="distance" select="1"/>
	    <xsl:with-param name="element" select="$element"/>
	  </xsl:call-template>      
	</xsl:for-each>
	</xsl:otherwise>
      </xsl:choose>
    </xsl:for-each>
</xsl:function>

  <xsl:template name="expandRef">
    <xsl:param name="distance"/>
    <xsl:param name="element" />
    <xsl:if test="not(@name=$element)">

    <!-- direct report -->
      <xsl:if test="key('CLASSES',@name)">
	<token><xsl:value-of   select="($element,@name,$distance,'class')"   separator=","/></token>
      </xsl:if>
	  
    <!-- report via macro -->
    <xsl:for-each select="key('MACROS',@name)">
	<token><xsl:value-of   select="($element,@ident,$distance,'macro')"   separator=","/></token>
      <xsl:variable name="class" select="@ident"/>
      <xsl:for-each select="content//rng:ref">
	<xsl:call-template name="expandRef">
	  <xsl:with-param name="distance" select="$distance + 1"/>
	  <xsl:with-param name="element" select="$element"/>
	</xsl:call-template>
      </xsl:for-each>
    </xsl:for-each>

    <!-- report via class -->
    <xsl:for-each select="key('CLASSES',@name)">
      <token><xsl:value-of   select="($element,@ident,$distance,'model')"   separator=","/></token>
      <xsl:variable name="class" select="@ident"/>
      <xsl:call-template name="expandModel">
	<xsl:with-param name="distance" select="$distance + 2"/>
	<xsl:with-param name="class" select="$element"/>
      </xsl:call-template>
    </xsl:for-each>
    </xsl:if>

  </xsl:template>


  <xsl:template name="expandModel">
    <xsl:param name="distance" />
    <xsl:param name="class" />
    <xsl:for-each select="key('MEMBERS',@ident)">
      <xsl:value-of   select="($class,@ident,$distance,'class')"   separator=","/>
      <xsl:text>&#10;</xsl:text>
    </xsl:for-each>
  </xsl:template>


<xsl:function name="tei:catalogueByClass" as="node()*">
    <xsl:for-each select="$TOP//*[classes]">
      <xsl:choose>
	<xsl:when test="@ident='egXML'"/>
	<xsl:when test="$includeheader='false' and @module='header'"/>
	<xsl:when test="not($module ='') and not(@module=$module)"/>
	<xsl:otherwise>
	  <xsl:call-template name="expandClass">
	    <xsl:with-param name="distance" select="1"/>
	  </xsl:call-template>      
	</xsl:otherwise>
      </xsl:choose>
    </xsl:for-each>
  </xsl:function>

  <xsl:template name="expandClass">
    <xsl:param name="distance"/>
    <xsl:variable name="current" select="@ident"/>
      <xsl:for-each select="classes/memberOf/key('CLASSES',@key)">
	<xsl:if test="@type=$classtype">
	  <token><xsl:value-of   select="($current,@ident,$distance,@type)"   separator=","/></token>
	  <xsl:call-template name="expandClass">
	    <xsl:with-param name="distance" select="$distance + 1"/>
	  </xsl:call-template>      
	</xsl:if>
      </xsl:for-each>
  </xsl:template>


</xsl:stylesheet>
