<?xml version="1.0" encoding="utf-8"?>
<!-- $Date: 
Text Encoding Initiative Consortium XSLT stylesheet family
2001/10/01 $, $Revision$, $Author$

XSL stylesheet to format TEI XML documents using ODD markup

 
##LICENSE
-->
<xsl:stylesheet 
     xmlns:s="http://www.ascc.net/xml/schematron" 
     xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
     xmlns:tei="http://www.tei-c.org/ns/1.0"
     xmlns:estr="http://exslt.org/strings"
     xmlns:t="http://www.thaiopensource.com/ns/annotations"
     xmlns:a="http://relaxng.org/ns/compatibility/annotations/1.0"
     xmlns:edate="http://exslt.org/dates-and-times"
     xmlns:exsl="http://exslt.org/common"
     xmlns:rng="http://relaxng.org/ns/structure/1.0"
     extension-element-prefixes="exsl estr edate"
     exclude-result-prefixes="exsl edate estr tei t a rng s" 
     version="1.0">


<xsl:import href="teiodds.xsl"/>

<xsl:param name="verbose"></xsl:param>
<xsl:param name="RNGDIR">Schema</xsl:param>
<xsl:variable name="appendixWords"/>
<xsl:variable name="headingNumberSuffix"/>
<xsl:variable name="numberBackHeadings"/>
<xsl:variable name="numberFrontHeadings"/>
<xsl:variable name="numberHeadings"/>
<xsl:variable name="numberHeadingsDepth"/>
<xsl:variable name="prenumberedHeadings"/>
<xsl:template name="italicize"/>
<xsl:template name="makeAnchor"/>
<xsl:template name="makeLink"/>
<xsl:output method="xml" indent="yes"/>

  <xsl:variable name="oddmode">dtd</xsl:variable>       
  <xsl:variable name="filesuffix"></xsl:variable>
<!-- get list of output files -->
<xsl:variable name="linkColor"/>



<xsl:template match="tei:moduleSpec[@type='decls']" />

<xsl:template match="/tei:TEI">
<xsl:choose>
  <xsl:when test=".//tei:schemaSpec">
    <xsl:apply-templates select=".//tei:schemaSpec" />
  </xsl:when>
  <xsl:otherwise>
    <xsl:apply-templates select="key('AllModules',1)"/>
  </xsl:otherwise>
</xsl:choose>
</xsl:template>

<xsl:template name="generateOutput">
<xsl:param name="body"/>
<xsl:choose>
  <xsl:when test="$RNGDIR='' or $RNGDIR='-'">
      <xsl:copy-of select="$body"/>
  </xsl:when>
  <xsl:otherwise>
    <xsl:if test="$verbose='true'">
    <xsl:message>   File [<xsl:value-of select="@ident"/>]      </xsl:message>
    </xsl:if>
    <exsl:document method="xml" indent="yes"
		   href="{$RNGDIR}/{@ident}.rng">
      <xsl:copy-of select="$body"/>
    </exsl:document>
  </xsl:otherwise>
</xsl:choose>
</xsl:template>

<xsl:template match="tei:schemaSpec" >
    <xsl:variable name="filename" select="@ident"/>
    <xsl:call-template name="generateOutput">
      <xsl:with-param name="body">
	<grammar
	 xmlns="http://relaxng.org/ns/structure/1.0"
	 xmlns:t="http://www.thaiopensource.com/ns/annotations"
	 xmlns:a="http://relaxng.org/ns/compatibility/annotations/1.0"
	 datatypeLibrary="http://www.w3.org/2001/XMLSchema-datatypes">
	  <xsl:attribute name="ns">
	    <xsl:choose>
	      <xsl:when test="@namespace"><xsl:value-of select="@namespace"/></xsl:when>
	      <xsl:otherwise>http://www.tei-c.org/ns/1.0</xsl:otherwise>
	    </xsl:choose>
	  </xsl:attribute>	  
	  <xsl:comment>
	    <xsl:text>Schema generated </xsl:text>
	    <xsl:value-of  select="edate:date-time()"/>
	    <xsl:text>&#010;</xsl:text>

	    <xsl:call-template name="copyright"/>
<!--
           <xsl:text>WARNING! Generated from a pre-release draft of TEI P5
from 1st October 2004. This is NOT the final P5</xsl:text>
-->
	  </xsl:comment>
	  <xsl:text>
	    
	  </xsl:text>
	  <xsl:apply-templates mode="tangle" select="tei:specGrpRef"/>
  	  <xsl:apply-templates mode="tangle" select="tei:moduleRef"/>
  	  <xsl:apply-templates mode="tangle"
			       select="tei:elementSpec[@mode='add']"/>
  	  <xsl:apply-templates mode="tangle"
			       select="tei:macroSpec[@mode='add']"/>
  	  <xsl:apply-templates mode="tangle"
			       select="tei:classSpec[@mode='add']"/>
	    <xsl:choose>
	      <xsl:when test="@start and contains(@start,' ')">
		<rng:start>
		  <rng:choice>
		    <xsl:call-template name="startNames">
		      <xsl:with-param name="toks" select="@start"/>
		    </xsl:call-template>
		  </rng:choice>
		</rng:start>
	      </xsl:when>
	      <xsl:when test="@start">
		<rng:start>
		  <rng:ref name="{@start}"/>
		</rng:start>
	      </xsl:when>
	    </xsl:choose>
	</grammar>
      </xsl:with-param>
    </xsl:call-template>
</xsl:template>

<xsl:template name="startNames">
  <xsl:param name="toks"/>
  <xsl:if test="not($toks='')">
    <xsl:choose>
      <xsl:when test="contains($toks,' ')">
	<rng:ref name="{substring-before($toks, ' ')}"/>
	<xsl:call-template name="startNames">
	  <xsl:with-param name="toks" select="substring-after($toks, ' ')"/>
	</xsl:call-template>
      </xsl:when>
      <xsl:otherwise>
	<rng:ref name="{$toks}"/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:if>
</xsl:template>

<xsl:template match="tei:moduleSpec" >
  <xsl:if test="@ident and not(@mode='change' or @mode='replace' or @mode='delete')">
    <xsl:variable name="filename" select="@ident"/>
    <xsl:call-template name="generateOutput">
      <xsl:with-param name="body">
	<grammar
	 xmlns="http://relaxng.org/ns/structure/1.0"
	 xmlns:t="http://www.thaiopensource.com/ns/annotations"
	 xmlns:a="http://relaxng.org/ns/compatibility/annotations/1.0"
	 datatypeLibrary="http://www.w3.org/2001/XMLSchema-datatypes">
	  
	  <xsl:text>
	    
	  </xsl:text> 
	  <xsl:comment>
	    <xsl:text>Schema generated </xsl:text>
	    <xsl:value-of  select="edate:date-time()"/>
	    <xsl:text>&#010;</xsl:text>

	    <xsl:call-template name="copyright"/>
	  </xsl:comment>
	  <xsl:text>
	    
	  </xsl:text>
	  
	  <xsl:if test="$filename='core'">
	    <rng:define name="IGNORE">
	      <rng:notAllowed/>
	    </rng:define>
	    <rng:define name="INCLUDE">
	      <rng:empty/>
	    </rng:define>
	    <xsl:comment>Weird special cases</xsl:comment>
	    <rng:define name="TEI...end"><rng:notAllowed/></rng:define>
	    <rng:define name="mix.dictionaries" combine="choice"><rng:choice><rng:notAllowed/></rng:choice></rng:define>
	    <rng:define name="mix.drama" combine="choice"><rng:choice><rng:notAllowed/></rng:choice></rng:define>
	    <rng:define name="mix.spoken" combine="choice"><rng:choice><rng:notAllowed/></rng:choice></rng:define>
	    <rng:define name="mix.verse"
			combine="choice"><rng:choice><rng:notAllowed/></rng:choice></rng:define>
	    <rng:define name="tei.comp.dictionaries" combine="choice"><rng:choice><rng:notAllowed/></rng:choice></rng:define>
	    <rng:define name="tei.comp.spoken" combine="choice"><rng:choice><rng:notAllowed/></rng:choice></rng:define>
	    <rng:define name="tei.comp.verse" combine="choice"><rng:choice><rng:notAllowed/></rng:choice></rng:define>
	    <rng:define name="tei.comp.terminology"><rng:choice><rng:notAllowed/></rng:choice></rng:define>
	    <rng:define name="mix.terminology" combine="choice"><rng:choice><rng:notAllowed/></rng:choice></rng:define>
	    <rng:define name="mix.seg"  combine="choice">
	      <rng:choice>
		<rng:notAllowed/>
	      </rng:choice>
	    </rng:define>
	  </xsl:if>
	  <xsl:if test="@type='core'">
	    <xsl:comment>0. predeclared classes</xsl:comment>
	    <xsl:for-each select="key('DefClasses',1)">
	      <xsl:choose>
		<xsl:when test="@type='model'">    
		  <xsl:apply-templates select="." mode="processModel"/>
		</xsl:when>
		<xsl:when test="@type='atts'">    
		  <xsl:apply-templates select="." mode="processDefaultAtts"/>
		</xsl:when>
		<xsl:when test="@type='default'">    
		  <xsl:apply-templates select="." mode="processDefaultAtts"/>
		</xsl:when>
		<xsl:when test="@type='both'">    
		  <xsl:apply-templates select="." mode="processDefaultAtts"/>
		</xsl:when>
	      </xsl:choose>
	    </xsl:for-each>
	  </xsl:if>
	  
	  <xsl:variable name="decl">
	    <xsl:value-of select="@ident"/>
	    <xsl:text>-decl</xsl:text>
	  </xsl:variable>
	  <xsl:if test="$verbose='true'">
	    <xsl:message>    Include contents of <xsl:value-of select="$decl"/></xsl:message>
	  </xsl:if>
	  <xsl:for-each select="key('DeclModules',$decl)">
	    <xsl:apply-templates select="key('ClassModule',$decl)"  mode="tangle"/>
	    
	    <xsl:apply-templates select="key('ElementModule',$decl)"  mode="tangle">      
	      <xsl:sort select="@ident"/>
	    </xsl:apply-templates>
	    
	    <xsl:apply-templates select="key('MacroModule',$decl)"
				 mode="tangle"/>
	    
	  </xsl:for-each>
	  <xsl:comment>1. classes</xsl:comment>
	  <xsl:apply-templates select="key('ClassModule',@ident)"  mode="tangle"/>
	  
	  <xsl:comment>2. elements</xsl:comment>
	  <xsl:apply-templates select="key('ElementModule',@ident)"  mode="tangle">      
	    <xsl:sort select="@ident"/>
	  </xsl:apply-templates>
	  <xsl:comment>3. macros</xsl:comment>
	  <xsl:apply-templates select="key('MacroModule',@ident)"
			       mode="tangle"/>
	  
	</grammar>
      </xsl:with-param>
    </xsl:call-template>
  </xsl:if>
</xsl:template>


<xsl:template match="tei:specGrpRef" mode="tangle">
  <xsl:param name="filename"/>
  <xsl:if test="$verbose='true'">
  <xsl:message>spec grp ref to <xsl:value-of
    select="@target"/></xsl:message>
  </xsl:if>
  <xsl:apply-templates select="key('IDS',@target)" mode="ok">
    <xsl:with-param name="filename" select="$filename"/>
  </xsl:apply-templates>
</xsl:template>

<xsl:template name="bitOut">
<xsl:param name="grammar"/>
<xsl:param name="TAG"/>
<xsl:param name="content"/>
<xsl:for-each  select="exsl:node-set($content)/Wrapper">
  <xsl:copy-of select="*"/>
</xsl:for-each>
</xsl:template>

<xsl:template name="refdoc"/>

<xsl:template name="typewriter"/>

<xsl:template name="ttembolden"/>

<xsl:template match="processing-instruction()"/>

<xsl:template match="processing-instruction()" mode="tangle"/>

<xsl:template match="s:*"/>

</xsl:stylesheet>


