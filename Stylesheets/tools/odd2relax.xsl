<?xml version="1.0" encoding="utf-8"?>
<!-- 
Text Encoding Initiative Consortium XSLT stylesheet family
$Date$, $Revision$, $Author$

XSL stylesheet to format TEI XML documents to HTML or XSL FO

 
##LICENSE
--> 
<xsl:stylesheet 
     xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
     xmlns:tei="http://www.tei-c.org/ns/1.0"
     xmlns:estr="http://exslt.org/strings"
     xmlns:t="http://www.thaiopensource.com/ns/annotations"
     xmlns:a="http://relaxng.org/ns/compatibility/annotations/1.0"
     xmlns:edate="http://exslt.org/dates-and-times"
     xmlns:exsl="http://exslt.org/common"
     xmlns:rng="http://relaxng.org/ns/structure/1.0"
     extension-element-prefixes="exsl estr edate"
     exclude-result-prefixes="exsl edate estr tei t a rng" 
     version="1.0">


<xsl:import href="../common/teiodds.xsl"/>

<xsl:output method="xml" indent="yes"/>

<xsl:key name="ENTS" match="tei:macroSpec" use="@ident"/>

<xsl:key name="PUBLICIDS" match="tei:publicID" use="@file"/>
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
  <xsl:output method="xml"/>
  <xsl:variable name="oddmode">dtd</xsl:variable>       
  <xsl:variable name="filesuffix"></xsl:variable>
<!-- get list of output files -->
<xsl:variable name="linkColor"/>



<xsl:template match="tei:module[@type='decls']" />

<xsl:template match="/tei:TEI">
    <xsl:apply-templates select=".//tei:schema" />
    <xsl:apply-templates select=".//tei:module" />
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

<xsl:template match="tei:module|tei:schema" >
  <xsl:if test="@ident and not(@mode='change' or @mode='replace' or @mode='delete')">
    <xsl:variable name="Master" select="."/>
    <xsl:variable name="filename" select="@ident"/>
    <xsl:call-template name="generateOutput">
      <xsl:with-param name="body">
      <grammar
       ns="http://www.tei-c.org/ns/1.0"
       xmlns="http://relaxng.org/ns/structure/1.0"
       xmlns:t="http://www.thaiopensource.com/ns/annotations"
       xmlns:a="http://relaxng.org/ns/compatibility/annotations/1.0"
       datatypeLibrary="http://www.w3.org/2001/XMLSchema-datatypes">
	
	<xsl:text>
	  
	</xsl:text> 
	
	<xsl:call-template name="copyright"/>
	
	<xsl:text>
	  
	</xsl:text>
	
	<xsl:if test="$filename='tei'">
	  <rng:start>
	    <rng:choice>
	      <rng:ref name="TEI"/>
	      <rng:ref name="teiCorpus"/>
	    </rng:choice>
	  </rng:start>
	</xsl:if>
	
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
	  <rng:define name="mix.verse"  combine="choice"><rng:choice><rng:notAllowed/></rng:choice></rng:define>
	  <rng:define name="tei.comp.dictionaries" combine="choice"><rng:choice><rng:notAllowed/></rng:choice></rng:define>
	  <rng:define name="tei.comp.spoken" combine="choice"><rng:choice><rng:notAllowed/></rng:choice></rng:define>
	  <rng:define name="tei.comp.verse" combine="choice"><rng:choice><rng:notAllowed/></rng:choice></rng:define>
	  <rng:define name="tei.comp.terminology"><rng:choice><rng:notAllowed/></rng:choice></rng:define>
	  <rng:define name="mix.terminology" combine="choice"><rng:choice><rng:notAllowed/></rng:choice></rng:define>
	</xsl:if>
	
	<!-- bits flagged as part of NAME-decls modules are included in
	     the module for schema purposes, and the -decls ones are ignored -->
	<xsl:for-each select="exsl:node-set($Master)">
    <xsl:if test="$verbose='true'">
<xsl:message>    Look for <xsl:value-of
select="$filename"/>-decl</xsl:message>
    </xsl:if>
	  <xsl:for-each
	   select="key('FILES',concat($filename,'-decl'))">
<xsl:message>Found <xsl:value-of select="$filename"/>-decl</xsl:message>
	    <xsl:apply-templates mode="tangle">
	      <xsl:with-param name="filename" select="$filename"/>
	    </xsl:apply-templates>
	  </xsl:for-each>
	</xsl:for-each>
	<xsl:apply-templates mode="tangle">
	  <xsl:with-param name="filename" select="$filename"/>
	</xsl:apply-templates>
	
      </grammar>
      </xsl:with-param>
    </xsl:call-template>
  </xsl:if>
</xsl:template>

<xsl:template match="tei:gloss|tei:remarks|tei:desc"/>

<xsl:template match="rng:optional">
  <xsl:if test="not(count(rng:*)=1 and rng:attribute[@name='xmlns'])">  
    <rng:optional>
      <xsl:copy-of select="@*"/>
      <xsl:apply-templates/>
    </rng:optional>
  </xsl:if>
</xsl:template>


<xsl:template match="tei:commDecl" mode="tangle">
  <xsl:text>
</xsl:text>
<xsl:comment><xsl:apply-templates/></xsl:comment>
  <xsl:text>
</xsl:text>
</xsl:template>


<xsl:template match="tei:msection" mode="tangle">
  <xsl:param name="filename"/>
  <xsl:message>     .... msection [<xsl:value-of select="$filename"/>] <xsl:value-of select="@keywords"/></xsl:message>
  <xsl:apply-templates mode="msectiondtd">
    <xsl:with-param name="filename" select="$filename"/>
  </xsl:apply-templates>
</xsl:template>

<xsl:template match="tei:moduleRef" mode="msectiondtd"/>

<xsl:template match="tei:macroSpec" mode="msectiondtd">
  <xsl:param name="filename"/>
  <xsl:message>     .. patterndoc [msection] <xsl:value-of select="@id"/></xsl:message>
  <xsl:if test="preceding-sibling::tei:macroSpec">
    <xsl:text>
</xsl:text>
  </xsl:if>
  <xsl:apply-templates select="." mode="tangle">
    <xsl:with-param name="msection" select="parent::tei:msection/@keywords"/>
    <xsl:with-param name="filename" select="$filename"/>
  </xsl:apply-templates>
</xsl:template>

<xsl:template match="tei:commDecl" mode="msectiondtd"/>

<xsl:template match="tei:classSpec" mode="tagatts">
<xsl:message>      .... link to attributes from class [<xsl:value-of select="@ident"/>]</xsl:message>
   <rng:ref name="{@ident}.attributes"/>
</xsl:template>


<xsl:template match="tei:specGrpRef" mode="tangle">
  <xsl:param name="filename"/>

<xsl:message>spec grp ref to <xsl:value-of select="@target"/></xsl:message>
  <xsl:apply-templates select="key('IDS',@target)" mode="ok">
      <xsl:with-param name="filename" select="$filename"/>
  </xsl:apply-templates>
</xsl:template>

<xsl:template match="processing-instruction()"/>

<xsl:template match="processing-instruction()" mode="tangle"/>

<xsl:template match="tei:defaultVal"/>



<xsl:template match="rng:ref">
  <xsl:if test="not(@name='TEI...end')">
    <xsl:copy-of select="."/>
  </xsl:if>
</xsl:template>
<!--
  <xsl:choose>
    <xsl:when test="@name='tei.common' and ancestor::tei:macroSpec[@ident='component']">
    </xsl:when>
    <xsl:otherwise>
      <xsl:message>TRY         <xsl:value-of select="@name"/>: <xsl:value-of select="ancestor::tei:macroSpec/@ident"/>      </xsl:message>

    </xsl:otherwise>
  </xsl:choose>
-->

<xsl:template match="text()">
    <xsl:value-of select="."/> <!-- could normalize() here -->
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
</xsl:stylesheet>


