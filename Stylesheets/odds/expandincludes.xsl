<?xml version="1.0" encoding="utf-8"?>
<!-- separate bits by David Tolpin, combined by Sebastian Rahtz January 2004 


expandincludes.xsl

Take a Relax NG spec and simplify it to remove
rng:includes

-->
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
  xmlns:s="http://www.ascc.net/xml/schematron"
  xmlns:exsl="http://exslt.org/common"
  extension-element-prefixes="exsl"
  xmlns:xsp="http://apache.org/xsp/core/v1"
  xmlns:xs="http://www.w3.org/2001/XInclude"
  xmlns:a="http://relaxng.org/ns/compatibility/annotations/1.0" 
  xmlns:f="http://axkit.org/NS/xsp/perform/v1" 
  xmlns:tei="http://www.tei-c.org/ns/1.0" 
  exclude-result-prefixes="exsl rng a f tei s" 
  xmlns:rng="http://relaxng.org/ns/structure/1.0" 
  version="1.0">

<xsl:output indent="yes"/>

<xsl:template match="/">
  <xsl:variable name="step1">
    <xsl:apply-templates/>
  </xsl:variable>
  <!--
    <exsl:document href="temp.rng" method="xml" indent="yes">
    <xsl:copy-of select="$step1"/>
    </exsl:document>
  -->
  <xsl:apply-templates select="exsl:node-set($step1)" mode="stage2"/>
  
</xsl:template>

<xsl:template match="rng:include">

  <rng:div><xsl:text>
  </xsl:text><xsl:comment>include "<xsl:value-of select="@href"/>"</xsl:comment><xsl:text>
  </xsl:text>	
  <xsl:apply-templates select="*|@*[name()!='href']|text()|comment()"/>
  <rng:include>
    <xsl:for-each select="document(@href,.)/rng:grammar">
      <xsl:apply-templates select="*|@*|text()|comment()"/>
    </xsl:for-each>
  </rng:include>
  </rng:div>
</xsl:template>

<xsl:template match="rng:start" mode="stage2">
  <xsl:if test="not(preceding::rng:start)">
    <xsl:copy>
      <xsl:apply-templates mode="stage2"/>
    </xsl:copy>
  </xsl:if>
</xsl:template>

<xsl:template match="rng:include" mode="stage2">
  <rng:div>
    <xsl:apply-templates  select="*|@*|text()|comment()" mode="stage2"/>
  </rng:div>
</xsl:template>

<xsl:template match="rng:define[not(@combine='choice')]" mode="stage2">
  <!-- can be overriden -->
  <!-- find if there is an overriding definition:
       two dimensional recursion - by ancestor::incelim,
       then by children of incelim, starting with 2 -->
  <xsl:call-template name="cp-unless-ovr">
    <xsl:with-param name="incelim" select="ancestor::rng:include[1]"/>
    <xsl:with-param name="define" select="."/>
  </xsl:call-template>
</xsl:template>

<xsl:template name="cp-unless-ovr">
  <xsl:param name="incelim"/>
  <xsl:param name="define"/>
  <xsl:choose>
    <xsl:when test="$incelim
	      and generate-id($define/ancestor::rng:grammar[1])
	      = generate-id($incelim/ancestor::rng:grammar[1])">
      
      <xsl:if test="not(
	      $incelim/preceding-sibling::*/descendant-or-self::rng:define[
	      @name=$define/@name
	      and generate-id(ancestor::rng:grammar[1])
	      = generate-id($incelim/ancestor::rng:grammar[1])])">
	<xsl:call-template name="cp-unless-ovr">
	  <xsl:with-param name="incelim"
			  select="$incelim/ancestor::rng:include[1]"/>
	  <xsl:with-param name="define" select="$define"/>
	</xsl:call-template>
      </xsl:if>
    </xsl:when>
    
    <xsl:otherwise>
      <rng:define>
	<xsl:for-each select="$define">
	  <xsl:apply-templates  select="*|@*|text()|comment()" mode="stage2"/>
	</xsl:for-each>
      </rng:define>
    </xsl:otherwise>
  </xsl:choose>
</xsl:template>

<!-- dull stuff, just copying -->

<xsl:template match="*|@*|text()|comment()">
  <xsl:copy>
    <xsl:apply-templates select="*|@*|text()|comment()"/>
  </xsl:copy>
</xsl:template>

<xsl:template match="*|@*|text()|comment()" mode="stage2">
  <xsl:copy>
    <xsl:apply-templates select="*|@*|text()|comment()" mode="stage2"/>
  </xsl:copy>
</xsl:template>

<xsl:template match="rng:grammar">
  <rng:grammar xmlns:xlink="http://www.w3.org/1999/xlink"
	       xmlns:xsp="http://apache.org/xsp/core/v1"
	       xmlns:xs="http://www.w3.org/2001/XInclude">
    <xsl:if test="not(@datatypeLibrary)">
      <xsl:attribute name="datatypeLibrary">
	<xsl:text>http://www.w3.org/2001/XMLSchema-datatypes</xsl:text>
      </xsl:attribute>
    </xsl:if>
    <xsl:apply-templates select="*|@*|text()|comment()"/>
  </rng:grammar>
</xsl:template>


</xsl:stylesheet>
