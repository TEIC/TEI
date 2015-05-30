<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet 
    xmlns:dbk="http://docbook.org/ns/docbook"
    xmlns:html="http://www.w3.org/1999/xhtml"
    xmlns:xlink="http://www.w3.org/1999/xlink"
    xmlns:its="http://www.w3.org/2005/11/its"
    xmlns:eg="http://www.tei-c.org/ns/Examples"
    xmlns:xd="http://www.pnp-software.com/XSLTdoc"
    xmlns:a="http://relaxng.org/ns/compatibility/annotations/1.0"
    xmlns:rng="http://relaxng.org/ns/structure/1.0"
    xmlns:xs="http://www.w3.org/2001/XMLSchema" 
    xmlns:tei="http://www.tei-c.org/ns/1.0"
    xmlns:teix="http://www.tei-c.org/ns/Examples"
    xmlns:local="http://www.pantor.com/ns/local"
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    xmlns:fo="http://www.w3.org/1999/XSL/Format"
    xmlns:edate="http://exslt.org/dates-and-times"
    xmlns:exsl="http://exslt.org/common"
    xmlns:estr="http://exslt.org/strings"
    xmlns:spec="http://example.com/xmlspec"
    exclude-result-prefixes="exsl estr edate fo a xd tei rng local teix xs eg its html dbk xlink" 
    extension-element-prefixes="edate exsl estr spec"
    version="1.0">


  <xsl:import href="/usr/share/xml/tei/stylesheet/odds/teiodds.xsl"/>

  <xsl:output method="xml" 
	      omit-xml-declaration="yes"
	      encoding="utf-8" />
  
  <xsl:param name="oddmode">tei</xsl:param>
  <xsl:param name="outputDir">.</xsl:param>

<xsl:key name="ELEMENTS"   match="tei:elementSpec" use="1"/>

<xsl:key name="CLASSREFS"   match="tei:elementSpec"
	   use="tei:classes/tei:memberOf/@key"/>
<xsl:key name="CLASSREFS"   match="tei:classSpec"
	   use="tei:classes/tei:memberOf/@key"/>

  
<xsl:param name="displayMode">rnc</xsl:param>
<xsl:variable name="top" select="/"/>

<xsl:template match="/rng:grammar">
    <xsl:call-template name="make-body-from-r-t-f">
      <xsl:with-param name="schema">
	  <xsl:call-template name="make-compact-schema"/>
      </xsl:with-param>
    </xsl:call-template>
</xsl:template>



<!-- rng to rnc mangling -->

<xsl:template match="ref" mode="flatten">
  <xsl:variable name="me">
    <xsl:choose>
      <xsl:when test="contains(@name,'.attributes')">
	<xsl:value-of select="substring-before(@name,'.attributes')"/>
      </xsl:when>
      <xsl:when test="contains(@name,'.attribute.')">
	<xsl:value-of select="substring-before(@name,'.attribute.')"/>
      </xsl:when>
      <xsl:otherwise>
	<xsl:value-of select="@name"/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:variable>
  <xsl:variable name="n" select="@name"/>
  
  <xsl:choose>
    <xsl:when test="contains($n,'.content')">
      <xsl:value-of select="$n"/>
    </xsl:when>
    <xsl:otherwise>
      <xsl:call-template name="linkTogether">
	<xsl:with-param name="name"><xsl:value-of select="$me"/></xsl:with-param>
	<xsl:with-param name="reftext"><xsl:value-of select="$n"/></xsl:with-param>
      </xsl:call-template>
    </xsl:otherwise>
  </xsl:choose>
</xsl:template>


  <xsl:template match="prod|lhs|rhs" mode="keep">
    <xsl:element name="{local-name(.)}">
      <xsl:copy-of select="@*"/>
      <xsl:apply-templates mode="keep"/>
    </xsl:element>
 </xsl:template>

  <xsl:template match="prod|lhs|rhs" mode="RNC">
    <xsl:element name="{local-name(.)}">
      <xsl:copy-of select="@*"/>
      <xsl:apply-templates mode="RNC"/>
    </xsl:element>
 </xsl:template>

  <xsl:template match="prod|lhs|rhs" mode="flatten">
    <xsl:element name="{local-name(.)}">
      <xsl:copy-of select="@*"/>
      <xsl:apply-templates mode="flatten"/>
    </xsl:element>
 </xsl:template>

  <xsl:template match="rng:define[contains(@name,'.att')]"
		mode="RNC"/>

  <xsl:template match="rng:define"  mode="RNC">
    <xsl:variable name="grammar" select="ancestor::rng:grammar [1]"/>
    <xsl:call-template name="annotations"/>
    <group>
      <define name="{@name}">
	<xsl:copy-of select="@combine"/>
	<xsl:call-template name="text">
	  <xsl:with-param name="text">
	    <xsl:call-template name="quote-keyword">
	      <xsl:with-param name="name" select="@name"/>
	    </xsl:call-template>
	  </xsl:with-param>
	</xsl:call-template>
      </define>
      <xsl:text> = </xsl:text>
      <xsl:choose>
	<xsl:when test="@combine = 'choice'"><t size="3"> |=</t></xsl:when>
	<xsl:when test="@combine = 'interleave'">
	  <t size="3"> &amp;=</t>
	</xsl:when>
      </xsl:choose>
      <indent>
	<nl size="1"/>
	<xsl:call-template name="group-body"/>
      </indent>
      <xsl:text> ; </xsl:text>
    </group>

    <xsl:call-template name="spacer"/>
  </xsl:template>


  <xsl:template name="linkTogether">
    <xsl:param name="name"/>
    <xsl:param name="reftext"/>
    <xsl:variable name="link">
      <xsl:choose>
	<xsl:when test="$reftext=''"><xsl:value-of select="$name"/></xsl:when>
	<xsl:otherwise><xsl:value-of select="$reftext"/></xsl:otherwise>
      </xsl:choose>
    </xsl:variable>
    <xsl:value-of select="$reftext"/>
  </xsl:template>

<!-- rewrite RNC as EBNF -->
  <xsl:template name="make-parenthesis">
    <xsl:param name="body"/>
    <indent>
      <nl size="1"/>
      <xsl:copy-of select="$body"/>
    </indent>
    <nl size="1"/>
  </xsl:template>

  <xsl:template match="rng:group" mode="RNC">
    <xsl:call-template name="expression">
      <xsl:with-param name="operator" select="' '"/>
    </xsl:call-template>
  </xsl:template>

  <xsl:template match="rng:choice" mode="RNC" name="make-choice">
    <xsl:text>(</xsl:text>
    <xsl:call-template name="expression">
      <xsl:with-param name="operator" select="'| '"/>
      <xsl:with-param name="is-prefix-operator" select="true ()"/>
    </xsl:call-template>
    <xsl:text>)</xsl:text>
  </xsl:template>

  <xsl:template match="rng:optional" mode="RNC">
    <xsl:text>[</xsl:text>
    <xsl:call-template name="expression">
      <xsl:with-param name="operator" select="' '"/>
    </xsl:call-template>
    <xsl:text>]</xsl:text>
  </xsl:template>

  <xsl:template match="rng:zeroOrMore" mode="RNC">
    <xsl:text>{</xsl:text>
    <xsl:call-template name="expression">
      <xsl:with-param name="operator" select="' '"/>
    </xsl:call-template>
    <xsl:text>}</xsl:text>
  </xsl:template>

  <xsl:template match="rng:oneOrMore" mode="RNC">
    <xsl:call-template name="expression">
      <xsl:with-param name="operator" select="' '"/>
    </xsl:call-template>
    <xsl:text>+</xsl:text>
  </xsl:template>

  <xsl:template name="quote-keyword">
    <xsl:param name="name"/>
    <xsl:value-of select="$name"/>
  </xsl:template>

  <xsl:template name="group-body">
    <xsl:param name="patterns" select="rng:*"/>
    <xsl:call-template name="expression-body">
      <xsl:with-param name="patterns" select="$patterns"/>
      <xsl:with-param name="operator" select="' '"/>
    </xsl:call-template>
  </xsl:template>

  <xsl:template match="rng:text" mode="RNC">
    <xsl:text>cdata</xsl:text>
  </xsl:template>

  <xsl:template match="rng:data" mode="RNC">
    <xsl:text>XSD_</xsl:text>
    <xsl:value-of select="@type"/>
  </xsl:template>

</xsl:stylesheet>
