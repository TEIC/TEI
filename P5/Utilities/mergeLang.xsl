<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet 
    xmlns:xd="http://www.pnp-software.com/XSLTdoc"
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
    exclude-result-prefixes="exsl estr edate teix fo a tei xs rng s xd" 
    extension-element-prefixes="edate exsl estr"
    version="1.0">

<xsl:output 
   method="xml"
   indent="yes"
   encoding="utf-8"
   cdata-section-elements="tei:eg"
   omit-xml-declaration="yes"/>

  <xsl:key name="IDENTS" use="@ident"
	   match="tei:elementSpec"/>
  <xsl:key name="IDENTS" use="@ident"
	   match="tei:classSpec"/>
  <xsl:key name="IDENTS" use="@ident"
	   match="tei:macroSpec"/>

  <xsl:key name="IDENTS"   use="concat(ancestor::tei:elementSpec/@ident,'_',@ident)"
	   match="tei:elementSpec/tei:attList/tei:attDef"/>

  <xsl:key name="IDENTS"   use="concat(ancestor::tei:classSpec/@ident,'_',@ident)"
	   match="tei:classSpec/tei:attList/tei:attDef"/>

  <xsl:key name="IDENTS"   
	   use="concat(ancestor::tei:elementSpec/@ident,'_',ancestor::tei:attDef/@ident,'_',@ident)"
	   match="tei:elementSpec/tei:attList/tei:attDef/tei:valList/tei:valItem"/>

  <xsl:key name="IDENTS"   
	   use="concat(ancestor::tei:classSpec/@ident,'_',ancestor::tei:attDef/@ident,'_',@ident)"
	   match="tei:classSpec/tei:attList/tei:attDef/tei:valList/tei:valItem"/>


  <xsl:param name="newFile"/>
  <xsl:param name="newLang"/>

  <xsl:output encoding="utf-8" indent="yes"/>

  <xsl:variable name="Original" select="/"/>

  <xsl:variable name="New" select="document($newFile)"/>

  <xsl:template match="/">
    <xsl:apply-templates/>
  </xsl:template>

<xsl:template match="@*|processing-instruction()|comment()|text()">
  <xsl:copy/>
</xsl:template>

<xsl:template match="*">
  <xsl:copy>
    <xsl:apply-templates
	select="*|@*|processing-instruction()|comment()|text()"/>
  </xsl:copy>
</xsl:template>

<xsl:template match="tei:gloss">
  <xsl:copy-of select="."/>
  <xsl:if test="not(preceding-sibling::tei:gloss)">

    <xsl:variable name="this">
      <xsl:value-of select="normalize-space(.)"/>
    </xsl:variable>

    <xsl:variable name="What">
      <xsl:choose>
	<xsl:when test="parent::tei:attDef">
	  <xsl:value-of
	      select="concat(ancestor::tei:elementSpec/@ident|ancestor::tei:classSpec/@ident,'_',../@ident)"/>
	</xsl:when>
	<xsl:when test="parent::tei:valItem">
	  <xsl:value-of
	      select="concat(ancestor::tei:elementSpec/@ident|ancestor::tei:classSpec/@ident,'_',ancestor::tei:attDef/@ident,'_',../@ident)"/>
	</xsl:when>
	<xsl:otherwise>
	  <xsl:value-of select="../@ident"/>
	</xsl:otherwise>
      </xsl:choose>
    </xsl:variable>
    <xsl:for-each select="$New">
      <xsl:for-each select="key('IDENTS',$What)/tei:gloss">
	<xsl:variable name="that">
	  <xsl:choose>
	    <xsl:when test="starts-with(.,'(')">
	      <xsl:value-of select="substring-before(substring-after(normalize-space(.),'('),')')"/>
	    </xsl:when>
	    <xsl:otherwise>
	      <xsl:value-of select="normalize-space(.)"/>
	    </xsl:otherwise>
	  </xsl:choose>
	</xsl:variable>
<!--<xsl:message>look for <xsl:value-of select="$What"/> giving <xsl:value-of select="$that"/></xsl:message>-->
	<xsl:if test="not($that=$this) and not($that='')">
  <xsl:variable name="date">
    <xsl:choose>
      <xsl:when test="@notBefore">
	<xsl:value-of select="@notBefore"/>
      </xsl:when>
      <xsl:otherwise>
	<xsl:value-of
	    select="substring-before(edate:date-time(),'T')"/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:variable>
	  <gloss xmlns="http://www.tei-c.org/ns/1.0"
		 version="{$date}"
		 >
	    <xsl:attribute name="xml:lang">
	      <xsl:value-of select="$newLang"/>
	    </xsl:attribute>
	    <xsl:apply-templates/>
	  </gloss>
	</xsl:if>
      </xsl:for-each>
    </xsl:for-each>
  </xsl:if>
</xsl:template>

<xsl:template match="tei:desc">
  <xsl:copy-of select="."/>
  <xsl:if test="not(preceding-sibling::tei:desc)">
    <xsl:variable name="this">
      <xsl:value-of select="normalize-space(.)"/>
    </xsl:variable>

    <xsl:variable name="What">
      <xsl:choose>
	<xsl:when test="parent::tei:attDef">
	  <xsl:value-of
	      select="concat(ancestor::tei:elementSpec/@ident|ancestor::tei:classSpec/@ident,'_',../@ident)"/>
	</xsl:when>
	<xsl:otherwise>
	  <xsl:value-of select="../@ident"/>
	</xsl:otherwise>
      </xsl:choose>
    </xsl:variable>

    <xsl:for-each select="$New">
      <xsl:for-each select="key('IDENTS',$What)/tei:desc">
	<xsl:variable name="that">
	  <xsl:value-of select="normalize-space(.)"/>
	</xsl:variable>
	<xsl:if test="not($that=$this) and not($that='')">
	  <xsl:variable name="date">
	    <xsl:choose>
	      <xsl:when test="@notBefore">
		<xsl:value-of select="@notBefore"/>
	      </xsl:when>
	      <xsl:otherwise>
		<xsl:value-of
		    select="substring-before(edate:date-time(),'T')"/>
	      </xsl:otherwise>
	    </xsl:choose>
	  </xsl:variable>
	  <desc xmlns="http://www.tei-c.org/ns/1.0"
		version="{$date}">
	    <xsl:attribute name="xml:lang">
	      <xsl:value-of select="$newLang"/>
	    </xsl:attribute>
	    <xsl:apply-templates/>
	  </desc>
	</xsl:if>
      </xsl:for-each>
    </xsl:for-each>
  </xsl:if>
</xsl:template>

<xsl:template match="tei:remarks">
  <xsl:copy-of select="."/>
  <xsl:if test="not(preceding-sibling::tei:remarks)">
    <xsl:variable name="this">
      <xsl:value-of select="normalize-space(.)"/>
    </xsl:variable>
    <xsl:variable name="What" select="concat(local-name(..),../@ident)"/>
    <xsl:for-each select="$New">
      <xsl:for-each select="key('IDENTS',$What)/tei:remarks">
	<xsl:variable name="that">
	  <xsl:value-of select="normalize-space(.)"/>
	</xsl:variable>
	<xsl:if test="not($that=$this) and not($that='')">
	  <remarks xmlns="http://www.tei-c.org/ns/1.0">
	    <xsl:attribute name="xml:lang">
	      <xsl:value-of select="$newLang"/>
	    </xsl:attribute>
	    <xsl:apply-templates/>
	  </remarks>
	</xsl:if>
      </xsl:for-each>
    </xsl:for-each>
 </xsl:if>
</xsl:template>

<xsl:template match="tei:exemplum">
  <xsl:copy-of select="."/>
  <xsl:if test="not(preceding-sibling::tei:exemplum)">
  <xsl:variable name="this">
    <xsl:value-of select="normalize-space(.)"/>
  </xsl:variable>
  <xsl:variable name="What" select="concat(local-name(..),../@ident)"/>
  <xsl:for-each select="$New">
    <xsl:for-each select="key('IDENTS',$What)/tei:exemplum">
      <xsl:variable name="that">
	<xsl:value-of select="normalize-space(.)"/>
      </xsl:variable>
      <xsl:if test="not($that=$this) and not($that='')">
	<exemplum xmlns="http://www.tei-c.org/ns/1.0">
	  <xsl:attribute name="xml:lang">
	    <xsl:value-of select="$newLang"/>
	  </xsl:attribute>
	  <xsl:apply-templates/>
	</exemplum>
      </xsl:if>
    </xsl:for-each>
  </xsl:for-each>
  </xsl:if>
</xsl:template>


</xsl:stylesheet>
