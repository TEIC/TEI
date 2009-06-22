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
   omit-xml-declaration="yes"/>
<xsl:key name="D" match="rng:define" use="@name"/>

<xsl:key name="E" match="rng:element" use="@name"/>

<xsl:key name="DLIST" match="rng:define" use="'1'"/>

<xsl:template match="/">
  <xsl:for-each select="key('DLIST','1')">
    <xsl:choose>
      <xsl:when test="rng:element">
	<xsl:for-each select="rng:element">
	  <tei:elementSpec ident="{@name}">
	      <xsl:for-each select="a:documentation">
	      <xsl:choose>
		<xsl:when test="starts-with(.,'(')">
		  <tei:gloss xml:lang="ja">
		    <xsl:value-of select="substring-before(substring-after(.,'('),')')"/>
		  </tei:gloss>
		  <tei:desc xml:lang="ja">
		    <xsl:value-of select="substring-after(.,')')"/>
		  </tei:desc>
		</xsl:when>
		<xsl:otherwise>
		  <tei:desc xml:lang="ja">
		    <xsl:value-of select="."/>
		  </tei:desc>
		</xsl:otherwise>
	      </xsl:choose>
	      </xsl:for-each>
	    <tei:attList>
	      <xsl:for-each select="key('D',concat(@name,'.attributes'))">
		<xsl:for-each select=".//rng:attribute">
		  <xsl:call-template name="goAtt"/>
		</xsl:for-each>
	      </xsl:for-each>
	    </tei:attList>
	  </tei:elementSpec>
	</xsl:for-each>
      </xsl:when>
      <xsl:when test="starts-with(@name,'model.')"/>
      <xsl:when test="@combine='choice'"/>
      <xsl:when test="starts-with(@name,'data.')"/>
      <xsl:when test="starts-with(@name,'macro.')"/>
      <xsl:when test="starts-with(@name,'mix.')"/>
      <xsl:when test="contains(@name,'.attribute.')"/>
      <xsl:when test="contains(@name,'.content')"/>
      <xsl:when test="rng:ref and starts-with(@name,'att.') and contains(@name,'.attributes')">
	<tei:classSpec ident="{substring-before(@name,'.attributes')}">
	  <tei:attList>
	    <xsl:for-each select="rng:ref">
	      <xsl:for-each select="key('D',@name)">
		<xsl:for-each select=".//rng:attribute">
		  <xsl:call-template name="goAtt"/>
		</xsl:for-each>	      
	      </xsl:for-each>
	    </xsl:for-each>
	  </tei:attList>
	</tei:classSpec>
      </xsl:when>
      <xsl:otherwise>
	<xsl:message>what is <xsl:value-of select="@name"/></xsl:message>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:for-each>
</xsl:template>


<xsl:template name="goAtt">
  <tei:attDef ident="{@name}">
    <tei:desc xml:lang="ja">
      <xsl:value-of select="a:documentation"/>
    </tei:desc>
    <xsl:if test="rng:choice/rng:value">
	<tei:valList type="closed">
	  <xsl:for-each select="rng:choice/rng:value">
	    <tei:valItem ident="{.}">
	      <tei:gloss>
		<xsl:value-of
		    select="following-sibling::a:documentation[1]"/>
	      </tei:gloss>
	    </tei:valItem>
	  </xsl:for-each>
	</tei:valList>
    </xsl:if>
  </tei:attDef>
</xsl:template>

</xsl:stylesheet>
