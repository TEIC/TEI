<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet 
 xmlns:rng="http://relaxng.org/ns/structure/1.0"
  xmlns:tei="http://www.tei-c.org/ns/1.0"
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
  xmlns:exsl="http://exslt.org/common"
  extension-element-prefixes="exsl"
  exclude-result-prefixes="tei exsl" 
  version="1.0">
  <xsl:output method="xml" indent="yes"/>
<xsl:key name="TAGMODS" match="Tag|AttClass" use="Tagset"/>
<xsl:key name="MODS" match="tei:moduleRef" use="@key"/>
<xsl:output method="xml" indent="yes"/>
<xsl:param name="lang">es</xsl:param>
<xsl:key name="ELEMENTS" match="element" use="@ident"/>
<xsl:key name="ATTRIBUTES" match="attribute" use="@ident"/>
<xsl:param name="TEITAGS">http://www.tei-c.org.uk/tei-bin/files.pl?name=tags.xml</xsl:param>
<xsl:param name="TEINAMES">http://www.tei-c.org.uk/tei-bin/files.pl?name=teinames.xml</xsl:param>

<xsl:template match="tei:*|rng:*">
  <xsl:copy>
    <xsl:apply-templates select="@*|*|text()|comment()"/>
  </xsl:copy>
</xsl:template>

<xsl:template match="@*|comment()|text()">
  <xsl:copy/>
</xsl:template>

<xsl:template match="tei:elementSpec[@mode='change']">
  <xsl:copy>
    <xsl:apply-templates select="@*|*|text()|comment()"/>
  </xsl:copy>
</xsl:template>

<xsl:template match="tei:schema">
  <xsl:for-each select="tei:moduleRef">
    <xsl:variable name="test" select="@key"/>
    <xsl:if test="not(key('MODS',$test))">
	  <moduleRef  xmlns="http://www.tei-c.org/ns/1.0" mode="change" ident="{$test}">
	  <xsl:call-template name="findTranslateNames">
	    <xsl:with-param name="modname">
	      <xsl:value-of select="$test"/>
	    </xsl:with-param>
	  </xsl:call-template>
	  </moduleRef>
    </xsl:if>
  </xsl:for-each>
  <xsl:copy>
    <xsl:apply-templates select="@*|*|text()|comment()"/>
  </xsl:copy>
</xsl:template>

<xsl:template match="tei:moduleRef">
  <xsl:copy>
    <xsl:apply-templates select="@*|*|text()|comment()"/>
	<xsl:call-template name="findTranslateNames">
	  <xsl:with-param name="modname">
	    <xsl:value-of select="@key"/>
	  </xsl:with-param>
	</xsl:call-template>
  </xsl:copy>
</xsl:template>

<xsl:template name="findTranslateNames">
  <xsl:param name="modname"/>
  <xsl:variable name="HERE" select="."/>

  <xsl:for-each
   select="document($TEITAGS)/Table">
    <xsl:for-each select="key('TAGMODS',$modname)">
      <xsl:variable name="thisthing" select="ident"/>
      <xsl:variable name="ename">	
	<xsl:choose>
	  <xsl:when test="$HERE/tei:elementSpec[@ident=$thisthing]">
	    <xsl:if
	     test="$HERE/tei:elementSpec[@ident=$thisthing]/tei:attList">
	    </xsl:if>
	  </xsl:when>
	  <xsl:otherwise>
	    <xsl:for-each  select="document($TEINAMES)">
	      <xsl:for-each select="key('ELEMENTS',$thisthing)">
		<xsl:if test="equiv[@lang=$lang]">
		  <altIdent type="lang" xmlns="http://www.tei-c.org/ns/1.0">
		  <xsl:value-of select="equiv[@lang=$lang]/@value"/>
		  </altIdent>
		</xsl:if>
	      </xsl:for-each>
	    </xsl:for-each>
	  </xsl:otherwise>
	</xsl:choose>
      </xsl:variable>
      <xsl:variable name="aname">
	<xsl:choose>
	  <xsl:when test="$HERE/tei:elementSpec[@ident=$thisthing]">
	    <xsl:if
	     test="$HERE/tei:elementSpec[@ident=$thisthing]/tei:attList">
	    </xsl:if>
	  </xsl:when>
	  <xsl:otherwise>
	<xsl:if test="Attributes">
	  <attList xmlns="http://www.tei-c.org/ns/1.0">
	    <xsl:for-each select="Attributes/Att">
	      <xsl:variable name="thisatt" select="@n"/>
	      <xsl:for-each  select="document($TEINAMES)">
		<xsl:for-each select="key('ATTRIBUTES',$thisatt)">
		  <xsl:if test="equiv[@lang=$lang]">
		    <attDef mode="change" xmlns="http://www.tei-c.org/ns/1.0" ident="{$thisatt}"> 
		      <altIdent type="lang" xmlns="http://www.tei-c.org/ns/1.0">
			<xsl:value-of select="equiv[@lang=$lang]/@value"/>
		      </altIdent>
		    </attDef>
		  </xsl:if>
		</xsl:for-each>
	      </xsl:for-each>
	    </xsl:for-each>
	  </attList>
	</xsl:if>
	  </xsl:otherwise>
	</xsl:choose>
      </xsl:variable>
      <xsl:if test="string-length($aname)&gt;0 or
	      string-length($ename)&gt;0">
	<xsl:choose>
	  <xsl:when test="starts-with($thisthing,'tei.')">
	    <classSpec ident="{$thisthing}" mode="change" xmlns="http://www.tei-c.org/ns/1.0">
	      <xsl:copy-of select="$aname"/>
	    </classSpec>
	  </xsl:when>
	  <xsl:otherwise>
	    <elementSpec ident="{$thisthing}" mode="change" xmlns="http://www.tei-c.org/ns/1.0">
	      <xsl:copy-of select="$ename"/>
	      <xsl:copy-of select="$aname"/>
	    </elementSpec>
	  </xsl:otherwise>
	</xsl:choose>
      </xsl:if>
    </xsl:for-each>
  </xsl:for-each>
</xsl:template>


<xsl:template name="findAttributes">

</xsl:template>

</xsl:stylesheet>
