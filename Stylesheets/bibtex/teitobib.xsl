<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                xpath-default-namespace="http://www.tei-c.org/ns/1.0"
                version="2.0">
  <xsl:output method="text"/>

  <xsl:key name="B" match="biblStruct" use="1"/>
    <xsl:strip-space elements="*"/>

  <xsl:template match="/">

    <xsl:for-each select="key('B',1)">
      <xsl:variable name="type">
	<xsl:choose>
	  <xsl:when test="idno[@type='url']">
	    <xsl:text>techreport</xsl:text>
	  </xsl:when>
	  <xsl:when test="not(analytic)">
	    <xsl:text>book</xsl:text>
	  </xsl:when>
	  <xsl:when test="monogr/imprint/biblScope[@type='volume']">
	    <xsl:text>article</xsl:text>	    
	  </xsl:when>
	  <xsl:otherwise>
	    <xsl:text>incollection</xsl:text>	    
	  </xsl:otherwise>
	</xsl:choose>
      </xsl:variable>
      <xsl:text>@</xsl:text>
      <xsl:value-of select="$type"/>
      <xsl:text>{</xsl:text>
      <xsl:value-of select="@xml:id"/>
      <xsl:text>,&#10;</xsl:text>
      <xsl:variable name="all">
	<xsl:apply-templates/>
      </xsl:variable>
      <xsl:for-each select="tokenize($all,'@')">
	<xsl:if test="not(.='')">
	  <xsl:text>	</xsl:text>
	  <xsl:value-of select="."/>
	  <xsl:if test="not(position()=last())">,</xsl:if>
	  <xsl:text>&#10;</xsl:text>
	</xsl:if>
	</xsl:for-each>
      <xsl:text>}&#10;&#10;</xsl:text>
    </xsl:for-each>
  </xsl:template>

<xsl:template match="publisher">
  <xsl:text>@publisher={</xsl:text>
  <xsl:value-of select="."/>
  <xsl:text>}</xsl:text>
</xsl:template>

<xsl:template match="note">
  <xsl:text>@note={</xsl:text>
  <xsl:value-of select="."/>
  <xsl:text>}</xsl:text>
</xsl:template>

<xsl:template match="idno[@type='url']">
  <xsl:text>@url={</xsl:text>
  <xsl:value-of select="."/>
  <xsl:text>}</xsl:text>
</xsl:template>

<xsl:template match="pubPlace">
  <xsl:text>@address={</xsl:text>
  <xsl:value-of select="."/>
  <xsl:text>}</xsl:text>
</xsl:template>

<xsl:template match="date">
  <xsl:text>@year={</xsl:text>
  <xsl:value-of select="."/>
  <xsl:text>}</xsl:text>
</xsl:template>

<xsl:template match="series/title">
  <xsl:apply-templates/>
</xsl:template>

<xsl:template match="title">
  <xsl:variable name="name">
    <xsl:choose>
      <xsl:when test="@level='a' or not(ancestor::biblStruct/analytic)">title</xsl:when>
      <xsl:when test="@level='j' or parent::monogr/imprint/biblScope[@type='volume']">journal</xsl:when>
      <xsl:when test="@level='m' or parent::monogr">booktitle</xsl:when>
      <xsl:when test="@level='s'">series</xsl:when>
      <xsl:otherwise>title</xsl:otherwise>
    </xsl:choose>
  </xsl:variable>
  <xsl:text>@</xsl:text>
  <xsl:value-of select="$name"/>
  <xsl:text>={{</xsl:text>
  <xsl:apply-templates/>
  <xsl:text>}}</xsl:text>
</xsl:template>

<xsl:template match="q">
  <xsl:text>``</xsl:text>
  <xsl:apply-templates/>
  <xsl:text>''</xsl:text>
</xsl:template>

<xsl:template match="series">
  <xsl:text>@type={</xsl:text>
  <xsl:apply-templates/>
  <xsl:text>}</xsl:text>
</xsl:template>

<xsl:template match="biblScope[@type='pages']">
  <xsl:text>@pages={</xsl:text>
  <xsl:apply-templates/>
  <xsl:text>}</xsl:text>
</xsl:template>

<xsl:template match="biblScope[@type='volume']">
  <xsl:text>@volume={</xsl:text>
  <xsl:apply-templates/>
  <xsl:text>}</xsl:text>
</xsl:template>

<xsl:template match="biblScope[@type='issue']">
  <xsl:text>@issue={</xsl:text>
  <xsl:apply-templates/>
  <xsl:text>}</xsl:text>
</xsl:template>

<xsl:template match="edition">
  <xsl:text>@edition={</xsl:text>
  <xsl:apply-templates/>
  <xsl:text>}</xsl:text>
</xsl:template>

<xsl:template match="editor">
  <xsl:if test="not(preceding-sibling::editor)">
    <xsl:text>@editor={</xsl:text>
    <xsl:for-each select="../editor">
      <xsl:choose>
	<xsl:when test="forename and surname">
	  <xsl:value-of select="forename"/>
	  <xsl:text> </xsl:text>
	  <xsl:value-of select="surname"/>
	</xsl:when>
	<xsl:otherwise>
	  <xsl:apply-templates/>
	</xsl:otherwise>
      </xsl:choose>
      <xsl:if test="following-sibling::editor"> and </xsl:if>
    </xsl:for-each>
    <xsl:text>}</xsl:text>
  </xsl:if>
</xsl:template>

<xsl:template match="author">
  <xsl:if test="not(preceding-sibling::author)">
    <xsl:text>@author={</xsl:text>
    <xsl:for-each select="../author">
      <xsl:choose>
	<xsl:when test="forename and surname">
	  <xsl:value-of select="forename"/>
	  <xsl:text> </xsl:text>
	  <xsl:value-of select="surname"/>
	</xsl:when>
	<xsl:otherwise>
	  <xsl:apply-templates/>
	</xsl:otherwise>
      </xsl:choose>
      <xsl:if test="following-sibling::author"> and </xsl:if>
    </xsl:for-each>
    <xsl:text>}</xsl:text>
  </xsl:if>
</xsl:template>

</xsl:stylesheet>
