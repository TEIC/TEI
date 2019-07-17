<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                xpath-default-namespace="http://www.tei-c.org/ns/1.0"
                version="2.0">
  <xsl:output method="text"/>


  <xsl:template name="biblStruct2bibtex">
    <xsl:variable name="type">
      <xsl:choose>
	<xsl:when test="idno[@type='url']">
	    <xsl:text>techreport</xsl:text>
	  </xsl:when>
	<xsl:when test="series">
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
	<xsl:apply-templates mode="tobib"/>
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
  </xsl:template>

<xsl:template mode="tobib" match="publisher">
  <xsl:choose>
    <xsl:when test="ancestor::biblStruct/series or  ancestor::biblStruct/idno[@type='url']">
      <xsl:text>@institution={</xsl:text>
      <xsl:value-of select="."/>
      <xsl:text>}</xsl:text>
    </xsl:when>
    <xsl:otherwise>
      <xsl:text>@publisher={</xsl:text>
      <xsl:value-of select="."/>
      <xsl:text>}</xsl:text>
    </xsl:otherwise>
  </xsl:choose>
</xsl:template>

<xsl:template mode="tobib" match="note">
  <xsl:text>@note={</xsl:text>
  <xsl:value-of select="."/>
  <xsl:text>}</xsl:text>
</xsl:template>

<xsl:template mode="tobib" match="idno[@type='url']">
  <xsl:text>@url={</xsl:text>
  <xsl:value-of select="."/>
  <xsl:text>}</xsl:text>
</xsl:template>

<xsl:template mode="tobib" match="idno[@type='isbn']">
  <xsl:text>@isbn={</xsl:text>
  <xsl:value-of select="."/>
  <xsl:text>}</xsl:text>
</xsl:template>

<xsl:template mode="tobib" match="pubPlace">
  <xsl:text>@address={</xsl:text>
  <xsl:value-of select="."/>
  <xsl:text>}</xsl:text>
</xsl:template>

<xsl:template mode="tobib" match="date">
  <xsl:text>@year={</xsl:text>
  <xsl:value-of select="."/>
  <xsl:text>}</xsl:text>
</xsl:template>

<xsl:template mode="tobib" match="title">
  <xsl:variable name="name">
    <xsl:choose>
      <xsl:when test="@level='a'">title</xsl:when>
      <xsl:when test="parent::monogr and not(ancestor::biblStruct/analytic)">title</xsl:when>
      <xsl:when test="@level='j' or parent::monogr/imprint/biblScope[@type='volume']">journal</xsl:when>
      <xsl:when test="@level='m' or parent::monogr">booktitle</xsl:when>
      <xsl:when test="@level='s'">series</xsl:when>
      <xsl:when test="parent::series">type</xsl:when>
      <xsl:otherwise>title</xsl:otherwise>
    </xsl:choose>
  </xsl:variable>
  <xsl:text>@</xsl:text>
  <xsl:value-of select="$name"/>
  <xsl:text>={{</xsl:text>
  <xsl:apply-templates mode="tobib"/>
  <xsl:text>}}</xsl:text>
</xsl:template>

<xsl:template mode="tobib" match="q">
  <xsl:text>``</xsl:text>
  <xsl:apply-templates mode="tobib"/>
  <xsl:text>''</xsl:text>
</xsl:template>

<xsl:template mode="tobib" match="series">
  <xsl:apply-templates mode="tobib"/>
</xsl:template>

<xsl:template mode="tobib" match="biblScope[@type='pages']">
  <xsl:text>@pages={</xsl:text>
  <xsl:apply-templates mode="tobib"/>
  <xsl:text>}</xsl:text>
</xsl:template>

<xsl:template mode="tobib" match="biblScope[@type='number']">
  <xsl:text>@number={</xsl:text>
  <xsl:apply-templates mode="tobib"/>
  <xsl:text>}</xsl:text>
</xsl:template>

<xsl:template mode="tobib" match="biblScope[@type='volume']">
  <xsl:text>@volume={</xsl:text>
  <xsl:apply-templates mode="tobib"/>
  <xsl:text>}</xsl:text>
</xsl:template>

<xsl:template mode="tobib" match="biblScope[@type='issue']">
  <xsl:text>@issue={</xsl:text>
  <xsl:apply-templates mode="tobib"/>
  <xsl:text>}</xsl:text>
</xsl:template>

<xsl:template mode="tobib" match="edition">
  <xsl:text>@edition={</xsl:text>
  <xsl:apply-templates mode="tobib"/>
  <xsl:text>}</xsl:text>
</xsl:template>

<xsl:template mode="tobib" match="editor">
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
	  <xsl:apply-templates mode="tobib"/>
	</xsl:otherwise>
      </xsl:choose>
      <xsl:if test="following-sibling::editor"> and </xsl:if>
    </xsl:for-each>
    <xsl:text>}</xsl:text>
  </xsl:if>
</xsl:template>

<xsl:template mode="tobib" match="author">
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
	  <xsl:text>{</xsl:text>
	  <xsl:apply-templates mode="tobib"/>
	  <xsl:text>}</xsl:text>
	</xsl:otherwise>
      </xsl:choose>
      <xsl:if test="following-sibling::author"> and </xsl:if>
    </xsl:for-each>
    <xsl:text>}</xsl:text>
  </xsl:if>
</xsl:template>

</xsl:stylesheet>
