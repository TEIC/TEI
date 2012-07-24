<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet version="2.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
  xmlns:atom="http://www.w3.org/2005/Atom"
  xmlns:html="http://www.w3.org/1999/xhtml" xmlns:d="data:,dpc"
  xmlns="http://www.w3.org/1999/xhtml" exclude-result-prefixes="#all">

  <xsl:import href="htmlparse.xsl"/>
  <xsl:output method="xml" indent="yes" encoding="UTF8"/>

  <!-- 

This takes the TEI SF wordpress atom feed and makes an html content <div> which contains 
a table of contents and individual atom:entry fields from the table of contents .
Written 2010-05-29 by James Cummings
-->

  <!--copyall-->

  <xsl:template match="@*|node()|comment() " priority="-1">
    <xsl:copy>
      <xsl:apply-templates select="@*|node()|comment()"/>
    </xsl:copy>
  </xsl:template>


  <xsl:template match="/">
    <div id="content" xmlns="http://www.w3.org/1999/xhtml">
      <a id="contentstart"/>
      <div class="empty">
        <a id="top"/>
      </div>
      <!--<div>
        <h1 class="maintitle">TEI News</h1>
        <hr/>
      </div>-->

      <xsl:comment>ToC section</xsl:comment>
      <div>
	      <p style="font-size:90%; margin-top:0">To submit a news item, email <a href="mailto:news@tei-c.org">news@tei-c.org</a></p>
        <h2>Contents</h2>
        <ul class="toc  toc_body">
          <xsl:apply-templates select="//atom:entry[position() &lt; 25]" mode="toc"/>

        	
        </ul>
      
      	
      	<p class="bold">Full news archive at: <a href="http://textencodinginitiative.wordpress.com">TEI-C Wordpress Blog</a></p>
      </div>
      <div class="separator">
        <hr/>
      </div>
      <xsl:comment>articles section</xsl:comment>

      <xsl:apply-templates select="//atom:entry[position() &lt; 25]"/>

      	<p class="bold">Full news archive at: <a href="http://textencodinginitiative.wordpress.com">TEI-C Wordpress Blog</a></p>
    </div>

  </xsl:template>
  <xsl:template match="atom:entry" mode="toc">
    <!-- The articleID variable takes the atom:link/@href unique identifier URI, removes the last
      character (a '/') and then takes the substring after the sourceforge url (leaving it as
      'tei/yyyy/mm/dd/title-here' and then translates the '-' of the title to '_' and then the '/'
      of the path to '-'. This is required since '/' is not a valid NCName for IDs.  -->
    <xsl:variable name="articleID">
      <xsl:value-of
        select="translate(translate(substring-after(substring(atom:link[@rel='alternate'][1]/@href, 0,
           string-length(atom:link[@rel='alternate'][1]/@href)),
      'https://textencodinginitiative.wordpress.com/'), '-', '_'), '/', '-')"
      />
    </xsl:variable>
    <li class="toc">
      <a class="toc toc_0" href="{concat('#', $articleID)}" title="{d:htmlparse(atom:title)}">
        <xsl:value-of select="d:htmlparse(atom:title)"/>
      </a>
    </li>

  </xsl:template>

  <xsl:template match="atom:entry">
    <xsl:variable name="articleID">
      <xsl:value-of
        select="translate(translate(substring-after(substring(atom:link[@rel='alternate'][1]/@href, 0,
        string-length(atom:link[@rel='alternate'][1]/@href)),
        'https://textencodinginitiative.wordpress.com/'), '-', '_'), '/', '-')"
      />
    </xsl:variable>

    <xsl:comment>An atom:entry </xsl:comment>
    <div class="teidiv0" id="{$articleID}">
      <h2>
        <a name="{$articleID}">
          <xsl:comment> This is a non-empty html:a element</xsl:comment>
        </a>
        <xsl:value-of select="d:htmlparse(atom:title)"/>
      </h2>
      <!-- atom:content piped through David Carlisle's htmlparse.xsl script -->
      <xsl:variable name="atomContent" as="element()" ><div class="articleContent"><xsl:copy-of
          select="d:htmlparse(atom:content)"/></div></xsl:variable>
        <xsl:apply-templates select="$atomContent"/>
            <div style="display:block" class="dateline">
        <xsl:if test="atom:author/atom:name">Posted by: <xsl:value-of select="atom:author/atom:name"
          /><xsl:text>  </xsl:text> </xsl:if>
        <xsl:value-of select="atom:published"/>
        <xsl:if test="not(atom:published/text() = atom:updated/text())">
          <xsl:text>  </xsl:text>(updated: <xsl:value-of select="atom:updated"
          />)</xsl:if>
      </div>
    </div>
    <hr style="color:gray; height:3px;"/>
  </xsl:template>


<xsl:template match="html:script"/>

<!-- JC/2012-07-24: Just commenting out at time of moving to wordpress.com... can't rely on sourceforge having these any more -->
<!--<xsl:template match="html:img">
<xsl:copy>
<xsl:choose>
<xsl:when test="not(starts-with(@src, 'http'))"><xsl:attribute name="src" select="concat('https://sourceforge.net', @src)"/>
<xsl:attribute name="atom:foo" select="'blort'"/>
</xsl:when>
<xsl:otherwise><xsl:apply-templates select="@src"/></xsl:otherwise>
</xsl:choose>
<xsl:apply-templates select="@*[not(name()='src')]"/>
</xsl:copy>
</xsl:template>
-->


</xsl:stylesheet>
