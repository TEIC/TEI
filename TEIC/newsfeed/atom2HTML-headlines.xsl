<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet version="2.0"
xmlns="http://www.w3.org/1999/xhtml"
  xmlns:tei="http://www.tei-c.org/ns/1.0"
  xmlns:atom="http://www.w3.org/2005/Atom" 
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
  xmlns:html="http://www.w3.org/1999/xhtml" xmlns:d="data:,dpc"
  exclude-result-prefixes="#all">

  <xsl:import href="htmlparse.xsl"/>
<xsl:output indent="yes"/>

<!--

This xslt stylesheet produces the iframe content for the TEI-C main page.
-James Cummings 2010-08-23
-->

  <xsl:param name="newsNum">7</xsl:param>
  <xsl:param name="otherNum">6</xsl:param>

<xsl:template match="/atom:feed">
<html>
  <head>
    <title>TEI News</title>
    <style type="text/css">
           html { margin:0; }
           body {margin-top:0; padding-top: 0;}
           div.news { font-size: 11px; font-family: Verdana, Tahoma, Geneva, Arial, Helvetica, sans-serif !important;}
           h2 { color:rgb(0, 72, 125); margin-bottom:6px; font-size:140%;}
	   ul { margin:0; padding:0; list-style-type:none; }
	   li {margin:0 0 2px 0;}
	   li.sticky {font-weight:bold;}
           span.newsDate{font-size:smaller;font-style:italic; font-weight:normal;}
	   a { text-decoration:none; }
        </style>
<meta http-equiv="Pragma" content="no-cache"/> 
  </head>
  <body>
    <div class="newsfeed">
      <xsl:if test=".//atom:entry[contains(atom:category[@term][1]/@term, 'News')]">
      <div class="news" id="TEINews">
      <h2>TEI-C News</h2>
      <ul>
      	<xsl:for-each select=".//atom:entry[atom:category/@term='News'][atom:category/@term='Sticky']  ">
      	<xsl:sort order="descending" select="atom:published"/>
      <xsl:comment>Sticky!</xsl:comment>
      <xsl:call-template name="makeHeadline"><xsl:with-param name="Sticky">Sticky</xsl:with-param></xsl:call-template>
      </xsl:for-each>
      	<xsl:variable name="numSticky" select="count(.//atom:entry[atom:category/@term='News'][atom:category/@term='Sticky'])"/>
      	<xsl:for-each select=".//atom:entry[atom:category/@term='News'][not(atom:category/@term='Sticky')][position() &lt; ($newsNum - $numSticky)]  ">
      		<xsl:sort order="descending" select="atom:published"/>
      		<xsl:call-template name="makeHeadline"/>
      	</xsl:for-each>
            </ul>
      </div>
      </xsl:if>
      
      <xsl:if test=".//atom:entry[atom:category/@term='Other']">
      <div class="news" id="OtherNews">
      <h2>Other News</h2>
      <ul>
      <xsl:for-each select=".//atom:entry[atom:category/@term='Other'][position() &lt; $otherNum]">
      	<xsl:sort order="descending" select="atom:published"/>
      	<xsl:call-template name="makeHeadline"/>
      	</xsl:for-each>
      </ul>
      <hr style="color: #225588;" size="4px" noshade="noshade"/>
      <p style="font-style:italic"><a href="http://www.tei-c.org/News/" target="_top">Older items...</a></p>
      </div>
      </xsl:if>
    </div>
  </body>
</html>
</xsl:template>

<xsl:template name="makeHeadline">
	<xsl:param name="Sticky">NotSticky</xsl:param>
	<xsl:variable name="articleID">
		<xsl:value-of
			select="translate(translate(substring-after(substring(atom:link[@rel='alternate'][1]/@href, 0,
			string-length(atom:link[@rel='alternate'][1]/@href)),
			'https://textencodinginitiative.wordpress.com/'), '-', '_'), '/', '-')"
		/>
	</xsl:variable>	
	<li><xsl:if test="$Sticky='Sticky'"><xsl:attribute name="class">sticky</xsl:attribute></xsl:if><a href="{concat('/News/#', $articleID)}" target="_parent"><xsl:value-of select="d:htmlparse(atom:title)[1]"/></a>
		<br/>
		<span class="newsDate">Posted on: 
			<xsl:value-of select="substring-before(atom:published, 'T')"/> 
		</span>
	</li>
</xsl:template>
</xsl:stylesheet>

