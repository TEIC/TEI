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

  <xsl:param name="newsNum">5</xsl:param>
  <xsl:param name="otherNum">5</xsl:param>

<xsl:template match="/atom:feed">
<html>
  <!-- iframe content -->
  <head>
    <title>TEI News</title>
    <style type="text/css">
           html { margin:0; }
           body {margin-top:0; padding-top: 0;}
           div.news { font-size: 11px; font-family: Verdana, Tahoma, Geneva, Arial, Helvetica, sans-serif !important;}
           h2 { color:rgb(0, 72, 125); margin-top: 0; }
           ul { padding-left: 1.5em; text-indent: -1em; list-style-type:none; }
           li:before { content: "※ ";}
           span.newsDate{text-align:right;display:block;font-size:8px;font-style:italic;}
	   li{margin-bottom:2px;}
           
        </style>
<meta http-equiv="Pragma" content="no-cache"/> 
  </head>
  <body>
    <div class="newsfeed">
      <xsl:if test=".//atom:entry[contains(atom:category[@term][1]/@term, 'News')]">
      <div class="news" id="TEINews">
      <h2>TEI-C News</h2>
      <ul>
      <xsl:for-each select=".//atom:entry[contains(atom:category[1]/@term, 'News')][position() &lt; $newsNum]
        [number(substring(atom:published, 1, 4)) ge 2010]">
        <xsl:variable name="articleID">
            <xsl:value-of
              select="translate(translate(substring-after(substring(atom:link[@rel='alternate'][1]/@href, 0,
              string-length(atom:link[@rel='alternate'][1]/@href)),
              'http://sourceforge.net/apps/wordpress/'), '-', '_'), '/', '-')"
             />
          </xsl:variable>
                <li> <a href="{concat('/News/#', $articleID)}" target="_parent"><xsl:value-of select="d:htmlparse(atom:title)[1]"/></a>
                <br/>
                <span class="newsDate">Posted on: 
                  <xsl:value-of select="substring-before(atom:published, 'T')"/> 
                </span>
              </li>
      </xsl:for-each>
      </ul>
      </div>
      </xsl:if>
      
      <xsl:if test=".//atom:entry[contains(atom:category[@term][1]/@term, 'Other')]">
      <div class="news" id="OtherNews">
      <h2>Other News</h2>
      <ul>
      <xsl:for-each select=".//atom:entry[contains(atom:category[@term][1]/@term, 'Other')][position() &lt; $otherNum]
        [number(substring(atom:published, 1, 4)) ge 2010]">
        <xsl:variable name="articleID">
            <xsl:value-of
              select="translate(translate(substring-after(substring(atom:link[@rel='alternate'][1]/@href, 0,
              string-length(atom:link[@rel='alternate'][1]/@href)),
              'http://sourceforge.net/apps/wordpress/'), '-', '_'), '/', '-')"
             />
          </xsl:variable>
            <li> <a href="{concat('/News/#', $articleID)}" target="_parent"><xsl:value-of select="d:htmlparse(atom:title)"/></a>
                <br/>
                <span class="newsDate">Posted on: 
                  <xsl:value-of select="substring-before(atom:published, 'T')"/> 
                </span>
              </li>
      </xsl:for-each>
      </ul>
      <hr style="color: #225588;" size="4px" noshade="noshade"/>
      <p style="font-style:italic"><a href="http://www.tei-c.org/News/" target="_top">Older items...</a></p>
      </div>
      </xsl:if>
<!-- loads in iframe on some clients -->
	<!--(<a href="http://www.tei-c.org/News/">more news</a>) -->

    </div>
  </body>
</html>
</xsl:template>

</xsl:stylesheet>

