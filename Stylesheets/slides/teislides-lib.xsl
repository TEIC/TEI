<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0"
                xmlns:xt="http://www.jclark.com/xt"
                extension-element-prefixes="xt">

<xsl:output method="html" doctype-public="-//W3C//DTD HTML 4.0 Transitional//EN"/>

<xsl:template match="tei:/">
 <html> 
 <head>
 <title><xsl:apply-templates select="//docTitle//text()"/></title>
 <link rel="stylesheet" type="text/css" href="teislides.css"/>
 </head>
  <frameset cols="20%,80%">
   <noframes>
  <h1><xsl:value-of select="/TEI.2//body//docTitle"/></h1>
 <xsl:for-each select="//body//div1">
 <xsl:variable name="divname" select="concat($MasterFile,concat(generate-id()),'.html')"/>
  <a href="{$divname}" target="slides">
	<xsl:call-template name="header"/></a><br/>
 </xsl:for-each>
   </noframes>
   <frame frameborder="0" src="{concat($MasterFile,'toc.html')}" name="toc"/>
   <frame frameborder="0" src="{concat($MasterFile,generate-id(//div1[1]))}.html" name="slides"/>
  </frameset>
 </html>
 <xsl:apply-templates select="TEI.2//body//div1"/>
 <xt:document method="html" doctype-public="-//W3C//DTD HTML 4.0 Transitional//EN" href="{concat($MasterFile,'toc.html')}">
 <html> 
 <head>
 <title><xsl:apply-templates select="//docTitle//text()"/></title>
 <link rel="stylesheet" type="text/css" href="teitoc.css"/>
 </head>
 <body>
  <h1><xsl:value-of select="/TEI.2//body//docTitle"/></h1>
 <xsl:for-each select="//body//div1">
 <xsl:variable name="divname" select="concat($MasterFile,concat(generate-id()),'.html')"/>
  <a href="{$divname}" target="slides">
	<xsl:call-template name="header"/></a><br/>
 </xsl:for-each>
 <xsl:call-template name="stdfooter">
    <xsl:with-param name="date" 
	select="/TEI.2/teiHeader//revisionDesc//date[1]"/>
 </xsl:call-template>
 </body>
 </html>
</xt:document>
</xsl:template>

<xsl:template name="stdfooter">
  <xsl:param name="date"/>
 <hr/><address>
 Last updated: <xsl:value-of select="$date"/>
 <br/>
 </address>
</xsl:template>

<!-- xref to previous and last articles -->
<xsl:template name="xrefpanel">
  <xsl:param name="home"/>
    <table class="slide" width="95%">
      <tr>
        <td>
<xsl:if test="preceding-sibling::div1">
  <a href="{concat($MasterFile,generate-id(preceding-sibling::div1[1]))}.html"><img src="left.gif" border="0" alt="PREVIOUS"/></a>
</xsl:if>
        </td>
        <td>
  <a href="{concat($MasterFile,generate-id(//div1[1]))}.html"><img src="up.gif" border="0" alt="FIRST"/></a>
  <a href="{concat($MasterFile,generate-id(//div1[last()]))}.html"><img src="down.gif" border="0"  alt="LAST"/></a>
        </td>
        <td align="right">
<xsl:if test="following-sibling::div1">
  <a href="{concat($MasterFile,generate-id(following-sibling::div1[1]))}.html"><img src="right.gif" border="0"  alt="NEXT"/></a>
</xsl:if>
        </td>
     </tr>
    </table>
</xsl:template>

<xsl:template match="tei:xref">
 <a>
<xsl:attribute name="href"><xsl:value-of select="@url"/></xsl:attribute>
 <tt><xsl:apply-templates/></tt>
 </a>
</xsl:template>


<xsl:template match='tei:figure'>
 <img src="{concat(@file,'.jpg')}"/>
</xsl:template>

<xsl:template match="tei:div1">
  <xt:document method="html" doctype-public="-//W3C//DTD HTML 4.0 Transitional//EN" href="{concat($MasterFile,concat(generate-id()),'.html')}">
	 <html> 
	 <head>
	 <title><xsl:apply-templates select="head//text()"/></title>
         <link rel="stylesheet" type="text/css" href="teislides.css"/>
	 </head>
	 <body>
         <div  class="slide">
         <xsl:variable name="n" select="concat($MasterFile,generate-id())"/>
	 <a name="{$n}"></a><h1><xsl:call-template name="header"/></h1>
         <hr/>
         <table cellspacing="0" border="0">
         <colgroup><col width="1%"/><col width="99%"/></colgroup>
         <tr valign="top">
            <td><img src="down.gif" width="1" height = "400"/></td>
            <td><xsl:apply-templates/></td>
         </tr>
         </table>
         <hr/>
         <xsl:call-template name="xrefpanel">
         <xsl:with-param name="home" select="concat($MasterFile,'.html')"/>
	</xsl:call-template>
        </div>
	</body>
	</html>
  </xt:document>
</xsl:template>

</xsl:stylesheet>
