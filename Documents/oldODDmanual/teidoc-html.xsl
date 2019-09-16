<xsl:stylesheet
    version="1.0"
    xmlns:xi="http://www.w3.org/2001/XInclude"
    xmlns:tei="http://www.tei-c.org/ns/1.0"
    xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#"
    xmlns:rss="http://purl.org/rss/1.0/"
    xmlns:dc="http://purl.org/dc/elements/1.1/"
    xmlns:syn="http://purl.org/rss/1.0/modules/syndication/"
    xmlns:taxo="http://purl.org/rss/1.0/modules/taxonomy/"
    xmlns:html="http://www.w3.org/1999/xhtml"
    exclude-result-prefixes="rdf dc syn taxo rss rdf html tei xi cc" 
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform" 
    xmlns:cc="http://web.resource.org/cc/">

  <xsl:import href="/usr/share/xml/tei/stylesheet/teic/teihtml-teic.xsl"/>
  
  <xsl:output 
      method="html"  
      doctype-public="-//W3C//DTD HTML 4.0 Transitional//EN" 
      doctype-system="http://www.w3.org/TR/html4/loose.dtd"
      indent="no"/>
  
  <xsl:param name="cssFile">teidoc.css</xsl:param>

<xsl:template name="logoPicture">
<img src="TEI-glow.png" alt="" width="100" height="89" />
</xsl:template>

<xsl:template match="tei:oList">
<ol>
  <xsl:apply-templates/>
</ol>
</xsl:template>

<xsl:template match="tei:uList">
<ul>
  <xsl:apply-templates/>
</ul>
</xsl:template>

<xsl:template match="tei:glossList/tei:item">
<dd><xsl:apply-templates/></dd>
</xsl:template>

<xsl:template match="tei:glossList/tei:label">
<dt><xsl:apply-templates/></dt>
</xsl:template>

<xsl:template match="tei:glossList">
<dl>
  <xsl:apply-templates/>
</dl>
</xsl:template>

<xsl:template match="tei:input[@url]">
<pre class="input">
<!--<xi:include href="{@url}"/>-->
#include <xsl:value-of select="@url"/><xsl:text>&#10;</xsl:text>
</pre>
</xsl:template>

<xsl:template match="tei:output[@url]">
<pre class="output">
#include <xsl:value-of select="@url"/><xsl:text>&#10;</xsl:text>
</pre>
</xsl:template>

<xsl:template match="tei:input">
<pre class="input">
  <xsl:apply-templates/>
</pre>
</xsl:template>

<xsl:template match="tei:output">
<pre class="output">
  <xsl:apply-templates/>
</pre>
</xsl:template>

<xsl:template match="tei:gi">
<code>&lt;<xsl:apply-templates/>&gt;</code>
</xsl:template>

<xsl:template match="tei:ident">
  <span class="ident">
    <xsl:apply-templates/>
  </span>
 </xsl:template>

 <xsl:template match="tei:code">
   <span class="code">
<xsl:apply-templates/>
   </span>
 </xsl:template>

<xsl:template match="tei:kw">
<span class="kw">
  <xsl:apply-templates/>
</span>
</xsl:template>


<xsl:template match="tei:path">
  <span class="path">
    <xsl:apply-templates/>
  </span>
</xsl:template>

<xsl:template match="tei:command">
  <span class="command">
    <xsl:apply-templates/>
  </span>
</xsl:template>

<xsl:template match="tei:software">
  <span class="software">
    <xsl:apply-templates/>
  </span>
</xsl:template>

<xsl:template match="tei:url">
  <span class="url">
    <xsl:apply-templates/>
  </span>
</xsl:template>

<xsl:template match="tei:value">
  <span class="value">
    <xsl:apply-templates/>
  </span>
</xsl:template>

</xsl:stylesheet>
