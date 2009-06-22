<xsl:stylesheet
 xmlns:tei="http://www.tei-c.org/ns/1.0"
 xmlns:xsl="http://www.w3.org/1999/XSL/Transform" 
 xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#"
 xmlns:rss="http://purl.org/rss/1.0/"
 xmlns:dc="http://purl.org/dc/elements/1.1/"
 xmlns:syn="http://purl.org/rss/1.0/modules/syndication/"
 xmlns:taxo="http://purl.org/rss/1.0/modules/taxonomy/"
 xmlns:svg="http://www.w3.org/2000/svg"
 xmlns:mathml="http://www.w3.org/1998/Math/MathML" 
 xmlns:html="http://www.w3.org/1999/xhtml"
 exclude-result-prefixes="rdf dc syn taxo rss rdf html" 
 xmlns="http://www.w3.org/1999/xhtml"
 version="1.0">


  <xsl:import
   href="/usr/share/xml/tei/stylesheet/oxford/p5/html/teihtml.xsl"/>

<xsl:output method="xml" encoding="UTF-8" indent="no" doctype-public="-//W3C//DTD XHTML 1.0 Transitional//EN" doctype-system="http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd"/>

<xsl:template match="tei:figure[svg:*]">
 <xsl:apply-templates/>
</xsl:template>

<xsl:template match="svg:*|mathml:*">
  <xsl:copy-of select="."/>
</xsl:template>


</xsl:stylesheet>
