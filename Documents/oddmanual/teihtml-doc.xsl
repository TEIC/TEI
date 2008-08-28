<xsl:stylesheet
    version="1.0"
    xmlns:tei="http://www.tei-c.org/ns/1.0"
    xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#"
    xmlns:rss="http://purl.org/rss/1.0/"
    xmlns:dc="http://purl.org/dc/elements/1.1/"
    xmlns:syn="http://purl.org/rss/1.0/modules/syndication/"
    xmlns:taxo="http://purl.org/rss/1.0/modules/taxonomy/"
    xmlns:html="http://www.w3.org/1999/xhtml"
    exclude-result-prefixes="rdf dc syn taxo rss rdf html tei" 
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform" 
    xmlns:cc="http://web.resource.org/cc/">

  <xsl:import href="http://localhost/stylesheet/base/p5/html/tei.xsl"/>
  <xsl:import href="http://localhost/stylesheet/teic/teidoc-html.xsl"/>


  <xsl:output 
      method="html"  
      doctype-public="-//W3C//DTD HTML 4.0 Transitional//EN" 
      doctype-system="http://www.w3.org/TR/html4/loose.dtd"
      indent="no"/>
  
</xsl:stylesheet>
