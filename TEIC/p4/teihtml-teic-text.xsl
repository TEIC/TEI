<!-- 
     Text Encoding Initiative Consortium XSLT stylesheet family version 3.0
     RCS: $Id: teihtml.xsl,v 1.9 2000/05/08 16:21:13 rahtz Exp rahtz $
     
     XSL stylesheet to format TEI XML documents to HTML or XSL FO
     
     Copyright 1999-2003 Text Encoding Initiative Consortium  
     #include LICENSE
--> 
<xsl:stylesheet
    version="1.0"
    xmlns:tei="http://www.tei-c.org/P5/"    
    xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#"
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform" 
    xmlns:html="http://www.w3.org/1999/xhtml"
    xmlns:cc="http://web.resource.org/cc/"
    exclude-result-prefixes="tei html rdf cc" >
  
  <xsl:import href="teihtml-teic.xsl"/>
  <xsl:param name="pageLayout">Simple</xsl:param>
 
</xsl:stylesheet>


