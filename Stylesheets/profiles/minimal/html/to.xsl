<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet 
    xmlns="http://www.w3.org/1999/xhtml"
    xmlns:html="http://www.w3.org/1999/xhtml"
    xmlns:tei="http://www.tei-c.org/ns/1.0"
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    exclude-result-prefixes="tei html"
    version="2.0">
    <!-- import base conversion style -->

    <xsl:import href="../../../xhtml2/tei.xsl"/>

    <xsl:param name="filePerPage">true</xsl:param>
   <xsl:output method="xhtml" omit-xml-declaration="yes"/>
    
   <xsl:template match="tei:speaker"/>

</xsl:stylesheet>
