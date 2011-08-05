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

    <xsl:param name="filePerPage">false</xsl:param>
   <xsl:output method="xhtml" omit-xml-declaration="yes"/>
    
   <xsl:template name="stdheader">
     <xsl:param name="title"/>
   </xsl:template>

   <xsl:template name="stdfooter"/>

   <xsl:template match="tei:hi|tei:emph">
       <xsl:apply-templates/>
   </xsl:template>

   <xsl:template match="tei:sp">
     <xsl:apply-templates select="tei:*[not(self::tei:speaker)]"/>
   </xsl:template>


</xsl:stylesheet>
