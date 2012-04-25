<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
  xmlns:xs="http://www.w3.org/2001/XMLSchema"
  xmlns:xd="http://www.oxygenxml.com/ns/doc/xsl"
  exclude-result-prefixes="xs xd"
  version="2.0"
  xmlns="http://www.w3.org/1999/xhtml"
  xpath-default-namespace="http://www.w3.org/1999/xhtml">
  <xd:doc scope="stylesheet">
    <xd:desc>
      <xd:p><xd:b>Created on:</xd:b> Apr 24, 2012</xd:p>
      <xd:p><xd:b>Author:</xd:b> mholmes</xd:p>
      <xd:p>This stylesheet is designed to post-process the TEI Guidelines HTML version in such a way that 
              all id attributes on elements are provided with a tei_ prefix, and all href attributes linking to 
              them are similarly tweaked to keep all the links working. This is worth doing because we have
              encountered difficulties with AdBlock Plus filters which have hidden elements arbitrarily on 
              the main Guidelines site based on their ids; we believe that the tei_ prefix will make this less 
              likely, and if it does happen, we will find it easier to convince the filter list maintainers to whitelist
              tei_ prefixed ids.
      </xd:p>
    </xd:desc>
  </xd:doc>
  
<!--  <xsl:output method="xhtml" doctype-public="-//W3C//DTD XHTML 1.1//EN" doctype-system="http://www.w3.org/TR/xhtml11/DTD/xhtml11.dtd"/>-->
  <xsl:output method="html" />
  
<!-- List of static ids referenced in CSS files, which we're leaving unchanged for now. -->
  <xsl:variable name="staticIds" select="('banner', 'onecol', 'udm', 'container', 'accessibility', 'hdr2', 'hdr3', 'azindex', 'byMod')"/>
  
<!-- Template for matching id attributes. -->
  <xsl:template match="@id">
    <xsl:choose>
     <xsl:when test=". = $staticIds">
       <xsl:copy-of select="."/>
     </xsl:when>
      <xsl:otherwise>
        <xsl:attribute name="id" select="concat('tei_', .)"/>
      </xsl:otherwise>
    </xsl:choose>
    
  </xsl:template>
  
<!-- Template for matching local links. -->
  <xsl:template match="@href[matches(., '.*#.+') and not(contains(., '://'))]">
    <xsl:choose>
      <xsl:when test=". = $staticIds">
        <xsl:copy-of select="."/>
      </xsl:when>
      <xsl:otherwise>
        <xsl:attribute name="href" select="replace(., '#', '#tei_')"/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  
<!-- Identity transform. -->
  <xsl:template match="@*|node()">
    <xsl:copy>
      <xsl:apply-templates select="@*|node()"/>
    </xsl:copy>
  </xsl:template>
  
</xsl:stylesheet>