<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet version="2.0"
xmlns="http://www.w3.org/1999/xhtml"
  xmlns:tei="http://www.tei-c.org/ns/1.0"
  xmlns:atom="http://www.w3.org/2005/Atom" 
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
  xmlns:html="http://www.w3.org/1999/xhtml" xmlns:d="data:,dpc"
  exclude-result-prefixes="#all">
  <!--
    
This xslt stylesheet produces a file to then check if newsfeed is updated.
-James Cummings 2013-01-01
-->
  
  <!-- We just want some text output -->
<xsl:output indent="yes"  method="text"/>

<!-- Incoming / Existing default to standard filenames but able to be set by wget-news.sh script -->
<xsl:param name="incoming" select="'incoming.xml'"/>
<xsl:param name="existing" select="'atom.xml'"/>

<!-- main template -->
<xsl:template name="main">
  <!-- grab variables of feed update -->
  <xsl:variable name="incomingUpdated" select="normalize-space(document($incoming)/atom:feed/atom:updated/text())"/>
  <xsl:variable name="existingUpdated" select="normalize-space(document($existing)/atom:feed/atom:updated/text())"/>
<xsl:choose>
  <!-- If they are the same then don't update -->
<xsl:when test="$incomingUpdated = $existingUpdated">NO</xsl:when>
  <!-- otherwise do update -->
<xsl:otherwise>YES</xsl:otherwise>
</xsl:choose>
</xsl:template>

</xsl:stylesheet>

