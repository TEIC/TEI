<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet 
    xmlns:xs="http://www.w3.org/2001/XMLSchema"
    xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" 
    xmlns:sch="http://purl.oclc.org/dsdl/schematron" 
    xmlns:s="http://www.ascc.net/xml/schematron" 
    xmlns:a="http://relaxng.org/ns/compatibility/annotations/1.0"
    xmlns:rng="http://relaxng.org/ns/structure/1.0"
    xmlns:tei="http://www.tei-c.org/ns/1.0" 
    xmlns:teix="http://www.tei-c.org/ns/Examples" 
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform" 
    version="2.0" 
    exclude-result-prefixes="teix a s tei xs rng sch xsi">
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" scope="stylesheet" type="stylesheet">
    <desc>
      <p> TEI stylesheet for simplifying TEI ODD markup </p>
      <p>This software is dual-licensed:

1. Distributed under a Creative Commons Attribution-ShareAlike 3.0
Unported License http://creativecommons.org/licenses/by-sa/3.0/ 

2. http://www.opensource.org/licenses/BSD-2-Clause
		
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are
met:

* Redistributions of source code must retain the above copyright
notice, this list of conditions and the following disclaimer.

* Redistributions in binary form must reproduce the above copyright
notice, this list of conditions and the following disclaimer in the
documentation and/or other materials provided with the distribution.

This software is provided by the copyright holders and contributors
"as is" and any express or implied warranties, including, but not
limited to, the implied warranties of merchantability and fitness for
a particular purpose are disclaimed. In no event shall the copyright
holder or contributors be liable for any direct, indirect, incidental,
special, exemplary, or consequential damages (including, but not
limited to, procurement of substitute goods or services; loss of use,
data, or profits; or business interruption) however caused and on any
theory of liability, whether in contract, strict liability, or tort
(including negligence or otherwise) arising in any way out of the use
of this software, even if advised of the possibility of such damage.
</p>
      <p>Author: See AUTHORS</p>
      <p>Id: $Id: odd2odd.xsl 11340 2013-01-05 15:24:07Z rahtz $</p>
      <p>Copyright: 2011, TEI Consortium</p>
    </desc>
  </doc>
  <xsl:param name="defaultSource"></xsl:param>
  <xsl:param name="defaultTEIVersion">current</xsl:param>
  <xsl:param name="defaultTEIServer">http://www.tei-c.org/Vault/P5/</xsl:param>
  <xsl:param name="currentDirectory"/>
  <xsl:param name="verbose">false</xsl:param>
  <xsl:param name="configDirectory"/>
  <xsl:variable name="DEFAULTSOURCE">
    <xsl:choose>
      <xsl:when test="$defaultSource != ''">
        <xsl:value-of select="$defaultSource"/>
      </xsl:when>
      <xsl:when test="$configDirectory != ''">
        <xsl:value-of select="$configDirectory"/>
        <xsl:text>odd/p5subset.xml</xsl:text>
      </xsl:when>
      <xsl:otherwise>
        <xsl:value-of select="$defaultTEIServer"/>
        <xsl:value-of select="$defaultTEIVersion"/>
	<xsl:text>/xml/tei/odd/p5subset.xml</xsl:text>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:variable>

  <xsl:function name="tei:workOutSource" as="xs:string*">
    <xsl:param name="e"/>
    <xsl:variable name="loc">
      <xsl:choose>
	<xsl:when test="$e/@source">
	  <xsl:value-of select="$e/@source"/>
	</xsl:when>
	<xsl:when test="$e/ancestor::tei:schemaSpec/@source">
	  <xsl:value-of select="$e/ancestor::tei:schemaSpec/@source"/>
	</xsl:when>
	<xsl:otherwise>
	  <xsl:value-of select="$DEFAULTSOURCE"/>
	</xsl:otherwise>
      </xsl:choose>
    </xsl:variable>
    <xsl:variable name="source">
      <xsl:choose>
	<xsl:when test="starts-with($loc,'/')">
	  <xsl:value-of select="$loc"/>
	</xsl:when>
	<xsl:when test="starts-with($loc,'file:')">
	  <xsl:value-of select="$loc"/>
	</xsl:when>
	<xsl:when test="starts-with($loc,'http:')">
	  <xsl:value-of select="$loc"/>
	</xsl:when>
	<xsl:when test="starts-with($loc,'https:')">
	  <xsl:value-of select="$loc"/>
	</xsl:when>
	<xsl:when test="starts-with($loc,'tei:')">
	  <xsl:value-of
	      select="replace($loc,'tei:',$defaultTEIServer)"/>
	  <xsl:text>/xml/tei/odd/p5subset.xml</xsl:text>
	</xsl:when>
	<xsl:otherwise>
	  <xsl:value-of select="$currentDirectory"/>
	  <xsl:value-of select="$loc"/>
	</xsl:otherwise>
      </xsl:choose>
    </xsl:variable>
    <xsl:choose>
      <xsl:when test="not(doc-available($source))">
	<xsl:call-template name="die">
	  <xsl:with-param name="message">
	    <xsl:text>Source </xsl:text>
	   <xsl:value-of select='$source'/>
	   <xsl:text> not readable</xsl:text>
	  </xsl:with-param>
	</xsl:call-template>
      </xsl:when>
      <xsl:otherwise>
	<xsl:if test="$verbose='true'">
	  <xsl:message>Setting source document to <xsl:value-of
	  select="$source"/></xsl:message>
	</xsl:if>
	<xsl:sequence select="$source"/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:function>

  <xsl:function name="tei:message" as="xs:string">
    <xsl:param name="message"/>
    <xsl:message><xsl:copy-of select="$message"/></xsl:message>
    <xsl:text/>
  </xsl:function>

  <xsl:function name="tei:uniqueName" as="xs:string">
    <xsl:param name="e"/>
    <xsl:for-each select="$e">
      <xsl:sequence select="concat(
	if (@ns='http://www.tei-c.org/ns/1.0') then ''
	else if (@ns) then @ns
	else if (ancestor::tei:schemaSpec/@ns) then
	ancestor::tei:schemaSpec/@ns else '',@ident)"/>
    </xsl:for-each>
  </xsl:function>

  <xsl:function name="tei:generate-nsprefix-schematron" as="xs:string">
    <xsl:param name="e"/>
    <xsl:for-each select="$e">
      <xsl:variable name="myns" select="ancestor::tei:elementSpec/@ns"/>
      <xsl:choose>
	<xsl:when test="not($myns) or $myns='http://www.tei-c.org/ns/1.0'">
	  <xsl:text>tei:</xsl:text>
	</xsl:when>
	<xsl:otherwise>
	  <xsl:choose>
	    <xsl:when test="sch:ns[@uri=$myns]">
	      <xsl:value-of
		  select="concat(sch:ns[@uri=$myns]/@prefix,':')"/>
	    </xsl:when>
	    <xsl:when test="parent::*/sch:ns[@uri=$myns]">
	      <xsl:value-of
		  select="concat(parent::*/sch:ns[@uri=$myns]/@prefix,':')"/>
	    </xsl:when>
	    <xsl:otherwise>
	      <xsl:message terminate="yes">schematron rule cannot work out prefix for <xsl:value-of select="../@ident"/></xsl:message>
	    </xsl:otherwise>
	  </xsl:choose>
	</xsl:otherwise>
      </xsl:choose>
    </xsl:for-each>
  </xsl:function>

  <xsl:template name="die">
    <xsl:param name="message"/>
    <xsl:message terminate="yes">
      <xsl:text>Error: odd2odd.xsl: </xsl:text> 
      <xsl:value-of select="$message"/>
    </xsl:message>
  </xsl:template>


</xsl:stylesheet>
