<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet 
    version="2.0" 
    xmlns:dc="http://purl.org/dc/elements/1.1/" 
    xmlns:iso="http://www.iso.org/ns/1.0" 
    xmlns="http://www.w3.org/1999/xhtml" 
    xmlns:html="http://www.w3.org/1999/xhtml"
    xmlns:tei="http://www.tei-c.org/ns/1.0" 
    xmlns:teix="http://www.tei-c.org/ns/Examples" 
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform" 
    xmlns:ncx="http://www.daisy.org/z3986/2005/ncx/" 
    exclude-result-prefixes="iso tei teix dc html ncx"
    xpath-default-namespace="http://www.tei-c.org/ns/1.0">

  

  <xsl:import href="../epub3/tei-to-epub3.xsl"/>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" scope="stylesheet" type="stylesheet">
    <desc>
      <p>
	TEI stylesheet for making ibooks.
      </p>
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
      <p>Id: $Id: tei-to-epub3.xsl 9646 2011-11-05 23:39:08Z rahtz $</p>
      <p>Copyright: 2008, TEI Consortium</p>
    </desc>
  </doc>

  <xsl:param name="epubMimetype">application/x-ibooks+zip</xsl:param>
  <xsl:param name="doctype-system"/>
  <xsl:param name="opfPackageVersion">2.0</xsl:param>

  <xsl:output method="xml" omit-xml-declaration="no"  encoding="utf-8"
	      doctype-system="" indent="no"/>

  <xsl:template name="linkCSS">
    <xsl:param name="file"/>
    <xsl:param name="media"/>
    <xsl:choose>
      <xsl:when test="not($media='')">
	<xsl:processing-instruction name="xml-stylesheet"> href='<xsl:value-of
	select="$file"/>' type='text/css' media='<xsl:value-of select="$media"/>'</xsl:processing-instruction>
      </xsl:when>
      <xsl:otherwise>
	<xsl:processing-instruction name="xml-stylesheet">href='<xsl:value-of
	select="$file"/>' type='text/css' </xsl:processing-instruction>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>

  <xsl:template name="opfmetadata">
    <xsl:param name="author"/>
    <xsl:param name="printAuthor"/>
    <xsl:param name="coverImageOutside"/>
    <metadata xmlns:dc="http://purl.org/dc/elements/1.1/"
	      xmlns="http://www.idpf.org/2007/opf" 
	      xmlns:opf="http://www.idpf.org/2007/opf">
      <dc:title>
	<xsl:call-template name="generateSimpleTitle"/>
      </dc:title>
      <dc:creator>
	<xsl:value-of select="$printAuthor"/>
      </dc:creator>
      <dc:contributor opf:role="bkp">TEI stylesheets</dc:contributor>
      <dc:language>
	<xsl:call-template name="generateLanguage"/>
      </dc:language>
      <xsl:call-template name="generateSubject"/>
      <dc:identifier id="pub-id">
	<xsl:call-template name="generateID"/>
      </dc:identifier>
      <dc:description>
	<xsl:call-template name="generateSimpleTitle"/>
	<xsl:text> / </xsl:text>
	<xsl:value-of select="$author"/>
      </dc:description>
      <dc:publisher>
	<xsl:call-template name="generatePublisher"/>
      </dc:publisher>
      <xsl:for-each select="tei:teiHeader/tei:profileDesc/tei:creation/tei:date[@notAfter]">
	<dc:date id="creation">
	  <xsl:value-of select="@notAfter"/>
	</dc:date>
      </xsl:for-each>
      <xsl:for-each select="tei:teiHeader/tei:fileDesc/tei:sourceDesc//tei:date[@when][1]">
	<dc:date id="original-publication">
	  <xsl:value-of select="@when"/>
	</dc:date>
      </xsl:for-each>
      <dc:date id="epub-publication">
	<xsl:call-template name="generateDate"/>
      </dc:date>
      <xsl:if test="not($coverImageOutside='')">
	<meta name="cover" content="cover-image"/>
      </xsl:if> 
      <meta name="ibooks:requiredVersion" content="1"/>
      <meta name="ibooks:searchReferenceText" content="searchReferenceText"/>
      <meta name="ibooks:currentVersion" content="1"/>
      <meta name="ibooks:autoHyphenate" content="yes"/>
      <meta name="ibooks:searchIndex" content="searchIndex"/>
   </metadata>
  </xsl:template>
      
</xsl:stylesheet>