<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet 
    xmlns="http://www.w3.org/1999/xhtml"
    xmlns:html="http://www.w3.org/1999/xhtml"
    xmlns:tei="http://www.tei-c.org/ns/1.0"
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    exclude-result-prefixes="#all"
    version="2.0">
    <!-- import base conversion style -->
  
  <xsl:import href="../../../html/html.xsl"/>
  
  <xsl:param name="feedbackURL">http://www.tei-c.org/About/contact.xml</xsl:param>
  <xsl:param name="institution"/>
  <xsl:param name="cssFile">en/html/guidelines.css</xsl:param>
  <xsl:template name="copyrightStatement">released under the
    Creative Commons Attribution 3.0 Unported License 
    http://creativecommons.org/licenses/by/3.0/</xsl:template>
  
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" scope="stylesheet" type="stylesheet">
    <desc>
      <p>This software is dual-licensed:        
1. Distributed under a Creative Commons Attribution-ShareAlike 3.0
Unported License http://creativecommons.org/licenses/by-sa/3.0/ 
2. http://www.opensource.org/licenses/BSD-2-Clause
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
of this software, even if advised of the possibility of such damage.</p>
      <p>$Id: to.xsl 9669 2011-11-07 19:17:54Z rahtz $</p>
      <p>Copyright: 2013, TEI Consortium</p>
    </desc>
  </doc>
  
  <xsl:output method="xhtml" omit-xml-declaration="yes"/>

  <!-- ********* START ********* -->
  <!--
    From here to END is a block of code that is essentially special-purpose
    code for building the readme-X.Y.Z.html files for TEI releases from the
    corresponding .xml input files.
    Input: .../ReleaseNotes/readme*xml
    Output: the Makefile puts the .html files of the same name except with an
      extension of '.html' intsead of '.xml' into 
      .../release/tei-p5-doc/share/doc/tei-p5-doc/
  -->
  <xsl:variable name="filename" select="tokenize( base-uri(/),'/')[last()]"/>
  <xsl:param name="version" select="replace( $filename, '^readme-([0-9aαbβ.-]+)\.xml$','$1')"/>
  <xsl:param name="vault" select="'http://www.tei-c.org/Vault/P5'"/>
  <xsl:param name="docPath" select="'doc/tei-p5-doc/en/html'"/>
  <xsl:variable name="testVersionedDocument" select="concat( $vault,'/',$version,'/VERSION')"/>
  <xsl:variable name="tagdocStart" select="concat( $vault,'/',$version,'/',$docPath,'/ref-')"/>
  
  <xsl:template match="tei:gi|tei:ident[@type eq 'class']">
    <xsl:variable name="class" select="if (@type) then @type else local-name(.)"/>
    <!-- If this is the first one, check veresion number and warn iff needed -->
    <xsl:if test="not( preceding::tei:gi | preceding::tei:ident[@type eq 'class'] )">
      <xsl:choose>
        <xsl:when test="$version eq ''">
          <xsl:message>WARNING: unable to parse version # from filename, so links will not work</xsl:message>
        </xsl:when>
        <xsl:when test="not( doc-available( $testVersionedDocument ) )">
          <xsl:message>WARNING: file <xsl:value-of select="$testVersionedDocument"/> cannot be read, so links will probably be broken</xsl:message>
        </xsl:when>
        <xsl:when test="normalize-space( unparsed-text( $testVersionedDocument ) ) ne normalize-space( $version )">
          <xsl:message>WARNING: supplied version (<xsl:value-of select="$version"/>) is not equal to content of <xsl:value-of select="$testVersionedDocument"/>.</xsl:message>
        </xsl:when>
      </xsl:choose>
    </xsl:if>
    <xsl:choose>
      <xsl:when test="@scheme = ('TEI','tei') or not(@scheme)">
        <xsl:variable name="content" select="normalize-space(.)"/>
        <xsl:variable name="tagdoc" select="concat( $tagdocStart, $content, '.html' )"/>
        <a class="{$class}" href="{$tagdoc}"><xsl:apply-templates/></a>
      </xsl:when>
      <xsl:otherwise>
        <!-- This is not a TEI scheme element or identifier -->
        <span class="{$class}"><xsl:apply-templates/></span>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  
  <!-- ********* END ********* -->
  
  <xsl:template match="html:*">
    <xsl:element name="{local-name()}">
      <xsl:copy-of select="@*"/>
      <xsl:apply-templates/>
    </xsl:element>
  </xsl:template>
  
  <xsl:template match="html:*/comment()">
    <xsl:copy-of select="."/>
  </xsl:template>
  
  <xsl:template match="tei:div[@type = ('frontispiece','illustration')]">
    <xsl:apply-templates/>
  </xsl:template>
  
</xsl:stylesheet>
